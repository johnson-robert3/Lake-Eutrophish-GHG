#~~~
# Scrip for calculating daily ecosystem metabolism (GPP, R, NEP)
# By: Robert Johnson
#~~~


library(LakeMetabolizer)


#---
# Data Prep
#---

# Calculate mixed-layer depth from Hobo t-chains

hobo_nest = hobo_temp %>%
   # remove dates when loggers were out of ponds
   mutate(doy = yday(date_time)) %>%
   filter(doy >= 143, doy <= 240) %>%
   # format for rLakeAnalyzer
   mutate(depth = str_replace(depth, "Anchor", "2.0"), 
          depth = paste("wtr", depth, sep="_")) %>%
   rename(datetime = date_time) %>%
   select(-doy) %>%
   # nest data in long format
   group_by(pond_id) %>%
   nest() %>%
   # widen data and run functions
   mutate(hobo_wide = map(data, ~pivot_wider(., names_from = "depth", values_from = "temp")),
          meta_depths = map(hobo_wide, ~ts.meta.depths(., na.rm=T)),
          thermocline = map(hobo_wide, ~ts.thermo.depth(., na.rm=T)),
          buoy_freq = map(hobo_wide, ~ts.buoyancy.freq(., na.rm=T)))


hobo_mld = hobo_nest %>%
   select(-data, -hobo_wide) %>%
   # can't have a variable duplicated across list-columns for unnesting
   mutate(thermocline = map(thermocline, ~select(., -datetime)),
          buoy_freq = map(buoy_freq, ~select(., -datetime))) %>%
   # unnest list-columns
   unnest(cols = c(meta_depths, thermocline, buoy_freq)) %>% 
   ungroup() %>%
   rename(meta_top = top,
          meta_bottom = bottom,
          thermocline = thermo.depth,
          buoy_freq = n2)


##__Combine data

# Temp and DO from miniDOTs
metab_data = minidot %>%
   # subtract 15 minutes from minidot data to match weather time-stamps
   mutate(date_time = date_time - minutes(15)) %>%
   # add weather
   left_join(weather_data) %>%
   # add surface salinity from sonde profiles
   left_join(sonde_surface %>%
                select(pond_id, doy, salinity)) %>%
   # remove unnecessary data
   filter(doy >= 145, doy <= 240) %>%
   select(-do_sat, -gust_speed)


# Add mixed-layer depth
metab_data = hobo_mld %>%
   # use thermocline for the mld
   select(pond_id, datetime, thermocline) %>%
   # add treatment ID; data for B and F will be used for all ponds in a treatment
   mutate(trt = case_when(.$pond_id=="B" ~ "pulse",
                          .$pond_id=="F" ~ "ref")) %>%
   select(-pond_id) %>%
   rename(date_time = datetime,
          z_mix = thermocline) %>%
   # set NaN's (times of turnover) to a depth of 1.75
   mutate(z_mix = replace(z_mix, .$z_mix=="NaN", 1.75)) %>%
   right_join(metab_data %>%
                 # treatment ID for combining
                 mutate(trt = case_when(.$pond_id %in% c("A", "B", "C") ~ "pulse",
                                        .$pond_id %in% c("D", "E", "F") ~ "ref"))) %>%
   relocate(pond_id) %>%
   relocate(z_mix, .after = salinity) %>%
   select(-trt) %>%
   arrange(pond_id, doy)


# Additional parameters for metabolism calcs
metab_data = metab_data %>%
   mutate(U10 = wind_speed * ((10 / wnd.z)^(1/7)),
          k_cole = k.cole.base(U10),
          k_gas = k600.2.kGAS.base(k600 = k_cole, 
                                   temperature = temp, 
                                   gas = "O2"),
          o2_eq_sat = o2.at.sat.base(temp = temp, 
                                     baro = 982.85, 
                                     salinity = salinity)) %>%
   # day/night for bookkeep
   mutate(daynight = is.day(date_time, lat = 42.11),
          daynight = as.character(daynight)) %>%
   mutate(daynight = case_when(.$daynight=="TRUE" ~ 1,
                               .$daynight=="FALSE" ~ 0)) %>%
   # n() measurements in each day
   add_count(pond_id, doy)  # doy 150 & 192 have n=47 for all ponds


#---
# Metabolism Calculations
#---


##__Bookkeeping method

metab_book = metab_data %>%
   # exclude days missing sonde profiles
   filter(!(pond_id=="B" & doy==151)) %>%
   filter(!(pond_id=="C" & doy==231)) %>%
   #
   filter(n == max(n)) %>%
   group_by(pond_id, doy) %>%
   group_modify(
      ~metab.bookkeep(do.obs = .$do,
                      do.sat = .$o2_eq_sat,
                      k.gas = .$k_gas,
                      z.mix = .$z_mix,
                      irr = .$daynight,
                      datetime = .$date_time)) %>%
   ungroup()



##__Maximum Likelihood Estimate method

  #--
  # metab.mle() returns a list (not a data frame)
  # need to use a nested workflow
  #--


# pond a
mle.a = metab_data %>%
   filter(pond_id=="A") %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("A", n())) %>%
   relocate(pond_id)

# pond b
mle.b = metab_data %>%
   filter(pond_id=="B") %>%
   filter(!(doy==151)) %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("B", n())) %>%
   relocate(pond_id)

# pond c
mle.c = metab_data %>%
   filter(pond_id=="C") %>%
   filter(!(doy==231)) %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("C", n())) %>%
   relocate(pond_id)

# pond d
mle.d = metab_data %>%
   filter(pond_id=="D") %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("D", n())) %>%
   relocate(pond_id)

# pond e
mle.e = metab_data %>%
   filter(pond_id=="E") %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("E", n())) %>%
   relocate(pond_id)

# pond f 
mle.f = metab_data %>%
   filter(pond_id=="F") %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("F", n())) %>%
   relocate(pond_id)


# all ponds
metab_mle = bind_rows(mle.a, mle.b, mle.c, mle.d, mle.e, mle.f)


   ## remove temporary objects
   rm(mle.a, mle.b, mle.c, mle.d, mle.e, mle.f)
   ##


