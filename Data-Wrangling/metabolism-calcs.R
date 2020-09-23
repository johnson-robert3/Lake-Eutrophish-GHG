#~~~
# Scrip for calculating daily ecosystem metabolism (GPP, R, NEP)
# By: Robert Johnson
#~~~


library(LakeMetabolizer)


## Data needed for metabolism calculations ##

# HAVE #
# DO (mg/L) time-series 
# Water temperature time-series 
# Wind speed time-series
# Anemometer height
# Barometric pressure at ponds
# Salinity time-series
# Irradiance (PAR) time-series (can use PAR data for metab.mle, or day/night for metab.bookkeep)
# Number of time-series measurements in each day

# STILL NEED #
# Mixed layer depth (z_mix)
   # need HOBO "t-chain" time-series data to calculate



#---
# Data Prep
#---


# Combine data

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
   # mixed-layer depth 
   # (update after getting HOBO t-chain data)
   mutate(z_mix = rep_len(0.5, n())) %>%
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


# Bookkeeping method

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



# Maximum Likelihood Estimate method

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


