#~~~
# Scrip for calculating daily ecosystem metabolism (GPP, R, NEP)
# By: Robert Johnson
#~~~


library(LakeMetabolizer)
library(slider)


#---
# Data Prep
#---

# Get and combine necessary data from various data sets


#-- Step 1: Address noisy DO time-series
minidot = minidot %>% 
   group_by(pond_id) %>% 
   arrange(date_time, .by_group=TRUE) %>%
   
   # identify large drops in DO (caused by water column turnover)
   # add variable for change in DO concentration from previous point
   mutate(delta_do = do - lag(do)) %>%
   
   # remove the offending point along with the next 2.5 hours (5 points) of data any time DO drops more than 2.0 mg/l
   mutate(drop_pt = case_when(delta_do <= -2.0 ~ 1,
                              TRUE ~ 0),
          # corrected DO concentration
          corr_do = case_when(drop_pt == 1 | 
                                 lag(drop_pt, n=1) == 1 | 
                                 lag(drop_pt, n=2) == 1 | 
                                 lag(drop_pt, n=3) == 1 | 
                                 lag(drop_pt, n=4) == 1 | 
                                 lag(drop_pt, n=5) == 1 ~ -9999,
                              TRUE ~ do),
          corr_do = na_if(corr_do, -9999),
          # linearly interpolate across removed points
          corr_do = zoo::na.approx(corr_do, na.rm=FALSE)) %>%
   
   # another method for dealing with noisy DO time-series data
   # 3-hour rolling window data
   mutate(
      # rolling window on original/raw data
      roll_do = slide_dbl(do, ~mean(.), .before=3, .after=2, .complete=F),
      # rolling window on new, corrected DO data (with large drops removed/interpolated)
      roll_corr_do = slide_dbl(corr_do, ~mean(.), .before=3, .after=2, .complete=F)) %>%
   ungroup()


#-- Step 2: Add weather and surface limno data
metab_data = minidot %>%
   # add weather
   left_join(weather_data) %>%
   # add surface salinity from sonde profiles
   left_join(sonde_surface %>%
                select(pond_id, doy, salinity)) %>%
   # remove unnecessary data from before experiment began and after experiment ended
   filter(doy >= 145, doy <= 240) %>%
   select(-do_sat, -gust_speed)


#-- Step 3: Add mixed-layer depth
metab_data = hobo_strat %>%
   # use thermocline for the mixed-layer depth 
   rename(z_mix = thermocline) %>%
   
   # Set Z_mix to 1.75m during times of turnover and mixing within the ponds  
   mutate(z_mix = case_when(
      # "NaN"s in the data (when rLakeAnalyzer functions can't calculate stratification variables) are also when water column is mixed
      z_mix == "NaN" ~ 1.75,
      # when the entire water column was considered Metalimnion
      meta_top==0 & meta_bottom==2 ~ 1.75,
      # rLakeAnalyzer functions seem to assign thermocline to 0.125m during times of mixing or weak, transient stratification 
      z_mix == 0.125 ~ 1.75,
      TRUE ~ z_mix)) %>%
   
   # add a treatment ID 
   #  data for B and F will be used for all ponds in a treatment; t-chains were only deployed in ponds B and F
   mutate(trt = case_when(.$pond_id=="B" ~ "pulse",
                          .$pond_id=="F" ~ "ref")) %>%
   select(-pond_id) %>%
   
   # add z_mix data back onto the metab dataset
   right_join(metab_data %>%
                 # treatment ID for combining
                 mutate(trt = case_when(.$pond_id %in% c("A", "B", "C") ~ "pulse",
                                        .$pond_id %in% c("D", "E", "F") ~ "ref"))) %>%
   relocate(pond_id) %>%
   relocate(z_mix, .after = salinity) %>%
   select(-trt) %>%
   arrange(pond_id, doy)


#-- Step 4: Additional parameters for metabolism calculations
metab_data = metab_data %>%
   mutate(U10 = wind_speed * ((10 / wind_z)^(1/7)),
          k_cole = k.cole.base(U10),
          k_gas = k600.2.kGAS.base(k600 = k_cole, 
                                   temperature = temp, 
                                   gas = "O2"),
          o2_eq_sat = o2.at.sat.base(temp = temp, 
                                     baro = 982.85, 
                                     salinity = salinity)) %>%
   # n() measurements in each day
   add_count(pond_id, doy)


#---
# Metabolism Calculations
#---


##__Bookkeeping method

metab_book = metab_data %>%
   # day/night for bookkeep
   mutate(daynight = is.day(date_time, lat = 42.11),
          daynight = as.character(daynight)) %>%
   mutate(daynight = case_when(.$daynight=="TRUE" ~ 1,
                               .$daynight=="FALSE" ~ 0)) %>%
   group_by(pond_id, doy) %>%
   group_modify(
      ~metab.bookkeep(do.obs = .$do,
                      do.sat = .$o2_eq_sat,
                      k.gas = .$k_gas,
                      z.mix = .$z_mix,
                      irr = .$daynight,
                      datetime = .$date_time)) %>%
   ungroup()



##__Kalman Filter method

  #--
  # metab.kalman() returns a list (not a data frame)
  # need to use a nested workflow
  #--


# pond a
mle.a = metab_data %>%
   filter(pond_id=="A") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       # error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("A", n())) %>%
   relocate(pond_id)

# pond b
mle.b = metab_data %>%
   filter(pond_id=="B") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       # error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("B", n())) %>%
   relocate(pond_id)

# pond c
mle.c = metab_data %>%
   filter(pond_id=="C") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       # error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("C", n())) %>%
   relocate(pond_id)

# pond d
mle.d = metab_data %>%
   filter(pond_id=="D") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       # error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("D", n())) %>%
   relocate(pond_id)

# pond e
mle.e = metab_data %>%
   filter(pond_id=="E") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       # error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("E", n())) %>%
   relocate(pond_id)

# pond f 
mle.f = metab_data %>%
   filter(pond_id=="F") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       # error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup() %>%
   mutate(pond_id = rep_len("F", n())) %>%
   relocate(pond_id)


# all ponds
metabolism = bind_rows(mle.a, mle.b, mle.c, mle.d, mle.e, mle.f)


   ## remove temporary objects
   rm(list = ls(pattern="mle.[abcdef]"))
   ##


