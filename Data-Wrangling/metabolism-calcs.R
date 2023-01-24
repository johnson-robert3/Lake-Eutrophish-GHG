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


### info about dropped points
do_cleaning_pts = 
minidot %>%
   # view just days that were part of the experiment
   filter(doy >= 145, doy <= 240) %>%
   # how many points were flagged to be dropped for each day for each pond
   mutate(points_interp = case_when(drop_pt == 1 | 
                                 lag(drop_pt, n=1) == 1 | 
                                 lag(drop_pt, n=2) == 1 | 
                                 lag(drop_pt, n=3) == 1 | 
                                 lag(drop_pt, n=4) == 1 | 
                                 lag(drop_pt, n=5) == 1 ~ 1,
                             TRUE ~ 0)) %>%
   group_by(pond_id, doy) %>%
   summarize(total = n(),
             drop = sum(drop_pt),
             interp = sum(points_interp)) %>%
   ungroup() %>%
   mutate(perc_drop = drop / total * 100,
          perc_interp = interp / total * 100) %>%
   # group_by(pond_id) %>%
   # summarize(min_perc_drop = min(perc_drop),
   #           max_perc_drop = max(perc_drop),
   #           mean_perc_drop = mean(perc_drop),
   #           min_perc_interp = min(perc_interp),
   #           max_perc_interp = max(perc_interp),
   #           mean_perc_interp = mean(perc_interp)) %>%
   # ungroup() %>%
   View
#
# next: view a distribution of number of points dropped across ponds and days, are there days where too much of the data is being removed with this 
# method (w/ interpolation) and the entire day should be removed prior to metabolism calcs?

# view histograms of number of points dropped within days by pond
windows(); hist(do_cleaning_pts %>% filter(pond_id=="A") %>% .$drop)

# number of points flagged and dropped (points that dropped by > 2.0 mg/l)
windows(); ggplot(do_cleaning_pts) +
   geom_histogram(aes(x = drop)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of flagged and dropped points") +
   theme_classic()

windows(); hist(do_cleaning_pts %>% filter(pond_id=="A") %>% .$interp)

# number of points dropped and interpolated over
windows(); ggplot(do_cleaning_pts) +
   geom_histogram(aes(x = interp)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of points removed and interpolated") +
   theme_classic()

# time series points dropped per day
windows(); ggplot(do_cleaning_pts) +
   geom_line(aes(x=doy, y = drop)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of points flagged and dropped") +
   theme_classic()

# time series points interpolated over per day
windows(); ggplot(do_cleaning_pts) +
   geom_line(aes(x=doy, y = interp)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of points removed and interpolated") +
   theme_classic()

# rank plot of percentage of points removed and interpolated for each pond
windows(); ggplot(do_cleaning_pts %>%
                     group_by(pond_id) %>%
                     arrange(perc_interp, .by_group=TRUE) %>%
                     mutate(rank = row_number()) %>%
                     ungroup()) +
   geom_point(aes(x = rank, y = perc_interp), shape=21) +
   
   geom_hline(yintercept = 33, color = "blue") +
   # geom_hline(yintercept = 50, color = "red") +
   # geom_hline(yintercept = 25, color = "green") +
   
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("Percent of data points each day removed and backfilled via linear interpolation") +
   theme_classic()

# how many days in each pond over thresholds of percent of points interpolated

do_cleaning_pts %>% filter(perc_interp > 50) %>% count(pond_id)
do_cleaning_pts %>% filter(perc_interp > 40) %>% count(pond_id)
do_cleaning_pts %>% filter(perc_interp > 33) %>% count(pond_id) # n = 10
do_cleaning_pts %>% filter(perc_interp > 25) %>% count(pond_id) # n = 20



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
   # use calculated thermocline depth for the mixed-layer depth 
   rename(z_mix = thermocline) %>%
   
   # Set Z_mix to 1.50m during times of turnover and mixing within the ponds  
   mutate(z_mix = case_when(
      # "NaN"s in the data (when rLakeAnalyzer functions can't calculate stratification variables) are also when water column is mixed
      z_mix == "NaN" ~ 1.5,
      # when the entire water column was considered Metalimnion
      meta_top==0 & meta_bottom==2 ~ 1.5,
      # rLakeAnalyzer functions seem to assign thermocline to 0.125m during times of mixing or weak, transient stratification 
      z_mix == 0.125 ~ 1.5,
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

# metab_book = metab_data %>%
#    # day/night for bookkeep
#    mutate(daynight = is.day(date_time, lat = 42.11),
#           daynight = as.character(daynight)) %>%
#    mutate(daynight = case_when(.$daynight=="TRUE" ~ 1,
#                                .$daynight=="FALSE" ~ 0)) %>%
#    group_by(pond_id, doy) %>%
#    group_modify(
#       ~metab.bookkeep(do.obs = .$do,
#                       do.sat = .$o2_eq_sat,
#                       k.gas = .$k_gas,
#                       z.mix = .$z_mix,
#                       irr = .$daynight,
#                       datetime = .$date_time)) %>%
#    ungroup()



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

# output total metabolism dataset (still containing erroneous estimates)
write.csv(metabolism, file = "Data/metabolism_total.csv", row.names = FALSE)
   
   
# number of days per pond with erroneous metabolism estimates
metabolism %>% filter(GPP<0 | R>0) %>% count(pond_id)
metabolism %>% filter(GPP<0 | R>0) %>% View

# view number of dropped DO points on days with erroneous estimates
metabolism %>% filter(GPP<0 | R>0) %>% left_join(test) %>% count(drop)
metabolism %>% filter(GPP<0 | R>0) %>% left_join(test) %>% arrange(drop) %>% View
# not as much overlap as I expected. 
# none of the days with >25% of points removed (and interpolated) overlapped with days with erroneous metabolism estimates (perhaps b/c we "fixed" these?)
# of 62 days with erroneous estimates, 40 were on days that did not have any DO data points dropped

# 62 days with erroneous estimates using corrected DO data
# 71 days with erroneous estimates using raw DO data
# 16 days with >=3 flagged DO points
# 20 days with >25% of points removed and interpolated


# combined dataset to view metabolism around days when a higher percent of DO data points were cleaned
test = full_join(metabolism, do_cleaning_pts) %>%
   rename(n_total = total, n_drop = drop, n_interp = interp) %>%
   mutate(flag_33 = case_when(perc_interp > 33 ~ 1,
                              TRUE ~ 0))


test %>% filter(flag_33 == 1) %>% count(pond_id)


windows(); ggplot(test,
                  aes(x = doy, y = NEP)) +
   # all metabolism
   geom_line() +
   # circle days in red where >33% of DO points have been cleaned
   geom_point(data = ~filter(.x, flag_33==1), shape = 21, color ="red", size=3) +
   # circle days in blue that have erroneous metabolism estimates
   geom_point(data = ~filter(.x, GPP<0 | R>0), shape = 21, color ="blue", size=3) +
   # 0 line
   geom_hline(yintercept = 0, linetype = 2) +
   #
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()

ggsave(filename = "NEP-highlight-cleaning-pts.png", height=5, width=8, units ="in")


