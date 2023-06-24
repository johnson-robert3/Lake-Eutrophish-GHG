# sonde profile interval tests



##__Sonde depth interval tests

## 10 cm
sonde_int = sonde_profiles %>%
   mutate(depth_int = case_when(.$vert_m >= 0.0 & vert_m <= 0.1 ~ 0.1,
                                .$vert_m > 0.1 & vert_m <= 0.2 ~ 0.2,
                                .$vert_m > 0.2 & vert_m <= 0.3 ~ 0.3,
                                .$vert_m > 0.3 & vert_m <= 0.4 ~ 0.4,
                                .$vert_m > 0.4 & vert_m <= 0.5 ~ 0.5,
                                .$vert_m > 0.5 & vert_m <= 0.6 ~ 0.6,
                                .$vert_m > 0.6 & vert_m <= 0.7 ~ 0.7,
                                .$vert_m > 0.7 & vert_m <= 0.8 ~ 0.8,
                                .$vert_m > 0.8 & vert_m <= 0.9 ~ 0.9,
                                .$vert_m > 0.9 & vert_m <= 1.0 ~ 1.0,
                                .$vert_m > 1.0 & vert_m <= 1.1 ~ 1.1,
                                .$vert_m > 1.1 & vert_m <= 1.2 ~ 1.2,
                                .$vert_m > 1.2 & vert_m <= 1.3 ~ 1.3,
                                .$vert_m > 1.3 & vert_m <= 1.4 ~ 1.4,
                                .$vert_m > 1.4 & vert_m <= 1.5 ~ 1.5,
                                .$vert_m > 1.5 & vert_m <= 1.6 ~ 1.6,
                                .$vert_m > 1.6 & vert_m <= 1.7 ~ 1.7,
                                .$vert_m > 1.7 & vert_m <= 1.8 ~ 1.8,
                                .$vert_m > 1.8 & vert_m <= 1.9 ~ 1.9,
                                .$vert_m > 1.9 & vert_m <= 2.0 ~ 2.0,
                                .$vert_m > 2 ~ 2.1)) %>%
   # mean values within each depth interval
   group_by(pond_id, doy, depth_int) %>%
   summarize(across(temp:salinity, mean, na.rm=T)) %>%
   ungroup() 


## 25 cm
sonde_int = sonde_profiles %>%
   mutate(depth_int = case_when(.$vert_m >= 0.0 & vert_m <= 0.25 ~ 0.25,
                                .$vert_m > 0.25 & vert_m <= 0.5 ~ 0.5,
                                .$vert_m > 0.5 & vert_m <= 0.75 ~ 0.75,
                                .$vert_m > 0.75 & vert_m <= 1.0 ~ 1.0,
                                .$vert_m > 1.0 & vert_m <= 1.25 ~ 1.25,
                                .$vert_m > 1.25 & vert_m <= 1.5 ~ 1.5,
                                .$vert_m > 1.5 & vert_m <= 1.75 ~ 1.75,
                                .$vert_m > 1.75 ~ 2.0)) %>%
   # mean values within each depth interval
   group_by(pond_id, doy, depth_int) %>%
   summarize(across(temp:salinity, mean, na.rm=T)) %>%
   ungroup() 


## 5 cm
sonde_int = sonde_profiles %>%
   mutate(depth_int = case_when(.$vert_m >= 0.0 & vert_m <= 0.05 ~ 0.05,
                                .$vert_m > 0.05 & vert_m <= 0.1 ~ 0.1,
                                .$vert_m > 0.1 & vert_m <= 0.15 ~ 0.15,
                                .$vert_m > 0.15 & vert_m <= 0.2 ~ 0.2,
                                .$vert_m > 0.2 & vert_m <= 0.25 ~ 0.25,
                                .$vert_m > 0.25 & vert_m <= 0.3 ~ 0.3,
                                .$vert_m > 0.3 & vert_m <= 0.35 ~ 0.35,
                                .$vert_m > 0.35 & vert_m <= 0.4 ~ 0.4,
                                .$vert_m > 0.4 & vert_m <= 0.45 ~ 0.45,
                                .$vert_m > 0.45 & vert_m <= 0.5 ~ 0.5,
                                .$vert_m > 0.5 & vert_m <= 0.55 ~ 0.55,
                                .$vert_m > 0.55 & vert_m <= 0.6 ~ 0.6,
                                .$vert_m > 0.6 & vert_m <= 0.65 ~ 0.65,
                                .$vert_m > 0.65 & vert_m <= 0.7 ~ 0.7,
                                .$vert_m > 0.7 & vert_m <= 0.75 ~ 0.75,
                                .$vert_m > 0.75 & vert_m <= 0.8 ~ 0.8,
                                .$vert_m > 0.8 & vert_m <= 0.85 ~ 0.85,
                                .$vert_m > 0.85 & vert_m <= 0.9 ~ 0.9,
                                .$vert_m > 0.9 & vert_m <= 0.95 ~ 0.95,
                                .$vert_m > 0.95 & vert_m <= 1.0 ~ 1.0,
                                .$vert_m > 1.0 & vert_m <= 1.05 ~ 1.05,
                                .$vert_m > 1.05 & vert_m <= 1.1 ~ 1.1,
                                .$vert_m > 1.1 & vert_m <= 1.15 ~ 1.15,
                                .$vert_m > 1.15 & vert_m <= 1.2 ~ 1.2,
                                .$vert_m > 1.2 & vert_m <= 1.25 ~ 1.25,
                                .$vert_m > 1.25 & vert_m <= 1.3 ~ 1.3,
                                .$vert_m > 1.3 & vert_m <= 1.35 ~ 1.35,
                                .$vert_m > 1.35 & vert_m <= 1.4 ~ 1.4,
                                .$vert_m > 1.4 & vert_m <= 1.45 ~ 1.45,
                                .$vert_m > 1.45 & vert_m <= 1.5 ~ 1.5,
                                .$vert_m > 1.5 & vert_m <= 1.55 ~ 1.55,
                                .$vert_m > 1.55 & vert_m <= 1.6 ~ 1.6,
                                .$vert_m > 1.6 & vert_m <= 1.65 ~ 1.65,
                                .$vert_m > 1.65 & vert_m <= 1.7 ~ 1.7,
                                .$vert_m > 1.7 & vert_m <= 1.75 ~ 1.75,
                                .$vert_m > 1.75 & vert_m <= 1.8 ~ 1.8,
                                .$vert_m > 1.8 & vert_m <= 1.85 ~ 1.85,
                                .$vert_m > 1.85 & vert_m <= 1.9 ~ 1.9,
                                .$vert_m > 1.9 & vert_m <= 1.95 ~ 1.95,
                                .$vert_m > 1.95 & vert_m <= 2.0 ~ 2.0,
                                .$vert_m > 2.0 ~ 2.05)) %>%
   # mean values within each depth interval
   group_by(pond_id, doy, depth_int) %>%
   summarize(across(temp:salinity, mean, na.rm=T)) %>%
   ungroup() 




##__Surface water means 
#  Depth: 10-30 cm

sonde_surface = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   filter(vert_m >= 0.1 & vert_m <= 0.30) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup()


##__Bottom water means
#  Depth: bottom 30 cm

sonde_bottom = sonde_int %>%
   group_by(pond_id, doy) %>%
   arrange(depth_int, .by_group=T) %>%
   slice_tail(n=3) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup()

