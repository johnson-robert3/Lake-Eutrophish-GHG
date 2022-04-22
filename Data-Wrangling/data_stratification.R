#~~~
# Script for working with t-chain data and calculating stratification variables
# By: R. Johnson
#~~~


library(rLakeAnalyzer)


# Create a nested, wide data frame of t-chain data on which to run functions
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
          thermocline = map(hobo_wide, ~ts.thermo.depth(., na.rm=T)))
          # buoy_freq = map(hobo_wide, ~ts.buoyancy.freq(., na.rm=T)))


# Unnest water column stratification variables into a useful data frame
hobo_strat = hobo_nest %>%
   # remove the nested list-columns of original long- and wide-format t-chain data
   select(-data, -hobo_wide) %>%
   # can't have a variable duplicated across list-columns for unnesting
   mutate(thermocline = map(thermocline, ~select(., -datetime))) %>%
          # buoy_freq = map(buoy_freq, ~select(., -datetime))) %>%
   # unnest list-columns
   unnest(cols = c(meta_depths, thermocline)) %>%
                   # , buoy_freq)) %>% 
   ungroup() %>%
   rename(date_time = datetime,
          meta_top = top,
          meta_bottom = bottom,
          thermocline = thermo.depth)
          # buoy_freq = n2)



