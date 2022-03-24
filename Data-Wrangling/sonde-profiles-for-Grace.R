#~~~
# Script to calculate depth-based means of variables from daily sonde profiles
#~~~

library(tidyverse)
library(lubridate)


# load data
sonde_profiles = read_csv("sonde-profiles_all-data.csv")


# Clean up profile dataset
sonde_profiles = sonde_profiles %>%
   mutate(pond_id = str_remove(pond_id, "Pond "),
          date_time = as.POSIXct(mdy(date) + hms(time)),
          doy = yday(date_time)) %>%
   arrange(doy, pond_id, date_time) %>%
   select(-date, -time) %>%
   relocate(date_time, doy) %>%
   # change temp data to Celcius
   mutate(temp = (temp - 32) / 1.8) %>%
   # remove errant measurements recorded as 1970-01-01
   filter(!(doy==1))
   


##__Calculate depth-based variable means
#  Depth: 10-20 cm

sonde_surface = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   # set depths (in meters)
   filter(vert_m >= 0.10 & vert_m <= 0.20) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup()


