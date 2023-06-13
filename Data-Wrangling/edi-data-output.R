#~~~
# Output data files for archiving in EDI
#~~~


library(tidyverse)


#- Stratification
hobo_strat %>% 
   mutate(doy = yday(date_time)) %>%
   relocate(doy, .after=date_time) %>%
   mutate(across(meta_top:thermocline, ~na_if(., "NaN"))) %>%
   mutate(across(meta_top:buoy_freq, ~replace(., is.na(.), "mix"))) %>%
   write.csv(., file = "stratification_data.csv", row.names=FALSE)



#- Weather station
weather_data %>%
   write.csv(., file = "weather-station_data.csv", row.names=FALSE)



#- GHG diffusive flux
lake_flux %>%
   select(pond_id, date, doy, surface_temp, ends_with("lake"), ch4_atmo:n2o_atmo, k_cole, ends_with("flux")) %>%
   relocate(n2o_lake, .after=co2_lake) %>%
   rename(k600 = k_cole) %>%
   write.csv(., file = "greenhouse-gas_data.csv", row.names=FALSE)



#- Metabolism
metabolism %>%
   # add data flag column to ID days with erroneous metabolism measurements
   mutate(flag = case_when(GPP < 0 | R > 0 ~ 1,
                           TRUE ~ 0)) %>%
   write.csv(., file = "daily-metabolism_data.csv", row.names=FALSE)



#- Sonde profiles

# just using file "Data/sonde-profiles_all-data_2022-07-20.csv", no further cleaning needed here
read_csv("Data/sonde-profiles_all-data_2022-07-20.csv") %>%
   write.csv(., file = "profiles_daily_deepsite.csv", row.names=FALSE)


#- High-frequency DO data (miniDOTs)

# just using file "Data/miniDOT_total.csv", no further cleaning needed here
read_csv("Data/miniDOT_total.csv") %>%
   write.csv(., file = "do_sensor_hf.csv", row.names=FALSE)




