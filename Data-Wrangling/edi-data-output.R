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
   write.csv(., file = "meteorological_pondStation.csv", row.names=FALSE)



#- GHG diffusive flux
lake_flux %>%
   select(pond_id, date, doy, surface_temp, ends_with("lake"), ch4_atmo:n2o_atmo, k_cole, ends_with("flux")) %>%
   relocate(n2o_lake, .after=co2_lake) %>%
   rename(k600 = k_cole) %>%
   write.csv(., file = "greenhouse-gas_data.csv", row.names=FALSE)



#- Metabolism
read_csv("Data/metabolism_total.csv") %>%
   # add data flag column to ID days with erroneous metabolism measurements
   mutate(flag = if_else(GPP < 0 | R > 0, "E", NULL)) %>%
   # replace metabolism estimates on days with erroneous estimates with NA (but leave the day in the data set)
   mutate(across(GPP:NEP, ~case_when(flag=="E" ~ -9999, 
                                     TRUE ~ .)),
          across(GPP:NEP, ~na_if(., -9999))) %>%
   write.csv(., file = "daily_metabolism.csv", row.names=FALSE)



#- Sonde profiles

# just using file "Data/sonde-profiles_all-data_2022-07-20.csv", no further cleaning needed here
read_csv("Data/sonde-profiles_all-data_2022-07-20.csv") %>%
   write.csv(., file = "profiles_daily_deepsite.csv", row.names=FALSE)


#- High-frequency DO data (miniDOTs)

# just using file "Data/miniDOT_total.csv", no further cleaning needed here
read_csv("Data/miniDOT_total.csv") %>%
   write.csv(., file = "do_sensor_hf.csv", row.names=FALSE)




