#~~~
# Output data files for archiving in EDI
#~~~


# Stratification
hobo_strat %>% 
   mutate(doy = yday(date_time)) %>%
   relocate(doy, .after=date_time) %>%
   mutate(across(meta_top:thermocline, ~na_if(., "NaN"))) %>%
   mutate(across(meta_top:buoy_freq, ~replace(., is.na(.), "mix"))) %>%
   write.csv(., file = "Data/stratification_data.csv", row.names=FALSE)



# Weather station
weather_data %>%
   write.csv(., file = "Data/weather-station_data.csv", row.names=FALSE)



# GHG diffusive flux
lake_flux %>%
   select(pond_id, date, doy, surface_temp, ends_with("lake"), ch4_atmo:n2o_atmo, k_cole, ends_with("flux")) %>%
   relocate(n2o_lake, .after=co2_lake) %>%
   rename(k600 = k_cole) %>%
   write.csv(., file = "Data/greenhouse-gas_data.csv", row.names=FALSE)



# Metabolism
metab_mle %>%
   write.csv(., file = "Data/daily-metabolism_data.csv", row.names=FALSE)



# Sonde profiles


