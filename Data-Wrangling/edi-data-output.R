#~~~
# Output data files for archiving in EDI
#~~~


library(tidyverse)


#- Weather station
# processed dataset from "data_import-and-process" script
weather_data %>%
   # output directly to the shared EDI submission folder in Box
   write.csv(., file = "C:/Users/johns/Box/Hort Farm Experiment/EDI Data Submission/meteorological_pondStation.csv", row.names=FALSE)



#- GHG diffusive flux
# processed dataset from "data_import-and-process" and "data_ghg-calculations" scripts
read_csv("Data/ghg_concentration_flux_total.csv") %>%
   select(pond_id, doy, ends_with("flux"), ends_with("lake"), ch4_atmo:n2o_atmo, k_ch4:k_n2o) %>%
   relocate(n2o_lake, .after=co2_lake) %>%
   rename(ch4_concentration = ch4_lake, co2_concentration = co2_lake, n2o_concentration = n2o_lake,
          ch4_atmosphere = ch4_atmo, co2_atmosphere = co2_atmo, n2o_atmosphere = n2o_atmo,
          ch4_k600 = k_ch4, co2_k600 = k_co2, n2o_k600 = k_n2o) %>%
   # output directly to the shared EDI submission folder in Box
   write.csv(., file = "C:/Users/johns/Box/Hort Farm Experiment/EDI Data Submission/ghg_diffusive_flux.csv", row.names=FALSE)



#- GHG process rate measurements
# processed datasets from "data_ghg-calculations" script
read_csv("Data/methanogenesis_rates_total.csv") %>%
   select(pond_id, week, doy, methanogenesis_potential = ch4_rate) %>%
   full_join(read_csv("Data/DEA_rates_total.csv") %>%
                select(pond_id, week, doy, DEA = n2o_rate)) %>%
   # output directly to the shared EDI submission folder in Box
   write.csv(., file = "C:/Users/johns/Box/Hort Farm Experiment/EDI Data Submission/ghg_production_assays.csv", row.names=FALSE)



#- Metabolism
read_csv("Data/metabolism_total.csv") %>%
   # add data flag column to ID days with erroneous metabolism measurements
   mutate(flag = if_else(GPP < 0 | R > 0, "E", NULL)) %>%
   # replace metabolism estimates on days with erroneous estimates with NA (but leave the day in the data set)
   mutate(across(GPP:NEP, ~case_when(flag=="E" ~ -9999, 
                                     TRUE ~ .)),
          across(GPP:NEP, ~na_if(., -9999))) %>%
   # output directly to the shared EDI submission folder in Box
   write.csv(., file = "C:/Users/johns/Box/Hort Farm Experiment/EDI Data Submission/daily_metabolism.csv", row.names=FALSE)



#- Sonde profiles
# just using file "Data/sonde-profiles_all-data_2022-07-20.csv", no further cleaning needed here
read_csv("Data/sonde-profiles_all-data_2022-07-20.csv") %>%
   # output directly to the shared EDI submission folder in Box
   write.csv(., file = "C:/Users/johns/Box/Hort Farm Experiment/EDI Data Submission/profiles_daily_deepsite.csv", row.names=FALSE)


# Surface Chl-a average from 10-30 cm (for file: "surface_nutrients_chla")
read_csv("Data/sonde-profiles_all-data_2022-07-20.csv") %>%
   group_by(pond_id, doy) %>%
   filter(between(vert_m, 0.10, 0.30)) %>%
   summarize(chla_10_30 = mean(chla, na.rm=T)) %>%
   ungroup() %>%
   # sonde profiles missing for Pond C (DOY 231) and Pond B (DOY 151)
   # add these days and linearly interpolate for the missing chla values
   add_row(pond_id = "B", doy = 151) %>%
   add_row(pond_id = "C", doy = 231) %>%
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(chla_10_30 = zoo::na.approx(chla_10_30)) %>%
   ungroup() %>%
   # output into the Hort Farm folder so we have a copy/record, but not into the EDI submission folder
   write.csv(., file = "C:/Users/johns/Box/Hort Farm Experiment/2020 Benthic Pelagic Experiment/surface_chla_10_30.csv", row.names=FALSE)


#- High-frequency DO data (miniDOTs)
# just using file "Data/miniDOT_total.csv", no further cleaning needed here
read_csv("Data/miniDOT_total.csv") %>%
   # output directly to the shared EDI submission folder in Box
   write.csv(., file = "C:/Users/johns/Box/Hort Farm Experiment/EDI Data Submission/do_sensor_hf.csv", row.names=FALSE)




