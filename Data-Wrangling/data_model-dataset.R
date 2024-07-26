
# Large dataset of all variables/data for analyses


#- Needed data sets

# lake_flux  [co2, ch4, and n2o flux]
# metabolism  [GPP, R, NEP]
# limno_field_data  [nutrients]
# sonde_surface  [all vars - temp, do, chla, phyco, cond, salinity]
# sonde_bottom  [all vars - temp, do, chla, phyco, cond, salinity]
# weather_data  [wind speed]
# sonde_strat [profile z_mix; profile mixed/stratified status]
# methano_dea [methanogenesis potential, DEA rates]
# ebu_flux_pond



## Prep the individual data sets

# Dissolved gas concentration and diffusive flux (from data set created and output in 'data_ghg-calculations' script)
# m10 = read_csv("Data/ghg_concentration_flux_total.csv") %>%
#    select(pond_id, doy, week, ends_with("flux"), ends_with("lake"))


# Dissolved gas concentration and diffusive flux (from 'data_import_EDI' script)
m10 = lake_flux %>%
   select(pond_id, doy, ends_with("flux"), ends_with("lake"))


# Daily metabolism estimates (from 'data_import_EDI' script)
m11 = metabolism %>%
   # filter(!(GPP < 0 | R > 0)) %>%
   filter(!(if_all(.cols = c(GPP, R, NEP), .fns = is.na)))


# Surface water limno samples (from 'data_import_EDI' script)
m12 = limno_field_data %>%
   select(pond_id, doy, period, tn, tp, nox, srp) %>%
   # align dates of nutrient samples with GHG samples (became offset after DOY 220; nutrients were sampled the day after GHGs)
   mutate(doy = if_else(doy > 221, doy - 1, doy))


# Sonde surface water values (from 'data_import_EDI' script then 'data_import-and-process' script)
m13 = sonde_surface %>%
   select(-contains("_rfu"))


# Sonde bottom water values (from 'data_import_EDI' script then 'data_import-and-process' script)
m14 = sonde_bottom %>%
   select(-contains("_rfu")) %>%
   rename_with(.fn = ~paste("bottom", ., sep="_"), .cols = temp:last_col())


# Weather (from 'data_import_EDI' script)
m15 = weather_data %>%
   mutate(U10 = wind_speed * ((10 / wind_z)^(1/7))) %>%
   select(doy, wind_speed, U10) %>%
   group_by(doy) %>%
   summarize(across(wind_speed:U10, ~mean(., na.rm=TRUE))) %>%
   ungroup() %>%
   rename(wind_U10 = U10)


# Methanogenesis and DEA (from 'data_import_EDI' script)
m16 = methano_dea %>%
   rename(methanogenesis = ch4_rate, DEA = n2o_rate) %>%
   select(-week)


# Sonde profile stratification (from 'data_import_EDI' script then 'data_stratification' script)
m21 = sonde_strat %>%
   select(pond_id, doy, sonde_zmix = z_mix, sonde_strat = stratification)


# Ebullition (from 'data_import_EDI' script)
m22 = ebu_flux_pond %>%
   select(-week, -flag)



#- Combined

test = m10 %>%          # GHG dissolved conc. and diffusive flux 
   full_join(m11) %>%   # metabolism
   full_join(m12) %>%   # limno samples
   full_join(m13) %>%   # sonde surface
   full_join(m14) %>%   # sonde bottom
   full_join(m15) %>%   # weather
   full_join(m16) %>%   # DEA and Methanogenesis
   full_join(m21) %>%   # sonde stratification
   full_join(m22)       # Ebullition



test = test %>%
   mutate(treatment = case_when(pond_id %in% c("A", "B", "C") ~ "pulsed",
                                pond_id %in% c("D", "E", "F") ~ "reference")) %>%
   relocate(treatment, .after=pond_id) %>%
   # remove 3 blank rows with extra wind speed data
   filter(!(is.na(pond_id))) %>%
   # add a variable for "week" of experiment here (df's were joining weird when 'week' was kept in "m" df's above)
   left_join(data.frame(doy = c(145:242), week = rep(1:14, each=7)))



model_dataset = test %>%
   # reorder variables
   select(pond_id, treatment, doy, week, period, everything()) %>%
   arrange(pond_id, doy) %>% 
   # fill in "period" variable for DOY 241
   mutate(period = if_else(is.na(period) & doy==241, "PULSE2", period)) %>%
   # add a Date variable
   #  updated dates; as_date() treats the 'origin' as day 0, not day 1, so need to set it as the previous day to the desired start
   mutate(date = as_date(doy, origin="2019-12-31")) %>%
   relocate(date, .before=doy)


# output the complete model dataset
write_csv(model_dataset, file = "Data/ghg-model-dataset_2024-07-26.csv")


   ## remove temporary individual data sets
   rm(list = ls(pattern = "m[[:digit:]]"), test)
   ##


