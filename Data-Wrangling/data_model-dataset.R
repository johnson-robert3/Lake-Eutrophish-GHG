
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


#- All datasets created using the 'data_import_EDI' script
source("Data-Wrangling/data_import_EDI.R")


## Prep the individual data sets

# Dissolved gas concentration and diffusive flux 
m10 = lake_flux %>%
   select(pond_id, doy, ends_with("flux"), ends_with("lake"))


# Daily metabolism estimates 
m11 = metabolism %>%
   # filter(!(GPP < 0 | R > 0)) %>%
   filter(!(if_all(.cols = c(GPP, R, NEP), .fns = is.na))) %>%
   # remove columns unnecessary for manuscript submission
   select(-flag)  # all erroneous days already excluded with previous line, flag no longer needed


# Surface water limno samples 
m12 = limno_field_data %>%
   select(pond_id, doy, period, tn, tp, nox, srp) %>%
   # align dates of nutrient samples with GHG samples (became offset after DOY 220; nutrients were sampled the day after GHGs)
   filter(!(doy==221)) %>%  # remove empty DOY 221 (otherwise the backwards shift creates two of doy 221)
   mutate(doy = if_else(doy > 221, doy - 1, doy)) %>%
   # remove columns unnecessary for manuscript submission
   select(-nox, -srp)


# Surface water values from sonde profiles 
#  means from 5-50 cm depth
m13 = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   filter(vert_m >= 0.05 & vert_m <= 0.50) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup() %>%
   # sonde profiles missing for Pond C (DOY 231) and Pond B (DOY 151)
   # add these days and linearly interpolate for all variables, so that surface values aren't missing
   add_row(pond_id = "B", doy = 151) %>%
   add_row(pond_id = "C", doy = 231) %>%
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(across(temp:salinity, ~zoo::na.approx(.))) %>%
   ungroup() %>%
   select(-contains("_rfu")) %>%
   # remove columns unnecessary for manuscript submission
   select(pond_id:do)


#--
   # set up temporary df of sonde profiles split into 10cm depth intervals
   sonde_int = sonde_profiles %>%
      # 10cm depth intervals
      mutate(depth_int = case_when(between(vert_m, -0.1, 0.1) ~ 0.1,
                                   between(vert_m, 0.1, 0.2) ~ 0.2,
                                   between(vert_m, 0.2, 0.3) ~ 0.3,
                                   between(vert_m, 0.3, 0.4) ~ 0.4,
                                   between(vert_m, 0.4, 0.5) ~ 0.5,
                                   between(vert_m, 0.5, 0.6) ~ 0.6,
                                   between(vert_m, 0.6, 0.7) ~ 0.7,
                                   between(vert_m, 0.7, 0.8) ~ 0.8,
                                   between(vert_m, 0.8, 0.9) ~ 0.9,
                                   between(vert_m, 0.9, 1.0) ~ 1.0,
                                   between(vert_m, 1.0, 1.1) ~ 1.1,
                                   between(vert_m, 1.1, 1.2) ~ 1.2,
                                   between(vert_m, 1.2, 1.3) ~ 1.3,
                                   between(vert_m, 1.3, 1.4) ~ 1.4,
                                   between(vert_m, 1.4, 1.5) ~ 1.5,
                                   between(vert_m, 1.5, 1.6) ~ 1.6,
                                   between(vert_m, 1.6, 1.7) ~ 1.7,
                                   between(vert_m, 1.7, 1.8) ~ 1.8,
                                   between(vert_m, 1.8, 1.9) ~ 1.9,
                                   between(vert_m, 1.9, 2.0) ~ 2.0,
                                   vert_m > 2.0 ~ 2.1)) %>%
      # mean values within each depth interval
      group_by(pond_id, doy, depth_int) %>%
      summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
      ungroup() 
#--


# Bottom water values from sonde profiles 
#  means from bottom 20 cm
m14 = sonde_int %>%
   group_by(pond_id, doy) %>%
   arrange(depth_int, .by_group=T) %>%
   slice_tail(n=2) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup() %>%
   # sonde profiles missing for Pond C (DOY 231) and Pond B (DOY 151)
   # add these days and linearly interpolate for all variables, so that bottom values aren't missing
   add_row(pond_id = "B", doy = 151) %>%
   add_row(pond_id = "C", doy = 231) %>%
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(across(temp:salinity, ~zoo::na.approx(.))) %>%
   ungroup() %>%
   select(-contains("_rfu")) %>%
   rename_with(.fn = ~paste("bottom", ., sep="_"), .cols = temp:last_col()) %>%
   # remove columns unnecessary for manuscript submission
   select(pond_id:bottom_do)


# Weather 
m15 = weather_data %>%
   mutate(U10 = wind_speed * ((10 / wind_z)^(1/7))) %>%
   select(doy, wind_speed, U10) %>%
   group_by(doy) %>%
   summarize(across(wind_speed:U10, ~mean(., na.rm=TRUE))) %>%
   ungroup() %>%
   rename(wind_U10 = U10)


# Methanogenesis and DEA 
m16 = methano_dea %>%
   rename(methanogenesis = ch4_rate) %>%
   select(-week)


# Sonde profile stratification 
library(rLakeAnalyzer)

m21 = sonde_int %>%
   select(pond_id:temp) %>%
   group_by(pond_id, doy) %>%
   summarize(pond_depth = max(depth_int),
             thermocline = thermo.depth(wtr = temp, depths = depth_int),
             meta_top = meta.depths(wtr = temp, depths = depth_int)[[1]],
             meta_bottom = meta.depths(wtr = temp, depths = depth_int)[[2]]) %>%
   ungroup() %>%
   # correct for when thermocline couldn't be calculated, or when meta was the whole water column
   mutate(z_mix = case_when(thermocline == 'NaN' ~ pond_depth,
                            meta_top < 0.3 & meta_bottom == pond_depth ~ pond_depth,
                            TRUE ~ thermocline)) %>%
   # add binary variable for if water column is mixed or stratified at time of profile
   mutate(stratification = case_when(z_mix == pond_depth ~ 'mixed',
                                     TRUE ~ 'stratified')) %>%
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
   # full_join(m21) %>%   # sonde stratification
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
# write_csv(model_dataset, file = "Data/ghg-model-dataset_2024-07-26.csv")

# output the dataset with just variables used in analyses/figures for the manuscript
write_csv(model_dataset, file = "Data/ghg-model-dataset_ms-data.csv")



   ## remove temporary individual data sets
   rm(list = ls(pattern = "m[[:digit:]]"), test, sonde_int)
   ##


