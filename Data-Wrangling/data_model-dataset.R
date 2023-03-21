
# Large dataset of all variables/data for GAMs and LMEs


# Needed data sets

# lake_flux  [co2, ch4, and n2o flux]
# metabolism  [GPP, R, NEP]
# limno_field_data  [nutrients]
# sonde_surface  [all vars - temp, do, chla, phyco, cond, salinity]
# sonde_bottom  [all vars - temp, do, chla, phyco, cond, salinity]
# weather_data  [wind speed]
# dea_rates  [DEA N2O]
# methano_rates  [methanogenesis]
# alk_data  [alkalinity]
# doc_data [DOC]
# hobo_strat  [daily mean z_mix; daily mean z_mix between 900a-1130a; mean z_mix of previous 24h; percent of previous 24h mixed/stratified]
# sonde_strat [profile z_mix; profile mixed/stratified status]
# ebullition


## Prep the individual data sets

# Dissolved gas concentration and diffusive flux
m10 = lake_flux %>%
   select(pond_id, doy, week, ends_with("flux"), ends_with("lake"))


# Daily metabolism estimates
m11 = metabolism %>%
   filter(!(GPP < 0 | R > 0))


# Surface water limno samples
m12 = limno_field_data %>%
   select(pond_id, doy, period, tn, tp, nox, srp) %>%
   # align dates of nutrient samples with GHG samples (became offset after DOY 220; nutrients were sampled the day after GHGs)
   mutate(doy = if_else(doy > 221, doy - 1, doy))


# Sonde surface water values
m13 = sonde_surface %>%
   select(-contains("_rfu"))


# Sonde bottom water values
m14 = sonde_bottom %>%
   select(-contains("_rfu")) %>%
   rename_with(.fn = ~paste("bottom", ., sep="_"), .cols = temp:last_col())


# Weather
m15 = weather_data %>%
   mutate(U10 = wind_speed * ((10 / wind_z)^(1/7))) %>%
   select(doy, wind_speed, U10) %>%
   group_by(doy) %>%
   summarize(across(wind_speed:U10, ~mean(., na.rm=TRUE))) %>%
   ungroup() %>%
   rename(wind_U10 = U10)


# DEA
m16 = dea_rates %>%
   select(pond_id, doy, n2o_rate) %>%
   rename(DEA = n2o_rate)


# Methanogenesis
m17 = methano_rates %>%
   select(pond_id, doy, ch4_rate) %>%
   rename(methanogenesis = ch4_rate)


# Alkalinity
m18 = alk_data %>%
   select(-sample_id, -notes) %>%
   relocate(pond_id) %>%
   rename(alk_ph = ph)


# DOC
m19 = doc_dat %>%
   select(pond_id, doy, doc_ppb) %>%
   arrange(pond_id, doy) %>%
   # align dates of DOC samples with GHG samples
   #  Pond A: DOC becomes offset from GHGs by one day starting at DOY 222
   #  Pond B: DOC is offset from GHGs by one day for DOYs 223, 225, 227, 230, 232
   #  Ponds C-F are all aligned correctly
   mutate(doy = case_when(pond_id=='A' & doy > 221 ~ doy - 1,
                          pond_id=='B' & doy %in% c(223, 225, 227, 230, 232) ~ doy - 1,
                          TRUE ~ doy))


# T-chain stratification

# mean daily z-mix
t1 = metab_data %>%
   select(pond_id, doy, z_mix) %>%
   group_by(pond_id, doy) %>%
   summarize(zmix_daily = mean(z_mix)) %>%
   ungroup()

# mean z-mix around time of GHG sample collection (9 - 1130 am)
t2 = metab_data %>%
   select(pond_id, date_time, doy, z_mix) %>%
   mutate(hour = hour(date_time)) %>%
   filter(hour %in% c(9:11)) %>%
   group_by(pond_id, doy) %>%
   summarize(zmix_ghg_time = mean(z_mix)) %>%
   ungroup()

# mean z-mix of previous 24 hours (10 - 10 am)
t3 = metab_data %>%
   select(pond_id, date_time, doy, z_mix) %>%
   mutate(lagzmix = lag(z_mix, n=27)) %>% 
   group_by(pond_id, doy) %>% 
   summarize(zmix_prev24 = mean(lagzmix, na.rm=TRUE)) %>%
   ungroup()

# percent of measurements in previous 24 hours that were "stratified"
t4 = metab_data %>%
   mutate(stratification = case_when(z_mix==1.75 ~ 0,
                                     z_mix==1.5 ~ 0,
                                     TRUE ~ 1)) %>%
   mutate(lagstrat = lag(stratification, n=27)) %>%
   group_by(pond_id, doy) %>%
   summarize(perc_strat_prev24 = mean(lagstrat, na.rm=TRUE) * 100) %>%
   ungroup()

m20 = full_join(t1, t2) %>% full_join(t3) %>% full_join(t4)


# Sonde profile stratification
m21 = sonde_strat %>%
   select(pond_id, doy, sonde_zmix = z_mix, sonde_strat = stratification)


# Ebullition
m22 = read_csv("Data/ebullition_total.csv") %>%
   select(-week)


#- Combined

test = m10 %>%  #full_join(t1, t10) %>%
   full_join(m11) %>%
   full_join(m12) %>%
   full_join(m13) %>%
   full_join(m14) %>%
   full_join(m15) %>%
   full_join(m16) %>%
   full_join(m17) %>%
   full_join(m18) %>%
   full_join(m19) %>%
   full_join(m20) %>%
   full_join(m21) %>%
   full_join(m22)


test = test %>%
   mutate(treatment = case_when(pond_id %in% c("A", "B", "C") ~ "pulsed",
                                pond_id %in% c("D", "E", "F") ~ "reference")) %>%
   relocate(treatment, .after=pond_id) %>%
   # remove 3 blank rows with extra wind speed data
   filter(!(is.na(pond_id)))


model_dataset = test %>%
   # reorder variables
   select(pond_id, treatment, doy, period, everything()) %>%
   arrange(pond_id, doy) %>%
   # add N:P ratio variable
   mutate(np_ratio = (tn/14.001) / ((tp/1000)/30.97),
          # convert DOC to ppm (mg/L)
          doc_ppm = doc_ppb / 1000) %>%
   # add a new 'period' variable to account for the fact that pulse periods don't mean anything for the reference treatment
   #  reference ponds are, in effect, always in the 'Base' period
   mutate(period2 = if_else(treatment=="reference", "BASE", period)) %>%
   relocate(period2, .after=period) %>%
   # add a Date variable
   #  updated dates; as_date() treats the 'origin' as day 0, not day 1, so need to set it as the previous day to the desired start
   mutate(date = as_date(doy, origin="2019-12-31")) %>%
   relocate(date, .before=doy)


# output the complete model dataset
write_csv(model_dataset, file = "Data/ghg-model-dataset_2023-03-21.csv")


   ## remove temporary individual data sets
   rm(list = ls(pattern = "m[[:digit:]]"), test, t1, t2, t3, t4)
   ##


