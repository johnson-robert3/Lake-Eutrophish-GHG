
# Large dataset of all variables/data for GAMs and LMEs


# Needed data sets

# lake_flux  [co2, ch4, and n2o flux]
# metabolism  [nep]
# limno_field_data  [nutrients]
# sonde_bottom  [SWI DO]
# sonde_surface  [Chla, temp]
# weather_data  [wind speed]
# dea_rates  [DEA N2O]
# methano_rates  [methanogenesis]
# hobo_strat  [buoyancy frequency]  ### we are no longer calculating/using buoyancy frequency. physical limnologists say this is not a good/reliable/useful variable
# alk_data  [alkalinity]


## Prep the individual data sets

m10 = lake_flux %>%
   select(pond_id, doy, week, ends_with("flux"), ends_with("lake"))


# t10 = lake_conc %>%
#    select(pond_id, doy, ends_with("lake"))


m11 = metabolism %>%
   filter(!(GPP < 0 | R > 0))


m12 = limno_field_data %>%
   select(pond_id, doy, period, tp:nhx)


m13 = sonde_bottom %>%
   select(pond_id, doy, do, do_sat) %>%
   rename(bottom_do = do,
          bottom_do_sat = do_sat)


m14 = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   filter(vert_m > 0.05 & vert_m < 0.60) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=TRUE))) %>%
   ungroup() %>%
   select(pond_id, doy, temp, chla)


m15 = weather_data %>%
   mutate(U10 = wind_speed * ((10 / wind_z)^(1/7))) %>%
   select(doy, wind_speed, U10) %>%
   group_by(doy) %>%
   summarize(across(wind_speed:U10, ~mean(., na.rm=TRUE))) %>%
   ungroup() %>%
   rename(wind_U10 = U10)


m16 = dea_rates %>%
   select(pond_id, doy, n2o_rate) %>%
   rename(DEA = n2o_rate)


m17 = methano_rates %>%
   select(pond_id, doy, ch4_rate) %>%
   rename(methanogenesis = ch4_rate)


m18 = alk_data %>%
   select(-sample_id, -notes) %>%
   relocate(pond_id)


m19 = doc_dat %>%
   select(pond_id, doy, doc_ppb)


# t9 = hobo_strat %>%
#    mutate(doy = yday(date_time)) %>%
#    group_by(pond_id, doy) %>%
#    summarize(buoy_freq = median(buoy_freq, na.rm=TRUE)) %>%
#    ungroup()


# combined

test = m10 %>%  #full_join(t1, t10) %>%
   full_join(m11) %>%
   full_join(m12) %>%
   full_join(m13) %>%
   full_join(m14) %>%
   full_join(m15) %>%
   full_join(m16) %>%
   full_join(m17) %>%
   full_join(m18) %>%
   full_join(m19)


test = test %>%
   mutate(treatment = case_when(pond_id %in% c("A", "B", "C") ~ "pulsed",
                                pond_id %in% c("D", "E", "F") ~ "reference")) %>%
   relocate(treatment, .after=pond_id) #%>%
   # left_join(t9 %>%
   #              mutate(treatment = case_when(.$pond_id=="B" ~ "pulsed",
   #                                           .$pond_id=="F" ~ "reference")) %>%
   #              select(-pond_id))


model_dataset = test %>%
   select(pond_id, treatment, doy, period, everything()) %>%
   arrange(pond_id, doy)


write_csv(model_dataset, file = "ghg-model-dataset_2022-04-30.csv")


## remove temporary individual data sets
rm(list = ls(pattern = "t[[:digit:]]"))
##

