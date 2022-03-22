
# Large dataset of all variables/data for GAMs and LMEs


# Needed data sets

# lake_flux  [co2, ch4, and n2o flux]
# metab_mle  [nep]
# limno_field_data  [nutrients]
# sonde_bottom  [SWI DO]
# sonde_surface  [Chla, temp]
# wind_k OR weather_data  [wind speed]
# dea_rates  [DEA N2O]
# methano_rates  [methanogenesis]
# hobo_mld  [buoyancy frequency]



## Prep the individual data sets

t1 = lake_flux %>%
   select(pond_id, doy, week, ends_with("flux"))


t10 = lake_conc %>%
   select(pond_id, doy, ends_with("lake"))


t2 = metab_mle %>%
   filter(!(GPP<0 | R>0))


t3 = limno_field_data %>%
   select(pond_id, doy, period, tp:nhx)


t4 = sonde_bottom %>%
   select(pond_id, doy, do, do_sat) %>%
   rename(bottom_do = do,
          bottom_do_sat = do_sat)


t5 = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   filter(vert_m > 0.05 & vert_m < 0.60) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=TRUE))) %>%
   ungroup() %>%
   select(pond_id, doy, temp, chla)


# t6 = wind_k %>%
#    select(doy, wnd, U10) %>%
#    rename(wind_speed = wnd,
#           wind_U10 = U10)

t6 = weather_data %>%
   mutate(U10 = wind_speed * ((10 / wind_z)^(1/7))) %>%
   select(doy, wind_speed, U10) %>%
   group_by(doy) %>%
   summarize(across(wind_speed:U10, ~mean(., na.rm=TRUE))) %>%
   ungroup() %>%
   rename(wind_U10 = U10)


t7 = dea_rates %>%
   select(pond_id, doy, n2o_rate) %>%
   rename(DEA = n2o_rate)


t8 = methano_rates %>%
   select(pond_id, doy, ch4_rate) %>%
   rename(methanogenesis = ch4_rate)


t9 = hobo_mld %>%
   mutate(doy = yday(date_time)) %>%
   group_by(pond_id, doy) %>%
   summarize(buoy_freq = median(buoy_freq, na.rm=TRUE)) %>%
   ungroup()


# combined

test = full_join(t1, t10) %>%
   full_join(t2) %>%
   full_join(t3) %>%
   full_join(t4) %>%
   full_join(t5) %>%
   full_join(t6) %>%
   full_join(t7) %>%
   full_join(t8)


test2 = test %>%
   mutate(treatment = case_when(.$pond_id %in% c("A", "B", "C") ~ "pulsed",
                                .$pond_id %in% c("D", "E", "F") ~ "reference")) %>%
   relocate(treatment, .after=pond_id) %>%
   left_join(t9 %>%
                mutate(treatment = case_when(.$pond_id=="B" ~ "pulsed",
                                             .$pond_id=="F" ~ "reference")) %>%
                select(-pond_id))


model_dataset = test2 %>%
   select(pond_id, treatment, doy, period, everything()) %>%
   arrange(pond_id, doy)


write_csv(model_dataset, file = "ghg-model-dataset_2022-01-10.csv")

