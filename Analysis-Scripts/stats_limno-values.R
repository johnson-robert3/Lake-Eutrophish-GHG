#~~~
# Summary statistics and values for pond nutrients
# By: Robert Johnson
#~~~


#--
# TN
#--

# Concentration values by treatment
limno_field_data %>%
   filter(!(is.na(tn))) %>%
   left_join(pond_data) %>%
   group_by(trt_nutrients) %>%
   summarize(min = min(tn),
             max = max(tn),
             mean = mean(tn),
             se = sd(tn)/sqrt(n())) %>%
   View

# Concentration values by treatment & period
limno_field_data %>%
   filter(!(is.na(tn))) %>%
   left_join(pond_data) %>%
   group_by(trt_nutrients, period) %>%
   summarize(min = min(tn),
             max = max(tn),
             mean = mean(tn),
             se = sd(tn)/sqrt(n())) %>%
   View


#--
# TP
#--

# Concentration values by treatment
limno_field_data %>%
   filter(!(is.na(tp))) %>%
   left_join(pond_data) %>%
   group_by(trt_nutrients) %>%
   summarize(min = min(tp),
             max = max(tp),
             mean = mean(tp),
             se = sd(tp)/sqrt(n())) %>%
   View

# Concentration values by treatment & period
limno_field_data %>%
   filter(!(is.na(tp))) %>%
   left_join(pond_data) %>%
   group_by(trt_nutrients, period) %>%
   summarize(min = min(tp),
             max = max(tp),
             mean = mean(tp),
             se = sd(tp)/sqrt(n())) %>%
   View


#--
# NOx
#--

# Concentration values by treatment
limno_field_data %>%
   filter(!(is.na(nox))) %>%
   # convert to ug/L
   mutate(nox = nox * 1000) %>%
   left_join(pond_data) %>%
   group_by(trt_nutrients) %>%
   summarize(min = min(nox),
             max = max(nox),
             mean = mean(nox),
             se = sd(nox)/sqrt(n())) %>%
   View

# Concentration values by treatment & period
limno_field_data %>%
   filter(!(is.na(nox))) %>%
   # convert to ug/L
   mutate(nox = nox * 1000) %>%
   left_join(pond_data) %>%
   group_by(trt_nutrients, period) %>%
   summarize(min = min(nox),
             max = max(nox),
             mean = mean(nox),
             se = sd(nox)/sqrt(n())) %>%
   View


#--
# Cumulative GHG flux
#--

# Tibble of all DOYs used for interpolating diffusive flux to estimate cumulative
all_doys = tibble(doy = 146:240,
                  pond_id = rep("A", length(doy))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("B", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("C", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("D", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("E", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("F", length(doy))))


### CO2
lake_flux %>%
   # add blank non-measurement days
   full_join(all_doys) %>%
   left_join(pond_data) %>%
   arrange(pond_id, doy) %>%
   group_by(pond_id) %>%
   # interpolate flux values for non-measurement days
   mutate(co2_interp = zoo::na.approx(co2_flux)) %>%
   # cumulative flux over summer
   mutate(cumm = slide_dbl(co2_interp, ~sum(.), .before=Inf)) %>%
   ungroup() %>%
   # treatment means at end
   filter(doy==240) %>%
   group_by(trt_nutrients) %>%
   summarize(mean(cumm)) %>%
   ungroup()


### CH4
lake_flux %>%
   # add blank non-measurement days
   full_join(all_doys) %>%
   left_join(pond_data) %>%
   arrange(pond_id, doy) %>%
   group_by(pond_id) %>%
   # interpolate flux values for non-measurement days
   mutate(ch4_interp = zoo::na.approx(ch4_flux)) %>%
   # cumulative flux over summer
   mutate(cumm = slide_dbl(ch4_interp, ~sum(.), .before=Inf)) %>%
   ungroup() %>%
   # treatment means at end
   filter(doy==240) %>% 
   group_by(trt_nutrients) %>%
   summarize(mean(cumm)) %>%
   ungroup()


### N2O
lake_flux %>%
   # add blank non-measurement days
   full_join(all_doys) %>%
   left_join(pond_data) %>%
   arrange(pond_id, doy) %>%
   # convert to umol
   mutate(n2o_flux = n2o_flux * 1000) %>%
   group_by(pond_id) %>%
   # interpolate flux values for non-measurement days
   mutate(n2o_interp = zoo::na.approx(n2o_flux)) %>%
   # cumulative flux over summer
   mutate(cumm = slide_dbl(n2o_interp, ~sum(.), .before=Inf)) %>%
   ungroup() %>%
   # treatment means at end
   filter(doy==240) %>% 
   group_by(trt_nutrients) %>%
   summarize(mean(cumm)) %>%
   ungroup()




