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


# more recent, using full model dataset
fdat %>%
          filter(!(is.na(n2o_flux))) %>%
          # convert from mmol to umol
          mutate(n2o_flux = n2o_flux * 1000) %>%
          # add in blank rows for days without GHG sampling
          full_join(tibble('A' = c(146:240), 'B' = c(146:240), 'C' = c(146:240), 'D' = c(146:240), 'E' = c(146:240), 'F' = c(146:240)) %>%
                       pivot_longer(cols = everything(), names_to = "pond_id", values_to = "doy") %>%
                       mutate(doy = as.numeric(doy))) %>%
          group_by(pond_id) %>%
          arrange(doy, .by_group=TRUE) %>%
          # add Dates for the DOYs that have now been added
          mutate(date = as_date(doy, origin="2020-01-01"),
                 date = ymd(date)) %>%
          # linearly interpolate CH4 flux for days without measurements
          mutate(n2o_interp = zoo::na.approx(n2o_flux)) %>%
          # cumulative flux during experiment
          mutate(n2o_net = slide_dbl(n2o_interp, ~sum(.), .before=Inf)) %>%
          ungroup() %>%
          left_join(pond_data) %>%
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   slice_tail(n=1) %>% 
   ungroup() %>%
   group_by(trt_nutrients) %>%
   summarize(mean = mean(n2o_net), se = sd(n2o_net)/sqrt(n())) %>% 
   View



