
# Statistics to measure variability



## Coefficient of Variation before and after 1st pulse

before = lake_flux %>%
   filter(doy %in% c(169:176)) %>%
   group_by(pond_id) %>%
   summarize(across(ends_with("flux"), ~((sd(.)/mean(.))/100))) %>%
   ungroup()


after = lake_flux %>%
   filter(doy %in% c(177:184)) %>%
   group_by(pond_id) %>%
   summarize(across(ends_with("flux"), ~((sd(.)/mean(.))/100))) %>%
   ungroup()



## Coefficient of variation during each pulse period

pre = lake_flux %>%
   filter(doy <= 176) %>%
   group_by(pond_id) %>%
   summarize(across(ends_with("flux"), ~((sd(.)/mean(.))/100))) %>%
   ungroup() %>%
   mutate(period = rep(1, nrow(.)))


pulse1 = lake_flux %>%
   filter(doy %in% c(177:211)) %>%
   group_by(pond_id) %>%
   summarize(across(ends_with("flux"), ~((sd(.)/mean(.))/100))) %>%
   ungroup() %>%
   mutate(period = rep(2, nrow(.)))


pulse2 = lake_flux %>%
   filter(doy >= 212) %>%
   group_by(pond_id) %>%
   summarize(across(ends_with("flux"), ~((sd(.)/mean(.))/100))) %>%
   ungroup() %>%
   mutate(period = rep(3, nrow(.)))


cv_period = bind_rows(pre, pulse1, pulse2)

