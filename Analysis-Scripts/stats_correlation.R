#~~~
# Analyses for correlations, regressions, and comparisons between variables
# By: Robert Johnson
#~~~


library(slider)


#--
# DEA vs mean weekly N2O flux
#--

# data
sdat = lake_flux %>%
   select(sample_id:doy, n2o_flux) %>%
   mutate(wday = wday(date, label=T)) %>%
   filter(!(week %in% c(5,6,7) & wday %in% c("Sun", "Tue", "Thu", "Sat"))) %>%
   group_by(pond_id, week) %>%
   summarize(n2o_flux = mean(n2o_flux)) %>%
   ungroup() %>%
   right_join(dea_rates)


# view data
plot(sdat$n2o_rate, sdat$n2o_flux)


# Spearman correlation

 # reference ponds
cor(x = sdat %>% filter(pond_id %in% c("D", "E", "F")) %>% pull(n2o_rate),
    y = sdat %>% filter(pond_id %in% c("D", "E", "F")) %>% pull(n2o_flux),
    method = "spearman")
 # pulsed ponds
cor(x = sdat %>% filter(pond_id %in% c("A", "B", "C")) %>% pull(n2o_rate),
    y = sdat %>% filter(pond_id %in% c("A", "B", "C")) %>% pull(n2o_flux),
    method = "spearman")


# Linear regression

 # reference ponds
summary(lm(n2o_flux ~ n2o_rate, sdat %>% filter(pond_id %in% c("D", "E", "F"))))
 # pulsed ponds
summary(lm(n2o_flux ~ n2o_rate, sdat %>% filter(pond_id %in% c("A", "B", "C"))))



#--
# Methanogenesis vs mean weekly CH4 flux
#--

# data
sdat = lake_flux %>%
   select(sample_id:doy, ch4_flux) %>%
   mutate(wday = wday(date, label=T)) %>%
   filter(!(week %in% c(5,6,7) & wday %in% c("Sun", "Tue", "Thu", "Sat"))) %>%
   group_by(pond_id, week) %>%
   summarize(ch4_flux = mean(ch4_flux)) %>%
   ungroup() %>%
   right_join(methano_rates)


# view data
plot(sdat$ch4_rate, sdat$ch4_flux)


# Spearman correlation

 # reference ponds
cor(x = sdat %>% filter(pond_id %in% c("D", "E", "F")) %>% pull(ch4_rate),
    y = sdat %>% filter(pond_id %in% c("D", "E", "F")) %>% pull(ch4_flux),
    method = "spearman")
 # pulsed ponds
cor(x = sdat %>% filter(pond_id %in% c("A", "B", "C")) %>% pull(ch4_rate),
    y = sdat %>% filter(pond_id %in% c("A", "B", "C")) %>% pull(ch4_flux),
    method = "spearman")


# Linear regression

 # reference ponds
summary(lm(ch4_flux ~ ch4_rate, sdat %>% filter(pond_id %in% c("D", "E", "F"))))
 # pulsed ponds
summary(lm(ch4_flux ~ ch4_rate, sdat %>% filter(pond_id %in% c("A", "B", "C"))))



#--
# DEA vs buoyancy frequency
#--

# data
sdat = test_hobo %>%
   group_by(pond_id, doy) %>%
   summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
   ungroup() %>%
   group_by(pond_id) %>%
   mutate(roll_buoy = slide_dbl(buoy_freq, mean, .before=7)) %>%
   ungroup() %>%
   mutate(trt = case_when(.$pond_id=="B" ~ "yes",
                          .$pond_id=="F" ~ "no")) %>%
   select(-pond_id) %>%
   right_join(dea_rates %>%
                 mutate(trt = case_when(.$pond_id %in% c("A", "B", "C") ~ "yes",
                                        .$pond_id %in% c("D", "E", "F") ~ "no")))


# view data
plot(sdat$roll_buoy, sdat$n2o_rate)



#--
# Methanogenesis vs buoyancy frequency
#--

# data
sdat = test_hobo %>%
   group_by(pond_id, doy) %>%
   summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
   ungroup() %>%
   group_by(pond_id) %>%
   mutate(roll_buoy = slide_dbl(buoy_freq, mean, .before=7)) %>%
   ungroup() %>%
   mutate(trt = case_when(.$pond_id=="B" ~ "yes",
                          .$pond_id=="F" ~ "no")) %>%
   select(-pond_id) %>%
   right_join(methano_rates %>%
                 mutate(trt = case_when(.$pond_id %in% c("A", "B", "C") ~ "yes",
                                        .$pond_id %in% c("D", "E", "F") ~ "no")))


# view data
plot(sdat$roll_buoy, sdat$ch4_rate)


# Spearman correlation

 # reference ponds
cor(x = sdat %>% filter(trt=="no") %>% pull(roll_buoy),
    y = sdat %>% filter(trt=="no") %>% pull(ch4_rate),
    method = "spearman")
 # pulsed ponds
cor(x = sdat %>% filter(trt=="yes") %>% pull(roll_buoy),
    y = sdat %>% filter(trt=="yes") %>% pull(ch4_rate),
    method = "spearman")


# Linear regression

 # reference ponds
summary(lm(ch4_rate ~ roll_buoy, sdat %>% filter(trt=="no")))
 # pulsed ponds
summary(lm(ch4_rate ~ roll_buoy, sdat %>% filter(trt=="yes")))





