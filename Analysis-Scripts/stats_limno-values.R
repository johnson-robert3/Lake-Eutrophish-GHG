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


