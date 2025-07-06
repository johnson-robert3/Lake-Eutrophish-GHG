#~~~
# Try using GAMs to investigate drivers of methane concentration/flux
# 
# By: R. Johnson
#~~~


library(mgcv)   # GAMs


# Data
gdat = mdat %>%
   # select variables (i.e., remove unnecessary variables)
   select(
      -date, -week, 
      -contains("n2o"), -contains("co2"), -contains("flux"), 
      -period, -period2,  # using "pulse_period" variable instead
      -wind_speed,  # use wind_U10 instead
      -doc_ppb  # use doc_ppm
   )



#---
# GAMs
#---


#- Using all ponds

   ## Most important variables: 
   # pond id, bottom chla, bottom phyco, pulse-period, do sat, foodweb trt, sp cond, salinity, percent strat prev 24-hr


# no interactions
gam1 = gam(ch4_lake ~ 
              s(bottom_chla) + 
              s(bottom_phyco) +
              s(do_sat) + 
              s(sp_cond) + 
              s(perc_strat_prev24) + 
              pond_id + 
              pulse_period + 
              trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake))), 
           method = "REML")

windows(height=6, width=8); plot(gam1, residuals=T, pages=1, pch=1, shade=T)

summary(gam1)


# vars interact with nutrient treatment
gam2 = gam(ch4_lake ~ 
              s(bottom_chla, by = treatment) + 
              s(bottom_phyco, by = treatment) + 
              s(do_sat, by = treatment) + 
              s(sp_cond, by = treatment) + 
              s(perc_strat_prev24, by = treatment) + 
              pond_id + 
              pulse_period + 
              trt_fw + 
              treatment, 
           data = gdat %>% filter(!(is.na(ch4_lake))) %>% mutate(treatment = fct_relevel(treatment, "reference")), 
           method = "REML")

windows(height=10, width=8); plot(gam2, pages=1, pch=1, shade=T)

summary(gam2)


# vars interact with food web treatment
gam3 = gam(ch4_lake ~ 
              s(bottom_chla, by = trt_fw) + 
              s(bottom_phyco, by = trt_fw) + 
              s(do_sat, by = trt_fw) + 
              s(sp_cond, by = trt_fw) + 
              s(perc_strat_prev24, by = trt_fw) + 
              pond_id + 
              pulse_period + 
              trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake))) %>% mutate(trt_fw = fct_relevel(trt_fw, "low", "medium", "high")), 
           method = "REML")

windows(height=10, width=12); plot(gam3, pages=1, pch=1, shade=T, scale=0)

summary(gam3)


# vars interact with pond_id
gam4 = gam(ch4_lake ~ 
              s(bottom_chla, by = pond_id) + 
              s(bottom_phyco, by = pond_id) + 
              s(do_sat, by = pond_id) + 
              s(sp_cond, by = pond_id) + 
              s(perc_strat_prev24, by = pond_id) + 
              pond_id + 
              pulse_period + 
              trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake))), 
           method = "REML")

windows(height=6, width=12); par(mfrow=c(2, 6)); plot(gam4, pages=0, pch=1, shade=T, scale=0)

summary(gam4)


# set variable interactions based on best fit from models
gam10 = gam(ch4_lake ~ 
              s(bottom_chla, by = pond_id) + 
              s(bottom_phyco, by = trt_fw) + 
              s(do_sat, by = trt_fw) + 
              s(sp_cond, by = trt_fw) + 
              s(perc_strat_prev24, by = pond_id) + 
              pond_id + 
              pulse_period + 
              trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake))) %>% mutate(trt_fw = fct_relevel(trt_fw, "low", "medium", "high")), 
           method = "REML")

windows(height=6, width=12); par(mfrow=c(2, 6)); plot(gam10, pages=0, pch=1, shade=T, scale=0)

summary(gam10)  # Pond A is still really driving a lot of the significance in variables 



#- Excluding Pond A

   ## Most important variables: 
   # percent strat prev 24-hr, doy, pulse-period, do sat, sp cond, salinity


# no interactions
gam5 = gam(ch4_lake ~ 
              s(perc_strat_prev24) + 
              s(doy) + 
              s(do_sat) + 
              s(sp_cond) + 
              pulse_period, 
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), 
           method = "REML")

windows(height=8, width=10); plot(gam5, residuals=T, pages=1, pch=1, shade=T, all.terms=T)

summary(gam5)


# vars interact with nutrient treatment
gam6 = gam(ch4_lake ~ 
              s(perc_strat_prev24, by = treatment) + 
              s(doy, by = treatment) + 
              s(do_sat, by = treatment) + 
              s(sp_cond, by = treatment) + 
              pulse_period + 
              treatment, 
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')) %>% mutate(treatment = fct_relevel(treatment, "reference")), 
           method = "REML")

windows(height=10, width=12); plot(gam6, pages=1, shade=T, all.terms=T)
windows(); plot(gam6, shade=T, pages=1, select=c(1,2))

summary(gam6)


# vars interact with food web treatment
gam7 = gam(ch4_lake ~ 
              s(perc_strat_prev24, by = trt_fw) + 
              s(doy, by = trt_fw) + 
              s(do_sat, by = trt_fw) + 
              s(sp_cond, by = trt_fw) + 
              pulse_period + 
              trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')) %>% mutate(trt_fw = fct_relevel(trt_fw, "low", "medium", "high")), 
           method = "REML")

windows(height=10, width=12); plot(gam7, pages=1, shade=T, all.terms=T)

summary(gam7)


# vars interact with pond_id
gam8 = gam(ch4_lake ~ 
              s(perc_strat_prev24, by = pond_id) + 
              s(doy, by = pond_id) + 
              s(do_sat, by = pond_id) + 
              s(sp_cond, by = pond_id) + 
              pulse_period + 
              pond_id,
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), 
           method = "REML")

windows(height=8, width=6); par(mfrow=c(3,2)); plot(gam8, pages=0, shade=T, scale=0)

summary(gam8)


# vars interact with pulse-period
gam9 = gam(ch4_lake ~ 
              s(perc_strat_prev24, by = pulse_period) + 
              s(doy, by = pulse_period) + 
              s(do_sat, by = pulse_period) + 
              s(sp_cond, by = pulse_period) + 
              pulse_period,
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), 
           method = "REML")

windows(height=8, width=6); par(mfrow=c(3,2)); plot(gam9, pages=0, shade=T, scale=0)

summary(gam9)


# set variable interactions based on best fit from models
gam20 = gam(ch4_lake ~ 
               s(perc_strat_prev24, by = treatment) + 
               s(doy, by = pulse_period) + 
               s(do_sat, by = pulse_period) + 
               s(sp_cond, by = pulse_period) + 
               pulse_period +
               treatment +
               pond_id,
            data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), 
            method = "REML")

windows(height=10, width=8); par(mfrow=c(3,2)); plot(gam20, pages=0, shade=T)

summary(gam20)


