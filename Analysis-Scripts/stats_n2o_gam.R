#~~~
# Try using GAMs to investigate drivers of nitrous oxide concentration/flux
# 
# By: R. Johnson
#~~~


library(party)  # conditional inference trees and random forests
library(mgcv)   # GAMs

set.seed(1987)


# Data
gdat = mdat %>%
   # select variables (i.e., remove unnecessary variables)
   select(
      -date, -week, 
      -contains("ch4"), -contains("co2"), -contains("flux"), 
      -period, -period2,  # using "pulse_period" variable instead
      -wind_speed,  # use wind_U10 instead
      -doc_ppb  # use doc_ppm
   )


#---
# Random Forest Models
#---

## Evaluate importance of driver variables

#- Use a control forest with manually specified parameters

# All ponds
cforest_n2o = cforest(n2o_lake ~ ., 
                      data = gdat %>% filter(!(is.na(n2o_lake))),
                      controls = cforest_control(ntree = 25000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_n2o = varimp(cforest_n2o)

# visualize
windows(height=6, width=12); barplot(sort(abs(varimp_n2o), decreasing=TRUE), las=2)

   ## Most important variables: 
   # do
   # doy
   # bottom ph
   # ph
   # do sat
   # bottom temp
   # (pulse period is lower, but is the first factor variable)


# Exclude pond A 
cforest_n2o_noA = cforest(n2o_lake ~ ., 
                      data = gdat %>% filter(!(is.na(n2o_lake)) & !(pond_id=='A')),
                      controls = cforest_control(ntree = 25000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_n2o_noA = varimp(cforest_n2o_noA)

# visualize
windows(height=6, width=12); barplot(sort(abs(varimp_n2o_noA), decreasing=TRUE), las=2)

   ## Most important variables: 
   # ph
   # bottom ph
   # doy
   # do
   # do sat
   # bottom do
   # bottom tmep
   # (pulse period is lower, but is the first factor variable)



#---
# GAMs
#---

#- Using all ponds

   ## Most important variables: 
   # do, doy, bottom ph, ph, bottom temp, (pulse period is lower, but is the first factor variable)

# no interactions
gam1 = gam(n2o_lake ~ 
              s(do) + 
              s(doy) + 
              s(bottom_ph) + 
              s(ph) + 
              s(bottom_temp) + 
              pulse_period,
           data = gdat, 
           method = 'REML')

windows(height=10, width=12); plot(gam1, residuals=T, pages=1, pch=1, shade=T, all.terms=T)

summary(gam1)


# vars interacting with nutrient treatment
gam2 = gam(n2o_lake ~ 
              s(do, by = treatment) + 
              s(doy, by = treatment) + 
              s(bottom_ph, by = treatment) + 
              s(ph, by = treatment) + 
              s(bottom_temp, by = treatment) + 
              pulse_period +
              treatment,
           data = gdat, 
           method = 'REML')

windows(height=10, width=12); plot(gam2, pages=1, shade=T, scale=0, all.terms=T)

summary(gam2)


# vars interacting with food web treatment
gam3 = gam(n2o_lake ~ 
              s(do, by = trt_fw) + 
              s(doy, by = trt_fw) + 
              s(bottom_ph, by = trt_fw) + 
              s(ph, by = trt_fw) + 
              s(bottom_temp, by = trt_fw) + 
              pulse_period +
              trt_fw,
           data = gdat, 
           method = 'REML')

windows(height=10, width=12); plot(gam3, pages=1, shade=T, scale=0, all.terms=T)

summary(gam3)


# vars interacting with pulse period
gam4 = gam(n2o_lake ~ 
              s(do, by = pulse_period) + 
              s(doy, by = pulse_period) + 
              s(bottom_ph, by = pulse_period) + 
              s(ph, by = pulse_period) + 
              s(bottom_temp, by = pulse_period) + 
              pulse_period,
           data = gdat, 
           method = 'REML')

windows(height=8, width=6); par(mfrow=c(3,2)); plot(gam4, pages=0, shade=T, scale=0, all.terms=T)

summary(gam4)





