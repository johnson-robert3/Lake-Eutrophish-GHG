#~~~
# Try using GAMs to investigate drivers of methane concentration/flux
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
      -contains("n2o"), -contains("co2"), -contains("flux"), 
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
cforest_ch4 = cforest(ch4_lake ~ ., 
                      data = gdat %>% filter(!(is.na(ch4_lake))),
                      controls = cforest_control(ntree = 25000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_ch4 = varimp(cforest_ch4)

# visualize
windows(height=6, width=12); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

## Most important variables: 
# pond id
# bottom chla
# bottom phyco
# pulse-period
# do sat
# foodweb trt
# sp cond
# salinity
# percent strat prev 24-hr


# Exclude pond A 
cforest_ch4_noA = cforest(ch4_lake ~ ., 
                      data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')),
                      controls = cforest_control(ntree = 25000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_ch4_noA = varimp(cforest_ch4_noA)

# visualize
windows(height=6, width=12); barplot(sort(abs(varimp_ch4_noA), decreasing=TRUE), las=2)

## Most important variables: 
# percent strat prev 24-hr
# doy
# pulse-period
# do sat
# sp cond
# salinity
# do


#- Use an 'unbiased' control forest

# All ponds
cforest_ch4 = cforest(ch4_lake ~ ., 
                      data = gdat %>% filter(!(is.na(ch4_lake))),
                      controls = cforest_unbiased())

# identify variables of importance from the forest
varimp_ch4 = varimp(cforest_ch4)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

## Most important variables: 


# Exclude pond A 
cforest_ch4 = cforest(ch4_lake ~ ., 
                      data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')),
                      controls = cforest_unbiased())

# identify variables of importance from the forest
varimp_ch4 = varimp(cforest_ch4)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

## Most important variables: 



#---
# GAMs
#---


#- Using all ponds

# no interactions
gam1 = gam(ch4_lake ~ s(bottom_chla) + s(bottom_phyco) +s(do_sat) + s(sp_cond) + 
              pond_id + pulse_period + trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake))), method = "REML")

windows(height=5, width=8); plot(gam1, residuals=T, pages=1, pch=1, shade=T)


# vars interact with nutrient treatment
gam2 = gam(ch4_lake ~ s(bottom_chla, by = treatment) + s(bottom_phyco, by = treatment) +s(do_sat, by = treatment) + s(sp_cond, by = treatment) + 
              pond_id + pulse_period + trt_fw + treatment, 
           data = gdat %>% filter(!(is.na(ch4_lake))), method = "REML")

windows(height=10, width=8); plot(gam2, pages=1, pch=1, shade=T)


# vars interact with food web treatment
gam3 = gam(ch4_lake ~ s(bottom_chla, by = trt_fw) + s(bottom_phyco, by = trt_fw) +s(do_sat, by = trt_fw) + s(sp_cond, by = trt_fw) + 
              pond_id + pulse_period + trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake))), method = "REML")

windows(height=10, width=12); plot(gam3, pages=1, pch=1, shade=T)


# vars interact with pond_id
# gam4 = gam(ch4_lake ~ s(bottom_chla, by = pond_id) + s(bottom_phyco, by = pond_id) +s(do_sat, by = pond_id) + s(sp_cond, by = pond_id) + 
#               pond_id + pulse_period + trt_fw, 
#            data = gdat %>% filter(!(is.na(ch4_lake))), method = "REML")
# 
# windows(height=10, width=12); par(mfrow=c(2, 6)); plot(gam4, pages=0, pch=1, shade=T)



#- Excluding Pond A

# no interactions
gam5 = gam(ch4_lake ~ s(perc_strat_prev24) + s(doy) + s(do_sat) + s(sp_cond) + 
              pulse_period, 
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), method = "REML")

windows(height=10, width=8); plot(gam5, residuals=T, pages=1, pch=1, shade=T)


# vars interact with nutrient treatment
gam6 = gam(ch4_lake ~ s(perc_strat_prev24, by = treatment) + s(doy, by = treatment) + s(do_sat, by = treatment) + s(sp_cond, by = treatment) + 
              pulse_period + treatment, 
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), method = "REML")

windows(height=10, width=12); plot(gam6, pages=1, pch=1, shade=T)


# vars interact with food web treatment
gam7 = gam(ch4_lake ~ s(perc_strat_prev24, by = trt_fw) + s(doy, by = trt_fw) + s(do_sat, by = trt_fw) + s(sp_cond, by = trt_fw) + 
              pulse_period + trt_fw, 
           data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), method = "REML")

windows(height=10, width=12); plot(gam7, pages=1, pch=1, shade=T)


# vars interact with pond_id
# gam8 = gam(ch4_lake ~ s(perc_strat_prev24, by = pond_id) + s(doy, by = pond_id) + s(do_sat, by = pond_id) + s(sp_cond, by = pond_id) +
#               pulse_period + pond_id,
#            data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')), method = "REML")
# 
# windows(height=10, width=8); par(mfrow=c(3,2)); plot(gam8, pages=0, pch=1, shade=T)




