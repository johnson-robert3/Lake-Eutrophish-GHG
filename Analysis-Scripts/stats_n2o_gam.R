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



# Exclude pond A 
cforest_n2o_noA = cforest(n2o_lake ~ ., 
                      data = gdat %>% filter(!(is.na(n2o_lake)) & !(pond_id=='A')),
                      controls = cforest_control(ntree = 25000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_n2o_noA = varimp(cforest_n2o_noA)

# visualize
windows(height=6, width=12); barplot(sort(abs(varimp_n2o_noA), decreasing=TRUE), las=2)

## Most important variables: 




#---
# GAMs
#---

#- Using all ponds

# no interactions
gam1 = gam(n2o_lake ~ s(do) + s(doy) + s(bottom_temp) + s(bottom_do) + s(NEP) + s(temp) + period,
           data = gdat, method = 'REML')

windows(height=10, width=12); plot(gam1, residuals=T, pages=1, pch=1, shade=T)


# vars interacting with period
gam2 = gam(n2o_lake ~ s(do, by = period) + s(doy) + s(bottom_temp) + s(bottom_do, by = period) + s(temp, by = period) + period,
           data = gdat, method = 'REML')

windows(height=10, width=12); plot(gam2, pages=1, pch=1, shade=T, scale=0)


# vars interacting with nutrient treatment
gam3 = gam(n2o_lake ~ s(do, by = treatment) + s(doy) + s(bottom_temp, by = treatment) + s(bottom_do, by = treatment) + s(temp) + period + treatment,
           data = gdat, method = 'REML')

windows(height=10, width=12); plot(gam3, pages=1, pch=1, shade=T, scale=0)





