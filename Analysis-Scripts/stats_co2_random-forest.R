#~~~
# Random Forest models to identify variables of importance for carbon dioxide
# 
# By: R. Johnson
#~~~


library(party)  # conditional inference trees and random forests

set.seed(1987)


# Data
gdat = mdat %>%
   # select variables (i.e., remove unnecessary variables)
   select(
      -date, -week, 
      -contains("n2o"), -contains("ch4"), -contains("flux"), 
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
cforest_co2 = cforest(co2_lake ~ ., 
                      data = gdat %>% filter(!(is.na(co2_lake))),
                      controls = cforest_control(ntree = 25000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_co2 = varimp(cforest_co2)

# visualize
windows(height=6, width=12); barplot(sort(abs(varimp_co2), decreasing=TRUE), las=2)

   ## Most important variables: 
   # sp cond
   # salinity
   # cond
   # doy
   # pulse-period
   # ph
   # pond id


# Exclude pond A 
cforest_co2_noA = cforest(co2_lake ~ ., 
                      data = gdat %>% filter(!(is.na(co2_lake)) & !(pond_id=='A')),
                      controls = cforest_control(ntree = 25000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_co2_noA = varimp(cforest_co2_noA)

# visualize
windows(height=6, width=12); barplot(sort(abs(varimp_co2_noA), decreasing=TRUE), las=2)

   ## Most important variables: 
   # sp cond
   # salinity
   # doy
   # pulse-period
   # cond
   # (break)
   # ph
   # zmix ghg time


