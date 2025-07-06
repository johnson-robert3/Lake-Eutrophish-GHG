#~~~
# Random Forest models to identify variables of importance for nitrous oxide
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



