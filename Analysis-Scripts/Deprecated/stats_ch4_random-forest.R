#~~~
# Random Forest models to identify variables of importance for methane
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


# #- Use an 'unbiased' control forest
# 
# # All ponds
# cforest_ch4 = cforest(ch4_lake ~ ., 
#                       data = gdat %>% filter(!(is.na(ch4_lake))),
#                       controls = cforest_unbiased())
# 
# # identify variables of importance from the forest
# varimp_ch4 = varimp(cforest_ch4)
# 
# # visualize
# windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)
# 
# ## Most important variables: 
# 
# 
# # Exclude pond A 
# cforest_ch4 = cforest(ch4_lake ~ ., 
#                       data = gdat %>% filter(!(is.na(ch4_lake)) & !(pond_id=='A')),
#                       controls = cforest_unbiased())
# 
# # identify variables of importance from the forest
# varimp_ch4 = varimp(cforest_ch4)
# 
# # visualize
# windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)
# 
# ## Most important variables: 

