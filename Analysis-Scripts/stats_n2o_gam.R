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
   select(-date, -time, -contains("ch4"), -contains("co2"), -contains("flux"), -period, -period2)


#---
# Random Forest Models
#---

## Evaluate importance of driver variables

#- Use a control forest with manually specified parameters

# All ponds
cforest_n2o = cforest(n2o_lake ~ ., 
                      data = gdat,
                      controls = cforest_control(ntree = 20000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_n2o = varimp(cforest_n2o)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_n2o), decreasing=TRUE), las=2)

## Most important variables: 
# surface do
# doy
# surface do sat
# period
# bottom temp
# bottom do
#  <- break point
# NEP
# bottom do sat
# temp


# Exclude the 'intermediate' food web treatment (ponds A + D)
cforest_n2o = cforest(n2o_lake ~ ., 
                      data = gdat %>% filter(!(trt_fw=="medium")),
                      controls = cforest_control(ntree = 20000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_n2o = varimp(cforest_n2o)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_n2o), decreasing=TRUE), las=2)

## Most important variables: 
# doy
# surface do
#  <- break point
# surface do sat
# period
# bottom temp
# bottom do
#  <- break point
# bottom do sat
# temp



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





