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
   # add food web treatment to data
   left_join(pond_data %>%
                select(pond_id, trt_fw = trt_fish) %>%
                mutate(pond_id = as_factor(pond_id),
                       trt_fw = as_factor(trt_fw))) %>%
   # select variables (i.e., remove unnecessary variables)
   select(-date, -time, -contains("n2o"), -contains("co2"), -contains("flux"))


#---
# Random Forest Models
#---

## Evaluate importance of driver variables

#- Use a control forest with manually specified parameters

# All ponds
cforest_ch4 = cforest(ch4_lake ~ ., 
                      data = gdat,
                      controls = cforest_control(ntree = 20000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_ch4 = varimp(cforest_ch4)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

## Most important variables: 
# pond_id
# surface-do-sat
# trt_fw
# surface-do
# period
# treatment


# Exclude the 'intermediate' food web treatment (ponds A + D)
cforest_ch4 = cforest(ch4_lake ~ ., 
                      data = gdat %>% filter(!(trt_fw=="medium")),
                      controls = cforest_control(ntree = 20000, mincriterion = 0.9, trace = TRUE))

# identify variables of importance from the forest
varimp_ch4 = varimp(cforest_ch4)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

## Most important variables: 
# period
# doy
# surface_do_sat
#  <- (break point here)
# chla
# surface_do
# pond_id
#  <- (break point here)
# trt_fw
# NEP
# alkalinity
# srp



#- Use an 'unbiased' control forest

# All ponds
cforest_ch4 = cforest(ch4_lake ~ ., 
                      data = gdat,
                      controls = cforest_unbiased())

# identify variables of importance from the forest
varimp_ch4 = varimp(cforest_ch4)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

## Most important variables: 
# pond_id
# surface_do_sat
# trt_fw
# surface_do
#  <- (large break here) (all vars similar and linearly declining after this)
# doy
# period
# temp
# treatment


# Exclude the 'intermediate' food web treatment (ponds A + D)
cforest_ch4 = cforest(ch4_lake ~ ., 
                      data = gdat %>% filter(!(trt_fw=="medium")),
                      controls = cforest_unbiased())

# identify variables of importance from the forest
varimp_ch4 = varimp(cforest_ch4)

# visualize
windows(height=6, width=8); barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

## Most important variables: 
# period
# doy
# surface_do_sat
#  <- (large break here)
# chla
# trt_fw
# NEP
# surface_do
# srp



#---
# GAMs
#---

## Important variables

# 'period' and 'surface_do_sat' are important in all forests (no need to use both 'surface_do_sat' and 'surface_do')
# 'pond_id', 'treatment', and 'temp' are important if Pond A is included
# 'doy', 'chla', and 'pond_id' are important if intermediate food web is excluded (i.e., Pond A)
# 'NEP', 'srp', and 'alkalinity' are also important if Pond A is excluded (but less than doy, chla, and pond_id)


#- Using all ponds

# no interactions
gam1 = gam(ch4_lake ~ s(surface_do_sat) + s(temp) + treatment + period + pond_id, 
           data = gdat, method = "REML")

windows(height=5, width=8); plot(gam1, residuals=T, pages=1, pch=1, shade=T)


# vars interact with nutrient treatment
gam2 = gam(ch4_lake ~ s(surface_do_sat, by = treatment) + s(temp, by = treatment) + treatment + period + pond_id,
           data = gdat, method = "REML")

windows(height=10, width=8); plot(gam2, pages=1, pch=1, shade=T)


# vars interact with food web treatment
gam3 = gam(ch4_lake ~ s(surface_do_sat, by = trt_fw) + s(temp, by = trt_fw) + trt_fw + period + pond_id,
           data = gdat, method = "REML")

windows(height=10, width=12); plot(gam3, pages=1, pch=1, shade=T)


# vars interact with pond_id
gam4 = gam(ch4_lake ~ s(surface_do_sat, by = pond_id) + s(temp, by = pond_id) + period + pond_id,
           data = gdat, method = "REML")

windows(height=10, width=12); par(mfrow=c(2, 6)); plot(gam4, pages=0, pch=1, shade=T)



#- Excluding intermediate food web (Ponds A + D)

# no interactions
gam5 = gam(ch4_lake ~ s(surface_do_sat) + s(log(chla+1)) + s(doy) + s(NEP) + period + pond_id, 
           data = gdat %>% filter(!(trt_fw=="medium")), method = "REML")

windows(height=10, width=8); plot(gam5, residuals=T, pages=1, pch=1, shade=T)


# vars interact with period
gam6 = gam(ch4_lake ~ s(surface_do_sat, by = period) + s(log(chla+1), by = period) + s(doy, by = period) + period + pond_id, 
           data = gdat %>% filter(!(trt_fw=="medium")), method = "REML")

windows(height=10, width=12); plot(gam6, pages=1, pch=1, shade=T)


# vars interact with pond_id
gam7 = gam(ch4_lake ~ s(surface_do_sat, by = pond_id) + s(log(chla+1), by = pond_id) + s(doy, by = pond_id) + period + pond_id, 
           data = gdat %>% filter(!(trt_fw=="medium")), method = "REML")

windows(height=10, width=12); plot(gam7, pages=1, pch=1, shade=T)


# vars interact with food web treatment
gam8 = gam(ch4_lake ~ s(surface_do_sat, by = trt_fw) + s(log(chla+1), by = trt_fw) + s(doy, by = trt_fw) + period + trt_fw, 
           data = gdat %>% filter(!(trt_fw=="medium")), method = "REML")

windows(height=10, width=8); par(mfrow=c(3,2)); plot(gam8, pages=0, pch=1, shade=T)




