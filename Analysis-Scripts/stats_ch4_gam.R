


library(party)
library(mgcv)

set.seed(1987)


# conditional inference tree for CH4

cforest_ch4 = cforest(ch4_lake ~ 
                         treatment + period + period2 + doy +
                         GPP + R + NEP + 
                         bottom_do + bottom_do_sat + bottom_temp +
                         surface_do + surface_do_sat + temp + 
                         chla + alkalinity + doc_ppm + 
                         tp + srp + tn + nox + np_ratio, 
                      data = mdat,
                      controls = cforest_control(ntree = 20000, mincriterion = 0.9, trace = TRUE))


# ID variables of importance from the forest

varimp_ch4 = varimp(cforest_ch4)


# visualize

windows()
barplot(sort(abs(varimp_ch4), decreasing=TRUE), las=2)

# most important variables: surface-do-sat, surface-do, (surface) temp, treatment, period



# construct GAM using important variables

test = gam(ch4_lake ~ 
              s(surface_do_sat, by = treatment, k=9) + 
              s(temp, by = treatment, k=9) + 
              treatment,
           data = mdat %>% mutate(treatment = as.factor(treatment)), method = "REML")



windows(); plot(test, residuals=T, pages=1, pch=1, shade=T)

