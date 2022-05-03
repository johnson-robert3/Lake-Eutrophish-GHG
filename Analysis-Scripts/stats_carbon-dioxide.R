#~~
# Script for analyzing the CO2 ghg data
# By: R. Johnson
#~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if (!require(lme4)) install.packages('lme4'); library(lme4)
if (!require(nlme)) install.packages('nlme'); library(nlme)


#-- Step 1: Prepare the data

# Full data set
fdat = read_csv("Data/ghg-model-dataset_2022-05-01.csv")


# Data for the CO2 model
mdat_co2 = fdat %>%
   # factor variables
   mutate(across(c(pond_id, treatment, period, period2), ~as.factor(.))) %>%
   # select desired variables
   select(
      pond_id:period2, 
      co2_flux, co2_lake, 
      R, NEP, 
      bottom_do, bottom_do_sat, temp, chla, alkalinity, doc_ppm
      # , tp, srp, tn, nox, np_ratio
      ) %>%
   # only keep rows that have values for all variables
   filter(!(if_any(co2_flux:last_col(), is.na))) %>%
   # filter(!(is.na(co2_lake))) %>%
   # add a "time" variable for autocorrelation
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup() %>%
   # force the reference treatment to be first (so model results show effect of the pulse treatment)
   mutate(treatment = fct_relevel(treatment, "reference"))



#-- Step 2: Build the full model

# start with the most complex (full) model
#  allow intercept to vary by pond (random effect) to account for the repeated-measures sampling
#  allow for temporal autocorrelation


## Full Model
f1 = lme(co2_lake ~ chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm + treatment + period + treatment:period,
         random = ~ 1 | pond_id, data = mdat_co2, method="ML")

# should the model include autocorrelation (AR(1))?
f2 = update(f1, correlation = corAR1())

anova(f1, f2)  # f2 is better

# CO2 full model
c.full = f2


## Is 'period2' better than 'period'? (period2 also encompasses treatment, so remove both period and treatment variables)
m1 = update(c.full, .~. - treatment - period - treatment:period + period2)

anova(m1, c.full)  # c.full is better


## use DOY for time instead of period or period2?
m2 = update(c.full, .~. - period - treatment:period + doy + treatment:doy)

anova(m2, c.full)  # ns; m2 has slightly lower AIC and fewer DF; c.full has slightly higher loglik



#-- Step 3: Iteratively remove components and find best-fit model

# Is the random effect significant (effect of repeated measures)?
g1 = gls(co2_lake ~ chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm + treatment + period + treatment:period,
         data = mdat_co2, method="ML")
g2 = update(g1, correlation = corAR1(form = ~ time|pond_id, value = ACF(g1, form = ~ time|pond_id)[2,2]))

anova(g1, g2)  # g2 is better
anova(g2, f2)  # f2 is better



## Pulsed ponds only - are the pulsed "periods" different from the pre-pulse (base) period?
p1 = update(f1, .~. - treatment - treatment:period + alkalinity:period, data = mdat_co2 %>% filter(treatment=="pulsed"), method="REML")
p2 = update(p1, correlation = corAR1(form = ~ time|pond_id, value = ACF(p1, form = ~ time|pond_id)[2,2]))

summary(p2)


## use nutrient concentration instead of treatment to represent differences
# t1 = lme(co2_lake ~ tp + tn + NEP + R + chla + alkalinity + bottom_do_sat + doc_ppm,
#          random = ~1|pond_id, data = mdat_co2, method="REML")
# t1 = update(t1, correlation = corAR1(form = ~ time|pond_id, value = ACF(t1, form = ~ time|pond_id)[2,2]))
# 
# summary(t1)


## Compare only post-pulse time periods

m2 = update(f1, data = mdat_co2 %>% filter(period!="BASE"), method='ML')
m2 = update(m2, correlation = corAR1(form = ~ time|pond_id, value = ACF(m2, form = ~ time|pond_id)[2,2]))

# anova(m2, f2)  # erroneous; cannot compare models with different n()

# without including pulse-period as an effect
m3 = update(f1, .~. - period - treatment:period, data = mdat_co2 %>% filter(period!="BASE"), method='ML')
m3 = update(m3, correlation = corAR1(form = ~ time|pond_id, value = ACF(m3, form = ~ time|pond_id)[2,2]))

anova(m2, m3)  # m2 is better




