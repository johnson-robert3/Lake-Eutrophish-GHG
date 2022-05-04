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

# Should the model include autocorrelation (AR(1))?
f2 = update(f1, correlation = corAR1())

anova(f1, f2)  # f2 is better


# Is 'period2' better than 'period'? (period2 also encompasses treatment, so remove both period and treatment variables)
f3 = update(f2, .~. - treatment - period - treatment:period + period2)

anova(f3, f2)  # f2 is better


# use DOY for time instead of period or period2?
f4 = update(f2, .~. - period - treatment:period + doy + treatment:doy)

anova(f4, f2)  # ns; f4 has slightly lower AIC and fewer DF; f2 has slightly higher loglik


## CO2 full model
c.full = lme(co2_lake ~ treatment + doy + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm + treatment:doy,
             random = ~ 1 | pond_id, 
             correlation = corAR1(),
             data = mdat_co2, method="ML")



#-- Step 3: Iteratively remove components and find best-fit model

summary(update(c.full, method='REML'))

# Is the random effect significant (effect of repeated measures)?
g1 = gls(co2_lake ~ treatment + doy + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm + treatment:doy,
         data = mdat_co2, method="ML")
g2 = update(g1, correlation = corAR1(form = ~ time|pond_id, value = ACF(g1, form = ~ time|pond_id)[2,2]))

anova(g1, g2)  # g2 is better
anova(g2, c.full)  # c.full is better


# Pulsed ponds only - are the pulsed "periods" different from the pre-pulse (base) period?
p1 = update(f2, .~. - treatment - treatment:period, data = mdat_co2 %>% filter(treatment=="pulsed"), method="REML")

summary(p1)  # pulse-period did not have a significant effect on CO2, which seems strange when looking at the figure...


# Use nutrient concentration instead of treatment to represent differences (need to recreate mdat_co2 df with adding nutrient variables)
t1 = lme(co2_lake ~ tn + tp + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm,
         random = ~1|pond_id, correlation = corAR1(), data = mdat_co2, method="REML")

summary(t1)


# Allow for a random effect (slope) of time within each pond
# DOY is the strongest predictor of CO2 conc., so allow it to vary, since we aren't interested in the effect of time as a driver
# lme4: (1+doy|pond_id)
m1 = update(c.full, random = ~ doy | pond_id)

anova(m1, c.full)  # m1 is better

summary(update(m1, method='REML'))


# Compare only post-pulse time periods
m2 = update(m1, data = mdat_co2 %>% filter(period!="BASE"))

anova(m1, m2)  # erroneous; cannot compare models with different n()


# Add interactions between treatment and all continuous variables
m3 = update(m1, .~ treatment * (doy + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm))

anova(m1, m3)  # m3 is better

summary(update(m3, method='REML'))

# only alkalinity (and interaction) is significant
# interactions between treatment and most of the continuous variables also do not make ecological sense 

# only keep interactions for DOY and alkalinity with treatment
m4 = update(m3, .~ treatment * (doy + alkalinity) + chla + NEP + R + bottom_do_sat + doc_ppm)
anova(m3, m4)  # ns; m4 is better (i.e., no need for all of the extra interaction terms)
summary(update(m4, method='REML'))

# remove DOC
m5 = update(m4, .~. - doc_ppm)
anova(m4, m5)  # ns
summary(update(m5, method='REML'))

# remove NEP
m6 = update(m5, .~. - NEP)
anova(m5, m6)  # ns
summary(update(m6, method='REML'))

# remove R
m7 = update(m6, .~. - R)
anova(m6, m7)  # ns
summary(update(m7, method='REML'))

# remove Bottom DO
m8 = update(m7, .~. - bottom_do_sat)
anova(m7, m8)  # ns
summary(update(m8, method='REML'))

# remove Chlorophyll
m9 = update(m8, .~. - chla)
anova(m8, m9)  # ns
summary(update(m9, method='REML'))

# R2
MuMIn::r.squaredGLMM(m9)
