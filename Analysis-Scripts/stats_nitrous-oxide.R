#~~
# Script for analyzing the N2O ghg data
# By: R. Johnson
#~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if (!require(lme4)) install.packages('lme4'); library(lme4)
if (!require(nlme)) install.packages('nlme'); library(nlme)

# use MuMIn::r.squaredGLMM() to calculate R2 values from mixed effects models


#-- Step 1: Prepare the data

# Full data set
fdat = read_csv("Data/ghg-model-dataset_2022-05-01.csv")


# Data for the N2O model
mdat_n2o = fdat %>%
   # factor variables
   mutate(across(c(pond_id, treatment, period, period2), ~as.factor(.))) %>%
   # select desired variables
   select(
      pond_id:period2, 
      n2o_flux, n2o_lake, 
      R, NEP, 
      bottom_do, bottom_do_sat, temp, chla, alkalinity, doc_ppm
      , tp, srp, tn, nox, np_ratio
      ) %>%
   # only keep rows that have values for all variables
   filter(!(if_any(n2o_flux:last_col(), is.na))) %>%
   filter(!(is.na(n2o_lake))) %>%
   # add a "time" variable for autocorrelation
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup() %>%
   # force the reference treatment to be first (so "treatment" shows the effect of the pulsed treatment in model results)
   # mutate(treatment = fct_relevel(treatment, "reference"))
   # force the pulsed treatment for be first (so model results show effect of continuous variables for the pulsed treatment)
   mutate(treatment = fct_relevel(treatment, "pulsed"))



#-- Step 2: Build the full model

# start with the most complex (full) model
#  allow intercept to vary by pond (random effect) to account for the repeated-measures sampling
#  allow for temporal autocorrelation


## Initial Model
f1 = lme(n2o_lake ~ tn + nox + tp + srp + np_ratio + chla + NEP + R + bottom_do + doc_ppm + treatment + period + treatment:period,
         random = ~ 1 | pond_id, data = mdat_n2o, method="ML")

# Should the model include autocorrelation (AR(1))?
f2 = update(f1, correlation = corAR1())

anova(f1, f2)  # f2 is better


# Is 'period2' better than 'period'? (period2 also encompasses treatment, so remove both period and treatment variables)
f3 = update(f2, .~. - treatment - period - treatment:period + period2)

anova(f2, f3)  # f2 is better


# is DOY a better temporal variable than pulse-period?
f4 = update(f2, .~. - period - treatment:period + doy + treatment:doy)

anova(f2, f4)  # f4 is better

summary(update(f4, method='REML'))


# DOY has a strong effect on N2O, allow effect of DOY to vary within pond (random slope)
f5 = update(f4, random = ~ doy | pond_id)

anova(f4, f5)  # ns; f4 has lower AIC and lower DF


## N2O Full Model
n.full = lme(n2o_lake ~ treatment * doy + tn + nox + tp + srp + np_ratio + chla + NEP + R + bottom_do + doc_ppm,
             random = ~ 1 | pond_id, 
             correlation = corAR1(),
             data = mdat_n2o, method="ML")

summary(update(n.full, method='REML'))
MuMIn::r.squaredGLMM(update(n.full, method='REML'))


