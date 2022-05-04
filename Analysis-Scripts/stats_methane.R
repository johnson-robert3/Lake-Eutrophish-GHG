#~~
# Script for analyzing the CH4 ghg data
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


# Data for the CH4 model
mdat_ch4 = fdat %>%
   # factor variables
   mutate(across(c(pond_id, treatment, period, period2), ~as.factor(.))) %>%
   # select desired variables
   select(
      pond_id:period2, 
      ch4_flux, ch4_lake, 
      R, NEP, 
      bottom_do, bottom_do_sat, temp, chla, alkalinity, doc_ppm
      # , tp, srp, tn, nox, np_ratio
      ) %>%
   # only keep rows that have values for all variables
   filter(!(if_any(ch4_flux:last_col(), is.na))) %>%
   # filter(!(is.na(ch4_lake))) %>%
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
f1 = lme(ch4_lake ~ chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm + treatment + period + treatment:period,
         random = ~ 1 | pond_id, data = mdat_ch4, method="ML")

# Should the model include autocorrelation (AR(1))?
f2 = update(f1, correlation = corAR1())

anova(f1, f2)  # f2 is better


# Is 'period2' better than 'period'? (period2 also encompasses treatment, so remove both period and treatment variables)
f3 = update(f2, .~. - treatment - period - treatment:period + period2)

anova(f2, f3)  # ns


# use DOY for time instead of period or period2?
f4 = update(f2, .~. - period - treatment:period + doy + treatment:doy)

anova(f2, f4)  # ns; f4 has lower AIC and lower DF; use f4, DOY makes more sense than pulse-period


## CH4 full model
m.full = lme(ch4_lake ~ treatment * doy + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm,
             random = ~ 1 | pond_id, 
             correlation = corAR1(),
             data = mdat_ch4, method="ML")



#-- Step 3: Iteratively remove components and find best-fit model

summary(update(m.full, method='REML'))
MuMIn::r.squaredGLMM(update(m.full, method='REML'))

# Is the random effect significant (effect of repeated measures)?
g1 = gls(ch4_lake ~ treatment * doy + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm,
         data = mdat_ch4, method="ML")
g2 = update(g1, correlation = corAR1(form = ~ 1|pond_id, value = ACF(g1, form = ~ 1|pond_id)[2,2]))

anova(g1, g2)  # g2 is better
anova(g2, m.full)  # ns


# Pulsed ponds only - are the pulsed "periods" different from the pre-pulse (base) period?
p1 = update(f2, .~. - treatment - treatment:period, data = mdat_ch4 %>% filter(treatment=="pulsed"), method="REML")

summary(p1)  # pulse-period did not have a significant effect on CH4, which seems strange when looking at the figure...





