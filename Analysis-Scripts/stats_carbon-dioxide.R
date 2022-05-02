#~~
# Script for analyzing the CO2 ghg data
# By: R. Johnson
#~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if (!require(lme4)) install.packages('lme4'); library(lme4)
if (!require(nlme)) install.packages('nlme'); library(nlme)
# if (!require(lmerTest)) install.packages('lmerTest'); library('lmerTest')
# if (!require(sjPlot)) install.packages('sjPlot'); library(sjPlot)
# if (!require(MuMIn)) install.packages('MuMIn'); library(MuMIn)


#-- Step 1: Prepare the data

# Full data set
fdat = read_csv("Data/ghg-model-dataset_2022-05-01.csv")


# Data for the CO2 model
mdat_co2 = fdat %>%
   # factor variables
   mutate(across(c(pond_id, treatment, period, period2), ~as.factor(.))) %>%
   # select desired variables
   select(pond_id:period2, 
          co2_flux, co2_lake, 
          NEP, R, 
          # tp, tn, np_ratio,
          bottom_do, bottom_do_sat, temp, chla, alkalinity, doc_ppm) %>%
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



#-- Step 2: Build the model

# start with the full (most complex) model
#  allow the intercept to vary by pond (random effect) to account for the repeated-measures sampling
#  allow for temporal autocorrelation

f1 = lme(co2_lake ~ treatment + period + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm + treatment:period,
         random = ~ 1 | pond_id, data = mdat_co2, method="ML")

# add AR(1) autocorrelation
f2 = update(f1, correlation = corAR1(form = ~ time|pond_id, value = ACF(f1, form = ~ time|pond_id)[2,2]))

anova(f1, f2)  # f2 is better


# is the random effect for repeated measures significant?
g1 = gls(co2_lake ~ treatment + period + chla + NEP + R + alkalinity + bottom_do_sat + treatment:period,
         data = mdat_co2, method="ML")
g2 = update(g1, correlation = corAR1(form = ~ time|pond_id, value = ACF(g1, form = ~ time|pond_id)[2,2]))

anova(g2, f2)  # f2 is better


# is 'period2' better than 'period'? (period2 also encompasses treatment, so remove both period and treatment variables)
m1 = update(f1, .~. - period - treatment - treatment:period + period2)
m2 = update(m1, correlation = corAR1(form = ~ time|pond_id, value = ACF(m1, form = ~ time|pond_id)[2,2]))

anova(m2, f2)  # f2 is better


# pulsed ponds only, what is significant when only using treatment ponds, viewing effects of the pulses?
p1 = update(f1, .~. - treatment - treatment:period + alkalinity:period, data = mdat_co2 %>% filter(treatment=="pulsed"), method="REML")
p2 = update(p1, correlation = corAR1(form = ~ time|pond_id, value = ACF(p1, form = ~ time|pond_id)[2,2]))

summary(p2)



