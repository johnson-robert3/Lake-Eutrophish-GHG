#~~
# Script for analyzing the CO2 ghg data
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
MuMIn::r.squaredGLMM(update(c.full, method='REML'))

# Is the random effect significant (effect of repeated measures)?
g1 = gls(co2_lake ~ treatment + doy + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm + treatment:doy,
         data = mdat_co2, method="ML")
g2 = update(g1, correlation = corAR1(form = ~ 1|pond_id, value = ACF(g1, form = ~ 1|pond_id)[2,2]))

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
# DOY is the strongest predictor of dissolved CO2 conc., so allow it to vary by pond, since we aren't 
#  interested in whether the effect of time is different across ponds, but rather just between treatments
# lme4: (doy|pond_id)  # same as (1+doy|pond_id)
m1 = update(c.full, random = ~ doy | pond_id)

anova(m1, c.full)  # m1 is better

summary(update(m1, method='REML'))
MuMIn::r.squaredGLMM(update(m1, method='REML'))


# Compare only post-pulse time periods
m2 = update(m1, data = mdat_co2 %>% filter(period!="BASE"))

# anova(m1, m2)  # erroneous; cannot compare models with different n()


# Add interactions between treatment and all continuous variables
m3 = update(m1, .~ treatment * (doy + chla + NEP + R + alkalinity + bottom_do_sat + doc_ppm))

anova(m1, m3)  # m3 is better

summary(update(m3, method='REML'))
MuMIn::r.squaredGLMM(update(m3, method='REML'))  # lower R2m than m1, but much higher R2c than m1

# only alkalinity (and interaction) is significant
# interactions between treatment and most of the continuous variables also do not make ecological sense 

# only keep interactions for DOY and alkalinity with treatment
m4 = update(m1, .~ treatment * (doy + alkalinity) + chla + NEP + R + bottom_do_sat + doc_ppm)
anova(m3, m4)  # ns; m4 is better with lower AIC and lower DF (i.e., no need for all of the extra interaction terms)
summary(update(m4, method='REML'))
MuMIn::r.squaredGLMM(update(m4, method='REML'))  # similar R2's to m3

# remove DOC
m5 = update(m4, .~. - doc_ppm)
anova(m4, m5)  # ns
summary(update(m5, method='REML'))
MuMIn::r.squaredGLMM(update(m5, method='REML'))  # similar R2's

# remove NEP
m6 = update(m5, .~. - NEP)
anova(m5, m6)  # ns
summary(update(m6, method='REML'))
MuMIn::r.squaredGLMM(update(m6, method='REML'))  # similar R2's

# remove R
m7 = update(m6, .~. - R)
anova(m6, m7)  # ns
summary(update(m7, method='REML'))
MuMIn::r.squaredGLMM(update(m7, method='REML'))  # similar R2's

# remove Bottom DO
m8 = update(m7, .~. - bottom_do_sat)
anova(m7, m8)  # ns
summary(update(m8, method='REML'))
MuMIn::r.squaredGLMM(update(m8, method='REML'))  # similar R2's

# remove Chlorophyll
m9 = update(m8, .~. - chla)
anova(m8, m9)  # ns
summary(update(m9, method='REML'))
MuMIn::r.squaredGLMM(update(m9, method='REML'))  # slightly higher R2m, slightly lower R2c


## Should we keep some variables (e.g., NEP, Chla) that make ecological sense for their effect on dissolved CO2, even if they aren't significant in the model?
m10 = update(m1, .~ treatment * (doy + alkalinity + NEP + chla))
anova(m10, m9)  # ns
anova(m10, m1)  # same
anova(m10, m3)  # ns; m10 is better with lower AIC and lower DF

summary(update(m10, method='REML'))
MuMIn::r.squaredGLMM(update(m10, method='REML'))


# Keep NEP, but remove Chla (NEP accounts for metabolism of both the phytoplankton and the macrophytes)
m11 = update(m1, .~ treatment * (doy + alkalinity + NEP))
anova(m10, m11)  # ns; m11 is better with lower AIC and lower DF
anova(m11, m1)  # sig.; m11 is better
summary(update(m11, method='REML'))
MuMIn::r.squaredGLMM(update(m11, method='REML'))

# remove treatment:nep interaction
m13 = update(m11, .~. - treatment:NEP)
anova(m13, m11)  # ns; lower AIC and DF
summary(update(m13, method='REML'))
MuMIn::r.squaredGLMM(update(m13, method='REML'))


# # R instead of NEP
# m12 = update(m1, .~ treatment * (doy + alkalinity + R))
# anova(m11, m12)  # same
# summary(update(m12, method='REML'))
# MuMIn::r.squaredGLMM(update(m12, method='REML'))


## keep NEP and allow an interaction (NEP may affect CO2 differently between treatments, because macrophyte dynamics differed between treatments)
## keep chla, but without an interaction (effect of chla on CO2 should not differ between treatments)

m14 = update(m1, .~ treatment * (doy + alkalinity + NEP) + chla)
anova(m1, m14, m10, m11)  # m14 or m11 is best
anova(m14, m11)  # ns

summary(update(m14, method='REML'))
MuMIn::r.squaredGLMM(update(m14, method='REML'))

## I think that ecologically, model m14 makes the most sense


## Best Model - m14
co2.lme = lme(co2_lake ~ treatment * (doy + alkalinity + NEP) + chla,
              random = ~ doy | pond_id, correlation = corAR1(),
              mdat_co2, method='REML')

# output
summary(co2.lme)
broom.mixed::tidy(co2.lme)
# R2
MuMIn::r.squaredGLMM(co2.lme)


#-- Step 4: Visualize Model Output

# Residuals
ggResidpanel::resid_panel(co2.lme)

# Table of model results as a figure
sjPlot::tab_model(co2.lme, show.re.var=TRUE)

# Effect size and significance of fixed effects
sjPlot::plot_model(co2.lme, show.p=TRUE, show.values=TRUE)



