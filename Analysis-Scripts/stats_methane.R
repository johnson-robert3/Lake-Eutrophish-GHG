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
fdat = read_csv("Data/ghg-model-dataset_2022-05-05.csv")


# Data for GHG models
mdat = fdat %>%
   # factor variables
   mutate(across(c(pond_id, treatment, period, period2), ~as.factor(.))) %>%
   # convert N2O flux and concentration data from units of micro-mole to nano-mole
   mutate(across(c(n2o_flux, n2o_lake), ~.*1000)) %>%
   # select desired variables
   select(pond_id:period2, 
          contains("_lake"), contains("_flux"),
          R, NEP, bottom_do, bottom_do_sat, 
          temp, chla, alkalinity, doc_ppm, 
          tp, srp, tn, nox, np_ratio) %>%
   # only keep rows that have values for all variables
   filter(!(if_any(where(is.numeric), is.na))) %>%
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


## Full Model
f1 = lme(ch4_lake ~ chla + NEP + R + bottom_do + doc_ppm + treatment + period + treatment:period,
         random = ~ 1 | pond_id, data = mdat, method="ML")

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
m.full = lme(ch4_lake ~ treatment * doy + chla + NEP + R + bottom_do + doc_ppm,
             random = ~ 1 | pond_id, 
             correlation = corAR1(),
             data = mdat, method="ML")



#-- Step 3: Iteratively remove components and find best-fit model

summary(update(m.full, method='REML'))
MuMIn::r.squaredGLMM(update(m.full, method='REML'))

# Is the random effect significant (effect of repeated measures)?
g1 = gls(ch4_lake ~ treatment * doy + chla + NEP + R + bottom_do + doc_ppm,
         data = mdat, method="ML")
g2 = update(g1, correlation = corAR1(form = ~ 1|pond_id, value = ACF(g1, form = ~ 1|pond_id)[2,2]))

anova(g1, g2)  # g2 is better
anova(g2, m.full)  # ns


# Pulsed ponds only - are the pulsed "periods" different from the pre-pulse (base) period?
p1 = update(f2, .~. - treatment - treatment:period, data = mdat %>% filter(treatment=="pulsed"), method="REML")

summary(p1)  # pulse-period did not have a significant effect on CH4, which seems strange when looking at the figure... (maybe visual diff. is driven by pond A?)



## DOY and the treatment:doy interaction are not significant in the full methane model (m.full)

## Use nutrient concentration instead of treatment to represent differences 

m1 = lme(ch4_lake ~ tn + tp + nox + srp + np_ratio + chla + NEP + R + bottom_do + doc_ppm,
         random = ~1|pond_id, correlation = corAR1(), data = mdat, method="ML")

anova(m1, m.full)  # ns
summary(update(m1, method="REML"))  # SRP is significant
MuMIn::r.squaredGLMM(update(m1, method='REML'))


# model using Phosphorus instead of "treatment" (i.e., remove nitrogen)
m2 = update(m1, .~. - tn - nox - np_ratio)
anova(m1, m2)  # ns; m2 is better with lower AIC and lower DF
summary(update(m2, method="REML"))
MuMIn::r.squaredGLMM(update(m2, method='REML'))

# transform p data
m3 = update(m2, .~. - tp + log(tp))
anova(m2, m3)  # no difference

# transform methane
m4 = update(m2, log(ch4_lake) ~.)
summary(update(m4, method="REML"))  # cannot compare with different response variables, but doesn't seem to be better than using raw ch4_lake

# add treatment back
m5 = lme(ch4_lake ~ treatment + tp + srp + chla + NEP + R + bottom_do + doc_ppm,
         random = ~1|pond_id, correlation = corAR1(), data = mdat, method="ML")
anova(m2, m5)  # ns
summary(update(m5, method="REML"))
MuMIn::r.squaredGLMM(update(m5, method='REML'))

# remove NEP (R is better metabolic variable here; much larger effect size)
m6 = update(m5, .~. - NEP)
anova(m5, m6)  # ns; NEP not needed

# is log-transforming TP needed?
m7 = update(m6, .~. - tp + log(tp))
anova(m6, m7)  # no difference

# Add interactions between treatment and continuous variables
m8 = update(m6, .~ treatment * (tp + srp + chla + R + bottom_do + doc_ppm))
anova(m6, m8)  # sig.; m8 is better
summary(update(m8, method="REML"))  # only SRP and DOC have sig. interactions with treatment
MuMIn::r.squaredGLMM(update(m8, method='REML'))

# remove ns interactions
m9 = update(m8, .~ treatment * (srp + doc_ppm) + tp + chla + R + bottom_do)
anova(m8, m9)  # ns; m9 is better
summary(update(m9, method="REML"))
MuMIn::r.squaredGLMM(update(m9, method='REML'))

# remove TP (very small effect size)
m10 = update(m9, .~. - tp)
anova(m9, m10)  # ns; m10 is better; TP does not improve the model
summary(update(m10, method="REML"))
MuMIn::r.squaredGLMM(update(m10, method='REML'))

# add N:P ratio
m11 = update(m10, .~. + np_ratio)
anova(m10, m11)  # ns; m10 is better
summary(update(m11, method="REML"))  # N:P ratio ns and has very small effect size
MuMIn::r.squaredGLMM(update(m11, method='REML'))


## I think m10 is the best model; bottom_do is not significant (p=0.06), but makes sense to leave in (had expected bottom_do to be a significant driver)


## Best Model - m10
ch4.lme = lme(ch4_lake ~ treatment * (srp + doc_ppm) + chla + R + bottom_do,
              random = ~ 1 | pond_id, 
              correlation = corAR1(),
              mdat, method='REML')

# output
summary(ch4.lme)
broom.mixed::tidy(ch4.lme)
# R2
MuMIn::r.squaredGLMM(ch4.lme)


#-- Step 4: Visualize Model Output

# Residuals
ggResidpanel::resid_panel(ch4.lme)

# Table of model results as a figure
sjPlot::tab_model(ch4.lme, show.re.var=TRUE)

# Effect size and significance of fixed effects
sjPlot::plot_model(ch4.lme, show.p=TRUE, show.values=TRUE)


# Output model results as a table for plotting
mtab_ch4 = coef(summary(ch4.lme)) %>% as.data.frame() %>% rownames_to_column(var="fixed.effect") %>% as_tibble()


