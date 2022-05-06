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

# Run the 'stats_model-data.R' script to create the 'mdat' dataset



#-- Step 2: Build the full model

# start with the most complex (full) model
#  allow intercept to vary by pond (random effect) to account for the repeated-measures sampling
#  allow for temporal autocorrelation


## Initial Model
f1 = lme(n2o_lake ~ tn + nox + tp + srp + np_ratio + chla + NEP + R + bottom_do + doc_ppm + treatment * period,
         random = ~ 1 | pond_id, data = mdat, method="ML")

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
f5 = update(f4, random = ~ doy | pond_id)  # doesn't work

# anova(f4, f5)  # ns; f4 has lower AIC and lower DF


# the treatment:doy interaction is not sig., remove the interaction and allow a random slope of doy within pond
f6 = update(f4, .~. - treatment:doy, random = ~ doy | pond_id)  # doesn't work

# anova(f5, f6)  # ns
# anova(f4, f6)  # ns


## N2O Full Model
n.full = lme(n2o_lake ~ treatment * doy + tn + nox + tp + srp + np_ratio + chla + NEP + R + bottom_do + doc_ppm,
             random = ~ 1 | pond_id, 
             correlation = corAR1(),
             data = mdat, method="ML")

summary(update(n.full, method='REML'))
MuMIn::r.squaredGLMM(update(n.full, method='REML'))


#-- Step 3: Iteratively remove components and find best-fit model

# Is the random effect significant (effect of repeated measures)?
g1 = gls(n2o_lake ~ treatment * doy + tn + nox + tp + srp + np_ratio + chla + NEP + R + bottom_do + doc_ppm,
         data = mdat, method="ML")
g2 = update(g1, correlation = corAR1(form = ~ 1|pond_id, value = ACF(g1, form = ~ 1|pond_id)[2,2]))

anova(g1, g2)  # g2 is better
anova(g2, n.full)  # ns


# Pulsed ponds only - are the pulsed "periods" different from the pre-pulse (base) period?
p1 = update(f2, .~. - treatment - treatment:period, data = mdat %>% filter(treatment=="pulsed"), method="REML")
summary(p1)
# yes, pulse-periods 1 and 2 are significantly different from the pre-pulse (base) period. this makes sense looking at figures
#  of n2o concentration or flux over time. the changes don't line up with the nutrient addition dates, however, and DOY still
#  looks to be the better temporal variable


# Add interactions between treatment and all continuous variables
m1 = update(n.full, .~ treatment * (doy + tn + nox + tp + srp + np_ratio + chla + NEP + R + bottom_do + doc_ppm))

anova(m1, n.full)  # sig.; n.full has lower AIC, but m1 has higher loglik
summary(update(m1, method='REML'))  # sig. interactions between treatment and NEP and R


# keep only sig. interactions
m2 = update(n.full, .~ treatment * (NEP + R) + doy + tn + nox + tp + srp + np_ratio + chla + bottom_do + doc_ppm)

anova(m1, m2)  # ns
anova(m2, n.full)  # m2 is better
summary(update(m2, method='REML'))
MuMIn::r.squaredGLMM(update(m2, method='REML'))


# remove chlorophyll
m3 = update(m2, .~. - chla)
anova(m2, m3)  # ns
summary(update(m3, method='REML'))

# is NEP or R a better metabolic variable
m4.1 = update(m3, .~. - R)
m4.2 = update(m3, .~. - NEP)

anova(m4.1, m4.2)  # no difference
anova(m3, m4.1, m4.2)  # no difference

# keep both; NEP and R are each significant fixed effects (and sig. interactions with treatment)


# remove TP
m5 = update(m3, .~. - tp)
anova(m3, m5)  # ns
summary(update(m5, method='REML'))
MuMIn::r.squaredGLMM(update(m5, method='REML'))

# remove DOC
m6 = update(m5, .~. - doc_ppm)
anova(m5, m6)  # ns
summary(update(m6, method='REML'))
MuMIn::r.squaredGLMM(update(m6, method='REML'))

# remove N:P ratio (small effect size)
m7 = update(m6, .~. - np_ratio)
anova(m6, m7)  # ns
summary(update(m7, method='REML'))
MuMIn::r.squaredGLMM(update(m7, method='REML'))

# TN and NOx are not significant, but we would have expected these (at least NOx) to play a role; leave in the model


## Best Model - m7
n2o.lme = lme(n2o_lake ~ treatment * (NEP + R) + doy + tn + nox + bottom_do,
              random = ~ 1 | pond_id, 
              correlation = corAR1(),
              data = mdat, method="REML")

# output
summary(n2o.lme)
broom.mixed::tidy(n2o.lme)
# R2
MuMIn::r.squaredGLMM(n2o.lme)



#-- Step 4: Visualize Model Output

# Residuals
ggResidpanel::resid_panel(n2o.lme)

# Table of model results as a figure
sjPlot::tab_model(n2o.lme, show.re.var=TRUE, show.se=TRUE)

# Effect size and significance of fixed effects
sjPlot::plot_model(n2o.lme, show.p=TRUE, show.values=TRUE)


# Output model results as a table for plotting
mtab_n2o = coef(summary(n2o.lme)) %>% as.data.frame() %>% rownames_to_column(var="fixed.effect") %>% as_tibble()


