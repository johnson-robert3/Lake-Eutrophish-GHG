# More visualization ideas: https://lmudge13.github.io/sample_code/mixed_effects.html

#Install package, if needed, and load
if (!require(lmerTest)) install.packages('lmerTest')
library('lmerTest')
if (!require(sjPlot)) install.packages('sjPlot')
library(sjPlot)
#
if (!require(lme4)) install.packages('lme4')
library(lme4)
if (!require(nlme)) install.packages('nlme')
library(nlme)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(MuMIn)) install.packages('MuMIn')
library(MuMIn)


# use MuMIn::r.squaredGLMM() to calculate R2 values from mixed effects models


# full data set
fdat = read_csv("ghg-model-dataset.csv")
fdat = read_csv("ghg-model-dataset_2022-01-10.csv")


#==================#
#     CO2 FLUX     #
#==================#
#=====GETTING STARTED
#Read in the data
# flux = read.csv("co2_noNA.csv")
# flux$period<-as.factor(flux$period) #make a new column with period as a factor

# data for this model
mdat = fdat %>%
   mutate(np_ratio = (tn/14.001) / ((tp/1000)/30.97)) %>%
   select(pond_id:period, co2_lake, NEP, np_ratio, tp, srp, tn, nox, bottom_do_sat, temp, chla, buoy_freq) %>%
   filter(across(co2_lake:buoy_freq, ~!(is.na(.)))) %>%
   # force the reference treatment to be first (so model results show effect of the pulse treatment)
   arrange(desc(treatment), pond_id, doy) %>%
   mutate(treatment = as_factor(treatment),
          period = as_factor(period))

#=====STEP 1: MIXED EFFECTS MODEL
# mod_mixed<-lmerTest::lmer(co2_flux ~ buoy_freq + (1|pond_id), data=flux, na.action="na.fail", REML=F)
# summary(mod_mixed)

# mod1 = lmer(co2_lake ~ treatment + buoy_freq + chla + NEP + np_ratio + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod1)

# mod2 = lmer(co2_lake ~ treatment + buoy_freq + chla + NEP + np_ratio + (1|pond_id), data = mdat, REML=F)
# summary(mod2)

# mod3 = lmer(co2_lake ~ treatment + buoy_freq + chla + NEP + log(tp) + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod3)

# mod4 = lmer(co2_lake ~ period + buoy_freq + log(tp) + (1|pond_id), data = mdat, REML=F)
# summary(mod4)

# mod5 = lmer(co2_lake ~ period + buoy_freq + log(tp) + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod5)

# mod6 = lmer(co2_lake ~ treatment + period + buoy_freq + log(tp) + (1|pond_id), data = mdat, REML=F)
# summary(mod6)

mod7 = lmer(co2_lake ~ treatment * period + buoy_freq + log(tp) + (1|pond_id), data = mdat, REML=F)
summary(mod7)

# mod8 = lmer(co2_lake ~ treatment + buoy_freq + log(tp) + (1|pond_id) + (0+treatment|period), data = mdat, REML=F)
# summary(mod8)

# mod9 = lmer(co2_lake ~ treatment + buoy_freq + log(tp) + (1|pond_id), data = mdat, REML=F)
# summary(mod9)

# mod10 = lmer(co2_lake ~ treatment * period + buoy_freq + log(tp) + (1|pond_id) + (0+treatment|pond_id), data = mdat, REML=F)
# summary(mod10)


# remove "Period" as a fixed effect
mod11 = lmer(co2_lake ~ treatment * log(tp) + buoy_freq + (1|pond_id), data = mdat, REML=F)
summary(mod11)

mod12 = lmer(co2_lake ~ treatment + log(tp) + buoy_freq + treatment:log(tp) + NEP  + (1|pond_id), data = mdat, REML=F)
summary(mod12)

# mod13 = lmer(co2_lake ~ treatment + log(tp) + chla + buoy_freq + NEP + treatment:log(tp) + treatment:chla + treatment:NEP + (1|pond_id), data = mdat, REML=F)
# summary(mod13)


#Calculate the variance explained by the model (R-squared)
# rsq(mdat$co2_lake, predict(mod7)) 
r.squaredGLMM(mod7)

#Plot the Effect Sizes
sjPlot::plot_model(mod7, 
                   # axis.labels=c("Treatment * Period \n(Pulse2)", "Treatment * Period \n(Pulse1)", "log(TP)",
                   #               "Buoyancy \nFrequency", "Period \n(Pulse2)", "Period \n(Pulse1)", "Treatment"),
                   show.values=TRUE, show.p=TRUE,
                   title = expression(Effect~on~CO[2]~Flux))

#Make a pretty table
sjPlot::tab_model(mod7,
                  # pred.labels = c("(Intercept)", "Treatment", "Period (Pulse1)", "Period (Pulse2)", "Buoyancy Frequency",
                  #                 "log(TP)", "Treatment * Period (Pulse1)", "Treatment * Period (Pulse2)"),
                  show.re.var= TRUE, 
                  dv.labels= "Effects on CO2 Flux")


#=====STEP 2: COMPARE TO NULL MODEL
# Null model does NOT allow intercept to vary by lake
mod_null<-lm(co2_lake ~ treatment * period + buoy_freq + log(tp), data = mdat)
summary(mod_null)

#Compare the mixed model to the null model
anova(mod7, mod_null) 
#A p<0.05 means that the two models are different from each other

# output model results for plotting
mod_co2 = summary(mod7)$coefficients %>% as.data.frame() %>% rownames_to_column(var="fixed.effect") %>% as_tibble()


#==================#
#     CH4 FLUX     #
#==================#
#=====GETTING STARTED
#Read in the data
# flux = read.csv("ch4_noNA_phys.csv")
# flux$period<-as.factor(flux$period) #make a new column with period as a factor

# data for this model
mdat = fdat %>%
   mutate(np_ratio = (tn/14.001) / ((tp/1000)/30.97)) %>%
   select(pond_id:period, ch4_lake, tp, tn, np_ratio, bottom_do, temp, chla, wind_speed, buoy_freq) %>%
   filter(across(ch4_lake:buoy_freq, ~!(is.na(.)))) %>%
   # force the reference treatment to be first (so model results show effect of the pulse treatment)
   arrange(desc(treatment), pond_id, doy) %>%
   mutate(treatment = as_factor(treatment),
          period = as_factor(period))


#=====STEP 1: MIXED EFFECTS MODEL
# mod_mixed<-lmerTest::lmer(ch4_flux ~ bottom_do + period + (1|pond_id), data=flux, na.action="na.fail", REML=F)
# summary(mod_mixed)

# mod1 = lmer(ch4_lake ~ treatment + bottom_do + buoy_freq + np_ratio + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod1)

# mod2 = lmer(ch4_lake ~ treatment + bottom_do + buoy_freq + np_ratio + (1|pond_id), data = mdat, REML=F)
# summary(mod2)

# mod3 = lmer(ch4_lake ~ treatment + bottom_do + buoy_freq + log(tp) + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod3)

# mod4 = lmer(ch4_lake ~ treatment + bottom_do + buoy_freq + log(tp) + (1|pond_id), data = mdat, REML=F)
# summary(mod4)

# mod5 = lmer(ch4_lake ~ period + bottom_do + buoy_freq + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod5)

# mod6 = lmer(ch4_lake ~ period + bottom_do + buoy_freq + (1|pond_id), data = mdat, REML=F)
# summary(mod6)

# mod7 = lmer(ch4_lake ~ period + bottom_do + (1|pond_id), data = mdat, REML=F)
# summary(mod7)

# mod8 = lmer(ch4_lake ~ period + bottom_do + doy + (1|pond_id) + (0+doy|pond_id), data = mdat, REML=F)
# summary(mod8)

# mod9 = lmer(ch4_lake ~ period + bottom_do + doy + (1|pond_id), data = mdat, REML=F)
# summary(mod9)

# mod10 = lmer(ch4_lake ~ treatment + period + bottom_do + (1|pond_id), data = mdat, REML=F)
# summary(mod10)

mod11 = lmer(ch4_lake ~ treatment * period + bottom_do + (1|pond_id), data = mdat, REML=F)
summary(mod11)

# mod12 = lmer(ch4_lake ~ treatment + bottom_do + (1|pond_id) + (0+treatment|period), data = mdat, REML=F)
# summary(mod12)

# mod13 = lmer(ch4_lake ~ treatment * period + bottom_do + wind_speed + (1|pond_id), data = mdat, REML=F)
# summary(mod13)

# mod14 = lmer(ch4_lake ~ treatment * period + bottom_do + buoy_freq + (1|pond_id), data = mdat, REML=F)
# summary(mod14)


# remove "Period" as a fixed effect
mod15 = lmer(ch4_lake ~ treatment * bottom_do + (1|pond_id), data = mdat, REML=F)
summary(mod15)


#Calculate the variance explained by the model (R-squared)
# rsq(mdat$ch4_lake, predict(mod11)) 
r.squaredGLMM(mod11)

#Plot the Effect Sizes
sjPlot::plot_model(mod11, 
                   # axis.labels = c("Treatment * Period \n(Pulse2)", "Treatment * Period \n(Pulse1)", "Bottom  \nWater DO",
                   #                 "Period \n(Pulse2)", "Period \n(Pulse1)", "Treatment"),
                   show.values=TRUE, show.p=TRUE,
                   title=expression(Effect~on~CH[4]~Flux))

#Make a pretty table
sjPlot::tab_model(mod11, 
                  # pred.labels = c("(Intercept)", "Treatment", "Period (Pulse1)", "Period (Pulse2)", "Bottom Water DO",
                  #                 "Treatment * Period (Pulse1)", "Treatment * Period (Pulse2)"),
                  show.re.var= TRUE, 
                  dv.labels= "Effects on CH4 Flux")


#=====STEP 2: COMPARE TO NULL MODEL
# Null model does NOT allow intercept to vary by lake
mod_null<-lm(ch4_lake ~ treatment * period + bottom_do, data=mdat)
summary(mod_null)

#Compare the mixed model to the null model
anova(mod11, mod_null) 
#A p<0.05 means that the two models are different from each other

# output model results for plotting
mod_ch4 = summary(mod11)$coefficients %>% as.data.frame() %>% rownames_to_column(var="fixed.effect") %>% as_tibble()


#==================#
#     N2O FLUX     #
#==================#
#=====GETTING STARTED
#Read in the data
# flux = read.csv("n2o_noNA_DEA.csv")
# flux$period<-as.factor(flux$period) #make a new column with period as a factor

# data for this model
mdat = fdat %>%
   mutate(np_ratio = (tn/14.001) / ((tp/1000)/30.97),
          n2o_lake = n2o_lake * 1000) %>%
   select(pond_id:period, n2o_lake, tp, tn, np_ratio, nox, bottom_do, temp, wind_speed, buoy_freq) %>%
   filter(across(n2o_lake:buoy_freq, ~!(is.na(.)))) %>%
   # force the reference treatment to be first (so model results show effect of the pulse treatment)
   arrange(desc(treatment), pond_id, doy) %>%
   mutate(treatment = as_factor(treatment),
          period = as_factor(period))


# mean weekly values to include DEA
wdat = fdat %>%
   filter(!(doy<146 | doy>240)) %>%
   fill(week) %>%
   mutate(period = as_factor(period),
          np_ratio = (tn/14.001) / ((tp/1000)/30.97),
          n2o_lake = n2o_lake * 1000,
          DEA = DEA * 1000) %>%
   select(pond_id:week, n2o_lake, tp, tn, nox, bottom_do, temp, wind_speed, buoy_freq, DEA) %>%
   group_by(pond_id, week) %>%
   summarize(across(n2o_lake:DEA, ~mean(., na.rm=T))) %>%
   ungroup() %>%
   mutate(period = case_when(.$week %in% c(1,2,3,4) ~ "BASE",
                             .$week %in% c(5,6,7,8,9) ~ "PULSE1",
                             .$week %in% c(10,11,12,13,14) ~ "PULSE2"))


#=====STEP 1: MIXED EFFECTS MODEL
# mod_mixed<-lmerTest::lmer(n2o_flux ~ doy + (1|pond_id), data=flux, na.action="na.fail", REML=F)
# summary(mod_mixed)

# mod1 = lmer(n2o_lake ~ treatment + nox + bottom_do + buoy_freq + (1|pond_id), data = mdat, REML=F)
# summary(mod1)

# mod2 = lmer(n2o_lake ~ period + nox + bottom_do + buoy_freq + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod2)

# mod3 = lmer(n2o_lake ~ period + nox + bottom_do + buoy_freq + (1|pond_id), data = mdat, REML=F)
# summary(mod3)

# mod4 = lmer(n2o_lake ~ period + nox + bottom_do + (1|pond_id) + (0+period|treatment), data = mdat, REML=F)
# summary(mod4)

# mod5 = lmer(n2o_lake ~ period + nox + bottom_do + (1|pond_id), data = mdat, REML=F)
# summary(mod5)

# mod6 = lmer(n2o_lake ~ period + nox + bottom_do + DEA + (1|pond_id), data = wdat, REML=F)
# summary(mod6)

# mod7 = lmer(n2o_lake ~ treatment + period + nox + bottom_do + (1|pond_id), data = mdat, REML=F)
# summary(mod7)

mod8 = lmer(n2o_lake ~ treatment * period + nox + bottom_do + (1|pond_id), data = mdat, REML=F)
summary(mod8)

# mod11 = lmer(n2o_lake ~ treatment + nox + bottom_do + (1|pond_id) + (0+treatment|period), data = mdat, REML=F)
# summary(mod11)

# mod9 = lmer(n2o_lake ~ treatment * period + np_ratio + bottom_do + (1|pond_id), data = mdat, REML=F)
# summary(mod9)

# mod10 = lmer(n2o_lake ~ treatment * period + tn + bottom_do + (1|pond_id), data = mdat, REML=F)
# summary(mod10)


#Calculate the variance explained by the model (R-squared)
# rsq(mdat$n2o_lake, predict(mod8)) 
r.squaredGLMM(mod8)

#Plot the Effect Sizes
sjPlot::plot_model(mod8, 
                   # axis.labels=c("Treatment * Period \n(Pulse2)", "Treatment * Period \n(Pulse1)", "Bottom  \nWater DO",
                   #               "NOx", "Period \n(Pulse2)", "Period \n(Pulse1)", "Treatment"),
                   show.values=TRUE, show.p=TRUE,
                   title=expression(Effect~on~N[2]*O~Flux))

#Make a pretty table
sjPlot::tab_model(mod8,
                  pred.labels = c("(Intercept)", "Treatment", "Period (Pulse1)", "Period (Pulse2)", "NOx", 
                                  "Bottom Water DO", "Treatment * Period (Pulse1)", "Treatment * Period (Pulse2)"),
                  show.re.var= TRUE, 
                  dv.labels= "Effects on N2O Flux")


#=====STEP 2: COMPARE TO NULL MODEL
# Null model does NOT allow intercept to vary by lake
mod_null<-lm(n2o_lake ~ treatment * period + nox + bottom_do, data=mdat)
summary(mod_null)

#Compare the mixed model to the null model
anova(mod8, mod_null) 
#A p<0.05 means that the two models are different from each other

# output model results for plotting
mod_n2o = summary(mod8)$coefficients %>% as.data.frame() %>% rownames_to_column(var="fixed.effect") %>% as_tibble()


