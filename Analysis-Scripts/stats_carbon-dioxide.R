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
fdat = read_csv("Data/ghg-model-dataset_2022-04-30.csv")

# changes relevant to all models
fdat = fdat %>%
   mutate(np_ratio = (tn/14.001) / ((tp/1000)/30.97),
          doc_ppm = doc_ppb / 1000) %>%
   # add a new 'period' variable to account for the fact that pulse periods don't mean anything for the reference treatment
   #  reference ponds are, in effect, always in the 'Base' period
   mutate(period2 = if_else(treatment=="reference", "BASE", period)) %>%
   relocate(period2, .after=period) %>%
   # factor variables
   mutate(across(c(pond_id, treatment, period, period2), ~as.factor(.)))


# Data for the CO2 model
mdat_co2 = fdat %>%
   # select desired variables
   select(pond_id:period, co2_lake, NEP, np_ratio, tp, srp, tn, nox, bottom_do_sat, temp, chla) %>%
   # only keep rows that have values for all variables
   # filter(!(if_any(co2_lake:last_col(), is.na))) %>%
   filter(!(is.na(co2_lake))) %>%
   # add a "time" variable for autocorrelation
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup() %>%
   # force the reference treatment to be first (so model results show effect of the pulse treatment)
   mutate(treatment = fct_relevel(treatment, "reference"))



#-- Step 2: Build the model

# start with the full model
#  allow the intercept to vary by pond (random effect) to account for the repeated-measures sampling
#  allow for temporal autocorrelation


