#~~~
# Create the full data set to be used for analyzing GHG data
#
# By: R. Johnson
#~~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)



#-- Step 1: Prepare the data

# Full data set
fdat = read_csv("Data/ghg-model-dataset_2022-05-13.csv")


# Data for GHG models
mdat = fdat %>%
   # factor variables
   mutate(across(c(pond_id, treatment, period, period2), ~as_factor(.))) %>%
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
   mutate(treatment = fct_relevel(treatment, "reference"))
   # force the pulsed treatment for be first (so model results show effect of continuous variables for the pulsed treatment)
   # mutate(treatment = fct_relevel(treatment, "pulsed"))


