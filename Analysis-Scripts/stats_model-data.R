#~~~
# Create the full data set to be used for analyzing GHG data
#
# By: R. Johnson
#~~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)



#-- Step 1: Prepare the data

# Full data set
fdat = read_csv("Data/ghg-model-dataset_2024-07-26.csv") %>%
   # force date format (sometimes date format can be weird coming from Excel)
   mutate(date = ymd(date)) %>%
   # add nutrient and foodweb treatment identifiers
   mutate(trt_nutrients = case_when(pond_id %in% c("A", "B", "C") ~ "pulsed",
                                    pond_id %in% c("D", "E", "F") ~ "reference"),
          trt_fish = case_when(pond_id %in% c("B", "F") ~ "low",
                               pond_id %in% c("A", "D") ~ "medium",
                               pond_id %in% c("C", "E") ~ "high")) %>%
   # reorder treatments for figures, so pulsed is plotted on top of reference
   mutate(trt_nutrients = factor(trt_nutrients, levels = c("reference", "pulsed")))


# Pond/Site Data (create if needed, since lots of older code still calls this)
pond_data = tibble(pond_id = c('A', 'B', 'C', 'D', 'E', 'F'),
                   trt_nutrients = rep(c("yes", "no"), each=3),
                   trt_fish = c('medium', 'low', 'high', 'medium', 'high', 'low'))



# # Data for GHG models
# mdat = fdat %>%
#    # factor variables (pond_id, treatment, period, period2, sonde stratification)
#    mutate(across(where(is.character), ~as_factor(.))) %>%
#    # add a variable for 'nutrient treatment X pulse period'
#    mutate(pulse_period = paste(treatment, period, sep = '_') %>% as_factor()) %>%
#    # convert N2O flux and concentration data from units of micro-mole to nano-mole
#    mutate(across(c(n2o_flux, n2o_lake), ~.*1000)) %>%
#    # add food web treatment to data
#    left_join(pond_data %>%
#                 select(pond_id, trt_fw = trt_fish) %>%
#                 mutate(pond_id = as_factor(pond_id),
#                        trt_fw = as_factor(trt_fw)))



# # Data for linear mixed effects models
# mdat = mdat %>%
#    # select desired variables
#    select(pond_id:period2, 
#           contains("_lake"), contains("_flux"),
#           GPP, R, NEP, 
#           bottom_do, bottom_do_sat, bottom_temp, 
#           do, do_sat, temp, 
#           chla, alkalinity, doc_ppm, 
#           tp, srp, tn, nox, np_ratio) %>%
#    # only keep rows that have values for all variables
#    filter(!(if_any(where(is.numeric), is.na))) %>%
#    # add a "time" variable for autocorrelation
#    group_by(pond_id) %>%
#    arrange(doy, .by_group=TRUE) %>%
#    mutate(time = seq_len(n())) %>%
#    ungroup() %>%
#    # force the reference treatment to be first (so "treatment" shows the effect of the pulsed treatment in model results)
#    mutate(treatment = fct_relevel(treatment, "reference"))
#    # force the pulsed treatment for be first (so model results show effect of continuous variables for the pulsed treatment)
#    # mutate(treatment = fct_relevel(treatment, "pulsed"))


