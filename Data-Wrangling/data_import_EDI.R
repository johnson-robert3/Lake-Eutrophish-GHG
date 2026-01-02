#~~~
# Script for processing and renaming datasets pulled in from the EDI repository
#
# By: R. Johnson
#~~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)

# Read in datasets using the 'pull-data-from-EDI' script 
source("Data-Wrangling/pull-data-from-EDI.R")


#-- Sonde Profiles --#
sonde_profiles = dt2


#-- T-Chains --#
hobo_temp = dt3 %>%
   # DOY variable has already been created
   # data have already been filtered to days of the experiment (DOY 143-240)
   # temperature data have already been converted to Celcius
   # Site 19 is the deep point of the ponds (Ellen had 3 t-chains in each pond)
   filter(site_id==19) %>%
   select(-doy_frac, -site_id) %>%
   rename(pond_id = pond, depth = temp_depth_m, temp = temp_c, date_time = datetime) %>%
   relocate(pond_id)


#-- Weather --#
weather_data = dt7


#-- Nutrients --# 
limno_field_data = dt13 %>%
   # select just the TP, TN, SRP, and NOx data
   select(-contains("chla"), -contains("nhx"))


#-- GHG Concentration and Flux --# 
lake_flux = dt10 %>%
   # change variable names back to originals, so they match and work across scripts
   rename(ch4_lake = ch4_concentration, co2_lake = co2_concentration, n2o_lake = n2o_concentration,
          ch4_atmo = ch4_atmosphere, co2_atmo = co2_atmosphere, n2o_atmo = n2o_atmosphere)


#-- GHG Methanogenesis --#
methano_dea = dt12 %>%
   # change variable names back to originals, so they match and work across scripts
   rename(ch4_rate = methanogenesis_potential)


#-- GHG Ebullition --#
ebu_flux_pond = dt11 %>%
   # change variable names back to originals, so they match and work across scripts
   rename(ch4_ebu_flux = ch4_ebullitive_flux)


#-- Metabolism --#
metabolism = dt8



