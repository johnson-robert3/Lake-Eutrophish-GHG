#~~~
# Script for importing final versions of data sets to be archived in EDI
#
# Currently, reading in these finalized data sets are spread across too many scripts. Consolidate them all here. 
#
# By: R. Johnson
#~~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)


# Read in data from the shared EDI submission folder on Box


#-- Sonde Profiles --#
sonde_profiles = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/profiles_daily_deepsite.csv")


#-- T-Chains --#
hobo_temp = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/temp_chains_hf.csv") %>%
   # DOY variable has already been created
   # data have already been filtered to days of the experiment (DOY 143-240)
   # temperature data have already been converted to Celcius
   # Site 19 is the deep point of the ponds (Ellen had 3 t-chains in each pond)
   filter(site_id==19) %>%
   select(-doy_frac, -site_id) %>%
   rename(pond_id = pond, depth = temp_depth_m, temp = temp_c, date_time = datetime) %>%
   relocate(pond_id)


#-- miniDOT DO --#
minidot = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/do_sensor_hf.csv")


#-- Weather --#
weather_data = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/meteorological_pondStation.csv")


#-- Nutrients --# 
limno_field_data = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/surface_nutrients_chla.csv") %>%
   # select just the TP, TN, SRP, and NOx data
   select(-contains("chla"), -contains("nhx"))


#-- GHG Concentration and Flux --# 
lake_flux = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/ghg_diffusive_flux.csv") %>%
   # change variable names back to originals, so they match and work across scripts
   rename(ch4_lake = ch4_concentration, co2_lake = co2_concentration, n2o_lake = n2o_concentration,
          ch4_atmo = ch4_atmosphere, co2_atmo = co2_atmosphere, n2o_atmo = n2o_atmosphere)


#-- GHG Methanogenesis and DEA --#
methano_dea = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/ghg_production_assays.csv") %>%
   # change variable names back to originals, so they match and work across scripts
   rename(ch4_rate = methanogenesis_potential,
          n2o_rate = DEA)


#-- GHG Ebullition --#
ebu_flux_pond = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/ghg_ebullition.csv") %>%
   # change variable names back to originals, so they match and work across scripts
   rename(ch4_ebu_flux = ch4_ebullitive_flux)


#-- Metabolism --#
metabolism = read_csv("C:/Users/rajohnson6/Box/Hort Farm Experiment/EDI Data Submission/daily_metabolism.csv")



