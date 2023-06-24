#~~~
# Script for calculating concentration of dissolved gases in lake water samples. 
# Preliminary data from summer 2019
# script by Robert Johnson
#~~~


library(tidyverse)


# load the data
site_raw = read_csv("Data/R-Data/2019_site_data.csv")
limno_raw = read_csv("Data/R-Data/2019_limno_data.csv")
ghg_raw = read_csv("Data/R-Data/2019_ghg_data.csv")


# Get mean values from individual syringe measurements for each site for GHG data
ghg_data = ghg_raw %>%
   group_by(location, site_id, date, sample_type, water_column) %>%
   summarize(methane = mean(methane, na.rm=T),
             carbon_dioxide = mean(carbon_dioxide, na.rm=T),
             nitrous_oxide = mean(nitrous_oxide, na.rm=T)) %>%
   ungroup()


# combine data sets
sample_data = left_join(ghg_data, limno_raw) #, by=c("location", "site_id", "date", "sample_depth", "water_column"))


# Partial Pressures of Gases (units = atm)
# convert measured headspace concentrations from ppm to atm (Ames = 0.97 atm)
sample_data = sample_data %>%
   mutate(pch4 = methane / 10^6 * 0.97,
          pco2 = carbon_dioxide / 10^6 * 0.97,
          pn2o = nitrous_oxide / 10^6 * 0.97)


###_Calculate concentration of dissolved gases in equilibrated water sample
# (units = uM)

# Select only water samples
sample_conc = sample_data %>%
   filter(sample_type!="atmosphere")

# Henry's Law constants (KH) (units = M/atm)
# KH = concentration / partial pressure
KH_ch4 = 0.00142
KH_co2 = 0.0344
KH_n2o = 0.024

# Temp-dependence constants for KH (unitless)
KH_ch4_dt = 1600
KH_co2_dt = 2400
KH_n2o_dt = 2600

# Temperature-corrected Henry's Law constants (KH)
sample_conc = sample_conc %>%
   mutate(KH_ch4_tcorr = KH_ch4 * exp(KH_ch4_dt * ((1/(water_temp+273.15)) - (1/298.15))),
          KH_co2_tcorr = KH_co2 * exp(KH_co2_dt * ((1/(water_temp+273.15)) - (1/298.15))),
          KH_n2o_tcorr = KH_n2o * exp(KH_n2o_dt * ((1/(water_temp+273.15)) - (1/298.15))))


#_Aqueous Concentration
# concentration (uM) dissolved in equilibrated water sample

# concentration = KH * partial pressure
sample_conc = sample_conc %>%
   mutate(ch4_aq = KH_ch4_tcorr * pch4 * 10^6,
          co2_aq = KH_co2_tcorr * pco2 * 10^6,
          n2o_aq = KH_n2o_tcorr * pn2o * 10^6)


#_Headspace concentration
# concentration (uM) in equilibrated gas headspace sample

# Ideal Gas Law: [gas] = (P/RT)*(10^6 umol/mol)

# convert measured gas headspace concentration from atm to uM
sample_conc = sample_conc %>%
   mutate(ch4_gas = (pch4 / (0.0821 * 298.15)) * 10^6,
          co2_gas = (pco2 / (0.0821 * 298.15)) * 10^6,
          n2o_gas = (pn2o / (0.0821 * 298.15)) * 10^6)


###_Calculate original concentration of dissolved gases in lake water sample
# prior to headspace equilibration
# (units = uM)

# Calculate total amount of gas present in syringe (units = umol)
lake_conc = sample_conc %>%
   mutate(ch4_tot_umol = (ch4_aq * 0.04) + (ch4_gas * 0.02),
          co2_tot_umol = (co2_aq * 0.04) + (co2_gas * 0.02),
          n2o_tot_umol = (n2o_aq * 0.04) + (n2o_gas * 0.02))

# Calculate concentrations (uM) in atmosphere
atm_conc = sample_data %>%
   filter(sample_type=="atmosphere") %>%
   # atmosphere concentrations
   mutate(ch4_atm = (pch4 / (0.0821 * 298.15)) * 10^6,
          co2_atm = (pco2 / (0.0821 * 298.15)) * 10^6,
          n2o_atm = (pn2o / (0.0821 * 298.15)) * 10^6) %>%
   # mean value from syringes
   group_by(location, date) %>%
   summarize_at(.vars = vars(pch4, pco2, pn2o, ends_with("_atm")), .funs = ~mean(.)) %>%
   rename(pch4_atm = pch4,
          pco2_atm = pco2,
          pn2o_atm = pn2o) %>%
   ungroup()

# add mean atmosphere values to dataset
lake_conc = full_join(lake_conc, atm_conc)

# Correct syringe values for amount contributed by atmosphere headspace
# Original concentrations in lake water
lake_conc = lake_conc %>%
   mutate(ch4_lake = (ch4_tot_umol - (ch4_atm * 0.02)) / 0.04,
          co2_lake = (co2_tot_umol - (co2_atm * 0.02)) / 0.04,
          n2o_lake = (n2o_tot_umol - (n2o_atm * 0.02)) / 0.04)


###_Calculate saturation of dissolved gases

lake_sat = lake_conc %>%
   # expected concentration under equilibration with atmosphere
   mutate(ch4_exp = KH_ch4_tcorr * pch4_atm * 10^6,
          co2_exp = KH_co2_tcorr * pco2_atm * 10^6,
          n2o_exp = KH_n2o_tcorr * pn2o_atm * 10^6) %>%
   # percent saturation of lake water
   mutate(ch4_sat = ch4_lake / ch4_exp * 100,
          co2_sat = co2_lake / co2_exp * 100,
          n2o_sat = n2o_lake / n2o_exp * 100)


