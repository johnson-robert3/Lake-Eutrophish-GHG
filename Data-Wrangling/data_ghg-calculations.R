#~~~
# Script for calculating GHG concentrations and rates
# By: Robert Johnson
#~~~


library(rLakeAnalyzer)
library(LakeMetabolizer)


### Constants, Equations, and Functions ----

# Atmospheric Pressure at Hort Farm pond site
# Ames, IA = 0.97 atm

   
#_Henry's Law Constants (KH) 
   
### Used to calculate dissolved gas concentration in a liquid based on equilibrium with a gaseous headspace

# KH (units = M/atm = mol/L/atm) (concentration / partial pressure)
KH_ch4 = 0.00142
KH_co2 = 0.0344
KH_n2o = 0.024

# Temperature-dependence constants for KH (unitless)
KH_td_ch4 = 1600
KH_td_co2 = 2400
KH_td_n2o = 2600


#_Ideal Gas Law

### Used to calculate the concentration of a gas from its partial pressure

# [gas] = (P/RT)*(10^6 umol/mol)   # units = umol/L (units = uM)

# P: gas partial pressure (units = atm)
# R: ideal gas law constant (units = L atm / K mol = L atm K-1 mol-1) (R=0.0821 L atm K-1 mol-1)
# T: temperature (units = K)

# ideal gas law gives units of mol/L, multiply by 10^6 to get micro-mol/L


ideal_gas_law = function(pp, temp) {
   
   conc = (pp / (0.0821 * (273.15 + temp))) * 10^6
   
   return(conc)
}



#---
#### Lake Dissolved GHG Concentrations ####
#---


#-
# Methane and Nitrous Oxide
#- 

##__Concentration of dissolved gases in equilibrated water sample (units = uM)

syr_conc = lake_samples %>%
   # select only pond samples, exclude atmosphere samples
   filter(!(str_detect(sample_id, "Y"))) %>%
   # calculate partial pressures of gases in equilibrated headspace from samples measured on GC (units = atm)
   # convert measured syringe values from ppm to atm
   mutate(pch4_head.eq = ch4_ppm / 10^6 * 0.97,
          pn2o_head.eq = n2o_ppm / 10^6 * 0.97) %>%
   # concentration of gases in equilibrated headspace (units = uM)
   mutate(ch4_head.eq = ideal_gas_law(pch4_head.eq, surface_temp),
          n2o_head.eq = ideal_gas_law(pn2o_head.eq, surface_temp)) %>%
   # temperature-corrected Henry's Law constants (KH) based on pond surface water temperatures
   mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (surface_temp + 273.15)) - (1 / 298.15))),
          tKH_co2 = KH_co2 * exp(KH_td_co2 * ((1 / (surface_temp + 273.15)) - (1 / 298.15))),
          tKH_n2o = KH_n2o * exp(KH_td_n2o * ((1 / (surface_temp + 273.15)) - (1 / 298.15)))) %>%
   # concentration of dissolved gases in equilibrated water sample (units = uM)
   mutate(ch4_water.eq = tKH_ch4 * pch4_head.eq * 10^6,
          n2o_water.eq = tKH_n2o * pn2o_head.eq * 10^6) %>%
   # calculate the total amount of gas in the syringe (units = uM)
   # total gas (umol) in equilibrated headspace plus total gas (umol) in equilibrated water sample
   mutate(ch4_tot_umol = (ch4_head.eq * vol_air) + (ch4_water.eq * vol_water),
          n2o_tot_umol = (n2o_head.eq * vol_air) + (n2o_water.eq * vol_water))


##__Atmosphere concentration (units = uM)
#  to correct for gas present in headspace prior to equilibration

atm_conc = lake_samples %>%
   # select only atmosphere samples
   filter(str_detect(sample_id, "Y")) %>%
   rename(ch4_atmo_ppm = ch4_ppm,
          co2_atmo_ppm = co2_ppm,
          n2o_atmo_ppm = n2o_ppm) %>%
   # calculate partial pressures of gases from samples measured on GC (units = atm)
   # convert measured syringe values from ppm to atm
   mutate(pch4_atmo = ch4_atmo_ppm / 10^6 * 0.97,
          pco2_atmo = co2_atmo_ppm / 10^6 * 0.97,
          pn2o_atmo = n2o_atmo_ppm / 10^6 * 0.97) %>%
   # concentration of atmosphere samples (units = uM)
   mutate(ch4_atmo = ideal_gas_law(pch4_atmo, 25),
          co2_atmo = ideal_gas_law(pco2_atmo, 25),
          n2o_atmo = ideal_gas_law(pn2o_atmo, 25))


#-
# Carbon Dioxide
#- 

# The original concentration of CO2 (or pCO2) in the lakes cannot be calculated directly from
#  the syringe headspace equilibration method to due carbonate buffering of CO2.

# Koschorreck et al. 2021 (Biogeosciences) provides a method to correctly calculate the original
#  pCO2 in an aqueous sample from a gaseous headspace accounting for the carbonate chemistry of
#  the aqueous sample by also using the alkalinity of the sample water. 


# Load the equation for calculating dissolved pCO2
source("Data-Wrangling/Rheadspace.R")


# Make a data frame with the necessary data
df_co2 = lake_samples %>%
   # pond sample data, measured equilibrated headspace pCO2 (units = ppm)
   filter(!(pond_id=="Y")) %>%
   select(sample_id:vol_air, surface_temp, co2_ppm) %>%
   rename(head_co2_ppm = co2_ppm) %>%
   # add atmosphere pCO2, i.e. headspace ppm prior to equilibration (units = ppm)
   left_join(lake_samples %>%
                filter(pond_id=="Y") %>%
                select(doy, co2_ppm) %>%
                rename(atmo_co2_ppm = co2_ppm)) %>%
   # add alkalinity data
   left_join(alk_data %>%
                select(-sample_id, -ph, -notes))

# Rename columns for use in equation
df_co2 = df_co2 %>%
   transmute(Sample.ID = sample_id,
             HS.mCO2.before = atmo_co2_ppm,
             HS.mCO2.after = head_co2_ppm,
             Temp.insitu = surface_temp,
             Temp.equil = surface_temp,
             Alkalinity.measured = alkalinity,
             Volume.gas = vol_air * 1000,
             Volume.water = vol_water * 1000,
             Bar.pressure = 98.285,  # = 101.325 kPa/atm * 0.97 atm
             Constants = 1,
             Salinity = 0)

# Calculate original pCO2 in water sample (prior to equilibration)
lake_co2 = Rheadspace(df_co2)

# Convert original pCO2 to atmospheres (units = atm)
lake_co2 = lake_co2 %>%
   clean_names() %>%
   transmute(sample_id = sample_id,
             # partial pressure in original (pre-equilibration) water sample
             # convert ppm to partial pressure in atm
             pco2_aq = m_co2_complete_headspace_ppmv / 10^6 * 0.97)


##__Combine CH4, N2O, and CO2 data and calculate original concentration dissolved in lake water (units = uM)

lake_conc = syr_conc %>%
   # add atmosphere concentrations to syringe concentrations
   left_join(atm_conc %>% select(doy, contains("atmo"))) %>%
   # add original lake water partial pressure from above 
   left_join(lake_co2) %>%
   # original gas concentration dissolved in lake water (units = uM)
   mutate(ch4_lake = (ch4_tot_umol - (ch4_atmo * vol_air)) / vol_water,
          n2o_lake = (n2o_tot_umol - (n2o_atmo * vol_air)) / vol_water,
          # do not need to correct CO2 for the amount contributed by the atmosphere headspace; already done within 'Rheadspace' equations
          co2_lake = tKH_co2 * pco2_aq * 10^6) %>%
   # drop unnecessary variables from calculations
   select(sample_id:doy, surface_temp, ends_with("lake"), ends_with("atmo"), starts_with("tKH"))


#---
#### Diffusive Flux ####
#---

# Flux rate across the gas-water interface
# Instantaneous flux, using wind-based k

## Calculate wind-based k (units = m / d)
wind_k = weather_data %>%
   mutate(U10 = wind_speed * ((10 / wind_z)^(1/7)),
          k_cole = k.cole.base(U10)) %>%
   # daily average k (units = m / d)
   group_by(doy) %>%
   summarize(across(wind_z:k_cole, ~mean(., na.rm=T))) %>%
   ungroup()


# Add k to lake dataset
lake_flux = left_join(lake_conc, wind_k) %>%
   # calculate gas-specific k600 values (units = m / d)
   mutate(k_ch4 = k600.2.kGAS.base(k600 = k_cole, temperature = surface_temp, gas = "CH4"),
          k_co2 = k600.2.kGAS.base(k600 = k_cole, temperature = surface_temp, gas = "CO2"),
          k_n2o = k600.2.kGAS.base(k600 = k_cole, temperature = surface_temp, gas = "N2O"))


## Calculate estimated daily rates of diffusive flux

# F = k(Cw - Ceq)

# To get a flux (F) in units of mmol / m2 / d, with k in units of m / d,
#  concentrations (Cw and Ceq) need to be in units of mmol / m3

lake_flux = lake_flux %>%
   # calculate expected dissolved surface water concentrations if in equilibrium with atmosphere (units = uM)
   mutate(ch4_exp = tKH_ch4 * pch4_atmo * 10^6,
          co2_exp = tKH_co2 * pco2_atmo * 10^6,
          n2o_exp = tKH_n2o * pn2o_atmo * 10^6) %>%
   # concentration data are in units of (umol / L), this is the same as (mmol / m3), so no conversion needed
   # calculate diffusive flux (units = mmol / m2 / d)
   mutate(ch4_flux = k_ch4 * (ch4_lake - ch4_exp),
          co2_flux = k_co2 * (co2_lake - co2_exp),
          n2o_flux = k_n2o * (n2o_lake - n2o_exp))


# Output file of total GHG dissolved concentration and diffusive flux data, so raw data don't need to be re-processed each time 
write.csv(lake_flux, file = "Data/ghg_concentration_flux_total.csv", row.names=FALSE)



#---
#### Methanogenesis Potential ####
#---

##__Gas concentrations at end of incubation

#_HEADSPACE
#  measured from vial collected from bottle headspace (units = ppm)

methano_samples = methano_samples %>%
   # partial pressure of gases (units = atm)
   mutate(pch4 = ch4_ppm / 10^6 * 0.97,
          pco2 = co2_ppm / 10^6 * 0.97,
          pn2o = n2o_ppm / 10^6 * 0.97) %>%
   # ideal gas law to get final concentration (units = uM)
   mutate(ch4_head = ideal_gas_law(pch4, incubation_temp),
          co2_head = ideal_gas_law(pco2, incubation_temp),
          n2o_head = ideal_gas_law(pn2o, incubation_temp))


#_AQUEOUS SAMPLE 
#  concentration of gases dissolved in water at end of incubation,
#  in equilibrium with bottle headspace

methano_samples = methano_samples %>%
   # temperature-corrected Henry's Law constants (tKH)
   mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (incubation_temp + 273.15)) - (1 / 298.15))),
          tKH_co2 = KH_co2 * exp(KH_td_co2 * ((1 / (incubation_temp + 273.15)) - (1 / 298.15))),
          tKH_n2o = KH_n2o * exp(KH_td_n2o * ((1 / (incubation_temp + 273.15)) - (1 / 298.15)))) %>%
   # concentration dissolved in water sample (units = uM)
   mutate(ch4_aq = tKH_ch4 * pch4 * 10^6,
          co2_aq = tKH_co2 * pco2 * 10^6,
          n2o_aq = tKH_n2o * pn2o * 10^6)


#_Add sediment bulk density and porosity to Methano dataset

methano_samples = methano_samples %>%
   left_join(bulk_density) %>%
   # calculate total aqueous volume in bottle using porosity data
   # aqueous volume = water sample + aqueous portion of sediment sample (units = L)
   mutate(vol_aq = vol_water + (vol_sediment * porosity)) %>%
   # calculate total sample mass in each assay bottle using sediment bulk density
   # vol_aq = water mass (since 1 cm^3 = 1 g)
   # need to convert volumes back to ml (i.e. cm^3) to calculate mass
   mutate(mass_aq = vol_aq * 1000,
          mass_sed = vol_sediment * DBD * 1000,
          mass_slurry = mass_aq + mass_sed)


##__Methane Production Rate (units = umol/g/h)

methano_samples = methano_samples %>%
   # total amount of gases in bottle at end of incubation (headspace + dissolved) (units = umol)
   mutate(ch4_tot_umol = (ch4_aq * vol_aq) + (ch4_head * vol_head),
          co2_tot_umol = (co2_aq * vol_aq) + (co2_head * vol_head),
          n2o_tot_umol = (n2o_aq * vol_aq) + (n2o_head * vol_head)) %>%
   # hourly rate of production per gram of slurry mass (units = umol/g/h)
   mutate(ch4_rate = ch4_tot_umol / mass_slurry / (incubation_length / 60),
          co2_rate = co2_tot_umol / mass_slurry / (incubation_length / 60),
          n2o_rate = n2o_tot_umol / mass_slurry / (incubation_length / 60))


# Mean rate per pond (from 3 bottle replicates)
#  SD not needed, all 3 bottle replicates came from the same sample
methano_rates = methano_samples %>%
   group_by(pond_id, week, doy) %>%
   summarize(across(ends_with("rate"), ~mean(., na.rm=T))) %>%
   ungroup()


# Output file of Methanogenesis rates, so raw data don't need to be re-processed each time 
write.csv(methano_rates, file = "Data/methanogenesis_rates_total.csv", row.names=FALSE)



#---
#### DEA ####
#---

##__Gas concentrations at end of incubation

#_HEADSPACE
#  measured from vial collected from bottle headspace (units = ppm)

dea_samples = dea_samples %>%
   # partial pressure of gases (units = atm)
   mutate(pch4 = ch4_ppm / 10^6 * 0.97,
          pco2 = co2_ppm / 10^6 * 0.97,
          pn2o = n2o_ppm / 10^6 * 0.97) %>%
   # headspace concentration (units = uM)
   mutate(ch4_head = ideal_gas_law(pch4, incubation_temp),
          co2_head = ideal_gas_law(pco2, incubation_temp),
          n2o_head = ideal_gas_law(pn2o, incubation_temp))


#_AQUEOUS SAMPLE 
#  concentration of gases dissolved in water at end of incubation,
#  in equilibrium with bottle headspace

dea_samples = dea_samples %>%
   # temperature-corrected Henry's Law constants (tKH)
   mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (incubation_temp + 273.15)) - (1 / 298.15))),
          tKH_co2 = KH_co2 * exp(KH_td_co2 * ((1 / (incubation_temp + 273.15)) - (1 / 298.15))),
          tKH_n2o = KH_n2o * exp(KH_td_n2o * ((1 / (incubation_temp + 273.15)) - (1 / 298.15)))) %>%
   # aqueous concentration (units = uM)
   mutate(ch4_aq = tKH_ch4 * pch4 * 10^6,
          co2_aq = tKH_co2 * pco2 * 10^6,
          n2o_aq = tKH_n2o * pn2o * 10^6)


#_Add sediment bulk density and porosity to DEA dataset

dea_samples = dea_samples %>%
   left_join(bulk_density) %>%
   # calculate total aqueous volume in bottle using porosity data
   # aqueous volume = water sample + aqueous portion of sediment sample (units = L)
   mutate(vol_aq = vol_water + vol_media + (vol_sediment * porosity)) %>%
   # calculate total sample mass in each assay bottle using sediment bulk density
   # vol_aq = water mass (since 1 cm^3 = 1 g)
   # need to convert volumes back to ml (i.e. cm^3) to calculate mass
   mutate(mass_aq = vol_aq * 1000,
          mass_sed = vol_sediment * DBD * 1000,
          mass_slurry = mass_aq + mass_sed)


##__Denitrification Enzyme Activity (N2O production rate) (units = umol/g/h)

dea_samples = dea_samples %>%
   # total amount of gases in bottle at end of incubation (headspace + dissolved) (units = umol)
   mutate(ch4_tot_umol = (ch4_aq * vol_aq) + (ch4_head * vol_head),
          co2_tot_umol = (co2_aq * vol_aq) + (co2_head * vol_head),
          n2o_tot_umol = (n2o_aq * vol_aq) + (n2o_head * vol_head)) %>%
   # hourly rate of production per gram of slurry mass (units = umol/g/h)
   mutate(ch4_rate = ch4_tot_umol / mass_slurry / (incubation_length / 60),
          co2_rate = co2_tot_umol / mass_slurry / (incubation_length / 60),
          n2o_rate = n2o_tot_umol / mass_slurry / (incubation_length / 60))


# Mean rate per pond (from 3 bottle replicates)
#  SD not needed, all 3 bottle replicates came from the same sample
dea_rates = dea_samples %>%
   group_by(pond_id, week, doy) %>%
   summarize(across(ends_with("rate"), ~mean(., na.rm=T))) %>%
   ungroup()


# Output file of DEA rates, so raw data don't need to be re-processed each time 
write.csv(dea_rates, file = "Data/DEA_rates_total.csv", row.names=FALSE)



#===
#### Ebullition ####
#===

#--
# Prep data
#--

# Remove instances when chamber samples were not collected
ebu_samples = ebu_samples %>%
   # pond C, week 5, chamber 2
   filter(!(pond_id=="C" & week==5 & replicate=="P2")) %>%
   # pond F, week 11, chamber 3
   filter(!(pond_id=="F" & week==11 & replicate=="P3"))


# Convert measured chamber headspace to partial pressure (units = atm)
ebu_samples = ebu_samples %>%
   mutate(pch4 = ch4_ppm / 10^6 * 0.97,
          pco2 = co2_ppm / 10^6 * 0.97,
          pn2o = n2o_ppm / 10^6 * 0.97)


# Create separate data sets for start and end of deployments

ebu_start = ebu_samples %>%
   group_by(week, pond_id, replicate) %>%
   slice_min(order_by = doy) %>%
   ungroup() %>%
   # update doy
   mutate(doy = doy + 1) %>%
   # rename partial pressure
   rename(pch4_t0 = pch4,
          pco2_t0 = pco2,
          pn2o_t0 = pn2o) %>%
   select(-ends_with("ppm"))


ebu_end = ebu_samples %>%
   group_by(week, pond_id, replicate) %>%
   slice_max(order_by = doy) %>%
   ungroup() %>%
   # rename partial pressure
   rename(pch4_t1 = pch4,
          pco2_t1 = pco2,
          pn2o_t1 = pn2o) %>%
   select(-ends_with("ppm"))


# Combine data, with partial pressures now separate for start and end
ebu_data = ebu_end %>%
   select(-sample_id) %>%
   left_join(ebu_start %>%
                select(pond_id, replicate, week, ends_with("_t0"))) %>%
   relocate(ends_with("_t0"), .before="pch4_t1")


# The Rheadspace() function no longer works with most recent versions of R, so can't recreate the 'lake_conc" data set without a bit of work
# 
# If ebullition data need to be worked with or re-calculated for anything, the 'lake_flux' data set from above - which was output and saved 
#  as 'ghg_concentration_flux_total' - contains all the variables needed for ebullition calcs here, so can just read that file in as needed. 

lake_flux = read_csv("Data/ghg_concentration_flux_total.csv")

# Add lake surface water dissolved gas data to ebullition data set
ebu_data = ebu_data %>%
   left_join(lake_flux %>%
                select(pond_id, doy, surface_temp, ends_with("lake"), starts_with("tKH_")))


#_Calculate additional dissolved gas variables for lake surface water

ebu_data = ebu_data %>%
   # partial pressure of gases dissolved in lake surface water (units = atm)
   mutate(pch4_lake = ch4_lake / 10^6 / tKH_ch4,
          pco2_lake = co2_lake / 10^6 / tKH_co2,
          pn2o_lake = n2o_lake / 10^6 / tKH_n2o) %>%
   # expected lake concentration if surface water at equilibrium with chamber headspace at deployment start (units = uM)
   mutate(ch4_eq_t0 = tKH_ch4 * pch4_t0 * 10^6,
          co2_eq_t0 = tKH_co2 * pco2_t0 * 10^6,
          n2o_eq_t0 = tKH_n2o * pn2o_t0 * 10^6) %>%
   # expected lake concentration if surface water at equilibrium with chamber headspace at deployment end (units = uM)
   mutate(ch4_eq_t1 = tKH_ch4 * pch4_t1 * 10^6,
          co2_eq_t1 = tKH_co2 * pco2_t1 * 10^6,
          n2o_eq_t1 = tKH_n2o * pn2o_t1 * 10^6)


#--
# Calculate chamber-specific k
#--

# Eq. (4) in Cole et al. 2010. L&O Methods

# k = ( V / (Kh*R*T*A)) * ln((Pw - Pi)/(Pw - Pf)) / (tf - ti)

# units of R (gas constant): L atm / K / mol

#--
# NOTE:
# Only one chamber-specific k value needs to be calculated per chamber (i.e. not for each gas individually).
#  This k value can then be converted to a k600 value which is standardized, and can be used to calculate a
#  gas-specific k value for the desired gas at the given temperature. 
#
# Previous work has demonstrated that CH4 is the preferable gas to use when calculating k using the floating 
#  chamber method. (e.g. Cole et al. 2010. L&O Methods; Galfalk et al. 2013. JGR Biogeosciences). 
#
# Only use methane data for calculating ebullition.
#--

#--
# NOTE:
# A chamber-specific k value cannot be calculated if the partial pressure in the chamber headspace at the end 
#  of the deployment is greater than the partial pressure of the gas dissolved in the lake surface water.
#
# This suggests that a chamber has received ebullitive flux, as the headspace concentration is beyond equilibrium 
#  with the surface water. 
#
# This can also be determined/seen if the measured gas concentration in the lake surface water is less than the 
#  expected concentration of the surface water if it were in equilibrium with the chamber headspace at the end of
#  the deployment. 
#--

#--
# UNITS:
#
# V - volumne {L}
# tKH - temp-corrected Henry's Law constant {mol / L / atm}
# R - universal gas constant {L atm / K / mol}
# T - temperature {K}
# A - area {m2}
# Pw, Pi, Pf - partial pressure {atm}
# tf, ti - time {days}
#
#_Unit conversions to get units of k in {m / d}
#
# ({L} / ({mol / L / atm} * {L atm / K / mol} * {K} * {m2})) * log({atm} / {atm}) / {days}
# =
# {L} / {m2} / {days}     ### 1000 L in 1 m3
# = 
# ({m3} / 1000{L}) / {m2} / {days}
# =
# {m} / 1000{days}
#
#--


ebu_data = ebu_data %>%
   # k (units = m / d)
   mutate(k_chamber = 
             (vol_chamber / (tKH_ch4 * 0.0821 * (surface_temp + 273.15) * area_chamber)) 
          * 
             log((pch4_lake - pch4_t0) / (pch4_lake - pch4_t1)) 
          / 
             (deployment_length * 1000))


#--
# Calculate chamber ebullitive flux rates
#-- 

ebu_data = ebu_data %>%
   # remove unnecessary columns
   select(-contains("co2"), -contains("n2o"),
          -site_id, -date_time, -notes_gc, -data_flag) %>%
   # change "NaN" to NA
   mutate(k_chamber = if_else(k_chamber=="NaN", NA_real_, k_chamber)) %>%
   # convert chamber-specific k to k600 for comparisons (units = m / d)
   mutate(Sc_ch4 = getSchmidt(temperature = surface_temp, gas = "CH4"),
          k600 = ((600 / Sc_ch4)^(-0.5)) * k_chamber)



## STEPS

# 1. find minimum k600 values (chamber specific) for each pond that could represent diffusive flux only
# 2. divide chamber k600 by minimum k600 to get k ratio for each chamber
# 3. find the cutoff k ratio for each pond suggesting ebullition 

# 4. use mean chamber-specific k600 from all chambers (on a given pond for a given day) below the cutoff k ratio to calculate expected 
#     diffusive flux rate (based upon expected lake surface concentration at T0, e.g. ch4_eq_t0)
#     - for chambers with a k ratio above the cutoff or for which a chamber-specific k could not be calculated due to ebullition

# 5. use this flux rate, and deployment length, to calculate the mass of gas that moved into the chamber via diffusion
# 6. calculate the actual mass of gas that moved into the chamber during the deployment (from T1 concentration data)

# 7. excess mass is due to ebullition. calculate the ebullition rate for the deployment 


##__STEPS 1, 2, & 3

test1 = ebu_data %>%
   # set pond to work with
   filter(pond_id=="A") %>%
   ##
   filter(!(is.na(k_chamber))) %>%
   ##
   group_by(pond_id, doy) %>%
   mutate(ratio = k_chamber / min(k_chamber),
          ratio_600 = k600 / min(k600)) %>%
   ungroup() %>%
   ##
   arrange(ratio) %>%
   mutate(n = 1:n())

# view chamber-specific k values over time
plot(test1 %>% pull(doy),
     test1 %>% pull(k_chamber))

# view ranked k ratios to determine cutoff
plot(test1 %>% pull(n),
     test1 %>% pull(ratio))

plot(test1 %>% pull(n),
     test1 %>% pull(ratio_600))


# Ebullition cutoff ratios
cutoff_a = 1.6
cutoff_b = 2.0
cutoff_c = 2.1
cutoff_d = 2.0
cutoff_e = 1.5
cutoff_f = 2.0


##__STEP 4

# calculate k ratio for each chamber
test4 = ebu_data %>%
   # remove NAs (k not calculated due to ebullition)
   filter(!(is.na(k600))) %>%
   # ratio of each chamber specific k value to the minimum chamber k value for each pond-day
   group_by(pond_id, doy) %>%
   mutate(k_ratio = k600 / min(k600)) %>%
   ungroup()

# calculate mean k600 value for chambers below cutoff k ratio for each pond
test41 = test4 %>%
   # remove all chambers with a k ratio above the pond-specific cutoff ratio
   filter(!(pond_id=="A" & k_ratio > cutoff_a)) %>%
   filter(!(pond_id=="B" & k_ratio > cutoff_b)) %>%
   filter(!(pond_id=="C" & k_ratio > cutoff_c)) %>%
   filter(!(pond_id=="D" & k_ratio > cutoff_d)) %>%
   filter(!(pond_id=="E" & k_ratio > cutoff_e)) %>%
   filter(!(pond_id=="F" & k_ratio > cutoff_f)) %>%
   # mean k600 value from all chambers below the pond-specific cutoff k ratio (units = m / d)
   # this is the k600 representing diffusive flux only within chambers
   group_by(pond_id, doy) %>%
   summarize(k_diffusion = mean(k600, na.rm=T)) %>%
   ungroup()


test42 = ebu_data %>%
   left_join(test41) 


test43 = test42 %>%
   # mean diffusive flux k value for each pond-day (leaving NAs in) (units = m / d)
   group_by(pond_id, doy) %>%
   summarize(k_diffusion = mean(k_diffusion)) %>%
   ungroup() %>%
   # linearly interpolate diffusive k values for when single weeks are missing (due to all chambers receiving ebullition)
   group_by(pond_id) %>%
   arrange(doy, .by_group=T) %>%
   mutate(k_diffusion = if_else(is.na(k_diffusion), ((lag(k_diffusion) + lead(k_diffusion))/2), k_diffusion)) %>%
   mutate(days_since = doy - lag(doy)) %>%
   ungroup()
   


# interpolate k_diffusion for times when k could not be calculated multiple weeks in a row (due to all chambers receiving ebullition)

# Pond B (DOY 174, 180, 189)
#  linearly interpolate between DOY 167 and 195
#  ((3.6169939 - 0.1788804) / (195 - 167)) * (days_since)
btest = test43 %>%
   filter(pond_id=="B") %>%
   mutate(k_diffusion = case_when(is.na(.$k_diffusion) ~ (0.1227898 * .$days_since) + lag(.$k_diffusion),
                            TRUE ~ as.numeric(k_diffusion))) %>%
   mutate(k_diffusion = case_when(is.na(.$k_diffusion) ~ (0.1227898 * .$days_since) + lag(.$k_diffusion),
                            TRUE ~ as.numeric(k_diffusion))) %>%
   mutate(k_diffusion = case_when(is.na(.$k_diffusion) ~ (0.1227898 * .$days_since) + lag(.$k_diffusion),
                            TRUE ~ as.numeric(k_diffusion)))

# Pond F (DOY 174, 180, 189)
#  linearly interpolate between DOY 167 and 195
#  ((4.0100759 - 2.2119181) / (195 - 167)) * (days_since)
ftest = test43 %>%
   filter(pond_id=="F") %>%
   mutate(k_diffusion = case_when(is.na(.$k_diffusion) ~ (0.06421992 * .$days_since) + lag(.$k_diffusion),
                            TRUE ~ as.numeric(k_diffusion))) %>%
   mutate(k_diffusion = case_when(is.na(.$k_diffusion) ~ (0.06421992 * .$days_since) + lag(.$k_diffusion),
                            TRUE ~ as.numeric(k_diffusion))) %>%
   mutate(k_diffusion = case_when(is.na(.$k_diffusion) ~ (0.06421992 * .$days_since) + lag(.$k_diffusion),
                            TRUE ~ as.numeric(k_diffusion)))


# complete, interpolated, data set of calculated diffusive flux k values
# add the interpolated B and F pond data back to the others
test44 = test43 %>%
   filter(!(pond_id=="B")) %>%
   filter(!(pond_id=="F")) %>%
   bind_rows(btest) %>%
   bind_rows(ftest) %>%
   select(-days_since) %>%
   arrange(pond_id, doy)


# add mean k diffusion values (from chambers below cutoff ratio) to dataset of chambers receiving ebullition

# all chambers for which k could not be calculated due to receiving ebullition
test4a = ebu_data %>%
   filter(is.na(k600))

# all chambers for which k was above the pond-specific cutoff k ratio
test4b = test4 %>%
   filter(!(pond_id=="A" & k_ratio < cutoff_a)) %>%
   filter(!(pond_id=="B" & k_ratio < cutoff_b)) %>%
   filter(!(pond_id=="C" & k_ratio < cutoff_c)) %>%
   filter(!(pond_id=="D" & k_ratio < cutoff_d)) %>%
   filter(!(pond_id=="E" & k_ratio < cutoff_e)) %>%
   filter(!(pond_id=="F" & k_ratio < cutoff_f))

# All chambers receiving ebullition
# add k_diffusion to all chambers having received ebullition
ebullition_chambers = bind_rows(test4a, test4b %>% select(-k_ratio)) %>%
   left_join(test44) %>%
   arrange(pond_id, doy, replicate)


# Calculate expected diffusive flux rate using chamber-specific k values from chambers receiving only diffusion (units = mmol / m2 / d)

#  F = k(Cw - Ceq)

# k units {m/d}
# Cw & Ceq units {mmol/m3} (same as umol/L)

ebullition_chambers = ebullition_chambers %>%
   # convert diffusive k600 to gas specific k (units = m / d)
   mutate(k_dif_ch4 = k600.2.kGAS.base(k600 = k_diffusion, temperature = surface_temp, gas = "CH4")) %>%
   # expected diffusive flux with chamber-specific k (units = mmol / m2 / d)
   mutate(ch4_exp_dif_flux = k_dif_ch4 * (ch4_lake - ch4_eq_t0))


##__STEP 5

# mass of gas moving into chamber during deployment via diffusion (units = mmol)

ebullition_chambers = ebullition_chambers %>%
   mutate(ch4_exp_dif_mass = ch4_exp_dif_flux * area_chamber * deployment_length)


##__STEP 6

# actual mass flux into chamber

ebullition_chambers = ebullition_chambers %>%
   mutate(
      # initial chamber concentration (units = uM)
      ch4_t0 = ideal_gas_law(pch4_t0, surface_temp),
      # final chamber concentration (units = uM)
      ch4_t1 = ideal_gas_law(pch4_t1, surface_temp),
      # actual measured mass flux (units = umol)
      ch4_total_mass = (ch4_t1 * vol_chamber) - (ch4_t0 * vol_chamber))


##__STEP 7

ebullition_chambers = ebullition_chambers %>%
   mutate(
      # mass of gas in chamber due to ebullition (units = mmol)
      ch4_ebu_mass = (ch4_total_mass / 1000) - ch4_exp_dif_mass,
      # ebullitive flux rate (units = mmol / m2 / d)
      ch4_ebu_flux = ch4_ebu_mass / area_chamber / deployment_length,
      # identify chambers as having received ebullition
      received_ebu = rep_len(1, n()))


#--
# Calculate ebullition across ponds
#--

## need to add ebullitive flux data back to the full data set (including chambers that did not receive ebullition)
## need all chambers from a pond to calculate a mean ebullition rate (including 0's)

diffusion_chambers = ebu_data %>%
   # remove NAs (k not calculated due to ebullition)
   filter(!(is.na(k600))) %>%
   # ratio of each chamber specific k value to the minimum chamber k value for each pond-day
   group_by(pond_id, doy) %>%
   mutate(k_ratio = k600 / min(k600)) %>%
   ungroup() %>%
   # remove all chambers with a k ratio above the pond-specific cutoff ratio
   filter(!(pond_id=="A" & k_ratio > cutoff_a)) %>%
   filter(!(pond_id=="B" & k_ratio > cutoff_b)) %>%
   filter(!(pond_id=="C" & k_ratio > cutoff_c)) %>%
   filter(!(pond_id=="D" & k_ratio > cutoff_d)) %>%
   filter(!(pond_id=="E" & k_ratio > cutoff_e)) %>%
   filter(!(pond_id=="F" & k_ratio > cutoff_f)) %>%
   # identify chambers as having not received ebullition
   mutate(received_ebu = rep_len(0, n()))


# add additional k and flux variables for diffusion-only chambers here if needed

# diffusion_chambers = diffusion_chambers %>%
#    # add k_diffusion
#    left_join(test44)
   

ebu_flux = full_join(ebullition_chambers, diffusion_chambers) %>%
   # change ebullition from NA to 0 for diffusion chambers
   mutate(ch4_ebu_flux = if_else(received_ebu==0, 0, ch4_ebu_flux)) %>%
   arrange(pond_id, doy, replicate) %>%
   
   # remove chambers with a negative ebullition rate
   #  from when estimated diffusive mass flux into chamber was greater than actual mass flux into chamber
   
   # filter(!(ch4_ebu_flux < 0))
   
   # ebullition should be changed to 0 for these chambers, the chambers shouldn't be removed
   # the 0 value should still contribute to the pond mean value, b/c the chamber just didn't receive ebullition
   mutate(ch4_ebu_flux = if_else(ch4_ebu_flux < 0, 0, ch4_ebu_flux))


# mean ebullitive flux rate from all chambers for each pond
ebu_flux_pond = ebu_flux %>%
   group_by(pond_id, week, doy) %>%
   summarize(ch4_ebu_flux = mean(ch4_ebu_flux)) %>%
   ungroup()


# add data flags to final dataset
ebu_flux_pond = ebu_flux_pond %>%
   # add data flag for when ebullition could not be calculated (e.g., not enough chambers deployed)
   #  only 1 chamber was deployed on ponds A, C, D, and E for the first 3 weeks
   mutate(flag = if_else(pond_id %in% c('A', 'C', 'D', 'E') & week %in% c(2, 3, 4), "a", NA_character_)) %>% 
   # replace ebullition rates for these 4 ponds for the first 3 weeks (currently all 0's) with NA
   mutate(ch4_ebu_flux = replace(ch4_ebu_flux, flag=="a", NA_real_))


   ## remove all temporary data sets from ebullition calculations
   rm(list=ls(pattern="test"))
   ##

   
write.csv(ebu_flux_pond, file="Data/ebullition_total.csv", row.names=FALSE)



