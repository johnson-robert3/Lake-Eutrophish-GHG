#~~~
# Script for calculating GHG concentrations and rates
# By: Robert Johnson
#~~~


### Constants, Equations, and Functions
{

# Atmospheric Pressure at Hort Farm pond site
# Ames, IA = 0.97 atm

   
#_Henry's Law Constants (KH) (units = M/atm)
   
### Used to calculate dissolved gas concentration in a liquid based on equilibrium with a gaseous headspace

# KH = concentration / partial pressure
KH_ch4 = 0.00142
KH_co2 = 0.0344
KH_n2o = 0.024

# Temperature-dependence constants for KH (unitless)
KH_td_ch4 = 1600
KH_td_co2 = 2400
KH_td_n2o = 2600


#_Ideal Gas Law

### Used to calculate concentration of a gas based on its partial pressure

# [gas] = (P/RT)*(10^6 umol/mol)

ideal_gas_law = function(pp) {
   
   conc = (pp / (0.0821 * 298.15)) * 10^6
   
   return(conc)
}


}


#---
# Lake Dissolved GHG Concentrations
#---

# Partial Pressure of Gases (units = atm)
#  convert measured syringe values from ppm to atm
lake_samples = lake_samples %>%
   mutate(pch4 = ch4_ppm / 10^6 * 0.97,
          pco2 = co2_ppm / 10^6 * 0.97,
          pn2o = n2o_ppm / 10^6 * 0.97)


##__Calculate concentration of dissolved gases in equilibrated water sample (units = uM)

lake_conc = lake_samples %>%
   # select only pond samples, exclude atmosphere samples
   filter(!(str_detect(sample_id, "Y"))) %>%
   # temperature-corrected Henry's Law constants (KH) based on pond surface water temperatures
   mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (surface_temp + 273.15)) - (1 / 298.15))),
          tKH_co2 = KH_co2 * exp(KH_td_co2 * ((1 / (surface_temp + 273.15)) - (1 / 298.15))),
          tKH_n2o = KH_n2o * exp(KH_td_n2o * ((1 / (surface_temp + 273.15)) - (1 / 298.15))))


#_Headspace concentration (units = uM)

# convert measured gas headspace from atm to uM
lake_conc = lake_conc %>%
   mutate(ch4_head = ideal_gas_law(pch4),
          co2_head = ideal_gas_law(pco2),
          n2o_head = ideal_gas_law(pn2o))


#_Aqueous concentration (units = uM)

# concentration = tKH * partial pressure
lake_conc = lake_conc %>%
   mutate(ch4_aq = tKH_ch4 * pch4 * 10^6,
          co2_aq = tKH_co2 * pco2 * 10^6,
          n2o_aq = tKH_n2o * pn2o * 10^6)


##__Calculate original concentration of dissolved gases in water sample
#  prior to equilibration with headspace
#  units = uM

# calculate total amount of gas in syringe (units = umol)
lake_conc = lake_conc %>%
   mutate(ch4_tot_umol = (ch4_aq * vol_water) + (ch4_head * vol_air),
          co2_tot_umol = (co2_aq * vol_water) + (co2_head * vol_air),
          n2o_tot_umol = (n2o_aq * vol_water) + (n2o_head * vol_air))

#_Atmosphere concentration (units = uM)
#  to correct for gas present in headspace prior to equilibration
atm_conc = lake_samples %>%
   # select only atmosphere samples
   filter(str_detect(sample_id, "Y")) %>%
   # Ideal Gas Law
   mutate(ch4_atm = ideal_gas_law(pch4),
          co2_atm = ideal_gas_law(pco2),
          n2o_atm = ideal_gas_law(pn2o)) %>%
   # rename partial pressure variables, to keep distinct from pp values of pond samples
   rename(pch4_atm = pch4,
          pco2_atm = pco2,
          pn2o_atm = pn2o)

# add atmospheric gas values to dataset
lake_conc = lake_conc %>%
   left_join(atm_conc %>% select(doy, ends_with("atm")))


#_Original gas concentrations in lake water (units = uM)
#  correct syringe values for gases contributed by atmosphere headspace
lake_conc = lake_conc %>%
   mutate(ch4_lake = (ch4_tot_umol - (ch4_atm * vol_air)) / vol_water,
          co2_lake = (co2_tot_umol - (co2_atm * vol_air)) / vol_water,
          n2o_lake = (n2o_tot_umol - (n2o_atm * vol_air)) / vol_water)


#---
# Methanogenesis Potential
#---

##__Gas concentrations at end of incubation

#_HEADSPACE
#  measured from vial collected from bottle headspace (ppm)

methano_samples = methano_samples %>%
   # partial pressure of gases (units = atm)
   mutate(pch4 = ch4_ppm / 10^6 * 0.97,
          pco2 = co2_ppm / 10^6 * 0.97,
          pn2o = n2o_ppm / 10^6 * 0.97) %>%
   # ideal gas law to get final concentration (units = uM)
   mutate(ch4_head = ideal_gas_law(pch4),
          co2_head = ideal_gas_law(pco2),
          n2o_head = ideal_gas_law(pn2o))


#_AQUEOUS SAMPLE 
#  concentration of gases dissolved in water sample at end of incubation,
#  in equilibrium with bottle headspace

methano_samples = methano_samples %>%
   # temperature-corrected Henry's Law constants (tKH)
   mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (incubation_temp + 273.15)) - (1 / 298.15))),
          tKH_co2 = KH_co2 * exp(KH_td_co2 * ((1 / (incubation_temp + 273.15)) - (1 / 298.15))),
          tKH_n2o = KH_n2o * exp(KH_td_n2o * ((1 / (incubation_temp + 273.15)) - (1 / 298.15)))) %>%
   # concentration dissolved in water sample
   mutate(ch4_aq = tKH_ch4 * pch4 * 10^6,
          co2_aq = tKH_co2 * pco2 * 10^6,
          n2o_aq = tKH_n2o * pn2o * 10^6)


##__Amount of gas produced by sediment + water sample

methano_samples = methano_samples %>%
   # total amount (umol) of gases in bottle at end of incubation
   mutate(vol_headspace = vol_bottle - (vol_sediment + vol_water),
          ch4_tot_umol = (ch4_aq * vol_water) + (ch4_head * vol_headspace),
          co2_tot_umol = (co2_aq * vol_water) + (co2_head * vol_headspace),
          n2o_tot_umol = (n2o_aq * vol_water) + (n2o_head * vol_headspace))




