
# test NEON equations for calculating original dissolved gas concentration

# https://github.com/NEONScience/NEON-dissolved-gas/tree/master/neonDissGas


# my interpretation of the final equation presented (point #7) in the Readme file for neonDissGas
# I have converted R (ideal gas constant) here to the correct value for the units I/we have out data in

# [gas] = (atm.press) * ( (vol_air*(head.eq_ppm - atm_ppm)) / ((0.0821 * temp * vol_water) + tKH * head.eq_ppm) )

#  I am not completely sure I have interpreted/transcribed this equation correctly. The equation is presented as an image in the Readme,
#  and the function source-code is hard to follow / doesn't seem to directly follow this equation.


test = lake_conc %>%
   mutate(
      # equation 1 (point #2) from the Readme (using total micromoles of gas in sample partitions that I have already calculated)
      neon1_ch4 = ((ch4_head.eq * vol_air) + (ch4_water.eq * vol_water) - (ch4_atmo * vol_air)) / vol_water, 
      # equation 6 (point #7) from the Readme (also detailed above), but with values (e.g., R) converted to the units my data are in
      neon2_ch4 = (0.97) * ((20 * (ch4_ppm - ch4_atmo_ppm)) / ((0.0821 * (surface_temp+273.15) * 40) + tKH_ch4 * ch4_ppm))) %>%
   #
   select(-contains("co2"), -contains("n2o"), -notes)


# CH4 concentrations calculated using equation 1 here (neon1_ch4) match my values calculated in the 'data_ghg-calculations' script
windows(); test %>% select(ch4_lake, neon1_ch4) %>% plot()

# CH4 concentrations calculated using equation 6 here (neon2_ch4) are similar to my calculated values, but are slightly lower.
windows(); test %>% select(ch4_lake, neon2_ch4) %>% plot()




#-- Calculate dissolved CH4 concentrations directly using the NEON-provided function

# Function to calculate original concentration of a gas dissolved in water following equilibration with a headspace
# https://github.com/NEONScience/NEON-dissolved-gas/blob/master/neonDissGas/R/def.calc.sdg.conc.R
source("Data-Wrangling/neonDissGas.R")

# set volumes in ml, and atmo. pressure in kPa
test2 = lake_conc %>%
   mutate(vol_water = 40, vol_air = 20, baro=98.285) %>%
   as.data.frame()


# using NEON R function
# outputs concentration of dissolved gases in mol/L
test3 = def.calc.sdg.conc(test2,
  volGas = 'vol_air',
  volH2O = 'vol_water',
  baro = 'baro',
  waterTemp = 'surface_temp',
  headspaceTemp = 'surface_temp',
  eqCO2 = 'co2_ppm',
  sourceCO2 = 'co2_atmo_ppm',
  eqCH4 = 'ch4_ppm',
  sourceCH4 = 'ch4_atmo_ppm',
  eqN2O = 'n2o_ppm',
  sourceN2O = 'n2o_atmo_ppm')

# convert calculated dissolved gas concentrations from mol/L to umol/L
test4 = test3 %>%
   mutate(across(c(dissolvedCO2, dissolvedCH4, dissolvedN2O), ~.*10^6))


# view
windows(); test4 %>% select(ch4_lake, dissolvedCH4) %>% plot()


