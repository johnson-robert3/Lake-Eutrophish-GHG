# 2020 Hort Farm Pond Experiment

Greenhouse Gas Dynamics in Shallow Aquatic Ecosystems in Response to a Pulse Nutrient Addition


## Data

### Data Processing Notes

**Surface values** of pond limno variables taken from sonde profiles are the mean of all measurements between 5 & 50 cm (0.05 - 0.50 m)

**Bottom values** of pond limno variables taken from sonde profiles are the mean of all measurements from the bottom 20 cm

**DO cleaning:** there were a few times when miniDOT loggers were removed from ponds for >30 minutes for data download (leading to a missed measurement), and DO data were linearly interpolated to backfill these gaps; for metabolism calculations, any time DO concentration (from miniDOT loggers) drops by 2.0 mg/L or more from the previous measurement, that point, along with the following 5 points (i.e., 3 hours total) are dropped, and DO concentration data are then backfilled via linear interpolation 

**Metabolism functions:** currently, the corrected/interpolated DO concentration data (as above) are used to calculate metabolism (not rolling window data, but this is another possibility)

**Z-mix:** currently the mixed-layer depth is set as the depth of the thermocline (from t-chains in ponds B and F), but during times of mixing/turnover or when the function couldn't correctly calculate a thermocline depth, Z-mix is set to 1.5 m

**DOC data:** sample values were corrected by subtracting the mean value of the blanks within the corresponding run. the first blank in each run was excluded (carryover from flushing) from this mean, as well as two blanks that were outliers (one had TOC > 1000 ppb, one had TOC < 10 ppb) 

**GHG Model Dataset:** days with erroneous metabolism values (i.e, negative GPP, positive R) are currently excluded from the total compiled dataset for use with GHG models; pond surface values of temperature and chlorophyll are calculated from sonde profiles and are each the mean of all values between 5 & 50 cm (0.05 - 0.50 m)



## Modeling

### Modeling Notes
To recreate the full model dataset from all individual datasets, run scripts in this order:

   1. data_import-and-process
   2. data_stratification
   3. data_ghg-calculations
   4. doc_data-sheet-processing
   5. metabolism-calcs
   6. data_model-dataset


### Next Steps / To Do 



## Figures

### Figs. To Make

- [ ] Ebullition time-series


