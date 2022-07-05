# 2020 Hort Farm Pond Experiment

Greenhouse Gas Dynamics in Shallow Aquatic Ecosystems in Response to a Pulse Nutrient Addition


## Data

### Data Processing Notes

**Surface values** of pond limno variables taken from sonde profiles are the mean of all measurements between 5 & 50 cm (0.05 - 0.5 m)

**Bottom values** of pond limno variables taken from sonde profiles are the mean of all measurements from the bottom 20 cm

**DO cleaning:** any time DO concentration (from miniDOT loggers) drops by 2.0 mg/L or more from the previous measurement, that point, along with the following 5 points (i.e., 3 hours total) are dropped, and DO concentration data are then linearly interpolated to fill the gap

**Metabolism functions:** currently, the corrected/interpolated DO concentration data (as above) are used to calculate metabolism (not rolling window data, but this is another possibility)

**Z-mix:** currently the mixed-layer depth is set as the depth of the thermocline (from t-chains in ponds B and F), but during times of mixing/turnover or when the function couldn't correctly calculate a thermocline depth, Z-mix is set to 1.75 m

**DOC data:** sample values were corrected by subtracting the mean value of the blanks within the corresponding run. the first blank in each run was excluded (carryover from flushing) from this mean, as well as two blanks that were outliers (one had TOC > 1000 ppb, one had TOC < 10 ppb) 

**GHG Model Dataset:** days with erroneous metabolism values (i.e, negative GPP, positive R) are currently excluded from the total compiled dataset for use with GHG models; pond surface values of temperature and chlorophyll are calculated from sonde profiles and are each the mean of all values between 5 & 60 cm (0.05 - 0.6 m); *this has been updated to use sonde surface values (as above) instead of recalculating, so these values are now means from 0.05 - 0.5 m (instead of 0.6)*


### Next Steps / To Do 

- [x] Update the total GHG model dataset for LME models with updated temp-profile, metabolism, and DOC data
- [ ] How should we treat erroneous metabolism estimates (GPP<0, R>0)?



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

- [ ] **CO2 LME model**
   - [x] Create the full model
   - [x] Iteratively remove components to find the best-fit model
   - [x] Test the model using just the pulsed ponds (difference between periods?)
   - [x] Compare treatments, but excluding the pre-pulse (i.e., BASE) period
   - [x] Are there better models (higher R-squared) but that have non-sig. fixed effects?
   - [ ] Add nutrient variables back to model, now that dates have been aligned so the full data are there
   - [ ] Remove alkalinity from the model (*alkalinity is used in the equation to calculate CO2, so of course it is a sig. effect*)


- [ ] Try using conditional inference trees to determine which predictor variables are most likely important (to then use to create an initial model)
- [ ] Try using GAM instead of LME to analyzes GHG flux data
- [ ] Use PCA or NMDS instead, since we have so many predictor variables?
- [ ] Should any data be transformed prior to analysis?



## Figures

### Figs. To Make

- [ ] **CO2**
   - [x] Over time
   - [ ] CO2 vs variables used in final model
   - [ ] Effect sizes from model output
- [x] 6-panel GHG figure (gas X nutrient treatment)


