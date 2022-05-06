# 2020 Hort Farm Pond Experiment

Greenhouse Gas Dynamics in Shallow Aquatic Ecosystems in Response to a Pulse Nutrient Addition


## Data

### Data Processing Notes

**Surface values** of pond limno variables taken from sonde profiles are the mean of all measurements between 5 & 50 cm (0.05 - 0.5 m)

**Bottom values** of pond limno variables taken from sonde profiles are the mean of all measurements from the bottom 20 cm

**DO cleaning:** any time DO concentration (from miniDOT loggers) drops by 2.0 mg/L or more from the previous measurement, that point, along with the following 5 points (i.e., 3 hours total) are dropped, and DO concentration data are then linearly interpolated to fill the gap

**Metabolism functions:** currently, the corrected/interpolated DO concentration data (as above) are used to calculate metabolism (not rolling window data, but this is another possibility)

**Z-mix:** currently the mixed-layer depth is set as the depth of the thermocline (from t-chains in ponds B and F), but during times of mixing/turnover or when the function couldn't correctly calculate a thermocline depth, Z-mix is set to 1.75 m

**GHG Model Dataset:** days with erroneous metabolism values (i.e, negative GPP, positive R) are currently excluded from the total compiled dataset for use with GHG models; pond surface values of temperature and chlorophyll are calculated from sonde profiles and are each the mean of all values between 5 & 60 cm (0.05 - 0.6 m)


### Next Steps / To Do 

- [x] Investigate times when thermocline depth is within 0.5m of the top or bottom. Are there many of these?
  - [x] What about thermocline within 0.3m?
- [x] Change thermo depth to 1.75m when the estimate is close to the top or bottom 
- [x] Investigate large, rapid drops in pond DO concentration
  - [x] Do they follow a consistent shape or response curve? (mostly, but not every single time)
  - [x] Make a new 6-panel figure (facet by pond) to view DO for specific days across all ponds
- [x] Write a function to remove (and interpolate) these drops in DO
- [x] Make a figure comparing metabolism output from metab.kalman() using raw DO data, corrected/interpolated data, and rolling window corrected data
- [x] Compare other cutoff values for removing DO drops (2.0, 1.0, 1.5 mg/l)
- [x] Correct DOC values for blanks within each run
- [x] Update the total GHG model dataset for LME models with updated temp-profile, metabolism, and DOC data
- [ ] How should we treat erroneous metabolism estimates (GPP<0, R>0)?



## Modeling

### Modeling Notes


### Next Steps / To Do 

- [ ] **CO2 model**
   - [x] Create the full model
   - [x] Iteratively remove components to find the best-fit model
   - [x] Test the model using just the pulsed ponds (difference between periods?)
   - [x] Compare treatments, but excluding the pre-pulse (i.e., BASE) period
   - [x] Are there better models (higher R-squared) but that have non-sig. fixed effects?
   - [ ] Add nutrient variables back to model, now that dates have been aligned so the full data are there
   - [ ] Remove alkalinity from the model [*alkalinity is used in the equation to calculate CO2, so of course it is a sig. effect*]
- [x] **CH4 model**
   - [x] Create the full model
   - [x] Iteratively remove components to find the best-fit model
   - [x] Test the model using just the pulsed ponds (difference between periods?)
   - [x] Compare treatments, but excluding the pre-pulse (i.e., BASE) period
- [x] **N2O model**
   - [x] Create the full model
   - [x] Iteratively remove components to find the best-fit model
   - [x] Test the model using just the pulsed ponds (difference between periods?)
   - [x] Compare treatments, but excluding the pre-pulse (i.e., BASE) period [*I don't think this makes sense for N2O; there is sig. DOY effect*]
- [x] Use DOY as a fixed effect variable in models and include a treatment:time interaction (gases follow different temporal dynamics due to differences in plant phenology). Is time a significant variable? Is DOY better than Period?
