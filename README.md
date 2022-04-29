# 2020 Hort Farm Pond Experiment

Greenhouse Gas Dynamics in Shallow Aquatic Ecosystems in Response to a Pulse Nutrient Addition


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
- [ ] How should we treat erroneous metabolism estimates (GPP>0, R<0)?
