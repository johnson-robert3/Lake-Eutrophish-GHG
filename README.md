# 2020 Hort Farm Pond Experiment

Greenhouse Gas Dynamics in Shallow Aquatic Ecosystems in Response to a Pulse Nutrient Addition


## Script Notes

**'data_import_EDI'**: Use this script to import any/all data into R. _Other scripts should no longer be used to import datasets._ 

**'edi_data_output'**: Where all dfs created throughout other R scripts are output in their final form into the shared EDI Box folder. Contains notes about which scripts the OG dfs were created in.  

**'data_model-dataset'**: Use this script to recreate the full, combined dataset to use in statistical models. The script also contains notes about parts of other scripts that have to be run after importing individual datasets from the 'data_import_EDI' script to perform any necessary calculations before combining all dfs into the full model dataset. 

**'stats_distro-comparison2_rj'**: Use this script to run the empirical cumulative distribution function analysis created by Jon used in the manuscript.



## Data Processing Notes

**Surface values** of pond limno variables taken from sonde profiles are the mean of all measurements between 5 & 50 cm (0.05 - 0.50 m)

**Bottom values** of pond limno variables taken from sonde profiles are the mean of all measurements from the bottom 20 cm

**DO cleaning:** there were a few times when miniDOT loggers were removed from ponds for >30 minutes for data download (leading to a missed measurement), and DO data were linearly interpolated to backfill these gaps; for metabolism calculations, any time DO concentration (from miniDOT loggers) drops by 2.0 mg/L or more from the previous measurement, that point, along with the following 5 points (i.e., 3 hours total) are dropped, and DO concentration data are then backfilled via linear interpolation 

**Metabolism functions:** currently, the corrected/interpolated DO concentration data (as above) are used to calculate metabolism

**Z-mix:** currently the mixed-layer depth is set as the depth of the thermocline (from t-chains in ponds B and F), but during times of mixing/turnover or when the function couldn't correctly calculate a thermocline depth, Z-mix is set to 1.5 m

**DOC data:** sample values were corrected by subtracting the mean value of the blanks within the corresponding run. the first blank in each run was excluded (carryover from flushing) from this mean, as well as two blanks that were outliers (one had TOC > 1000 ppb, one had TOC < 10 ppb) 

**GHG Model Dataset:** days with erroneous metabolism values (i.e, negative GPP, positive R) are currently excluded from the total compiled dataset for use with GHG models; pond surface values of temperature and chlorophyll are calculated from sonde profiles and are each the mean of all values between 5 & 50 cm (0.05 - 0.50 m)



## Modeling Notes

To recreate the full model dataset from all individual datasets, run scripts in this order:

   1. data_import_EDI
   2. data_model-dataset
   3. stats_model-data


