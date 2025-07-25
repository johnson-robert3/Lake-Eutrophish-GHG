# 2020 Hort Farm Pond Experiment

This Github repository contains the data and code files needed to recreate all results and figures from the following manuscript: 

Title: "*Immediate and lagged effects of nutrient and extreme weather disturbances on pond greenhouse gas emissions*"

Authors: Johnson, RA, TJ Butts, EA Albright, SJ Hall, JA Walter, and GM Wilkinson.

Data used in the manuscript are currently available in this Github repository (Data/ghg-model-dataset_ms-data.csv), with accompanying metadata (Data/ghg-model-dataset_ms-metadata.xlsx). Individual datasets will be archived in the Environmental Data Initiative repository prior to manuscript acceptance.


## Modeling Notes

To recreate the full model dataset from all individual datasets, run scripts in this order:

   1. data_import_EDI
   2. data_model-dataset
   3. stats_model-data



## Script Notes

**'data_import_EDI'**: Use this script to import any/all data into R. _Other scripts should no longer be used to import datasets._ 

**'edi_data_output'**: Where all dfs created throughout other R scripts are output in their final form into the shared EDI Box folder. Contains notes about which scripts the OG dfs were created in. 

**'data_model-dataset'**: Use this script to recreate the full, combined dataset to use in statistical models. 

**'stats_distro-comparison2_rj'**: Use this script to run the window comparison analysis created by Jon used in the manuscript. 
**'distro_comparison_fig_update'**: Use this script to create the window comparison analysis figure used in the manuscript. 

**'stats_foodwebOrder_rj'**: Script for creating the foodweb treatment Spearman rank correlation figure. 



## Data Processing Notes

**Surface values** of pond limno variables taken from sonde profiles are the mean of all measurements between 5 & 50 cm (0.05 - 0.50 m)

**Bottom values** of pond limno variables taken from sonde profiles are the mean of all measurements from the bottom 20 cm

**DO cleaning:** there were a few times when miniDOT loggers were removed from ponds for >30 minutes for data download (leading to a missed measurement), and DO data were linearly interpolated to backfill these gaps; for metabolism calculations, any time DO concentration (from miniDOT loggers) drops by 2.0 mg/L or more from the previous measurement, that point, along with the following 5 points (i.e., 3 hours total) are dropped, and DO concentration data are then backfilled via linear interpolation 

**Metabolism functions:** currently, the corrected/interpolated DO concentration data (as above) are used to calculate metabolism

**GHG Model Dataset:** days with erroneous metabolism values (i.e, negative GPP, positive R) are currently excluded from the total compiled dataset for use with GHG models


