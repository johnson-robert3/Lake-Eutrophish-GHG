# 2020 Hort Farm Pond Experiment

This repository contains the code needed to recreate results and figures from the following manuscript: 

Title: "*Immediate and cumulative effects of nutrient and extreme weather disturbances on pond greenhouse gas emissions*"

Authors: Johnson, RA, TJ Butts, EA Albright, SJ Hall, JA Walter, and GM Wilkinson.

Data and metadata from this study are available in the Environmental Data Initiative repository and can be accessed [here](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2238.1). 

Copies of all scripts and files used for analyses and figures/tables at the time of manuscript acceptance have been archived in Zenodo. [![DOI](https://zenodo.org/badge/226179406.svg)](https://doi.org/10.5281/zenodo.18990039)

The compiled dataset used for the manuscript is available in this Github repository (Data/ghg-model-dataset_ms-data.csv) with accompanying metadata (Data/ghg-model-dataset_ms-metadata.xlsx). 

Repository maintained by RA Johnson (email: robert.a.johnson@wisc.edu).


## Modeling Notes

To recreate the full model dataset from all individual datasets, run scripts in this order:

   1. Data-Wrangling/pull-data-from-EDI.R
   2. Data-Wrangling/data_import_EDI.R
   3. Data-Wrangling/data_model-dataset.R
   4. Analysis-Scripts/stats_model-data.R



## Script Notes

### Data

**'pull-data-from-EDI'**: Import datasets directly from the EDI repository. _Other scripts should no longer be used to import datasets._ 

**'data_import_EDI'**: Renames datasets and variables after pulling from EDI to match code used across other scripts.

**'data_model-dataset'**: Recreate the full, combined dataset for use in analyses from the individual datasets pulled from EDI.

**'edi_data_output'**: This script is where all dfs created throughout other R scripts were output in their final form into the shared EDI Box folder. Contains notes about which scripts the OG dfs were created in. 

### Analyses

**'stats_model-data'**: Slightly process the full, combined dataset to recreate the dataset actually used for analyses. 

**'stats_distro-comparison2_rj'**: Run the window comparison analysis used in the manuscript. 

**'flux_emissions'**: Calculate flux values for specific time intervals. 

**'rate_comparisons'**: ANOVAs with pairwise comparisons for methanogenesis potential and ebullition. 

**'stats_event-windows-mean-se'**: Values from comparison analysis event windows to create Table S1 from the supplement. 

**'stats_foodwebOrder_rj'**: Recreate the Spearman Rank Correlation analysis and figure from the Supplement. 

### Figures

**'figs_manuscript'**: Recreate figures 1, 2, and 4 from the manuscript. 

**'figs_supplement'**: Recreate figures from the supplement. 

**'distro_comparison_fig_update'**: Recreate the window comparison analysis figure from the manuscript (figure 3). 

**'figs_functions'**: Functions and aesthetics used across multiple figure scripts.


## Data Processing Notes

**Surface values** of pond limno variables taken from sonde profiles are the mean of all measurements between 5 & 50 cm (0.05 - 0.50 m)

**Bottom values** of pond limno variables taken from sonde profiles are the mean of all measurements from the bottom 20 cm

**DO cleaning:** there were a few times when miniDOT loggers were removed from ponds for >30 minutes for data download (leading to a missed measurement), and DO data were linearly interpolated to backfill these gaps; for metabolism calculations, any time DO concentration (from miniDOT loggers) drops by 2.0 mg/L or more from the previous measurement, that point, along with the following 5 points (i.e., 3 hours total) are dropped, and DO concentration data are then backfilled via linear interpolation 

**Metabolism functions:** currently, the corrected/interpolated DO concentration data (as above) are used to calculate metabolism

**GHG Model Dataset:** days with erroneous metabolism values (i.e, negative GPP, positive R) are currently excluded from the total compiled dataset for use with GHG models


