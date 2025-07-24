#~~~
# Create the full data set to be used for analyzing GHG data
#
# By: R. Johnson
#~~~


# Load packages
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)



#-- Step 1: Prepare the data

# Full data set
fdat = read_csv("Data/ghg-model-dataset_ms-data.csv") %>%
   # force date format (sometimes date format can be weird coming from Excel)
   mutate(date = ymd(date)) %>%
   # add nutrient and foodweb treatment identifiers
   mutate(trt_nutrients = case_when(pond_id %in% c("A", "B", "C") ~ "pulsed",
                                    pond_id %in% c("D", "E", "F") ~ "reference"),
          trt_fish = case_when(pond_id %in% c("B", "F") ~ "low",
                               pond_id %in% c("A", "D") ~ "medium",
                               pond_id %in% c("C", "E") ~ "high")) %>%
   # reorder treatments for figures, so pulsed is plotted on top of reference
   mutate(trt_nutrients = factor(trt_nutrients, levels = c("reference", "pulsed")))



# Pond/Site Data (create if needed, since lots of older code still calls this)
pond_data = tibble(pond_id = c('A', 'B', 'C', 'D', 'E', 'F'),
                   trt_nutrients = rep(c("yes", "no"), each=3),
                   trt_fish = c('medium', 'low', 'high', 'medium', 'high', 'low'))


