#~~~
# Examine weekly differences in methanogenesis and ebullition rates
#
#~~~


library(tidyverse)
library(emmeans)

# create the 'fdat' and 'pond_data' data sets from the "stats_model-data" script 
source("Analysis-Scripts/stats_model-data.R")



#- Pairwise comparisons of methanogenesis rates

mtest = fdat %>%
  filter(week %in% c(4:9)) %>%
  drop_na("methanogenesis") %>%
  mutate(week = paste0("wk", week))


# anova and post hoc
tmod = aov(methanogenesis ~ treatment * week, data = mtest) 
summary(tmod)
TukeyHSD(tmod)


# pairwise comparisons
emmeans(tmod, ~week|treatment)

contrast(emmeans(tmod, ~week|treatment),  # contrast by week within treatment
         method = "pairwise")

contrast(emmeans(tmod, ~treatment|week),  # contrast by treatment within week
         method = "pairwise")




#- Pairwise comparisons of ebullition rates

etest = fdat %>%
  filter(week %in% c(4:9)) %>%
  drop_na("ch4_ebu_flux") %>%
  mutate(week = paste0("wk", week))


# anova and post hoc
emod = aov(ch4_ebu_flux ~ treatment * week, data = etest)
summary(emod)
TukeyHSD(emod)


# pairwise comparisons
emmeans(emod, ~week|treatment)

contrast(emmeans(emod, ~week|treatment),  # contrast by week within treatment
         method = "pairwise")

contrast(emmeans(emod, ~treatment|week),  # contrast by treatment within week
         method = "pairwise")



