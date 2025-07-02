#--
# Calculate the mean and SE of each variable used in the ecdf() comparison analysis 
# for both pulsed and reference ponds for the analysis window for each disturbance event
#-- 


library(tidyverse)

source("Analysis-Scripts/stats_model-data.R")


# focal variables from comparison analysis
varlist.main <- c("ch4_flux","co2_flux",
                  "GPP","R","NEP",
                  "do_sat","bottom_do_sat",
                  "tn","tp",
                  "temp","sonde_zmix")

wwidth <- 5 #days: width of windows, e.g. following nutrient pulses
pulse1 <- 177: (177 + wwidth - 1)#doy
pulse2 <- 212: (212 + wwidth - 1)
derecho <- 224: (224 + wwidth - 1)
heatwave <- 185:190


# pulse 1
v1 = fdat %>%
  filter(doy %in% pulse1) %>%
  summarize(across(all_of(varlist.main), ~mean(., na.rm=T)), .by=pond_id) %>%
  mutate(event = "P1")

# heat event
vh = fdat %>%
  filter(doy %in% heatwave) %>%
  summarize(across(all_of(varlist.main), ~mean(., na.rm=T)), .by=pond_id) %>%
  mutate(event = "H")

# pulse 2
v2 = fdat %>%
  filter(doy %in% pulse2) %>%
  summarize(across(all_of(varlist.main), ~mean(., na.rm=T)), .by=pond_id) %>%
  mutate(event = "P2")

# derecho
vd = fdat %>%
  filter(doy %in% derecho) %>%
  summarize(across(all_of(varlist.main), ~mean(., na.rm=T)), .by=pond_id) %>%
  mutate(event = "D")


# combine
df = bind_rows(v1, vh, v2, vd) %>%
  left_join(fdat %>% select(pond_id, treatment) %>% slice(1, .by=pond_id))


# treatment mean and SE
se = function(.x) { sd(.x) / sqrt(length(.x)) }

df2 = df %>%
  summarize(across(where(is.numeric), list(mean=mean, se=se)), .by=c(treatment, event))



