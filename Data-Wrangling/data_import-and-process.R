#~~~
# Script for importing, processing, and combining data files
# By: Robert Johnson
#~~~


library(tidyverse)
library(lubridate)
library(janitor)


# Pond/Site Data
pond_data = read_csv("Data/R-Data/2020_pond-data.csv")


#---
#### Limno Data ####
#---


#---
# Daily Sonde Profiles
#---

# Function to wrangle each sonde profile datasheet as it is read in
#  select only the desired columns and rename to match across files
read_profile = function(.dat, .skip) {
   read_csv(.dat, skip = .skip) %>%
      clean_names() %>%
      select(date, time, site, temp_u_fffd_f, odo_percent_sat, odo_mg_l, p_h, contains("chlorophyll"), contains("cond"), pc_rfu, pc_ug_l,
             sal_psu, pressure_psi_a, depth_m, vertical_position_m, -contains("n_lf_cond")) %>%
      rename(pond_id = site,
             temp = temp_u_fffd_f,
             do_sat = odo_percent_sat,
             do = odo_mg_l,
             ph = p_h,
             chla = chlorophyll_u_fffd_g_l,
             chla_rfu = chlorophyll_rfu,
             phyco = pc_ug_l,
             phyco_rfu = pc_rfu,
             cond = cond_u_fffd_s_cm,
             sp_cond = sp_cond_u_fffd_s_cm,
             salinity = sal_psu)
}


# Profile Data

# Files without extra header rows
sonde_early = list.files(path = "./Data/R-Data/2020_sonde-profiles/without-extra-header-rows",
                         pattern = "hortsonde*",
                         full.names = T) %>%
   map_dfr(~read_profile(., .skip=0))

# Files with extra header rows at top
sonde_main = list.files(path = "./Data/R-Data/2020_sonde-profiles",
                        pattern = "hortsonde*",
                        full.names = T) %>%
   map_dfr(~read_profile(., .skip=5))


# Combined profile data
sonde_profiles = full_join(sonde_early, sonde_main)


# Clean up profile dataset
sonde_profiles = sonde_profiles %>%
   mutate(pond_id = str_remove(pond_id, "Pond "),
          date_time = as.POSIXct(mdy(date) + hms(time)),
          doy = yday(date_time)) %>%
   arrange(doy, pond_id, date_time) %>%
   # change temp data to Celcius
   mutate(temp = (temp - 32) / 1.8) %>%
   select(-date, -time) %>%
   relocate(date_time, doy)


   ## remove temporary objects
   rm(sonde_early, sonde_main)
   ##

   
#---
# MiniDOT Temp-DO Data
#---
{
# Data
mini_a = read_csv("Data/R-Data/2020_minidot_A.csv", skip=8) %>% remove_empty(which=c("rows", "cols"))
mini_b = read_csv("Data/R-Data/2020_minidot_B.csv", skip=8) %>% remove_empty(which=c("rows", "cols"))
mini_c = read_csv("Data/R-Data/2020_minidot_C.csv", skip=8) %>% remove_empty(which=c("rows", "cols"))
mini_d = read_csv("Data/R-Data/2020_minidot_D.csv", skip=8) %>% remove_empty(which=c("rows", "cols"))
mini_e = read_csv("Data/R-Data/2020_minidot_E.csv", skip=8) %>% remove_empty(which=c("rows", "cols"))
mini_f = read_csv("Data/R-Data/2020_minidot_F.csv", skip=8) %>% remove_empty(which=c("rows", "cols"))


# Add a pond identifier
mini_a = mini_a %>% mutate(pond_id = rep("A", nrow(.)))
mini_b = mini_b %>% mutate(pond_id = rep("B", nrow(.)))
mini_c = mini_a %>% mutate(pond_id = rep("C", nrow(.)))
mini_d = mini_a %>% mutate(pond_id = rep("D", nrow(.)))
mini_e = mini_a %>% mutate(pond_id = rep("E", nrow(.)))
mini_f = mini_a %>% mutate(pond_id = rep("F", nrow(.)))

# combine files
minidot_data = bind_rows(mini_a, mini_b, mini_c, mini_d, mini_e, mini_f)

# Clean up file
minidot_data = minidot_data %>%
   clean_names() %>%
   select(-unix_timestamp, -utc_date, -utc_time, -battery, -q) %>%
   mutate(date_time = as.POSIXct(mdy(cst_date) + hms(cst_time)),
          doy = yday(date_time)) %>%
   relocate(pond_id, date_time, doy) %>%
   select(-cst_date, -cst_time) %>%
   rename(surface_temp = temperature,
          surface_do = dissolved_oxygen,
          surface_do_sat = dissolved_oxygen_saturation)


   ## remove individual files
   rm(mini_a, mini_b, mini_c, mini_d, mini_e, mini_f)
   ##

}

   
#---
#### Lake Concentration GHG Samples ####
#---

# GHG data
ghg_lake_raw = read_csv("Data/R-Data/2020_ghgs_lake-conc.csv")

# Sample meta data
lake_sample_data = read_csv("Data/R-Data/2020_sample-metadata_lake-conc.csv") %>%
   # convert volumes from ml to L
   mutate(across(starts_with("vol"), ~(./1000)))


# Mean GHG concentration from syringes for lake samples (2 syringes per pond, 3 syringes for atmosphere)
lake_samples = ghg_lake_raw %>%
   # remove any inadvertent methanogenesis or ebullition samples
   filter(!(str_detect(sample_id, "R") | str_detect(sample_id, "P"))) %>%
   # mean value from syringe replicates
   group_by(sample_id) %>%
   summarise(across(ends_with("ppm"), ~mean(., na.rm=T))) %>%
   ungroup()


# Add GHG concentration data to sample meta data
lake_samples = left_join(lake_sample_data, lake_samples)

# Add DOY
lake_samples = lake_samples %>%
   mutate(date = mdy(date),
          doy = yday(date)) %>%
   relocate(doy, .after = date)


# Add surface water temperature to the lake GHG dataset
#  calculate mean temp between 2 - 20 cm depth from sonde profiles
lake_samples = lake_samples %>%
   left_join(sonde_profiles %>%
                group_by(pond_id, doy) %>%
                filter(depth_m > 0.02 & depth_m < 0.20) %>%
                summarise(surface_temp = mean(temp, na.rm=T)) %>%
                ungroup())


#---
#### Methanogenesis GHG Samples ####
#---

# GHG data
ghg_methano_raw = read_csv("Data/R-Data/2020_ghgs_methano-assay.csv")

# Sample meta data
methano_sample_data = read_csv("Data/R-Data/2020_sample-metadata_methano-assay.csv") %>%
   # convert volumes from ml to L
   mutate(across(starts_with("vol"), ~(./1000)))


# Add GHG concentration data to sample meta data
methano_samples = methano_sample_data %>%
   left_join(ghg_methano_raw) %>%
   # add date and time
   mutate(datetime_start = as.POSIXct(mdy(date_start) + hms(time_start)),
          datetime_end = as.POSIXct(mdy(date_end) + hms(time_end)),
          doy = yday(datetime_start)) %>%
   # add length of incubation period
   mutate(incubation_length = difftime(datetime_end, datetime_start, units="mins"),
          incubation_length = as.numeric(incubation_length))


#---
#### Ebullition Chamber GHG Samples ####
#---

# GHG data
ghg_ebu_raw = read_csv("Data/R-Data/2020_ghgs_ebullition.csv")

# Sample meta data
ebu_sample_data = read_csv("Data/R-Data/2020_sample-metadata_ebullition.csv") %>%
   # add a column for sample replicate
   mutate(replicate = str_sub(sample_id, -2, -1)) %>%
   relocate(replicate, .after = pond_id)


# Add GHG concentration data to sample meta data
ebu_samples = ebu_sample_data %>%
   left_join(ghg_ebu_raw) %>%
   # add DOY
   mutate(date_time = as.POSIXct(mdy(date) + hms(time)),
          doy = yday(date_time)) %>%
   select(-date, -time) %>%
   relocate(date_time, doy, .after = week)


# Calculate deployment length
ebu_samples = ebu_samples %>%
   group_by(week, pond_id, replicate) %>%
   arrange(doy, .by_group=T) %>%
   mutate(deployment_length = difftime(last(date_time), first(date_time), units="mins"),
          deployment_length = as.numeric(deployment_length)) %>%
   ungroup()


# Create separate data frames for the start and end of the deployments
ebu_start = ebu_samples %>%
   group_by(week, pond_id, replicate) %>%
   slice_min(order_by = doy) %>%
   ungroup()

ebu_end = ebu_samples %>%
   group_by(week, pond_id, replicate) %>%
   slice_max(order_by = doy) %>%
   ungroup()


# Add deployment time variables for use in combining later
ebu_start = ebu_start %>%
   mutate(doy = doy + 1,
          deployment_time = rep_len("t0", n()))

ebu_end = ebu_end %>%
   mutate(deployment_time = rep_len("t1", n()))


#---
#### Weather Data ####
#---

# Data
weather_data_raw = read_csv("Data/R-Data/2020_weather-data.csv", skip=1)


# Clean up variable names
weather_data = weather_data_raw %>%
   clean_names() %>%
   select(-number) %>%
   rename(date_time = date_time_gmt_05_00,
          wind_speed = wind_speed_m_s_lgr_s_n_20849581_sen_s_n_20843154,
          gust_speed = gust_speed_m_s_lgr_s_n_20849581_sen_s_n_20843154,
          par = par_u_fffd_mol_m_u_fffd_s_lgr_s_n_20849581_sen_s_n_20856725)


# Date and Time
weather_data = weather_data %>%
   mutate(date_time = mdy_hm(date_time),
          doy = yday(date_time)) %>%
   relocate(doy, .after = date_time)


# Anemometer height


