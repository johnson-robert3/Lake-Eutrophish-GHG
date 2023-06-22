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

# Import, process, and combine individual sonde profiles
{
# Function to wrangle each sonde profile data sheet as it is read in
#  select only the desired columns and rename to match across files
read_profile = function(.dat, .skip) {
   # read_csv(.dat, skip = .skip) %>%
   read.csv(.dat, skip = .skip) %>%
      clean_names() %>%
      # select(date, time, site, temp_f, odo_percent_sat, odo_mg_l, p_h, contains("chlorophyll"), contains("cond"), pc_rfu, pc_ug_l,
      #        sal_psu, pressure_psi_a, depth_m, vertical_position_m, -contains("n_lf_cond")) %>%
      select(date, time, site, temp_f, odo_sat, odo_mg_l, p_h, contains("chlorophyll"), contains("cond"), pc_rfu, pc_ug_l,
             sal_psu, pressure_psi_a, depth_m, vertical_position_m, -contains("n_lf_cond")) %>%
      rename(pond_id = site,
             temp = temp_f,
             # do_sat = odo_percent_sat,
             do_sat = odo_sat,
             do = odo_mg_l,
             ph = p_h,
             # chla = chlorophyll_g_l,         # units are actually ug/L; new clean_names() drops "u" for some reason
             chla = chlorophyll_mg_l,         # units are actually ug/L; new clean_names() makes this "mg" for some reason
             chla_rfu = chlorophyll_rfu,
             phyco = pc_ug_l,
             phyco_rfu = pc_rfu,
             # cond = cond_s_cm,               # units are actually uS/cm; new clean_names() drops "u" for some reason
             cond = cond_m_s_cm,               # units are actually uS/cm; new clean_names() makes this "m_g" for some reason
             # sp_cond = sp_cond_u_fffd_s_cm,
             sp_cond = sp_cond_m_s_cm,         # units are actually uS/cm; new clean_names() makes this "m_g" for some reason
             salinity = sal_psu,
             vert_m = vertical_position_m)
}


# Profile Data

# Files without extra header rows
sonde_nohead = list.files(path = "./Data/R-Data/2020_sonde-profiles/without-extra-header-rows",
                         pattern = "hortsonde*",
                         full.names = T) %>%
   map_dfr(~read_profile(., .skip=0))

# Files with extra header rows at top
sonde_head = list.files(path = "./Data/R-Data/2020_sonde-profiles",
                        pattern = "hortsonde*",
                        full.names = T) %>%
   map_dfr(~read_profile(., .skip=5))


# Combined profile data
sonde_profiles = full_join(sonde_nohead, sonde_head)


   # View the number of data points in each profile
   sonde_profiles %>% 
      group_by(date, pond_id) %>% 
      summarize(N = n()) %>% 
      ungroup() %>% 
      mutate(pond_id = str_remove(pond_id, pattern="Pond ")) %>%
      # pivot_wider(id_cols = date, names_from = pond_id, values_from = N) %>%
      mutate(doy = yday(mdy(date))) %>%
      filter(doy!=1) %>%
      relocate(doy, .after=date) %>%
      # arrange(doy) %>%
      arrange(desc(N)) %>%
      View
   
      ## DOY 217 Pond E has over 1200 data points; two profiles were recorded and the sonde sat recording out of water for several minutes between the profiles
      ## want to keep just the first profile taken - the first 123 rows/measurements of the   
   
   
   # identify "blank" profiles
   sonde_profiles %>%
      filter(if_all(temp:vert_m, ~is.na(.))) %>%
      mutate(doy = yday(mdy(date))) %>%
      relocate(doy, .after=date) %>%
      select(doy, pond_id) %>%
      distinct() %>%
      View
   
      ## DOY 162 Pond C; DOY 185 Pond F; DOY 210 Pond D
      ## blank profiles (no data) were recorded on these 3 days along with the actual profiles (with data) in these ponds for some reason


# Clean up profile dataset
sonde_clean = sonde_profiles %>%
   
   # remove the times (n=3) when a duplicate profile appears in the data set, but with no data
   # DOY 162 Pond C; DOY 185 Pond F; DOY 210 Pond D
   filter(!(is.na(temp) & is.na(do) & is.na(chla) & is.na(vert_m))) %>%
   # DOY 162 Pond C also has some rows where all data are zeros in addition to the blank profile
   filter(!(pond_id=="Pond C" & temp==32 & do==0 & chla==0 & vert_m==0)) %>%
   
   # clean up data
   mutate(pond_id = str_remove(pond_id, "Pond "),
          date_time = as.POSIXct(mdy(date) + hms(time)),
          doy = yday(date_time)) %>%
   arrange(doy, pond_id, date_time) %>%
   select(-date, -time) %>%
   relocate(date_time, doy) %>%
   # change temp data to Celcius
   mutate(temp = (temp - 32) / 1.8) %>%
   # remove errant measurements recorded as 1970-01-01
   filter(!(doy==1))

   # correct vertical position (depth sensor and probes not at same height)
   # add 5 cm to all
   # mutate(vert_m = vert_m + 0.05,
   #        vert_m = if_else(vert_m<0, 0, vert_m)) %>%


# correct messed up double profile for Pond E on DOY 217
sonde_profiles = sonde_clean %>%
   # remove entire pond E for doy 217
   filter(!(pond_id=='E' & doy==217)) %>%
   # add just the first profile from pond E back to the full dataset (first 123 rows)
   bind_rows(sonde_clean %>%
                filter(pond_id=='E' & doy==217) %>%
                slice_head(n=123))


# Output full sonde profile dataset to a CSV file, so individual files don't all need to be read in & processed each time
write.csv(sonde_profiles, file = "Data/sonde-profiles_all-data_2022-07-20.csv", row.names=FALSE)

   ## remove temporary objects
   rm(sonde_nohead, sonde_head, sonde_clean)
   ##
}

   
#- Create the 'sonde_profiles' data set from the "data_import_EDI" script
   

##__Surface water means 
#  Depth: 5-50 cm

sonde_surface = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   filter(vert_m >= 0.05 & vert_m <= 0.50) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup() %>%
   # sonde profiles missing for Pond C (DOY 231) and Pond B (DOY 151)
   # add these days and linearly interpolate for all variables, so that surface values aren't missing
   add_row(pond_id = "B", doy = 151) %>%
   add_row(pond_id = "C", doy = 231) %>%
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(across(temp:salinity, ~zoo::na.approx(.))) %>%
   ungroup()


# Assign depth intervals to data for plotting 
sonde_int = sonde_profiles %>%
   # 10cm depth intervals
   mutate(depth_int = case_when(between(vert_m, -0.1, 0.1) ~ 0.1,
                                between(vert_m, 0.1, 0.2) ~ 0.2,
                                between(vert_m, 0.2, 0.3) ~ 0.3,
                                between(vert_m, 0.3, 0.4) ~ 0.4,
                                between(vert_m, 0.4, 0.5) ~ 0.5,
                                between(vert_m, 0.5, 0.6) ~ 0.6,
                                between(vert_m, 0.6, 0.7) ~ 0.7,
                                between(vert_m, 0.7, 0.8) ~ 0.8,
                                between(vert_m, 0.8, 0.9) ~ 0.9,
                                between(vert_m, 0.9, 1.0) ~ 1.0,
                                between(vert_m, 1.0, 1.1) ~ 1.1,
                                between(vert_m, 1.1, 1.2) ~ 1.2,
                                between(vert_m, 1.2, 1.3) ~ 1.3,
                                between(vert_m, 1.3, 1.4) ~ 1.4,
                                between(vert_m, 1.4, 1.5) ~ 1.5,
                                between(vert_m, 1.5, 1.6) ~ 1.6,
                                between(vert_m, 1.6, 1.7) ~ 1.7,
                                between(vert_m, 1.7, 1.8) ~ 1.8,
                                between(vert_m, 1.8, 1.9) ~ 1.9,
                                between(vert_m, 1.9, 2.0) ~ 2.0,
                                vert_m > 2.0 ~ 2.1)) %>%
   # mean values within each depth interval
   group_by(pond_id, doy, depth_int) %>%
   summarize(across(temp:salinity, mean, na.rm=T)) %>%
   ungroup() 


##__Bottom water means 
#  Depth: bottom 20 cm

### I think we should actually use 20 cm instead of 30 (need to check with group)

sonde_bottom = sonde_int %>%
   group_by(pond_id, doy) %>%
   arrange(depth_int, .by_group=T) %>%
   slice_tail(n=2) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup() %>%
   # sonde profiles missing for Pond C (DOY 231) and Pond B (DOY 151)
   # add these days and linearly interpolate for all variables, so that bottom values aren't missing
   add_row(pond_id = "B", doy = 151) %>%
   add_row(pond_id = "C", doy = 231) %>%
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(across(temp:salinity, ~zoo::na.approx(.))) %>%
   ungroup()


# alternative way for bottom water (using measured max depth)
# sonde_bottom2 = sonde_profiles %>%
#    group_by(pond_id, doy) %>%
#    filter(vert_m <= max(vert_m) & vert_m >= (max(vert_m) - 0.30)) %>%
#    summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
#    ungroup()


#---
# HOBO T-chains
#---

# Import, process, and combine individual Hobo data files
{
# Function to wrangle each hobo file as it is read in
#  Keep only date-time and temp columns and match across files
read_hobo = function(.dat) {
   read_csv(.dat, skip=1) %>%
      clean_names() %>%
      select(starts_with("date"), starts_with("temp")) %>%
      rename(date_time = starts_with("date"), 
             temp = starts_with("temp")) %>%
      filter(!(is.na(temp)))
}


# Read in all Hobo t-chain files
#  extract pond ID and depth from file name

hobo_files = list.files(path = "./Data/R-Data/2020_hobo-tchains",
                        pattern = "*.csv",
                        full.names = T)


hobo_temp = tibble(file_name = hobo_files) %>%
   # run the read_hobo() function over all files
   mutate(file_contents = map(file_name, ~read_hobo(.))) %>%
   # unpack the files into a data frame
   unnest(cols = c(file_contents)) %>%
   # extract pond and depth from file name
   mutate(file_name = str_remove(file_name, "./Data/R-Data/2020_hobo-tchains/"),
          file_name = str_remove(file_name, ".csv")) %>%
   separate(col = file_name, into = c("pond_id", "depth"), sep="_") %>%
   mutate(pond_id = str_remove(pond_id, "Pond")) %>%
   # update formats
   mutate(date_time = mdy_hms(date_time)) %>%
   # convert temperatures to Celcius
   mutate(temp = (temp - 32) / 1.8)


# output processed t-chain data file, so raw files don't need to be read in and processed each time
write.csv(hobo_temp, file = "Data/hobo_t-chain_profiles.csv", row.names=FALSE)
}


# Data (from full, processed t-chain dataset)
# hobo_temp = read_csv("Data/hobo_t-chain_profiles.csv")

   # This t-chain dataset (hobo_temp), processed by me, contains one additional measurement not present in the dataset from Ellen
   # Extra measurement is from:  2020-08-21 14:00:00, Pond F, 0.5m depth
   # The logger at 0.5m in pond F stopped logging at this time and didn't make it through the end of the experiment
   # This caused a "logged" event in the datasheet next to this temp measurement (at 14:00), which would normally be logged in
   #  a new row, after the final temperature measurement (when logging is manually stopped)
   # If rows containing a "logged" event in datasheets were filtered out, this could explain the omission of this final measurement. 


#- Create the 'hobo_temp' data set from the "data_import_EDI" script


#---
# MiniDOT Surface Temp DO Time-Series Data
#---

# Import, process, and combine all miniDOT temp/DO files
{
# Function to pre-process each file as it is read in
read_minidot = function(.dat, .skip=9) {
   read_csv(.dat, skip = .skip,
            col_names = c("unix", "UTC", "date_time", "battery", "temp", "do", "do_sat", "q")) %>%
      remove_empty(which = c("rows", "cols")) %>%
      select(-unix, -UTC, -battery, -q)
}


# MiniDOT loggers were removed from ponds to download data and re-deployed on DOYs 150, 164, & 192

# DOY 150 [2020-05-29], DOY 164 [2020-06-12], DOY 192 [2020-07-10]

   
# Pond A
mini_a = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_A.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("A", nrow(.))) %>%
   # change all times to be on the hour or half-hour
   # fix time-stamps that were altered during re-deployment
   mutate(date_time = case_when(minute(date_time) %in% c(15,45) ~ floor_date(date_time, "30 minutes"),
                                TRUE ~ round_date(date_time, unit="30 minutes")))
   

# Pond B
mini_b = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_B.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("B", nrow(.))) %>%
   # change all times to be on the hour or half-hour
   # fix time-stamps that were altered during re-deployment
   mutate(date_time = round_date(date_time, "30 minutes"))


# Pond C
mini_c = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_C.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("C", nrow(.))) %>%
   # change all times to be on the hour or half-hour
   # fix time-stamps that were altered during re-deployment
   mutate(date_time = round_date(date_time, "30 minutes"))


# Pond D
mini_d = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_D.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("D", nrow(.))) %>%
   # change all times to be on the hour or half-hour
   # fix time-stamps that were altered during re-deployment
   mutate(date_time = round_date(date_time, "30 minutes"))


# Pond E
mini_e = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_E.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("E", nrow(.))) %>%
   # change all times to be on the hour or half-hour
   # fix time-stamps that were altered during re-deployment
   mutate(date_time = round_date(date_time, "30 minutes"))


# Pond F
mini_f = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_F.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("F", nrow(.))) %>%
   # change all times to be on the hour or half-hour
   # fix time-stamps that were altered during re-deployment
   mutate(date_time = round_date(date_time, "30 minutes"))


# combine all
minidot = bind_rows(mini_a, mini_b, mini_c, mini_d, mini_e, mini_f) %>%
   mutate(doy = yday(date_time)) %>%
   relocate(pond_id) %>%
   relocate(doy, .after=date_time)

   
# view times of missing data points (when loggers were removed for >30 min. for data download)
missing = minidot %>%
   mutate(doy = yday(date_time)) %>%
   group_by(pond_id) %>%
   mutate(diff = difftime(date_time, lag(date_time), units="mins"),
          diff = str_remove(diff, pattern = " mins"),
          diff = as.numeric(diff)) %>%
   filter(diff > 30) %>%
   ungroup()


# add rows for missing measurements and interpolate data
minidot = minidot %>%
   full_join(missing %>% 
                select(pond_id, date_time, doy) %>%
                # subtract 30 minutes to get the missing time point
                mutate(date_time = date_time - minutes(30))) %>%
   group_by(pond_id) %>% 
   arrange(date_time, .by_group=TRUE) %>%
   mutate(across(c(temp, do, do_sat), ~zoo::na.approx(.))) %>%
   ungroup()


   ## remove temporary objects
   rm(mini_a, mini_b, mini_c, mini_d, mini_e, mini_f, missing)
   ##


# output processed miniDOT data, so raw files don't need to be read in and processed each time
write.csv(minidot, file = "Data/miniDOT_total.csv", row.names=FALSE)
}


#- Create the 'minidot' data set from the "data_import_EDI" script


#---
# Alkalinity
#---

# Data
alk_data = read_csv("Data/R-Data/2020_alkalinity-data.csv") %>%
   rename(sample_id = SampleID,
          doy = DOY,
          pond_id = Pond,
          alkalinity = Alkalinity,
          ph = pH)


#---
# Field Samples
#---

# Field data
limno_field_data = read_csv("Data/R-Data/2020_limno-field-data.csv") %>%
   rename(pond_id = pond,
          chla_rfu = chl,
          phyco_rfu = phyco,
          chla_filter = chlfil,
          phyco_filter = phycofil)


#---
#### Weather Data ####
#---

# Data
weather_data_raw = read.csv("Data/R-Data/2020_weather-data.csv", skip=1)


# Clean up variable names
weather_data = weather_data_raw %>%
   clean_names() %>%
   # select(-number) %>%
   select(-x) %>%
   rename(date_time = starts_with("date_time"),
          wind_speed = starts_with("wind_speed"),  # units are m/s
          gust_speed = starts_with("gust_speed"),   # units are m/s
          par = starts_with("par_"))    # units are umol/m2/s; new clean_names() makes this "mmol" for some reason


# Additional variables
weather_data = weather_data %>%
   # date and DOY
   mutate(date_time = mdy_hm(date_time),
          doy = yday(date_time)) %>%
   relocate(doy, .after = date_time) %>%
   # anemometer height (4 m) (units = m)
   mutate(wind_z = rep_len(4, n()))


#- Create the 'weather_data' data set from the "data_import_EDI" script


#---
#### Lake Concentration GHG Samples ####
#---

# GHG data
ghg_lake_raw = read_csv("Data/R-Data/2020_ghg-data_lake-conc.csv")

# Sample meta data
lake_sample_data = read_csv("Data/R-Data/2020_sample-data_lake-conc.csv") %>%
   # convert volumes from ml to L
   mutate(across(starts_with("vol"), ~(./1000))) %>%
   # date and DOY
   mutate(date = mdy(date),
          doy = yday(date)) %>%
   relocate(doy, .after = date)


##__Find and flag GHG outliers
{
# Determine the % difference between the two equilibrated syringes
#  for each of the three gases for each sampling day.
#
# Flag all instances when syringes differ by more than 50% for any gas.
#
# Manually view these flagged data in the data sheet to determine 
#  which syringe is the outlier.
#
# For most of these it is quite obvious which vial is the outlier, and most
#  appear to be the result of one vial being contaminated with atmosphere. 
#
# After outliers are identified and flagged in the data sheet, this part 
#  between the {} is no longer needed when normally running the script. 


# combine raw ghg values and some meta data for processing
ghg_view = lake_sample_data %>%
   select(sample_id, pond_id, doy) %>%
   left_join(ghg_lake_raw)

# CH4
m.diff = ghg_view %>%
   filter(!(pond_id=="Y")) %>%
   select(pond_id, doy, ch4_ppm) %>%
   group_by(pond_id, doy) %>%
   arrange(ch4_ppm, .by_group=T) %>%
   summarize(ch4_diff = (1 - (first(ch4_ppm) / last(ch4_ppm))) * 100) %>%
   ungroup() %>%
   # flag all days differing by more than 50%
   mutate(ch4_flag = if_else(ch4_diff > 50, 1, 0))


# CO2
c.diff = ghg_view %>%
   filter(!(pond_id=="Y")) %>%
   select(pond_id, doy, co2_ppm) %>%
   group_by(pond_id, doy) %>%
   arrange(co2_ppm, .by_group=T) %>%
   summarize(co2_diff = (1 - (first(co2_ppm) / last(co2_ppm))) * 100) %>%
   ungroup() %>%
   # flag all days differing by more than 50%
   mutate(co2_flag = if_else(co2_diff > 50, 1, 0))


# N2O
n.diff = ghg_view %>%
   filter(!(pond_id=="Y")) %>%
   select(pond_id, doy, n2o_ppm) %>%
   group_by(pond_id, doy) %>%
   arrange(n2o_ppm, .by_group=T) %>%
   summarize(n2o_diff = (1 - (first(n2o_ppm) / last(n2o_ppm))) * 100) %>%
   ungroup() %>%
   # flag all days differing by more than 50%
   mutate(n2o_flag = if_else(n2o_diff > 50, 1, 0))


# View flagged pond-days and manually inspect the raw data
m.diff %>% arrange(doy) %>% filter(ch4_flag==1) %>% View
c.diff %>% arrange(doy) %>% filter(co2_flag==1) %>% View
n.diff %>% arrange(doy) %>% filter(n2o_flag==1) %>% View


   #- remove temporary cleaning datasets
   rm(ghg_view, m.diff, c.diff, n.diff)
   #-

}


# Remove flagged outlier GHG vials
ghg_clean = ghg_lake_raw %>%
   # mutate(data_flag = if_else(is.na(data_flag), 0, data_flag)) %>%
   filter(is.na(data_flag))


# Mean GHG concentration from syringes for lake samples (2 syringes per pond, 3 syringes for atmosphere)
lake_ghg = ghg_clean %>%
   select(sample_id, ends_with("ppm")) %>%
   # mean value from syringe replicates
   group_by(sample_id) %>%
   summarize(across(ends_with("ppm"), ~mean(., na.rm=T))) %>%
   ungroup()


# Add sample data to GHG values
lake_samples = left_join(lake_sample_data, lake_ghg) %>%
   # add surface water temperature
   left_join(sonde_surface %>% select(pond_id, doy, surface_temp=temp))


#---
#### Methanogenesis GHG Samples ####
#---

# GHG data
ghg_methano_raw = read_csv("Data/R-Data/2020_ghg-data_methanogenesis.csv")

# Sample data
methano_sample_data = read_csv("Data/R-Data/2020_sample-data_methanogenesis.csv") %>%
   # add a variable for bottle headspace (units = ml) 
   mutate(vol_head = vol_bottle - (vol_sediment + vol_water)) %>%
   # convert volumes from ml to L
   mutate(across(starts_with("vol"), ~(./1000)))


# Add GHG concentration data to sample meta data
methano_samples = methano_sample_data %>%
   left_join(ghg_methano_raw) %>%
   # add date and time
   mutate(datetime_start = as.POSIXct(mdy(date_start) + hms(time_start)),
          datetime_end = as.POSIXct(mdy(date_end) + hms(time_end)),
          doy = yday(datetime_start)) %>%
   # add length of incubation period (units = minutes)
   mutate(incubation_length = difftime(datetime_end, datetime_start, units="mins"),
          incubation_length = as.numeric(incubation_length))


# Remove flagged outlier GHG vials
methano_samples = methano_samples %>%
   # mutate(data_flag = if_else(is.na(data_flag), 0, data_flag)) %>%
   filter(is.na(data_flag))


#---
#### Denitrification Enzyme Activity GHG Samples ####
#---

# GHG data
ghg_dea_raw = read_csv("Data/R-Data/2020_ghg-data_DEA.csv")

# Sample data
dea_sample_data = read_csv("Data/R-Data/2020_sample-data_DEA.csv") %>%
   # add a variable for bottle headspace (units = ml) 
   mutate(vol_head = vol_bottle - (vol_sediment + vol_water + vol_media)) %>%
   # convert volumes from ml to L
   mutate(across(starts_with("vol"), ~(./1000)))


# Add GHG concentration data to sample meta data
dea_samples = dea_sample_data %>%
   left_join(ghg_dea_raw) %>%
   # date and time
   filter(!(is.na(time_start))) %>%
   mutate(date_collect = mdy(date_collect),
          doy = yday(date_collect)) %>%
   # add length of incubation period (units = minutes)
   mutate(incubation_length = difftime(time_end, time_start, units="mins"),
          incubation_length = as.numeric(incubation_length))


# Remove flagged outlier GHG vials
dea_samples = dea_samples %>%
   # mutate(data_flag = if_else(is.na(data_flag), 0, data_flag)) %>%
   filter(is.na(data_flag))


#---
#### Ebullition Chamber GHG Samples ####
#---

# GHG data
ghg_ebu_raw = read_csv("Data/R-Data/2020_ghg-data_ebullition.csv")

# Sample meta data
ebu_sample_data = read_csv("Data/R-Data/2020_sample-data_ebullition.csv") %>%
   # add a column for sample replicate
   mutate(replicate = str_sub(sample_id, -2, -1)) %>%
   relocate(replicate, .after = pond_id) %>%
   # calculate chamber area
   mutate(area_chamber = pi * ((diam_chamber / 100) / 2)^2)


# Add GHG concentration data to sample meta data
ebu_samples = ebu_sample_data %>%
   left_join(ghg_ebu_raw) %>%
   # add DOY
   mutate(date_time = as.POSIXct(mdy(date) + hms(time)),
          doy = yday(date_time)) %>%
   select(-date, -time) %>%
   relocate(date_time, doy, .after = week)


# Calculate deployment length (units = days)
ebu_samples = ebu_samples %>%
   group_by(week, pond_id, replicate) %>%
   arrange(doy, .by_group=T) %>%
   mutate(deployment_length = difftime(last(date_time), first(date_time), units="days"),
          deployment_length = as.numeric(deployment_length)) %>%
   ungroup()


#---
#### Sediment Data ####
#---

# Data
bulk_density_raw = read_csv("Data/R-Data/2020_sediment_bulk-density.csv")

organic_matter_raw = read_csv("Data/R-Data/2020_sediment_om.csv")


# Clean up bulk density file and calculate Dry Bulk Density (DBD) for each sample
bulk_density = bulk_density_raw %>%
   mutate(date = mdy(date),
          doy = yday(date)) %>%
   filter(!(sample_type=="nutrients")) %>%
   # sample weights (units = g)
   mutate(mass_wet = mass_wet_tin.sample - mass_tin,
          mass_dry = mass_dry_tin.sample - mass_tin) %>%
   # dry bulk density and porosity (units = g / cm3)
   mutate(DBD = mass_dry / vol_sample,
          porosity = (mass_wet - mass_dry) / vol_sample)

# calculate mean sediment bulk density and porosity for each pond (summer mean value)
bulk_density = bulk_density %>%
   group_by(pond_id) %>%
   # units = g/cm3
   summarize(DBD = mean(DBD, na.rm=T),
             porosity = mean(porosity, na.rm=T)) %>%
   ungroup()


# Clean up OM file and calculate organic matter content
om_data = organic_matter_raw %>%
   mutate(date = mdy(date),
          doy = yday(date)) %>%
   # sample weights (units = g)
   mutate(mass_dry = mass_dry_tin.sample - mass_tin,
          mass_ash = mass_ash_tin.sample - mass_tin) %>%
   # OM content (%)
   mutate(perc_om = (1 - (mass_ash / mass_dry)) * 100)

# calculate mean OM content for each pond (summer mean value)
om_data = om_data %>%
   group_by(pond_id) %>%
   summarize(perc_om = mean(perc_om, na.rm=T)) %>%
   ungroup()



