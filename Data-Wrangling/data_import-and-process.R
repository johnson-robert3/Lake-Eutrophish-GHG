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

# Function to wrangle each sonde profile data sheet as it is read in
#  select only the desired columns and rename to match across files
read_profile = function(.dat, .skip) {
   read_csv(.dat, skip = .skip) %>%
      clean_names() %>%
      select(date, time, site, temp_f, odo_percent_sat, odo_mg_l, p_h, contains("chlorophyll"), contains("cond"), pc_rfu, pc_ug_l,
             sal_psu, pressure_psi_a, depth_m, vertical_position_m, -contains("n_lf_cond")) %>%
      rename(pond_id = site,
             temp = temp_f,
             do_sat = odo_percent_sat,
             do = odo_mg_l,
             ph = p_h,
             chla = chlorophyll_g_l,         # units are actually ug/L; new clean_names() drops this for some reason
             chla_rfu = chlorophyll_rfu,
             phyco = pc_ug_l,
             phyco_rfu = pc_rfu,
             cond = cond_s_cm,               # units are actually uS/cm; new clean_names() drops this for some reason
             sp_cond = sp_cond_u_fffd_s_cm,
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


# Clean up profile dataset
sonde_profiles = sonde_profiles %>%
   # remove the times (n=3) when a duplicate profile appears in the data set, but with no data
   # DOY 162 Pond C; DOY 185 Pond F; DOY 210 Pond D
   filter(!(is.na(temp) & is.na(do) & is.na(chla) & is.na(vert_m))) %>%
   mutate(pond_id = str_remove(pond_id, "Pond "),
          date_time = as.POSIXct(mdy(date) + hms(time)),
          doy = yday(date_time)) %>%
   arrange(doy, pond_id, date_time) %>%
   select(-date, -time) %>%
   relocate(date_time, doy) %>%
   # change temp data to Celcius
   mutate(temp = (temp - 32) / 1.8) %>%
   # correct vertical position (depth sensor and probes not at same height)
   # add 5 cm to all
   # mutate(vert_m = vert_m + 0.05,
   #        vert_m = if_else(vert_m<0, 0, vert_m)) %>%
   # remove errant measurements recorded as 1970-01-01
   filter(!(doy==1))
   

   ## remove temporary objects
   rm(sonde_nohead, sonde_head)
   ##


##__Surface water means 
#  Depth: 10-30 cm

sonde_surface = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   filter(vert_m >= 0.1 & vert_m <= 0.30) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup()


# Assign depth intervals to data for plotting 
sonde_int = sonde_profiles %>%
   # 10cm depth intervals
   mutate(depth_int = case_when(.$vert_m >= 0.0 & vert_m <= 0.1 ~ 0.1,
                                .$vert_m > 0.1 & vert_m <= 0.2 ~ 0.2,
                                .$vert_m > 0.2 & vert_m <= 0.3 ~ 0.3,
                                .$vert_m > 0.3 & vert_m <= 0.4 ~ 0.4,
                                .$vert_m > 0.4 & vert_m <= 0.5 ~ 0.5,
                                .$vert_m > 0.5 & vert_m <= 0.6 ~ 0.6,
                                .$vert_m > 0.6 & vert_m <= 0.7 ~ 0.7,
                                .$vert_m > 0.7 & vert_m <= 0.8 ~ 0.8,
                                .$vert_m > 0.8 & vert_m <= 0.9 ~ 0.9,
                                .$vert_m > 0.9 & vert_m <= 1.0 ~ 1.0,
                                .$vert_m > 1.0 & vert_m <= 1.1 ~ 1.1,
                                .$vert_m > 1.1 & vert_m <= 1.2 ~ 1.2,
                                .$vert_m > 1.2 & vert_m <= 1.3 ~ 1.3,
                                .$vert_m > 1.3 & vert_m <= 1.4 ~ 1.4,
                                .$vert_m > 1.4 & vert_m <= 1.5 ~ 1.5,
                                .$vert_m > 1.5 & vert_m <= 1.6 ~ 1.6,
                                .$vert_m > 1.6 & vert_m <= 1.7 ~ 1.7,
                                .$vert_m > 1.7 & vert_m <= 1.8 ~ 1.8,
                                .$vert_m > 1.8 & vert_m <= 1.9 ~ 1.9,
                                .$vert_m > 1.9 & vert_m <= 2.0 ~ 2.0,
                                .$vert_m > 2 ~ 2.1)) %>%
   # mean values within each depth interval
   group_by(pond_id, doy, depth_int) %>%
   summarize(across(temp:salinity, mean, na.rm=T)) %>%
   ungroup() 


##__Bottom water means
#  Depth: bottom 30 cm

sonde_bottom = sonde_int %>%
   group_by(pond_id, doy) %>%
   arrange(depth_int, .by_group=T) %>%
   slice_tail(n=3) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup()


#---
# HOBO T-chains
#---

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
   mutate(date_time = mdy_hms(date_time))


#---
# MiniDOT Surface Temp DO Time-Series Data
#---

# Function to pre-process each file as it is read in
read_minidot = function(.dat, .skip=9) {
   read_csv(.dat, skip = .skip,
            col_names = c("unix", "UTC", "date_time", "battery", "temp", "do", "do_sat", "q")) %>%
      remove_empty(which = c("rows", "cols")) %>%
      select(-unix, -UTC, -battery, -q)
}

# Function to correct time-stamps from when loggers were downloaded and re-deployed
fix_times = function(.dat) {
   .dat %>%
      mutate(date = date(date_time),
             hour = hour(date_time),
             minute = minute(date_time)) %>%
      select(-date_time) %>%
      mutate(new_min = case_when(.$minute %in% c(1:29) ~ 15,
                                 .$minute %in% c(31:59) ~ 45),
             new_dt = as.POSIXct(ymd(date) + hm(paste(hour, new_min, sep=" ")))) %>%
      select(-date, -hour, -minute, -new_min) %>%
      rename(date_time = new_dt) %>%
      mutate(doy = yday(date_time)) %>%
      relocate(pond_id, date_time, doy)
}
   
# Pond A
mini_a = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_A.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("A", nrow(.))) %>%
   fix_times()

# Pond B
mini_b = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_B.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("B", nrow(.))) %>%
   fix_times()

# Pond C
mini_c = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_C.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("C", nrow(.))) %>%
   fix_times()

# Pond D
mini_d = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_D.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("D", nrow(.))) %>%
   fix_times()

# Pond E
mini_e = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_E.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("E", nrow(.))) %>%
   fix_times()

# Pond F
mini_f = list.files(path = "./Data/R-Data/2020_MiniDOTs",
                    pattern = "*_F.csv",
                    full.names = T) %>%
   map_dfr(~read_minidot(.)) %>%
   mutate(pond_id = rep("F", nrow(.))) %>%
   fix_times()


# combine all
minidot = bind_rows(mini_a, mini_b, mini_c, mini_d, mini_e, mini_f)


   ## remove temporary objects
   rm(mini_a, mini_b, mini_c, mini_d, mini_e, mini_f)
   ##

   
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
#### Lake Concentration GHG Samples ####
#---

# GHG data
ghg_lake_raw = read_csv("Data/R-Data/2020_ghgs_lake-conc.csv")

# Sample meta data
lake_sample_data = read_csv("Data/R-Data/2020_sample-metadata_lake-conc.csv") %>%
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
   mutate(data_flag = if_else(is.na(data_flag), 0, data_flag)) %>%
   filter(!(data_flag==1))


# Mean GHG concentration from syringes for lake samples (2 syringes per pond, 3 syringes for atmosphere)
lake_ghg = ghg_clean %>%
   select(sample_id, ends_with("ppm")) %>%
   # mean value from syringe replicates
   group_by(sample_id) %>%
   summarize(across(ends_with("ppm"), ~mean(., na.rm=T))) %>%
   ungroup()


# Add meta data to GHG values
lake_samples = left_join(lake_sample_data, lake_ghg) %>%
   # add surface water temperature
   left_join(sonde_surface %>% select(pond_id, doy, surface_temp=temp)) %>%
   # fill in a surface temperature for Pond C, DOY 231 (profile not recorded)
   # linearly interpolate between prior and following measurements
   group_by(pond_id) %>%
   arrange(doy, .by_group=T) %>%
   mutate(surface_temp = if_else(is.na(surface_temp), (lead(surface_temp) + lag(surface_temp)) / 2, surface_temp)) %>%
   ungroup()


#---
#### Methanogenesis GHG Samples ####
#---

# GHG data
ghg_methano_raw = read_csv("Data/R-Data/2020_ghgs_methano-assay.csv")

# Sample meta data
methano_sample_data = read_csv("Data/R-Data/2020_sample-metadata_methano-assay.csv") %>%
   # add a bottle headspace variable
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
   # add length of incubation period
   mutate(incubation_length = difftime(datetime_end, datetime_start, units="mins"),
          incubation_length = as.numeric(incubation_length))


# Remove flagged outlier GHG vials
methano_samples = methano_samples %>%
   mutate(data_flag = if_else(is.na(data_flag), 0, data_flag)) %>%
   filter(!(data_flag==1))


#---
#### Denitrification Enzyme Activity GHG Samples ####
#---

# GHG data
ghg_dea_raw = read_csv("Data/R-Data/2020_ghgs_dea-assay.csv")

# Sample meta data
dea_sample_data = read_csv("Data/R-Data/2020_sample-metadata_dea-assay.csv") %>%
   # add bottle headspace variable
   mutate(vol_head = vol_bottle - (vol_sediment + vol_water + vol_media)) %>%
   # convert volumns from ml to L
   mutate(across(starts_with("vol"), ~(./1000)))


# Add GHG concentration data to sample meta data
dea_samples = dea_sample_data %>%
   left_join(ghg_dea_raw) %>%
   # date and time
   filter(!(is.na(time_start))) %>%
   mutate(date_collect = mdy(date_collect),
          doy = yday(date_collect)) %>%
   # add length of incubation period
   mutate(incubation_length = difftime(time_end, time_start, units="mins"),
          incubation_length = as.numeric(incubation_length))


# Remove flagged outlier GHG vials
dea_samples = dea_samples %>%
   mutate(data_flag = if_else(is.na(data_flag), 0, data_flag)) %>%
   filter(!(data_flag==1))


#---
#### Ebullition Chamber GHG Samples ####
#---

# GHG data
ghg_ebu_raw = read_csv("Data/R-Data/2020_ghgs_ebullition.csv")

# Sample meta data
ebu_sample_data = read_csv("Data/R-Data/2020_sample-metadata_ebullition.csv") %>%
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


# Calculate deployment length
ebu_samples = ebu_samples %>%
   group_by(week, pond_id, replicate) %>%
   arrange(doy, .by_group=T) %>%
   mutate(deployment_length = difftime(last(date_time), first(date_time), units="days"),
          deployment_length = as.numeric(deployment_length)) %>%
   ungroup()


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
          par = par_mol_m_u_fffd_s_lgr_s_n_20849581_sen_s_n_20856725)


# Additional variables
weather_data = weather_data %>%
   # date and DOY
   mutate(date_time = mdy_hm(date_time),
          doy = yday(date_time)) %>%
   relocate(doy, .after = date_time) %>%
   # anemometer height (4 m)
   mutate(wind_z = rep_len(4, n()))


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
   # sample weights
   mutate(mass_wet = mass_wet_tin.sample - mass_tin,
          mass_dry = mass_dry_tin.sample - mass_tin) %>%
   # dry bulk density and porosity (units = g / cm3)
   mutate(DBD = mass_dry / vol_sample,
          porosity = (mass_wet - mass_dry) / vol_sample)

# calculate mean sediment bulk density and porosity for each pond (summer mean value)
bulk_density = bulk_density %>%
   group_by(pond_id) %>%
   summarize(DBD = mean(DBD, na.rm=T),
             porosity = mean(porosity, na.rm=T)) %>%
   ungroup()


# Clean up OM file and calculate organic matter content
om_data = organic_matter_raw %>%
   mutate(date = mdy(date),
          doy = yday(date)) %>%
   # sample weights
   mutate(mass_dry = mass_dry_tin.sample - mass_tin,
          mass_ash = mass_ash_tin.sample - mass_tin) %>%
   # OM content (%)
   mutate(perc_om = (1 - (mass_ash / mass_dry)) * 100)

# calculate mean OM content for each pond (summer mean value)
om_data = om_data %>%
   group_by(pond_id) %>%
   summarize(perc_om = mean(perc_om, na.rm=T)) %>%
   ungroup()



