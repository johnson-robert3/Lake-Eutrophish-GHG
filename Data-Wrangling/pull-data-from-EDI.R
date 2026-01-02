#~~~
# Script to pull all datasets directly from the EDI repository.
#
# This code is pulled directly from the EDI data package website. Only code necessary to pull in the necessary datasets for the current
# greenhouse gas project is retained (other code is commented out). 
#~~~


# Package ID: edi.2238.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Summer water chemistry, high frequency sensors, zooplankton and benthic macroinvertebrate community composition, periphyton, fish, and macrophyte biomass, along with lake metabolism and greenhouse gas dynamics in six experimental ponds in central Iowa, USA (2020).
# Data set creator:  Tyler Butts - University of Wisconsin-Madison 
# Data set creator:  Ellen Albright - University of Wisconsin-Madison 
# Data set creator:  Quin Shingai - Dartmouth College 
# Data set creator:  Robert Johnson - University of Wisconsin-Madison 
# Data set creator:  Grace Wilkinson - University of Wisconsin-Madison 
# Metadata Provider:  Tyler Butts - University of Minnesota-Twin Cities 
# Contact:  Grace Wilkinson - Associate Professor University of Wisconsin-Madison  - gwilkinson@wisc.edu
# Contact:  Tyler Butts - Postdoctoral Associate University of Wisconsin-Madison  - tyler.james.butts@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")
	      

# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/29257dc86ba5fa9ea2d40362bb02a74c" 
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# 
#                    
#  dt1 <-read.csv(infile1,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "doy",     
#                     "pond",     
#                     "site_id",     
#                     "vertical_m",     
#                     "odo_mgL",     
#                     "odo_sat",     
#                     "temp_c",     
#                     "barometer",     
#                     "sp_cond",     
#                     "tds",     
#                     "chla_RFU",     
#                     "chla_ugL",     
#                     "pc_RFU",     
#                     "pc_ugL"    ), check.names=TRUE)
#                
# unlink(infile1)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt1$doy)=="factor") dt1$doy <-as.numeric(levels(dt1$doy))[as.integer(dt1$doy) ]               
# if (class(dt1$doy)=="character") dt1$doy <-as.numeric(dt1$doy)
# if (class(dt1$pond)!="factor") dt1$pond<- as.factor(dt1$pond)
# if (class(dt1$site_id)!="factor") dt1$site_id<- as.factor(dt1$site_id)
# if (class(dt1$vertical_m)=="factor") dt1$vertical_m <-as.numeric(levels(dt1$vertical_m))[as.integer(dt1$vertical_m) ]               
# if (class(dt1$vertical_m)=="character") dt1$vertical_m <-as.numeric(dt1$vertical_m)
# if (class(dt1$odo_mgL)=="factor") dt1$odo_mgL <-as.numeric(levels(dt1$odo_mgL))[as.integer(dt1$odo_mgL) ]               
# if (class(dt1$odo_mgL)=="character") dt1$odo_mgL <-as.numeric(dt1$odo_mgL)
# if (class(dt1$odo_sat)=="factor") dt1$odo_sat <-as.numeric(levels(dt1$odo_sat))[as.integer(dt1$odo_sat) ]               
# if (class(dt1$odo_sat)=="character") dt1$odo_sat <-as.numeric(dt1$odo_sat)
# if (class(dt1$temp_c)=="factor") dt1$temp_c <-as.numeric(levels(dt1$temp_c))[as.integer(dt1$temp_c) ]               
# if (class(dt1$temp_c)=="character") dt1$temp_c <-as.numeric(dt1$temp_c)
# if (class(dt1$barometer)=="factor") dt1$barometer <-as.numeric(levels(dt1$barometer))[as.integer(dt1$barometer) ]               
# if (class(dt1$barometer)=="character") dt1$barometer <-as.numeric(dt1$barometer)
# if (class(dt1$sp_cond)=="factor") dt1$sp_cond <-as.numeric(levels(dt1$sp_cond))[as.integer(dt1$sp_cond) ]               
# if (class(dt1$sp_cond)=="character") dt1$sp_cond <-as.numeric(dt1$sp_cond)
# if (class(dt1$tds)=="factor") dt1$tds <-as.numeric(levels(dt1$tds))[as.integer(dt1$tds) ]               
# if (class(dt1$tds)=="character") dt1$tds <-as.numeric(dt1$tds)
# if (class(dt1$chla_RFU)=="factor") dt1$chla_RFU <-as.numeric(levels(dt1$chla_RFU))[as.integer(dt1$chla_RFU) ]               
# if (class(dt1$chla_RFU)=="character") dt1$chla_RFU <-as.numeric(dt1$chla_RFU)
# if (class(dt1$chla_ugL)=="factor") dt1$chla_ugL <-as.numeric(levels(dt1$chla_ugL))[as.integer(dt1$chla_ugL) ]               
# if (class(dt1$chla_ugL)=="character") dt1$chla_ugL <-as.numeric(dt1$chla_ugL)
# if (class(dt1$pc_RFU)=="factor") dt1$pc_RFU <-as.numeric(levels(dt1$pc_RFU))[as.integer(dt1$pc_RFU) ]               
# if (class(dt1$pc_RFU)=="character") dt1$pc_RFU <-as.numeric(dt1$pc_RFU)
# if (class(dt1$pc_ugL)=="factor") dt1$pc_ugL <-as.numeric(levels(dt1$pc_ugL))[as.integer(dt1$pc_ugL) ]               
# if (class(dt1$pc_ugL)=="character") dt1$pc_ugL <-as.numeric(dt1$pc_ugL)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# 
# 
# # Here is the structure of the input data frame:
# str(dt1)                            
# attach(dt1)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(doy)
# summary(pond)
# summary(site_id)
# summary(vertical_m)
# summary(odo_mgL)
# summary(odo_sat)
# summary(temp_c)
# summary(barometer)
# summary(sp_cond)
# summary(tds)
# summary(chla_RFU)
# summary(chla_ugL)
# summary(pc_RFU)
# summary(pc_ugL) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt1$pond)) 
# summary(as.factor(dt1$site_id))
# detach(dt1)               
        
	      

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/4173c3e85dbc4decd294567f69a4e3f8" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "date_time",     
                    "doy",     
                    "pond_id",     
                    "temp",     
                    "do_sat",     
                    "do",     
                    "ph",     
                    "chla_rfu",     
                    "chla",     
                    "cond",     
                    "sp_cond",     
                    "phyco_rfu",     
                    "phyco",     
                    "salinity",     
                    "pressure_psi_a",     
                    "depth_m",     
                    "vert_m"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt2$date_time dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp2date_time<-as.POSIXct(dt2$date_time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$date_time != "",]) == length(tmp2date_time[!is.na(tmp2date_time)])){dt2$date_time <- tmp2date_time } else {print("Date conversion failed for dt2$date_time. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt2$doy)=="factor") dt2$doy <-as.numeric(levels(dt2$doy))[as.integer(dt2$doy) ]               
if (class(dt2$doy)=="character") dt2$doy <-as.numeric(dt2$doy)
if (class(dt2$pond_id)!="factor") dt2$pond_id<- as.factor(dt2$pond_id)
if (class(dt2$temp)=="factor") dt2$temp <-as.numeric(levels(dt2$temp))[as.integer(dt2$temp) ]               
if (class(dt2$temp)=="character") dt2$temp <-as.numeric(dt2$temp)
if (class(dt2$do_sat)=="factor") dt2$do_sat <-as.numeric(levels(dt2$do_sat))[as.integer(dt2$do_sat) ]               
if (class(dt2$do_sat)=="character") dt2$do_sat <-as.numeric(dt2$do_sat)
if (class(dt2$do)=="factor") dt2$do <-as.numeric(levels(dt2$do))[as.integer(dt2$do) ]               
if (class(dt2$do)=="character") dt2$do <-as.numeric(dt2$do)
if (class(dt2$ph)=="factor") dt2$ph <-as.numeric(levels(dt2$ph))[as.integer(dt2$ph) ]               
if (class(dt2$ph)=="character") dt2$ph <-as.numeric(dt2$ph)
if (class(dt2$chla_rfu)=="factor") dt2$chla_rfu <-as.numeric(levels(dt2$chla_rfu))[as.integer(dt2$chla_rfu) ]               
if (class(dt2$chla_rfu)=="character") dt2$chla_rfu <-as.numeric(dt2$chla_rfu)
if (class(dt2$chla)=="factor") dt2$chla <-as.numeric(levels(dt2$chla))[as.integer(dt2$chla) ]               
if (class(dt2$chla)=="character") dt2$chla <-as.numeric(dt2$chla)
if (class(dt2$cond)=="factor") dt2$cond <-as.numeric(levels(dt2$cond))[as.integer(dt2$cond) ]               
if (class(dt2$cond)=="character") dt2$cond <-as.numeric(dt2$cond)
if (class(dt2$sp_cond)=="factor") dt2$sp_cond <-as.numeric(levels(dt2$sp_cond))[as.integer(dt2$sp_cond) ]               
if (class(dt2$sp_cond)=="character") dt2$sp_cond <-as.numeric(dt2$sp_cond)
if (class(dt2$phyco_rfu)=="factor") dt2$phyco_rfu <-as.numeric(levels(dt2$phyco_rfu))[as.integer(dt2$phyco_rfu) ]               
if (class(dt2$phyco_rfu)=="character") dt2$phyco_rfu <-as.numeric(dt2$phyco_rfu)
if (class(dt2$phyco)=="factor") dt2$phyco <-as.numeric(levels(dt2$phyco))[as.integer(dt2$phyco) ]               
if (class(dt2$phyco)=="character") dt2$phyco <-as.numeric(dt2$phyco)
if (class(dt2$salinity)=="factor") dt2$salinity <-as.numeric(levels(dt2$salinity))[as.integer(dt2$salinity) ]               
if (class(dt2$salinity)=="character") dt2$salinity <-as.numeric(dt2$salinity)
if (class(dt2$pressure_psi_a)=="factor") dt2$pressure_psi_a <-as.numeric(levels(dt2$pressure_psi_a))[as.integer(dt2$pressure_psi_a) ]               
if (class(dt2$pressure_psi_a)=="character") dt2$pressure_psi_a <-as.numeric(dt2$pressure_psi_a)
if (class(dt2$depth_m)=="factor") dt2$depth_m <-as.numeric(levels(dt2$depth_m))[as.integer(dt2$depth_m) ]               
if (class(dt2$depth_m)=="character") dt2$depth_m <-as.numeric(dt2$depth_m)
if (class(dt2$vert_m)=="factor") dt2$vert_m <-as.numeric(levels(dt2$vert_m))[as.integer(dt2$vert_m) ]               
if (class(dt2$vert_m)=="character") dt2$vert_m <-as.numeric(dt2$vert_m)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(date_time)
summary(doy)
summary(pond_id)
summary(temp)
summary(do_sat)
summary(do)
summary(ph)
summary(chla_rfu)
summary(chla)
summary(cond)
summary(sp_cond)
summary(phyco_rfu)
summary(phyco)
summary(salinity)
summary(pressure_psi_a)
summary(depth_m)
summary(vert_m) 
                # Get more details on character variables
                 
summary(as.factor(dt2$pond_id))
detach(dt2)               
        
	      

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/e9e50941023824c2c2e012bdf454c9e5" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "datetime",     
                    "doy",     
                    "doy_frac",     
                    "pond",     
                    "site_id",     
                    "temp_depth_m",     
                    "temp_c"    ), check.names=TRUE)
               
unlink(infile3)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt3$datetime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp3datetime<-as.POSIXct(dt3$datetime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt3[dt3$datetime != "",]) == length(tmp3datetime[!is.na(tmp3datetime)])){dt3$datetime <- tmp3datetime } else {print("Date conversion failed for dt3$datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt3$doy)=="factor") dt3$doy <-as.numeric(levels(dt3$doy))[as.integer(dt3$doy) ]               
if (class(dt3$doy)=="character") dt3$doy <-as.numeric(dt3$doy)
if (class(dt3$doy_frac)=="factor") dt3$doy_frac <-as.numeric(levels(dt3$doy_frac))[as.integer(dt3$doy_frac) ]               
if (class(dt3$doy_frac)=="character") dt3$doy_frac <-as.numeric(dt3$doy_frac)
if (class(dt3$pond)!="factor") dt3$pond<- as.factor(dt3$pond)
if (class(dt3$site_id)!="factor") dt3$site_id<- as.factor(dt3$site_id)
if (class(dt3$temp_depth_m)=="factor") dt3$temp_depth_m <-as.numeric(levels(dt3$temp_depth_m))[as.integer(dt3$temp_depth_m) ]               
if (class(dt3$temp_depth_m)=="character") dt3$temp_depth_m <-as.numeric(dt3$temp_depth_m)
if (class(dt3$temp_c)=="factor") dt3$temp_c <-as.numeric(levels(dt3$temp_c))[as.integer(dt3$temp_c) ]               
if (class(dt3$temp_c)=="character") dt3$temp_c <-as.numeric(dt3$temp_c)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(datetime)
summary(doy)
summary(doy_frac)
summary(pond)
summary(site_id)
summary(temp_depth_m)
summary(temp_c) 
                # Get more details on character variables
                 
summary(as.factor(dt3$pond)) 
summary(as.factor(dt3$site_id))
detach(dt3)               
        
	      

# inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/ae068d504167d700231a6f95c13f6ff6" 
# infile4 <- tempfile()
# try(download.file(inUrl4,infile4,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")
# 
#                    
#  dt4 <-read.csv(infile4,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "depths",     
#                     "areas"    ), check.names=TRUE)
#                
# unlink(infile4)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt4$depths)=="factor") dt4$depths <-as.numeric(levels(dt4$depths))[as.integer(dt4$depths) ]               
# if (class(dt4$depths)=="character") dt4$depths <-as.numeric(dt4$depths)
# if (class(dt4$areas)=="factor") dt4$areas <-as.numeric(levels(dt4$areas))[as.integer(dt4$areas) ]               
# if (class(dt4$areas)=="character") dt4$areas <-as.numeric(dt4$areas)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# 
# 
# # Here is the structure of the input data frame:
# str(dt4)                            
# attach(dt4)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(depths)
# summary(areas) 
#                 # Get more details on character variables
#                 
# detach(dt4)               
        
	      

# inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/55ad212a294113949b452c2ab145f3f1" 
# infile5 <- tempfile()
# try(download.file(inUrl5,infile5,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")
# 
#                    
#  dt5 <-read.csv(infile5,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "doy",     
#                     "pond",     
#                     "secchi_m",     
#                     "secchi_flag"    ), check.names=TRUE)
#                
# unlink(infile5)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt5$doy)=="factor") dt5$doy <-as.numeric(levels(dt5$doy))[as.integer(dt5$doy) ]               
# if (class(dt5$doy)=="character") dt5$doy <-as.numeric(dt5$doy)
# if (class(dt5$pond)!="factor") dt5$pond<- as.factor(dt5$pond)
# if (class(dt5$secchi_m)=="factor") dt5$secchi_m <-as.numeric(levels(dt5$secchi_m))[as.integer(dt5$secchi_m) ]               
# if (class(dt5$secchi_m)=="character") dt5$secchi_m <-as.numeric(dt5$secchi_m)
# if (class(dt5$secchi_flag)!="factor") dt5$secchi_flag<- as.factor(dt5$secchi_flag)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# dt5$secchi_m <- ifelse((trimws(as.character(dt5$secchi_m))==trimws("NA")),NA,dt5$secchi_m)               
# suppressWarnings(dt5$secchi_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$secchi_m))==as.character(as.numeric("NA"))),NA,dt5$secchi_m))
# 
# 
# # Here is the structure of the input data frame:
# str(dt5)                            
# attach(dt5)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(doy)
# summary(pond)
# summary(secchi_m)
# summary(secchi_flag) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt5$pond)) 
# summary(as.factor(dt5$secchi_flag))
# detach(dt5)               
        
	      

# inUrl6  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/b4da841fe9b5cc107f412a157c317c70" 
# infile6 <- tempfile()
# try(download.file(inUrl6,infile6,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")
# 
#                    
#  dt6 <-read.csv(infile6,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "datetime",     
#                     "doy_frac",     
#                     "doy",     
#                     "ShortWave_Wm2",     
#                     "AirTemp_C",     
#                     "RelHum_pct",     
#                     "WindSpeed_ms",     
#                     "Rain_m"    ), check.names=TRUE)
#                
# unlink(infile6)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                                                    
# # attempting to convert dt6$datetime dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
# tmp6datetime<-as.POSIXct(dt6$datetime,format=tmpDateFormat)
# # Keep the new dates only if they all converted correctly
# if(nrow(dt6[dt6$datetime != "",]) == length(tmp6datetime[!is.na(tmp6datetime)])){dt6$datetime <- tmp6datetime } else {print("Date conversion failed for dt6$datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
#                                 
# if (class(dt6$doy_frac)=="factor") dt6$doy_frac <-as.numeric(levels(dt6$doy_frac))[as.integer(dt6$doy_frac) ]               
# if (class(dt6$doy_frac)=="character") dt6$doy_frac <-as.numeric(dt6$doy_frac)
# if (class(dt6$doy)=="factor") dt6$doy <-as.numeric(levels(dt6$doy))[as.integer(dt6$doy) ]               
# if (class(dt6$doy)=="character") dt6$doy <-as.numeric(dt6$doy)
# if (class(dt6$ShortWave_Wm2)=="factor") dt6$ShortWave_Wm2 <-as.numeric(levels(dt6$ShortWave_Wm2))[as.integer(dt6$ShortWave_Wm2) ]               
# if (class(dt6$ShortWave_Wm2)=="character") dt6$ShortWave_Wm2 <-as.numeric(dt6$ShortWave_Wm2)
# if (class(dt6$AirTemp_C)=="factor") dt6$AirTemp_C <-as.numeric(levels(dt6$AirTemp_C))[as.integer(dt6$AirTemp_C) ]               
# if (class(dt6$AirTemp_C)=="character") dt6$AirTemp_C <-as.numeric(dt6$AirTemp_C)
# if (class(dt6$RelHum_pct)=="factor") dt6$RelHum_pct <-as.numeric(levels(dt6$RelHum_pct))[as.integer(dt6$RelHum_pct) ]               
# if (class(dt6$RelHum_pct)=="character") dt6$RelHum_pct <-as.numeric(dt6$RelHum_pct)
# if (class(dt6$WindSpeed_ms)=="factor") dt6$WindSpeed_ms <-as.numeric(levels(dt6$WindSpeed_ms))[as.integer(dt6$WindSpeed_ms) ]               
# if (class(dt6$WindSpeed_ms)=="character") dt6$WindSpeed_ms <-as.numeric(dt6$WindSpeed_ms)
# if (class(dt6$Rain_m)=="factor") dt6$Rain_m <-as.numeric(levels(dt6$Rain_m))[as.integer(dt6$Rain_m) ]               
# if (class(dt6$Rain_m)=="character") dt6$Rain_m <-as.numeric(dt6$Rain_m)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# 
# 
# # Here is the structure of the input data frame:
# str(dt6)                            
# attach(dt6)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(datetime)
# summary(doy_frac)
# summary(doy)
# summary(ShortWave_Wm2)
# summary(AirTemp_C)
# summary(RelHum_pct)
# summary(WindSpeed_ms)
# summary(Rain_m) 
#                 # Get more details on character variables
#                 
# detach(dt6)               
        
	      

inUrl7  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/611e5b6668a91292809fe54f29688be6" 
infile7 <- tempfile()
try(download.file(inUrl7,infile7,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile7))) download.file(inUrl7,infile7,method="auto")

                   
 dt7 <-read.csv(infile7,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "date_time",     
                    "doy",     
                    "wind_speed",     
                    "gust_speed",     
                    "par",     
                    "wind_z"    ), check.names=TRUE)
               
unlink(infile7)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                                                   
# attempting to convert dt7$date_time dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp7date_time<-as.POSIXct(dt7$date_time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt7[dt7$date_time != "",]) == length(tmp7date_time[!is.na(tmp7date_time)])){dt7$date_time <- tmp7date_time } else {print("Date conversion failed for dt7$date_time. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt7$doy)=="factor") dt7$doy <-as.numeric(levels(dt7$doy))[as.integer(dt7$doy) ]               
if (class(dt7$doy)=="character") dt7$doy <-as.numeric(dt7$doy)
if (class(dt7$wind_speed)=="factor") dt7$wind_speed <-as.numeric(levels(dt7$wind_speed))[as.integer(dt7$wind_speed) ]               
if (class(dt7$wind_speed)=="character") dt7$wind_speed <-as.numeric(dt7$wind_speed)
if (class(dt7$gust_speed)=="factor") dt7$gust_speed <-as.numeric(levels(dt7$gust_speed))[as.integer(dt7$gust_speed) ]               
if (class(dt7$gust_speed)=="character") dt7$gust_speed <-as.numeric(dt7$gust_speed)
if (class(dt7$par)=="factor") dt7$par <-as.numeric(levels(dt7$par))[as.integer(dt7$par) ]               
if (class(dt7$par)=="character") dt7$par <-as.numeric(dt7$par)
if (class(dt7$wind_z)=="factor") dt7$wind_z <-as.numeric(levels(dt7$wind_z))[as.integer(dt7$wind_z) ]               
if (class(dt7$wind_z)=="character") dt7$wind_z <-as.numeric(dt7$wind_z)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt7)                            
attach(dt7)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(date_time)
summary(doy)
summary(wind_speed)
summary(gust_speed)
summary(par)
summary(wind_z) 
                # Get more details on character variables
                
detach(dt7)               
        
	      

inUrl8  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/49c14e2ee72cef5114281e1b3614d84d" 
infile8 <- tempfile()
try(download.file(inUrl8,infile8,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile8))) download.file(inUrl8,infile8,method="auto")

                   
 dt8 <-read.csv(infile8,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "pond_id",     
                    "doy",     
                    "GPP",     
                    "R",     
                    "NEP",     
                    "flag"    ), check.names=TRUE)
               
unlink(infile8)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt8$pond_id)!="factor") dt8$pond_id<- as.factor(dt8$pond_id)
if (class(dt8$doy)=="factor") dt8$doy <-as.numeric(levels(dt8$doy))[as.integer(dt8$doy) ]               
if (class(dt8$doy)=="character") dt8$doy <-as.numeric(dt8$doy)
if (class(dt8$GPP)=="factor") dt8$GPP <-as.numeric(levels(dt8$GPP))[as.integer(dt8$GPP) ]               
if (class(dt8$GPP)=="character") dt8$GPP <-as.numeric(dt8$GPP)
if (class(dt8$R)=="factor") dt8$R <-as.numeric(levels(dt8$R))[as.integer(dt8$R) ]               
if (class(dt8$R)=="character") dt8$R <-as.numeric(dt8$R)
if (class(dt8$NEP)=="factor") dt8$NEP <-as.numeric(levels(dt8$NEP))[as.integer(dt8$NEP) ]               
if (class(dt8$NEP)=="character") dt8$NEP <-as.numeric(dt8$NEP)
if (class(dt8$flag)!="factor") dt8$flag<- as.factor(dt8$flag)
                
# Convert Missing Values to NA for non-dates
                
dt8$GPP <- ifelse((trimws(as.character(dt8$GPP))==trimws("NA")),NA,dt8$GPP)               
suppressWarnings(dt8$GPP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt8$GPP))==as.character(as.numeric("NA"))),NA,dt8$GPP))
dt8$R <- ifelse((trimws(as.character(dt8$R))==trimws("NA")),NA,dt8$R)               
suppressWarnings(dt8$R <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt8$R))==as.character(as.numeric("NA"))),NA,dt8$R))
dt8$NEP <- ifelse((trimws(as.character(dt8$NEP))==trimws("NA")),NA,dt8$NEP)               
suppressWarnings(dt8$NEP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt8$NEP))==as.character(as.numeric("NA"))),NA,dt8$NEP))


# Here is the structure of the input data frame:
str(dt8)                            
attach(dt8)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(pond_id)
summary(doy)
summary(GPP)
summary(R)
summary(NEP)
summary(flag) 
                # Get more details on character variables
                 
summary(as.factor(dt8$pond_id)) 
summary(as.factor(dt8$flag))
detach(dt8)               
        
	      

# inUrl9  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/c7912a8ba25094f6b71df59ec9c40032" 
# infile9 <- tempfile()
# try(download.file(inUrl9,infile9,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile9))) download.file(inUrl9,infile9,method="auto")
# 
#                    
#  dt9 <-read.csv(infile9,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "pond_id",     
#                     "date_time",     
#                     "doy",     
#                     "temp",     
#                     "do",     
#                     "do_sat"    ), check.names=TRUE)
#                
# unlink(infile9)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt9$pond_id)!="factor") dt9$pond_id<- as.factor(dt9$pond_id)                                   
# # attempting to convert dt9$date_time dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
# tmp9date_time<-as.POSIXct(dt9$date_time,format=tmpDateFormat)
# # Keep the new dates only if they all converted correctly
# if(nrow(dt9[dt9$date_time != "",]) == length(tmp9date_time[!is.na(tmp9date_time)])){dt9$date_time <- tmp9date_time } else {print("Date conversion failed for dt9$date_time. Please inspect the data and do the date conversion yourself.")}                                                                    
#                                 
# if (class(dt9$doy)=="factor") dt9$doy <-as.numeric(levels(dt9$doy))[as.integer(dt9$doy) ]               
# if (class(dt9$doy)=="character") dt9$doy <-as.numeric(dt9$doy)
# if (class(dt9$temp)=="factor") dt9$temp <-as.numeric(levels(dt9$temp))[as.integer(dt9$temp) ]               
# if (class(dt9$temp)=="character") dt9$temp <-as.numeric(dt9$temp)
# if (class(dt9$do)=="factor") dt9$do <-as.numeric(levels(dt9$do))[as.integer(dt9$do) ]               
# if (class(dt9$do)=="character") dt9$do <-as.numeric(dt9$do)
# if (class(dt9$do_sat)=="factor") dt9$do_sat <-as.numeric(levels(dt9$do_sat))[as.integer(dt9$do_sat) ]               
# if (class(dt9$do_sat)=="character") dt9$do_sat <-as.numeric(dt9$do_sat)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# 
# 
# # Here is the structure of the input data frame:
# str(dt9)                            
# attach(dt9)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(pond_id)
# summary(date_time)
# summary(doy)
# summary(temp)
# summary(do)
# summary(do_sat) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt9$pond_id))
# detach(dt9)               
        
	      

inUrl10  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/a7ad389f095cdef3814d5b89de3314b3" 
infile10 <- tempfile()
try(download.file(inUrl10,infile10,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile10))) download.file(inUrl10,infile10,method="auto")

                   
 dt10 <-read.csv(infile10,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "pond_id",     
                    "doy",     
                    "ch4_flux",     
                    "co2_flux",     
                    "n2o_flux",     
                    "ch4_concentration",     
                    "co2_concentration",     
                    "n2o_concentration",     
                    "ch4_atmosphere",     
                    "co2_atmosphere",     
                    "n2o_atmosphere",     
                    "ch4_k600",     
                    "co2_k600",     
                    "n2o_k600"    ), check.names=TRUE)
               
unlink(infile10)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt10$pond_id)!="factor") dt10$pond_id<- as.factor(dt10$pond_id)
if (class(dt10$doy)=="factor") dt10$doy <-as.numeric(levels(dt10$doy))[as.integer(dt10$doy) ]               
if (class(dt10$doy)=="character") dt10$doy <-as.numeric(dt10$doy)
if (class(dt10$ch4_flux)=="factor") dt10$ch4_flux <-as.numeric(levels(dt10$ch4_flux))[as.integer(dt10$ch4_flux) ]               
if (class(dt10$ch4_flux)=="character") dt10$ch4_flux <-as.numeric(dt10$ch4_flux)
if (class(dt10$co2_flux)=="factor") dt10$co2_flux <-as.numeric(levels(dt10$co2_flux))[as.integer(dt10$co2_flux) ]               
if (class(dt10$co2_flux)=="character") dt10$co2_flux <-as.numeric(dt10$co2_flux)
if (class(dt10$n2o_flux)=="factor") dt10$n2o_flux <-as.numeric(levels(dt10$n2o_flux))[as.integer(dt10$n2o_flux) ]               
if (class(dt10$n2o_flux)=="character") dt10$n2o_flux <-as.numeric(dt10$n2o_flux)
if (class(dt10$ch4_concentration)=="factor") dt10$ch4_concentration <-as.numeric(levels(dt10$ch4_concentration))[as.integer(dt10$ch4_concentration) ]               
if (class(dt10$ch4_concentration)=="character") dt10$ch4_concentration <-as.numeric(dt10$ch4_concentration)
if (class(dt10$co2_concentration)=="factor") dt10$co2_concentration <-as.numeric(levels(dt10$co2_concentration))[as.integer(dt10$co2_concentration) ]               
if (class(dt10$co2_concentration)=="character") dt10$co2_concentration <-as.numeric(dt10$co2_concentration)
if (class(dt10$n2o_concentration)=="factor") dt10$n2o_concentration <-as.numeric(levels(dt10$n2o_concentration))[as.integer(dt10$n2o_concentration) ]               
if (class(dt10$n2o_concentration)=="character") dt10$n2o_concentration <-as.numeric(dt10$n2o_concentration)
if (class(dt10$ch4_atmosphere)=="factor") dt10$ch4_atmosphere <-as.numeric(levels(dt10$ch4_atmosphere))[as.integer(dt10$ch4_atmosphere) ]               
if (class(dt10$ch4_atmosphere)=="character") dt10$ch4_atmosphere <-as.numeric(dt10$ch4_atmosphere)
if (class(dt10$co2_atmosphere)=="factor") dt10$co2_atmosphere <-as.numeric(levels(dt10$co2_atmosphere))[as.integer(dt10$co2_atmosphere) ]               
if (class(dt10$co2_atmosphere)=="character") dt10$co2_atmosphere <-as.numeric(dt10$co2_atmosphere)
if (class(dt10$n2o_atmosphere)=="factor") dt10$n2o_atmosphere <-as.numeric(levels(dt10$n2o_atmosphere))[as.integer(dt10$n2o_atmosphere) ]               
if (class(dt10$n2o_atmosphere)=="character") dt10$n2o_atmosphere <-as.numeric(dt10$n2o_atmosphere)
if (class(dt10$ch4_k600)=="factor") dt10$ch4_k600 <-as.numeric(levels(dt10$ch4_k600))[as.integer(dt10$ch4_k600) ]               
if (class(dt10$ch4_k600)=="character") dt10$ch4_k600 <-as.numeric(dt10$ch4_k600)
if (class(dt10$co2_k600)=="factor") dt10$co2_k600 <-as.numeric(levels(dt10$co2_k600))[as.integer(dt10$co2_k600) ]               
if (class(dt10$co2_k600)=="character") dt10$co2_k600 <-as.numeric(dt10$co2_k600)
if (class(dt10$n2o_k600)=="factor") dt10$n2o_k600 <-as.numeric(levels(dt10$n2o_k600))[as.integer(dt10$n2o_k600) ]               
if (class(dt10$n2o_k600)=="character") dt10$n2o_k600 <-as.numeric(dt10$n2o_k600)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt10)                            
attach(dt10)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(pond_id)
summary(doy)
summary(ch4_flux)
summary(co2_flux)
summary(n2o_flux)
summary(ch4_concentration)
summary(co2_concentration)
summary(n2o_concentration)
summary(ch4_atmosphere)
summary(co2_atmosphere)
summary(n2o_atmosphere)
summary(ch4_k600)
summary(co2_k600)
summary(n2o_k600) 
                # Get more details on character variables
                 
summary(as.factor(dt10$pond_id))
detach(dt10)               
        
	      

inUrl11  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/132d5e61d3a62d5924eff19287898868" 
infile11 <- tempfile()
try(download.file(inUrl11,infile11,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile11))) download.file(inUrl11,infile11,method="auto")

                   
 dt11 <-read.csv(infile11,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "pond_id",     
                    "week",     
                    "doy",     
                    "ch4_ebullitive_flux",     
                    "flag"    ), check.names=TRUE)
               
unlink(infile11)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt11$pond_id)!="factor") dt11$pond_id<- as.factor(dt11$pond_id)
if (class(dt11$week)=="factor") dt11$week <-as.numeric(levels(dt11$week))[as.integer(dt11$week) ]               
if (class(dt11$week)=="character") dt11$week <-as.numeric(dt11$week)
if (class(dt11$doy)=="factor") dt11$doy <-as.numeric(levels(dt11$doy))[as.integer(dt11$doy) ]               
if (class(dt11$doy)=="character") dt11$doy <-as.numeric(dt11$doy)
if (class(dt11$ch4_ebullitive_flux)=="factor") dt11$ch4_ebullitive_flux <-as.numeric(levels(dt11$ch4_ebullitive_flux))[as.integer(dt11$ch4_ebullitive_flux) ]               
if (class(dt11$ch4_ebullitive_flux)=="character") dt11$ch4_ebullitive_flux <-as.numeric(dt11$ch4_ebullitive_flux)
if (class(dt11$flag)!="factor") dt11$flag<- as.factor(dt11$flag)
                
# Convert Missing Values to NA for non-dates
                
dt11$ch4_ebullitive_flux <- ifelse((trimws(as.character(dt11$ch4_ebullitive_flux))==trimws("NA")),NA,dt11$ch4_ebullitive_flux)               
suppressWarnings(dt11$ch4_ebullitive_flux <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt11$ch4_ebullitive_flux))==as.character(as.numeric("NA"))),NA,dt11$ch4_ebullitive_flux))


# Here is the structure of the input data frame:
str(dt11)                            
attach(dt11)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(pond_id)
summary(week)
summary(doy)
summary(ch4_ebullitive_flux)
summary(flag) 
                # Get more details on character variables
                 
summary(as.factor(dt11$pond_id)) 
summary(as.factor(dt11$flag))
detach(dt11)               
        
	      

inUrl12  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/275fcaa1e2e7f6d40942dc3734a0aa13" 
infile12 <- tempfile()
try(download.file(inUrl12,infile12,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile12))) download.file(inUrl12,infile12,method="auto")

                   
 dt12 <-read.csv(infile12,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "pond_id",     
                    "week",     
                    "doy",     
                    "methanogenesis_potential"    ), check.names=TRUE)
               
unlink(infile12)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt12$pond_id)!="factor") dt12$pond_id<- as.factor(dt12$pond_id)
if (class(dt12$week)=="factor") dt12$week <-as.numeric(levels(dt12$week))[as.integer(dt12$week) ]               
if (class(dt12$week)=="character") dt12$week <-as.numeric(dt12$week)
if (class(dt12$doy)=="factor") dt12$doy <-as.numeric(levels(dt12$doy))[as.integer(dt12$doy) ]               
if (class(dt12$doy)=="character") dt12$doy <-as.numeric(dt12$doy)
if (class(dt12$methanogenesis_potential)=="factor") dt12$methanogenesis_potential <-as.numeric(levels(dt12$methanogenesis_potential))[as.integer(dt12$methanogenesis_potential) ]               
if (class(dt12$methanogenesis_potential)=="character") dt12$methanogenesis_potential <-as.numeric(dt12$methanogenesis_potential)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt12)                            
attach(dt12)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(pond_id)
summary(week)
summary(doy)
summary(methanogenesis_potential) 
                # Get more details on character variables
                 
summary(as.factor(dt12$pond_id))
detach(dt12)               
        
	      

inUrl13  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/d9242b81ac23e66402120ba119c5e7d2"
infile13 <- tempfile()
try(download.file(inUrl13,infile13,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile13))) download.file(inUrl13,infile13,method="auto")


 dt13 <-read.csv(infile13,header=F
          ,skip=1
            ,sep=","
                ,quot='"'
        , col.names=c(
                    "pond_id",
                    "treatment",
                    "period",
                    "doy",
                    "chla_10_30",
                    "chla_10_30_flag",
                    "tp",
                    "tp_flag",
                    "srp",
                    "srp_flag",
                    "tn",
                    "tn_flag",
                    "nox",
                    "nox_flag",
                    "nhx",
                    "nhx_flag"    ), check.names=TRUE)

unlink(infile13)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt13$pond_id)!="factor") dt13$pond_id<- as.factor(dt13$pond_id)
if (class(dt13$treatment)!="factor") dt13$treatment<- as.factor(dt13$treatment)
if (class(dt13$period)!="factor") dt13$period<- as.factor(dt13$period)
if (class(dt13$doy)=="factor") dt13$doy <-as.numeric(levels(dt13$doy))[as.integer(dt13$doy) ]
if (class(dt13$doy)=="character") dt13$doy <-as.numeric(dt13$doy)
if (class(dt13$chla_10_30)=="factor") dt13$chla_10_30 <-as.numeric(levels(dt13$chla_10_30))[as.integer(dt13$chla_10_30) ]
if (class(dt13$chla_10_30)=="character") dt13$chla_10_30 <-as.numeric(dt13$chla_10_30)
if (class(dt13$chla_10_30_flag)!="factor") dt13$chla_10_30_flag<- as.factor(dt13$chla_10_30_flag)
if (class(dt13$tp)=="factor") dt13$tp <-as.numeric(levels(dt13$tp))[as.integer(dt13$tp) ]
if (class(dt13$tp)=="character") dt13$tp <-as.numeric(dt13$tp)
if (class(dt13$tp_flag)!="factor") dt13$tp_flag<- as.factor(dt13$tp_flag)
if (class(dt13$srp)=="factor") dt13$srp <-as.numeric(levels(dt13$srp))[as.integer(dt13$srp) ]
if (class(dt13$srp)=="character") dt13$srp <-as.numeric(dt13$srp)
if (class(dt13$srp_flag)!="factor") dt13$srp_flag<- as.factor(dt13$srp_flag)
if (class(dt13$tn)=="factor") dt13$tn <-as.numeric(levels(dt13$tn))[as.integer(dt13$tn) ]
if (class(dt13$tn)=="character") dt13$tn <-as.numeric(dt13$tn)
if (class(dt13$tn_flag)!="factor") dt13$tn_flag<- as.factor(dt13$tn_flag)
if (class(dt13$nox)=="factor") dt13$nox <-as.numeric(levels(dt13$nox))[as.integer(dt13$nox) ]
if (class(dt13$nox)=="character") dt13$nox <-as.numeric(dt13$nox)
if (class(dt13$nox_flag)!="factor") dt13$nox_flag<- as.factor(dt13$nox_flag)
if (class(dt13$nhx)=="factor") dt13$nhx <-as.numeric(levels(dt13$nhx))[as.integer(dt13$nhx) ]
if (class(dt13$nhx)=="character") dt13$nhx <-as.numeric(dt13$nhx)
if (class(dt13$nhx_flag)!="factor") dt13$nhx_flag<- as.factor(dt13$nhx_flag)

# Convert Missing Values to NA for non-dates

dt13$tp <- ifelse((trimws(as.character(dt13$tp))==trimws("NA")),NA,dt13$tp)
suppressWarnings(dt13$tp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt13$tp))==as.character(as.numeric("NA"))),NA,dt13$tp))
dt13$srp <- ifelse((trimws(as.character(dt13$srp))==trimws("NA")),NA,dt13$srp)
suppressWarnings(dt13$srp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt13$srp))==as.character(as.numeric("NA"))),NA,dt13$srp))
dt13$tn <- ifelse((trimws(as.character(dt13$tn))==trimws("NA")),NA,dt13$tn)
suppressWarnings(dt13$tn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt13$tn))==as.character(as.numeric("NA"))),NA,dt13$tn))
dt13$nox <- ifelse((trimws(as.character(dt13$nox))==trimws("NA")),NA,dt13$nox)
suppressWarnings(dt13$nox <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt13$nox))==as.character(as.numeric("NA"))),NA,dt13$nox))
dt13$nhx <- ifelse((trimws(as.character(dt13$nhx))==trimws("NA")),NA,dt13$nhx)
suppressWarnings(dt13$nhx <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt13$nhx))==as.character(as.numeric("NA"))),NA,dt13$nhx))


# Here is the structure of the input data frame:
str(dt13)
attach(dt13)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(pond_id)
summary(treatment)
summary(period)
summary(doy)
summary(chla_10_30)
summary(chla_10_30_flag)
summary(tp)
summary(tp_flag)
summary(srp)
summary(srp_flag)
summary(tn)
summary(tn_flag)
summary(nox)
summary(nox_flag)
summary(nhx)
summary(nhx_flag)
                # Get more details on character variables

summary(as.factor(dt13$pond_id))
summary(as.factor(dt13$treatment))
summary(as.factor(dt13$period))
summary(as.factor(dt13$chla_10_30_flag))
summary(as.factor(dt13$tp_flag))
summary(as.factor(dt13$srp_flag))
summary(as.factor(dt13$tn_flag))
summary(as.factor(dt13$nox_flag))
summary(as.factor(dt13$nhx_flag))
detach(dt13)
        
	      

# inUrl14  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/af963fe34cc4b68adc869eb408bb89f9" 
# infile14 <- tempfile()
# try(download.file(inUrl14,infile14,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile14))) download.file(inUrl14,infile14,method="auto")
# 
#                    
#  dt14 <-read.csv(infile14,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "pond_id",     
#                     "launch",     
#                     "collect",     
#                     "biomass_area_m2",     
#                     "biomass_area_cm2"    ), check.names=TRUE)
#                
# unlink(infile14)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt14$pond_id)!="factor") dt14$pond_id<- as.factor(dt14$pond_id)
# if (class(dt14$launch)=="factor") dt14$launch <-as.numeric(levels(dt14$launch))[as.integer(dt14$launch) ]               
# if (class(dt14$launch)=="character") dt14$launch <-as.numeric(dt14$launch)
# if (class(dt14$collect)=="factor") dt14$collect <-as.numeric(levels(dt14$collect))[as.integer(dt14$collect) ]               
# if (class(dt14$collect)=="character") dt14$collect <-as.numeric(dt14$collect)
# if (class(dt14$biomass_area_m2)=="factor") dt14$biomass_area_m2 <-as.numeric(levels(dt14$biomass_area_m2))[as.integer(dt14$biomass_area_m2) ]               
# if (class(dt14$biomass_area_m2)=="character") dt14$biomass_area_m2 <-as.numeric(dt14$biomass_area_m2)
# if (class(dt14$biomass_area_cm2)=="factor") dt14$biomass_area_cm2 <-as.numeric(levels(dt14$biomass_area_cm2))[as.integer(dt14$biomass_area_cm2) ]               
# if (class(dt14$biomass_area_cm2)=="character") dt14$biomass_area_cm2 <-as.numeric(dt14$biomass_area_cm2)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# 
# 
# # Here is the structure of the input data frame:
# str(dt14)                            
# attach(dt14)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(pond_id)
# summary(launch)
# summary(collect)
# summary(biomass_area_m2)
# summary(biomass_area_cm2) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt14$pond_id))
# detach(dt14)               
        
	      

# inUrl15  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/b27e4622e4daf5d174d55e98e6485630" 
# infile15 <- tempfile()
# try(download.file(inUrl15,infile15,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile15))) download.file(inUrl15,infile15,method="auto")
# 
#                    
#  dt15 <-read.csv(infile15,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "doy",     
#                     "pond",     
#                     "site_type",     
#                     "site_id",     
#                     "p_foliosus",     
#                     "p_nodosus",     
#                     "depth_m",     
#                     "canopy_m",     
#                     "canopy_flag",     
#                     "biomass_g",     
#                     "biomass_flag",     
#                     "area_m2"    ), check.names=TRUE)
#                
# unlink(infile15)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt15$doy)=="factor") dt15$doy <-as.numeric(levels(dt15$doy))[as.integer(dt15$doy) ]               
# if (class(dt15$doy)=="character") dt15$doy <-as.numeric(dt15$doy)
# if (class(dt15$pond)!="factor") dt15$pond<- as.factor(dt15$pond)
# if (class(dt15$site_type)!="factor") dt15$site_type<- as.factor(dt15$site_type)
# if (class(dt15$site_id)!="factor") dt15$site_id<- as.factor(dt15$site_id)
# if (class(dt15$p_foliosus)!="factor") dt15$p_foliosus<- as.factor(dt15$p_foliosus)
# if (class(dt15$p_nodosus)!="factor") dt15$p_nodosus<- as.factor(dt15$p_nodosus)
# if (class(dt15$depth_m)=="factor") dt15$depth_m <-as.numeric(levels(dt15$depth_m))[as.integer(dt15$depth_m) ]               
# if (class(dt15$depth_m)=="character") dt15$depth_m <-as.numeric(dt15$depth_m)
# if (class(dt15$canopy_m)=="factor") dt15$canopy_m <-as.numeric(levels(dt15$canopy_m))[as.integer(dt15$canopy_m) ]               
# if (class(dt15$canopy_m)=="character") dt15$canopy_m <-as.numeric(dt15$canopy_m)
# if (class(dt15$canopy_flag)!="factor") dt15$canopy_flag<- as.factor(dt15$canopy_flag)
# if (class(dt15$biomass_g)=="factor") dt15$biomass_g <-as.numeric(levels(dt15$biomass_g))[as.integer(dt15$biomass_g) ]               
# if (class(dt15$biomass_g)=="character") dt15$biomass_g <-as.numeric(dt15$biomass_g)
# if (class(dt15$biomass_flag)!="factor") dt15$biomass_flag<- as.factor(dt15$biomass_flag)
# if (class(dt15$area_m2)=="factor") dt15$area_m2 <-as.numeric(levels(dt15$area_m2))[as.integer(dt15$area_m2) ]               
# if (class(dt15$area_m2)=="character") dt15$area_m2 <-as.numeric(dt15$area_m2)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# dt15$p_foliosus <- as.factor(ifelse((trimws(as.character(dt15$p_foliosus))==trimws("NA")),NA,as.character(dt15$p_foliosus)))
# dt15$p_nodosus <- as.factor(ifelse((trimws(as.character(dt15$p_nodosus))==trimws("NA")),NA,as.character(dt15$p_nodosus)))
# dt15$depth_m <- ifelse((trimws(as.character(dt15$depth_m))==trimws("NA")),NA,dt15$depth_m)               
# suppressWarnings(dt15$depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt15$depth_m))==as.character(as.numeric("NA"))),NA,dt15$depth_m))
# dt15$canopy_m <- ifelse((trimws(as.character(dt15$canopy_m))==trimws("NA")),NA,dt15$canopy_m)               
# suppressWarnings(dt15$canopy_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt15$canopy_m))==as.character(as.numeric("NA"))),NA,dt15$canopy_m))
# dt15$biomass_g <- ifelse((trimws(as.character(dt15$biomass_g))==trimws("NA")),NA,dt15$biomass_g)               
# suppressWarnings(dt15$biomass_g <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt15$biomass_g))==as.character(as.numeric("NA"))),NA,dt15$biomass_g))
# dt15$area_m2 <- ifelse((trimws(as.character(dt15$area_m2))==trimws("NA")),NA,dt15$area_m2)               
# suppressWarnings(dt15$area_m2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt15$area_m2))==as.character(as.numeric("NA"))),NA,dt15$area_m2))
# 
# 
# # Here is the structure of the input data frame:
# str(dt15)                            
# attach(dt15)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(doy)
# summary(pond)
# summary(site_type)
# summary(site_id)
# summary(p_foliosus)
# summary(p_nodosus)
# summary(depth_m)
# summary(canopy_m)
# summary(canopy_flag)
# summary(biomass_g)
# summary(biomass_flag)
# summary(area_m2) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt15$pond)) 
# summary(as.factor(dt15$site_type)) 
# summary(as.factor(dt15$site_id)) 
# summary(as.factor(dt15$p_foliosus)) 
# summary(as.factor(dt15$p_nodosus)) 
# summary(as.factor(dt15$canopy_flag)) 
# summary(as.factor(dt15$biomass_flag))
# detach(dt15)               
        
	      

# inUrl16  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/71680109f864e5aec8a796c81b103d61" 
# infile16 <- tempfile()
# try(download.file(inUrl16,infile16,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile16))) download.file(inUrl16,infile16,method="auto")
# 
#                    
#  dt16 <-read.csv(infile16,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "sampleid",     
#                     "pond_id",     
#                     "doy",     
#                     "treatment",     
#                     "period",     
#                     "taxa",     
#                     "order_class",     
#                     "density"    ), check.names=TRUE)
#                
# unlink(infile16)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt16$sampleid)!="factor") dt16$sampleid<- as.factor(dt16$sampleid)
# if (class(dt16$pond_id)!="factor") dt16$pond_id<- as.factor(dt16$pond_id)
# if (class(dt16$doy)=="factor") dt16$doy <-as.numeric(levels(dt16$doy))[as.integer(dt16$doy) ]               
# if (class(dt16$doy)=="character") dt16$doy <-as.numeric(dt16$doy)
# if (class(dt16$treatment)!="factor") dt16$treatment<- as.factor(dt16$treatment)
# if (class(dt16$period)!="factor") dt16$period<- as.factor(dt16$period)
# if (class(dt16$taxa)!="factor") dt16$taxa<- as.factor(dt16$taxa)
# if (class(dt16$order_class)!="factor") dt16$order_class<- as.factor(dt16$order_class)
# if (class(dt16$density)=="factor") dt16$density <-as.numeric(levels(dt16$density))[as.integer(dt16$density) ]               
# if (class(dt16$density)=="character") dt16$density <-as.numeric(dt16$density)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# 
# 
# # Here is the structure of the input data frame:
# str(dt16)                            
# attach(dt16)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(sampleid)
# summary(pond_id)
# summary(doy)
# summary(treatment)
# summary(period)
# summary(taxa)
# summary(order_class)
# summary(density) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt16$sampleid)) 
# summary(as.factor(dt16$pond_id)) 
# summary(as.factor(dt16$treatment)) 
# summary(as.factor(dt16$period)) 
# summary(as.factor(dt16$taxa)) 
# summary(as.factor(dt16$order_class))
# detach(dt16)               
        
	      

# inUrl17  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/0836df7cd69e3f9912cc6bd4f7eec4e7" 
# infile17 <- tempfile()
# try(download.file(inUrl17,infile17,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile17))) download.file(inUrl17,infile17,method="auto")
# 
#                    
#  dt17 <-read.csv(infile17,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "sampleid",     
#                     "pond_id",     
#                     "taxon",     
#                     "biomass",     
#                     "doy",     
#                     "treatment",     
#                     "period",     
#                     "group"    ), check.names=TRUE)
#                
# unlink(infile17)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt17$sampleid)!="factor") dt17$sampleid<- as.factor(dt17$sampleid)
# if (class(dt17$pond_id)!="factor") dt17$pond_id<- as.factor(dt17$pond_id)
# if (class(dt17$taxon)!="factor") dt17$taxon<- as.factor(dt17$taxon)
# if (class(dt17$biomass)=="factor") dt17$biomass <-as.numeric(levels(dt17$biomass))[as.integer(dt17$biomass) ]               
# if (class(dt17$biomass)=="character") dt17$biomass <-as.numeric(dt17$biomass)
# if (class(dt17$doy)=="factor") dt17$doy <-as.numeric(levels(dt17$doy))[as.integer(dt17$doy) ]               
# if (class(dt17$doy)=="character") dt17$doy <-as.numeric(dt17$doy)
# if (class(dt17$treatment)!="factor") dt17$treatment<- as.factor(dt17$treatment)
# if (class(dt17$period)!="factor") dt17$period<- as.factor(dt17$period)
# if (class(dt17$group)!="factor") dt17$group<- as.factor(dt17$group)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# 
# 
# # Here is the structure of the input data frame:
# str(dt17)                            
# attach(dt17)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(sampleid)
# summary(pond_id)
# summary(taxon)
# summary(biomass)
# summary(doy)
# summary(treatment)
# summary(period)
# summary(group) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt17$sampleid)) 
# summary(as.factor(dt17$pond_id)) 
# summary(as.factor(dt17$taxon)) 
# summary(as.factor(dt17$treatment)) 
# summary(as.factor(dt17$period)) 
# summary(as.factor(dt17$group))
# detach(dt17)               
        
	      

# inUrl18  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/af48b2c28f8911790f7e3703753eff79" 
# infile18 <- tempfile()
# try(download.file(inUrl18,infile18,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile18))) download.file(inUrl18,infile18,method="auto")
# 
#                    
#  dt18 <-read.csv(infile18,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "pond",     
#                     "spp",     
#                     "length",     
#                     "weight",     
#                     "mortality",     
#                     "source",     
#                     "doy",     
#                     "experiment"    ), check.names=TRUE)
#                
# unlink(infile18)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt18$pond)!="factor") dt18$pond<- as.factor(dt18$pond)
# if (class(dt18$spp)!="factor") dt18$spp<- as.factor(dt18$spp)
# if (class(dt18$length)=="factor") dt18$length <-as.numeric(levels(dt18$length))[as.integer(dt18$length) ]               
# if (class(dt18$length)=="character") dt18$length <-as.numeric(dt18$length)
# if (class(dt18$weight)=="factor") dt18$weight <-as.numeric(levels(dt18$weight))[as.integer(dt18$weight) ]               
# if (class(dt18$weight)=="character") dt18$weight <-as.numeric(dt18$weight)
# if (class(dt18$mortality)!="factor") dt18$mortality<- as.factor(dt18$mortality)
# if (class(dt18$source)!="factor") dt18$source<- as.factor(dt18$source)
# if (class(dt18$doy)=="factor") dt18$doy <-as.numeric(levels(dt18$doy))[as.integer(dt18$doy) ]               
# if (class(dt18$doy)=="character") dt18$doy <-as.numeric(dt18$doy)
# if (class(dt18$experiment)!="factor") dt18$experiment<- as.factor(dt18$experiment)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# dt18$weight <- ifelse((trimws(as.character(dt18$weight))==trimws("NA")),NA,dt18$weight)               
# suppressWarnings(dt18$weight <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt18$weight))==as.character(as.numeric("NA"))),NA,dt18$weight))
# dt18$source <- as.factor(ifelse((trimws(as.character(dt18$source))==trimws("NA")),NA,as.character(dt18$source)))
# 
# 
# # Here is the structure of the input data frame:
# str(dt18)                            
# attach(dt18)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(pond)
# summary(spp)
# summary(length)
# summary(weight)
# summary(mortality)
# summary(source)
# summary(doy)
# summary(experiment) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt18$pond)) 
# summary(as.factor(dt18$spp)) 
# summary(as.factor(dt18$mortality)) 
# summary(as.factor(dt18$source)) 
# summary(as.factor(dt18$experiment))
# detach(dt18)               
        
	      

# inUrl19  <- "https://pasta.lternet.edu/package/data/eml/edi/2238/1/fb9a21342c173c45b437e79b1dbe41fe" 
# infile19 <- tempfile()
# try(download.file(inUrl19,infile19,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
# if (is.na(file.size(infile19))) download.file(inUrl19,infile19,method="auto")
# 
#                    
#  dt19 <-read.csv(infile19,header=F 
#           ,skip=1
#             ,sep=","  
#                 ,quot='"' 
#         , col.names=c(
#                     "pond",     
#                     "treatment",     
#                     "doy",     
#                     "fish_id",     
#                     "length",     
#                     "weight",     
#                     "diet_id",     
#                     "broad_taxa",     
#                     "abundance"    ), check.names=TRUE)
#                
# unlink(infile19)
# 		    
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt19$pond)!="factor") dt19$pond<- as.factor(dt19$pond)
# if (class(dt19$treatment)!="factor") dt19$treatment<- as.factor(dt19$treatment)
# if (class(dt19$doy)=="factor") dt19$doy <-as.numeric(levels(dt19$doy))[as.integer(dt19$doy) ]               
# if (class(dt19$doy)=="character") dt19$doy <-as.numeric(dt19$doy)
# if (class(dt19$fish_id)!="factor") dt19$fish_id<- as.factor(dt19$fish_id)
# if (class(dt19$length)=="factor") dt19$length <-as.numeric(levels(dt19$length))[as.integer(dt19$length) ]               
# if (class(dt19$length)=="character") dt19$length <-as.numeric(dt19$length)
# if (class(dt19$weight)=="factor") dt19$weight <-as.numeric(levels(dt19$weight))[as.integer(dt19$weight) ]               
# if (class(dt19$weight)=="character") dt19$weight <-as.numeric(dt19$weight)
# if (class(dt19$diet_id)!="factor") dt19$diet_id<- as.factor(dt19$diet_id)
# if (class(dt19$broad_taxa)!="factor") dt19$broad_taxa<- as.factor(dt19$broad_taxa)
# if (class(dt19$abundance)=="factor") dt19$abundance <-as.numeric(levels(dt19$abundance))[as.integer(dt19$abundance) ]               
# if (class(dt19$abundance)=="character") dt19$abundance <-as.numeric(dt19$abundance)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# dt19$weight <- ifelse((trimws(as.character(dt19$weight))==trimws("na")),NA,dt19$weight)               
# suppressWarnings(dt19$weight <- ifelse(!is.na(as.numeric("na")) & (trimws(as.character(dt19$weight))==as.character(as.numeric("na"))),NA,dt19$weight))
# 
# 
# # Here is the structure of the input data frame:
# str(dt19)                            
# attach(dt19)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(pond)
# summary(treatment)
# summary(doy)
# summary(fish_id)
# summary(length)
# summary(weight)
# summary(diet_id)
# summary(broad_taxa)
# summary(abundance) 
#                 # Get more details on character variables
#                  
# summary(as.factor(dt19$pond)) 
# summary(as.factor(dt19$treatment)) 
# summary(as.factor(dt19$fish_id)) 
# summary(as.factor(dt19$diet_id)) 
# summary(as.factor(dt19$broad_taxa))
# detach(dt19)               
        