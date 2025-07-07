


# comparing values between 10-30 and 5-50 for sonde surface value averages

# how different are they 


# 10 - 30 cm
sonde_10 = sonde_profiles %>%
   group_by(pond_id, doy) %>%
   filter(vert_m >= 0.10 & vert_m <= 0.30) %>%
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

# 5 - 50 cm
sonde_50 = sonde_profiles %>%
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


# combine
test = full_join(sonde_10 %>% rename_with(.fn = ~paste(., "10", sep="_"), .cols = temp:salinity), 
                 sonde_50 %>% rename_with(.fn = ~paste(., "50", sep="_"), .cols = temp:salinity))


# figs and correlation 


# salinity (used in metabolism calculations)

windows(); plot(sonde_10 %>% filter(pond_id=="A") %>% .$salinity,
                sonde_50 %>% filter(pond_id=="A") %>% .$salinity)

cor.test(sonde_10 %>% filter(pond_id=="A") %>% .$salinity, 
         sonde_50 %>% filter(pond_id=="A") %>% .$salinity)  # 0.99772

windows(); plot(sonde_10 %>% filter(pond_id=="B") %>% .$salinity,
                sonde_50 %>% filter(pond_id=="B") %>% .$salinity)

cor.test(sonde_10 %>% filter(pond_id=="B") %>% .$salinity, 
         sonde_50 %>% filter(pond_id=="B") %>% .$salinity)  # 0.99968

windows(); plot(sonde_10 %>% filter(pond_id=="C") %>% .$salinity,
                sonde_50 %>% filter(pond_id=="C") %>% .$salinity)

cor.test(sonde_10 %>% filter(pond_id=="C") %>% .$salinity, 
         sonde_50 %>% filter(pond_id=="C") %>% .$salinity)  # 0.99977

windows(); plot(sonde_10 %>% filter(pond_id=="D") %>% .$salinity,
                sonde_50 %>% filter(pond_id=="D") %>% .$salinity)

cor.test(sonde_10 %>% filter(pond_id=="D") %>% .$salinity, 
         sonde_50 %>% filter(pond_id=="D") %>% .$salinity)  # 0.99953

windows(); plot(sonde_10 %>% filter(pond_id=="E") %>% .$salinity,
                sonde_50 %>% filter(pond_id=="E") %>% .$salinity)

cor.test(sonde_10 %>% filter(pond_id=="E") %>% .$salinity, 
         sonde_50 %>% filter(pond_id=="E") %>% .$salinity)  # 0.99956

windows(); plot(sonde_10 %>% filter(pond_id=="F") %>% .$salinity,
                sonde_50 %>% filter(pond_id=="F") %>% .$salinity)

cor.test(sonde_10 %>% filter(pond_id=="F") %>% .$salinity, 
         sonde_50 %>% filter(pond_id=="F") %>% .$salinity)  # 0.99983

   # all
   cor.test(test$salinity_10, test$salinity_50)  # 0.99940


# surface temp (used in GHG concentration calculations)

windows(); plot(sonde_10 %>% filter(pond_id=="A") %>% .$temp,
                sonde_50 %>% filter(pond_id=="A") %>% .$temp)

cor.test(sonde_10 %>% filter(pond_id=="A") %>% .$temp, 
         sonde_50 %>% filter(pond_id=="A") %>% .$temp)  # 0.99789


windows(); plot(sonde_10 %>% filter(pond_id=="B") %>% .$temp,
                sonde_50 %>% filter(pond_id=="B") %>% .$temp)

cor.test(sonde_10 %>% filter(pond_id=="B") %>% .$temp, 
         sonde_50 %>% filter(pond_id=="B") %>% .$temp)  # 0.99982

windows(); plot(sonde_10 %>% filter(pond_id=="C") %>% .$temp,
                sonde_50 %>% filter(pond_id=="C") %>% .$temp)

cor.test(sonde_10 %>% filter(pond_id=="C") %>% .$temp, 
         sonde_50 %>% filter(pond_id=="C") %>% .$temp)  # 0.99985

windows(); plot(sonde_10 %>% filter(pond_id=="D") %>% .$temp,
                sonde_50 %>% filter(pond_id=="D") %>% .$temp)

cor.test(sonde_10 %>% filter(pond_id=="D") %>% .$temp, 
         sonde_50 %>% filter(pond_id=="D") %>% .$temp)  # 0.99902

windows(); plot(sonde_10 %>% filter(pond_id=="E") %>% .$temp,
                sonde_50 %>% filter(pond_id=="E") %>% .$temp)

cor.test(sonde_10 %>% filter(pond_id=="E") %>% .$temp, 
         sonde_50 %>% filter(pond_id=="E") %>% .$temp)  # 0.99926

windows(); plot(sonde_10 %>% filter(pond_id=="F") %>% .$temp,
                sonde_50 %>% filter(pond_id=="F") %>% .$temp)

cor.test(sonde_10 %>% filter(pond_id=="F") %>% .$temp, 
         sonde_50 %>% filter(pond_id=="F") %>% .$temp)  # 0.99985

   # all
   cor.test(test$temp_10, test$temp_50)  # 0.99925



# big figs

# salinity comparison
windows(); ggplot(test) +
   #
   geom_point(aes(x = salinity_10, y = salinity_50), size=1.5) +
   geom_abline(intercept = 0, slope = 1, linetype=2, color="gray50") +
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()


# surface temperature comparison
windows(); ggplot(test) +
   #
   geom_point(aes(x = temp_10, y = temp_50), size=1.5) +
   geom_abline(intercept = 0, slope = 1, linetype=2, color="gray50") +
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()



# compare my 10-30 vs Tyler's file

tyler = read_csv("C:/Users/johns/Box/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic/daily-sonde-profiles_mean-values_10-30cm_gapfilled_avg.csv")


test2 = full_join(tyler %>% rename_with(.fn = ~paste(., "ty", sep="_"), .cols = temp:last_col()),
                  sonde_10 %>% rename_with(.fn = ~paste(., "10", sep="_"), .cols = temp:salinity) %>% filter(doy!=241))

cor.test(test2$chla_ty, test2$chla_10)

windows(); plot(test2$chla_ty, test2$chla_10)


test2 %>% select(pond_id, doy, chla_ty, chla_10) %>% mutate(diff = chla_10/chla_ty) %>% arrange(desc(diff)) %>% View
# 4 pond-days where Chla does not match (??); A-198, B-232, F-232, E-217


# The A, B, and F pond data points changed within Tyler's files (as far as I can tell). 
# The E-217 data point changed when I found and removed the extra duplicate profile for pond E on this day. I updated the E-217 profile data on 2022-07-20.

# There are multiple copies (some with differing names, some not) of the "daily-sonde-profiles_mean-values_10-30cm" data set within the 'Hort Farm Experiment' Box folder. 

# In 'Hort Farm Experiment//2020 Benthic Pelagic Experiment' there are "daily-sonde-profiles_mean-values_10-30cm" and a "daily-sonde-profiles_mean-values_10-30cm_gapfilled" files. 
#  The "_gapfilled" file was created 14 minutes after the original. This is when the 3 differing Chla data points (A-198, B-232, F-232) were introduced/changed. I don't know why or how. 
# The original file "daily-sonde-profiles_mean-values_10-30cm" matches the values I get when I recalculate surface averages from raw sonde profile data in R for 10-30 cm. 

# In 'Hort Farm Experiment//2020 Benthic Pelagic Experiment//Tyler Hort Resilience//Datasets used for cleaning' there is a file "daily-sonde-profiles_mean-values_10-30cm_gapfilled". 
#  This matches the file of the same name back out in the '2020 Benthic Pelagic Experiment' folder (same time stamp as well). 

# In 'Hort Farm Experiment//2020 Benthic Pelagic Experiment//Tyler Hort Resilience//hort-benthic-pelagic' there is a file "daily-sonde-profiles_mean-values_10-30cm_gapfilled_avg". 
#  I'm not sure what the "_avg" means. This file also contains the different Chla values mentioned above. 

# In 'Hort Farm Experiment//2020 Benthic Pelagic Experiment//Tyler Hort Resilience//hort-benthic-pelagic' there is a file "hort20_surface_dat" that contains what are reportedly sonde profile
#  surface values averaged from 10-30 cm, along with nutrient data from limno grab samples. This "hort20_surface_dat" contains the "original" (what I am able to recreate in R currently using
#  profile means from 10-30cm) Chla values. 

# Looking at the raw sonde profile data for A-198, the Chla value matches my value. 
# Looking at the raw sonde profile data for B-232, the Chla value matches my value. 
# Looking at the raw sonde profile data for F-232, the Chla value matches my value. 
# Looking at the raw sonde profile data for E-217, the Chla value matches both (the mean just depends on which data are averaged, before or after removing all of the extra data/rows). 



# compare my 10-30 vs the hort20_surface_dat file

hort = read_csv("C:/Users/johns/Box/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic/hort20_surface_dat.csv")


test3 = full_join(hort %>% rename_with(.fn = ~paste(., "h", sep="_"), .cols = temp:salinity),
                  sonde_10 %>% rename_with(.fn = ~paste(., "10", sep="_"), .cols = temp:salinity))

cor.test(test3$chla_h, test3$chla_10)

windows(); plot(test3$chla_h, test3$chla_10)

test3 %>% select(pond_id, doy, chla_h, chla_10) %>% mutate(diff = chla_10/chla_h) %>% arrange(diff) %>% View  # only the Chla data point from E-217 differs, as to be expected. 



