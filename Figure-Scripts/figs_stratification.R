#~~~
# Figures for water column stratification and thermal stability
# By: Robert Johnson
#~~~


source("Figure-Scripts/figs_functions.R")


#--
# Using HOBO t-chain data
#--

# Set up data
test_hobo = hobo_strat %>%
   mutate(doy = yday(date_time)) %>%
   filter(doy >= 145, doy <= 240)


# Buoyancy Frequency

windows()
ggplot(test_hobo %>% 
          group_by(pond_id, doy) %>%
          summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = buoy_freq)) +
   # reference
   geom_line(data = ~filter(.x, pond_id=="F"),
             size=1.5, color="cornflowerblue", alpha=0.8) +
   # pulse
   geom_line(data = ~filter(.x, pond_id=="B"),
             size=1.5, color="seagreen3", alpha=0.8) +
   geom_vline(xintercept = c(176, 211), linetype=2, color = "gray60") +
   labs(x = "DOY",
        y = "median buoy_freq") +
   theme_classic()



# Metalimnion and thermocline depths

windows()
ggplot(test_hobo %>%
          group_by(pond_id, doy) %>%
          summarize(across(meta_top:buoy_freq, ~mean(., na.rm=T))) %>%
          ungroup() %>%
          filter(pond_id=="B"))+
   geom_line(aes(x = doy, y = thermocline), color="black") +
   geom_line(aes(x = doy, y = meta_top), color="red") +
   geom_line(aes(x = doy, y = meta_bottom), color="blue") +
   scale_y_reverse() +
   theme_classic()



#--
# Using daily sonde profiles
#--

# calculate stratification variables

test_sonde = sonde_int %>%
   # select needed data
   filter(doy >= 145, doy <= 240) %>%
   filter(!(is.na(temp))) %>%
   select(pond_id:temp) %>%
   # calculate mean buoyancy frequency from sonde profile
   group_by(pond_id, doy) %>%
   group_modify(~profile_bf(.)) %>%
   summarize(buoy_freq = mean(buoy_freq, na.rm=T)) %>%
   ungroup() %>%
   filter(!(buoy_freq < 0))



windows()
ggplot(test_sonde,
       aes(x = doy, y = buoy_freq)) +
   # reference
   geom_line(data = ~filter(.x, pond_id=="F"),
             size=1.5, color="cornflowerblue", alpha=0.8) +
   # pulse
   geom_line(data = ~filter(.x, pond_id=="B"),
             size=1.5, color="seagreen3", alpha=0.8) +
   geom_vline(xintercept = c(176, 211), linetype=2, color = "gray60") +
   labs(x = "DOY",
        y = "sonde buoy_freq") +
   theme_classic()

   



