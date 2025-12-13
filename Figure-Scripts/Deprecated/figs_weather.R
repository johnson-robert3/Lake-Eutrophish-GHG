#~~~
# Script for viewing weather data
# By: R. Johnson
#~~~

# create weather_data df from the 'data_import_EDI' script


#--
 # Derecho, Aug. 10, 2020
 weather_data %>% filter(wind_speed == max(wind_speed)) %>% View
#--


#--
# Max wind speed 
#  (to view Derecho compared to summer)
#--

windows(height=7/3, width=3.25)
ggplot(weather_data %>%
          summarise(max_wind = max(gust_speed)*3.6, .by=doy),  # convert from m/s to km/h (*3.6)
       aes(x = doy, y = max_wind)) +
   geom_line() +
   geom_point() +
   labs(y = expression(Max.~wind~speed~(km~h^-1)),
        x = "Day of year") +
   theme_classic() +
   theme(legend.position = "none") %>%
   fig_theme()

# ggsave(filename = "daily-max-wind-speed.png", height=7/3, width=3.25, units='in')



#--
# Wind Speed
#--

## View all wind data
windows()
ggplot(weather_data) +
   geom_line(aes(x = date_time, y = wind_speed)) +
   labs(x = "Date",
        y = expression(Wind~speed~(m~s^-1))) +
   theme_classic()


## Select and zoom in on specific days of wind data
windows()
ggplot(weather_data %>%
          
          filter(doy %in% c(146))) +
   
   geom_line(aes(x = date_time, y = wind_speed)) +
   labs(x = "Date",
        y = expression(Wind~speed~(m~s^-1))) +
   theme_classic()


## Daily wind values
windows(height=6, width=10)
ggplot(weather_data %>%
          group_by(doy) %>%
          summarize(min = min(wind_speed), max = max(wind_speed), mean = mean(wind_speed), med = median(wind_speed)) %>%
          ungroup() %>%
          pivot_longer(cols = min:med,
                       names_to = "stat",
                       values_to = "wind")) +
   #
   geom_line(aes(x = doy, y = wind, group = stat, color = stat)) +
   #
   scale_color_manual(breaks = c('min', 'max', 'mean', 'med'), 
                      values = c('min' = 'black', 'max' = 'black', 'mean' = 'firebrick2', 'med' = 'royalblue2')) +
   scale_x_continuous(name = "Day of year",
                      breaks = seq(140, 240, 10)) +
   scale_y_continuous(name = expression(Wind~speed~(m~s^-1))) +
   #
   theme_classic() +
   theme(legend.position = c(0.1, 0.85))

# ggsave(filename = "daily-wind-speed.png")



#--
# PAR
#--

## View all PAR data
windows()
ggplot(weather_data) +
   geom_line(aes(x = date_time, y = par)) +
   labs(x = "Date", 
        y = expression(PAR~(mu*mol~m^-2~s^-1))) +
   theme_classic()


## Select and zoom in on specific days of PAR data
windows()
ggplot(weather_data %>%
          
          filter(doy %in% c(170:174))) +
   
   geom_line(aes(x = date_time, y = par)) +
   labs(x = "Date", 
        y = expression(PAR~(mu*mol~m^-2~s^-1))) +
   theme_classic()


## Daily PAR values
windows(height=6, width=10)
ggplot(weather_data %>%
          # exclude nighttime values
          filter(par > 1.2) %>%
          group_by(doy) %>%
          summarize(min = min(par), max = max(par), mean = mean(par), med = median(par)) %>%
          ungroup() %>%
          pivot_longer(cols = min:med,
                       names_to = "stat",
                       values_to = "par")) +
   #
   geom_line(aes(x = doy, y = par, group = stat, color = stat)) +
   #
   scale_color_manual(breaks = c('min', 'max', 'mean', 'med'), 
                      values = c('min' = 'black', 'max' = 'black', 'mean' = 'firebrick2', 'med' = 'royalblue2')) +
   scale_x_continuous(name = "Day of year",
                      breaks = seq(140, 240, 10)) +
   scale_y_continuous(name = expression(PAR~(mu*mol~m^-2~s^-1))) +
   #
   theme_classic()

# ggsave(filename = "daily-PAR.png")



#--
# k600
#--

# mean daily k600 values
windows(height=7/3, width=3.25)
ggplot(weather_data %>%
          mutate(U10 = wind_speed * ((10 / wind_z)^(1/7)),
                 k_cole = LakeMetabolizer::k.cole.base(U10)) %>%
          summarise(k600 = mean(k_cole), .by=doy),
       aes(x = doy, y = k600)) +
   geom_line() +
   geom_point() +
   labs(y = expression(k[600]~(m~d^-1)),
        x = "Day of year") +
   theme_classic() +
   theme(legend.position = "none") %>%
   fig_theme()

# ggsave(filename = "k600.png", height=7/3, width=3.25, units='in')



