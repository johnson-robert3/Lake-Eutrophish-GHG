#~~~
# Script for viewing weather data
# By: R. Johnson
#~~~


#--
 # Derecho, Aug. 10, 2020
 weather_data %>% filter(wind_speed == max(wind_speed)) %>% View
#--


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
          
          filter(doy %in% c(170:174))) +
   
   geom_line(aes(x = date_time, y = wind_speed)) +
   labs(x = "Date",
        y = expression(Wind~speed~(m~s^-1))) +
   theme_classic()


## Daily wind values
windows()
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
   labs(x = "Date", 
        y = expression(Wind~speed~(m~s^-1))) +
   #
   theme_classic()



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
windows()
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
   labs(x = "Date", 
        y = expression(PAR~(mu*mol~m^-2~s^-1))) +
   #
   theme_classic()

