#~~~
# Script for viewing weather data
# By: R. Johnson
#~~~




#--
# Wind Speed
#--

windows()
ggplot(weather_data) +
   geom_line(aes(x = date_time, y = wind_speed)) +
   theme_classic()





#--
# PAR
#--

windows()
ggplot(weather_data) +
   geom_line(aes(x = date_time, y = par)) +
   theme_classic()

