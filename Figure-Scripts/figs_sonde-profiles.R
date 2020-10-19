#~~~
# Exploratory figures to view data from daily sonde profiles
# By: Robert Johnson
#~~~


library(viridis)


#---
# Profiles over time
#---

myfill = magma(n=10)


# Is the sonde position in the water column determined by the "depth_m" column or the "vertical_position_m" column?
# Why don't these match up?
# At what point does the sonde enter the water?
# Which is the first data point in the pond?
#
# Currently I exclude data if the "depth_m" column values are negative?
# Should I instead exclude when the "vertical_position_m" values are negative?



# need to assign depth intervals to data for plotting (10cm increments)

test = sonde_profiles %>%
   filter(doy > 140) %>%
   mutate(depth_int = case_when(.$vert_m >= 0.000 & vert_m <= 0.1 ~ 0.1,
                                .$vert_m >= 0.101 & vert_m <= 0.2 ~ 0.2,
                                .$vert_m >= 0.201 & vert_m <= 0.3 ~ 0.3,
                                .$vert_m >= 0.301 & vert_m <= 0.4 ~ 0.4,
                                .$vert_m >= 0.401 & vert_m <= 0.5 ~ 0.5,
                                .$vert_m >= 0.501 & vert_m <= 0.6 ~ 0.6,
                                .$vert_m >= 0.601 & vert_m <= 0.7 ~ 0.7,
                                .$vert_m >= 0.701 & vert_m <= 0.8 ~ 0.8,
                                .$vert_m >= 0.801 & vert_m <= 0.9 ~ 0.9,
                                .$vert_m >= 0.901 & vert_m <= 1.0 ~ 1.0,
                                .$vert_m >= 1.001 & vert_m <= 1.1 ~ 1.1,
                                .$vert_m >= 1.101 & vert_m <= 1.2 ~ 1.2,
                                .$vert_m >= 1.201 & vert_m <= 1.3 ~ 1.3,
                                .$vert_m >= 1.301 & vert_m <= 1.4 ~ 1.4,
                                .$vert_m >= 1.401 & vert_m <= 1.5 ~ 1.5,
                                .$vert_m >= 1.501 & vert_m <= 1.6 ~ 1.6,
                                .$vert_m >= 1.601 & vert_m <= 1.7 ~ 1.7,
                                .$vert_m >= 1.701 & vert_m <= 1.8 ~ 1.8,
                                .$vert_m >= 1.801 & vert_m <= 1.9 ~ 1.9,
                                .$vert_m >= 1.901 & vert_m <= 2.0 ~ 2.0,
                                .$vert_m > 2 ~ 2.1))

# mean values per depth interval

test = test %>%
   group_by(pond_id, doy, depth_int) %>%
   summarize(across(temp:salinity, mean, na.rm=T)) %>%
   ungroup() 



# Temperature

windows(height=6, width=10)
ggplot(test) + 
   geom_tile(aes(x = doy, y = depth_int, fill = temp)) +
   scale_fill_gradientn(colors = myfill, 
                        name = "Temp") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# DO saturation

windows(height=6, width=10)
ggplot(test) + 
   geom_tile(aes(x = doy, y = depth_int, fill = do_sat)) +
   scale_fill_gradientn(colors = myfill, 
                        name = "DO Sat") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# pH

windows(height=6, width=10)
ggplot(test) + 
   geom_tile(aes(x = doy, y = depth_int, fill = ph)) +
   scale_fill_gradientn(colors = myfill, 
                        name = "pH") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# Chlorophyll

windows(height=6, width=10)
ggplot(test) + 
   geom_tile(aes(x = doy, y = depth_int, fill = log(chla))) +
   scale_fill_gradientn(colors = myfill, 
                        name = "Log Chl-a") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()




#---
# Viewing individual daily profiles
#---


# data - adjust pond, depth, and day

sdat = sonde_profiles %>%
   filter(pond_id=="A") %>%
   filter(doy == 170)



# depth over time

windows()
ggplot(sdat) +
   geom_point(aes(x = date_time, 
                  y = vert_m)) +
   scale_y_reverse() +
   theme_classic()


# do sat over depth

windows()
ggplot(sdat) +
   geom_point(aes(x = do_sat, 
                  y = vert_m)) +
   scale_y_reverse() +
   theme_classic()


# temperature over depth

windows()
ggplot(sdat) +
   geom_point(aes(x = temp, 
                  y = vert_m)) +
   scale_y_reverse() +
   theme_classic()


# chlorophyll over depth

windows()
ggplot(sdat) +
   geom_point(aes(x = chla, 
                  y = vert_m)) +
   scale_y_reverse() +
   theme_classic()


# ph over depth

windows()
ggplot(sdat) +
   geom_point(aes(x = ph, 
                  y = vert_m)) +
   scale_y_reverse() +
   theme_classic()


# conductivity over depth

windows()
ggplot(sdat) +
   geom_point(aes(x = sp_cond, 
                  y = vert_m)) +
   scale_y_reverse() +
   theme_classic()


#---
# Surface water values over time
#---


# pH

windows(height=6, width=10)
ggplot(sonde_surface) +
   geom_line(aes(x = doy, y = ph)) +
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# chla

windows(height=6, width=10)
ggplot(sonde_surface) +
   geom_line(aes(x = doy, y = chla)) +
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# DO saturation

windows(height=6, width=10)
ggplot(sonde_surface) +
   geom_line(aes(x = doy, y = do_sat)) +
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()




