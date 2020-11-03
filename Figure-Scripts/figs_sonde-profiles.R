#~~~
# Exploratory figures to view data from daily sonde profiles
# By: Robert Johnson
#~~~


library(viridis)

source("Figure-Scripts/figs_functions.R")


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



# Temperature

windows(height=6, width=10)
ggplot(sonde_int) + 
   geom_tile(aes(x = doy, y = depth_int, fill = temp)) +
   scale_fill_gradientn(colors = myfill, 
                        name = "Temp") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# DO saturation

windows(height=6, width=10)
ggplot(sonde_int) + 
   geom_tile(aes(x = doy, y = depth_int, fill = do_sat)) +
   scale_fill_gradientn(colors = myfill, 
                        name = "DO Sat") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()

# DO concentration

windows(height=6, width=10)
ggplot(sonde_int) + 
   geom_tile(aes(x = doy, y = depth_int, fill = do)) +
   scale_fill_gradientn(colors = myfill, 
                        name = "DO (mg/L)") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# pH

windows(height=6, width=10)
ggplot(sonde_int) + 
   geom_tile(aes(x = doy, y = depth_int, fill = ph)) +
   scale_fill_gradientn(colors = myfill, 
                        name = "pH") +
   scale_y_reverse(name = "Depth (m)") + 
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


# Chlorophyll

windows(height=6, width=10)
ggplot(sonde_int) + 
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


# DO concentration

a = 
ggplot(sonde_surface %>% left_join(pond_data) %>% filter(trt_fish=="high"),
       aes(x = doy, y = do)) %>%
   fig_aes_fw()

b = 
ggplot(sonde_surface %>% left_join(pond_data) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = do)) %>%
   fig_aes_fw()

c = 
ggplot(sonde_surface %>% left_join(pond_data) %>% filter(trt_fish=="low"),
       aes(x = doy, y = do)) %>%
   fig_aes_fw()


windows(height=10, width=6)
a / b / c


#--
# Bottom water values over time
#--

# Mean values from bottom 30cm of water
sonde_bottom = sonde_int %>%
   group_by(pond_id, doy) %>%
   arrange(depth_int, .by_group=T) %>%
   slice_tail(n=3) %>%
   summarize(across(temp:salinity, ~mean(., na.rm=T))) %>%
   ungroup()


# DO concentration

a = 
ggplot(sonde_bottom %>% left_join(pond_data) %>% filter(trt_fish=="high"),
       aes(x = doy, y = do_sat)) %>%
   fig_aes_fw()

b = 
ggplot(sonde_bottom %>% left_join(pond_data) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = do_sat)) %>%
   fig_aes_fw()

c = 
ggplot(sonde_bottom %>% left_join(pond_data) %>% filter(trt_fish=="low"),
       aes(x = doy, y = do_sat)) %>%
   fig_aes_fw()


windows(height=10, width=6)
a / b / c




