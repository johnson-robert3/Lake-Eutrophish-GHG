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

# DO concentration

a = 
ggplot(sonde_bottom %>% left_join(pond_data) %>% filter(trt_fish=="high") %>%
          mutate(roll_sat = slide_dbl(do_sat, mean, .before=7)),
       aes(x = doy, y = roll_sat)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = "DO sat (%) (8-day window)", limits = c(0, 210))

b = 
ggplot(sonde_bottom %>% left_join(pond_data) %>% filter(trt_fish=="medium") %>%
          mutate(roll_sat = slide_dbl(do_sat, mean, .before=7)),
       aes(x = doy, y = roll_sat)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = "DO sat (%) (8-day window)", limits = c(0, 210))

c = 
ggplot(sonde_bottom %>% left_join(pond_data) %>% filter(trt_fish=="low") %>%
          mutate(roll_sat = slide_dbl(do_sat, mean, .before=7)),
       aes(x = doy, y = roll_sat)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = "DO sat (%) (8-day window)", limits = c(0, 210))


windows(height=10, width=6)
a / b / c




