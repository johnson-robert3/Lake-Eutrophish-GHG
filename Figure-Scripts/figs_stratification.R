#~~~
# Figures for water column stratification and thermal stability
# By: Robert Johnson
#~~~


library(viridis)

source("Figure-Scripts/figs_functions.R")



#---
#### HOBO t-chain data ####
#--


# color values for stratification heat maps
mycolors = magma(n=10)

# Format t-chain data for figures

#--Using daily mean values
tdat = hobo_temp %>%
   # DOY variable for grouping
   # mutate(doy = yday(date_time)) %>%
   # select(-date_time) %>%
   # remove beg/end dates
   # filter(doy >= 143, doy <= 240) %>%
   group_by(pond_id, doy, depth) %>%
   # calculate daily mean temps
   summarize(temp = mean(temp, na.rm=T)) %>%
   # mutate(depth = replace(depth, depth=="Anchor", "2.0"),
   #        depth = as.numeric(depth)) %>%
   ungroup() %>%
   #
   # add a 1.75m depth
   pivot_wider(id_cols = c(pond_id, doy),
               names_from = 'depth',
               values_from = 'temp') %>%
   mutate("1.75" = NA) %>%
   pivot_longer(cols = '0':last_col(),
                names_to = "depth",
                values_to = "temp") %>%
   mutate(depth = as.numeric(depth)) %>%
   #
   # interpolate missing temp data (for figs/visualization only)
   group_by(pond_id, doy) %>%
   arrange(depth, .by_group = TRUE) %>%
   mutate(temp = zoo::na.approx(temp)) %>%
   ungroup()


#--Using original 30-minute data
tdat = hobo_temp %>%
   # DOY variable for grouping
   # mutate(doy = yday(date_time)) %>%
   # remove beg/end dates
   # filter(doy >= 143, doy <= 240) %>%
   #
   # add a 1.75m depth
   pivot_wider(id_cols = c(pond_id, date_time, doy),
               names_from = 'depth',
               values_from = 'temp') %>%
   mutate("1.75" = NA) %>%
   # rename("2.0" = Anchor) %>%
   pivot_longer(cols = '0':last_col(),
                names_to = "depth",
                values_to = "temp") %>%
   mutate(depth = as.numeric(depth)) %>%
   #
   # interpolate missing temp data (for figs/visualization only)
   group_by(pond_id, doy, date_time) %>%
   arrange(depth, .by_group = TRUE) %>%
   mutate(temp = zoo::na.approx(temp)) %>%
   ungroup()



##__Stratification heat maps

## Daily mean temperatures

# Pond B
windows(height=6, width=10)
ggplot(tdat %>% filter(pond_id=="B")) +
   #
   geom_tile(aes(x = doy, y = depth, fill = temp)) +
   #
   scale_fill_gradientn(name = "Temp", 
                        colors = mycolors) +
   scale_x_continuous(name = "Day of year",
                      breaks = seq(140, 240, 10)) +
   scale_y_reverse(name = "Depth (m)") +
   labs(title = "Pond B",
        subtitle = "daily means") +
   #
   theme_classic()

# ggsave(filename = "t-chain_heat-map_daily_Pond-B.png")

# Pond B as lines
windows(height=5, width=8)
ggplot(tdat %>% 
          filter(pond_id=="B") %>%
          # exclude 1.75m depth, since this it is interpolated
          filter(depth!=1.75)) +
   geom_line(aes(x = doy, y = temp, group = as.character(depth), color = as.character(depth))) + 
   ggtitle("Pond B") +
   theme_classic()

# ggsave(filename = "t-chain_time-series_daily_B.png")


# Pond F
windows(height=6, width=10)
ggplot(tdat %>% filter(pond_id=="F")) +
   #
   geom_tile(aes(x = doy, y = depth, fill = temp)) +
   #
   scale_fill_gradientn(name = "Temp", 
                        colors = mycolors) +
   scale_x_continuous(name = "Day of year",
                      breaks = seq(140, 240, 10)) +
   scale_y_reverse(name = "Depth (m)") +
   labs(title = "Pond F",
        subtitle = "daily means") +
   #
   theme_classic()

# ggsave(filename = "t-chain_heat-map_daily_Pond-F.png")

# Pond F as lines
windows(height=5, width=8)
ggplot(tdat %>% 
          filter(pond_id=="F") %>%
          # exclude 1.75m depth, since this it is interpolated
          filter(depth!=1.75)) +
   geom_line(aes(x = doy, y = temp, group = as.character(depth), color = as.character(depth))) + 
   ggtitle("Pond F") +
   theme_classic()

# ggsave(filename = "t-chain_time-series_daily_F.png")



# Original high frequency 30-min. temperatures

# Pond B
windows(height=6, width=10)
ggplot(tdat %>% filter(pond_id=="B")) +
   #
   geom_tile(aes(x = date_time, y = depth, fill = temp)) +
   #
   scale_fill_gradientn(name = "Temp", 
                        colors = mycolors) +
   # scale_x_discrete(name = "Day of year",
   #                  breaks = seq(140, 240, 10)) +
   scale_y_reverse(name = "Depth (m)") +
   labs(title = "Pond B",
        subtitle = "30-min data") +
   #
   theme_classic()

# ggsave(filename = "t-chain_heat-map_30-min_Pond-B.png")

# Pond B as lines
windows(height=5, width=8)
ggplot(tdat %>% 
          filter(pond_id=="B") %>%
          # exclude 1.75m depth, since this it is interpolated
          filter(depth!=1.75)) +
   geom_line(aes(x = date_time, y = temp, group = as.character(depth), color = as.character(depth))) + 
   ggtitle("Pond B") +
   theme_classic()

# ggsave(filename = "t-chain_time-series_30min_B.png")


# Pond F
windows(height=6, width=10)
ggplot(tdat %>% filter(pond_id=="F")) +
   #
   geom_tile(aes(x = date_time, y = depth, fill = temp)) +
   #
   scale_fill_gradientn(name = "Temp", 
                        colors = mycolors) +
   # scale_x_discrete(name = "Day of year",
   #                  breaks = seq(140, 240, 10)) +
   scale_y_reverse(name = "Depth (m)") +
   labs(title = "Pond F",
        subtitle = "30-min data") +
   #
   theme_classic()

# ggsave(filename = "t-chain_heat-map_30-min_Pond-F.png")

# Pond F as lines
windows(height=5, width=8)
ggplot(tdat %>% 
          filter(pond_id=="F") %>%
          # exclude 1.75m depth, since this it is interpolated
          filter(depth!=1.75)) +
   geom_line(aes(x = date_time, y = temp, group = as.character(depth), color = as.character(depth))) + 
   ggtitle("Pond F") +
   theme_classic()

# ggsave(filename = "t-chain_time-series_30min_F.png")



##__Daily stratification values

# Set up data
test_hobo = hobo_strat %>%
   mutate(doy = yday(date_time)) %>%
   filter(doy >= 145, doy <= 240)


# Daily thermocline and metalimnion depths

# Pond B
windows(height=6, width=10)
ggplot(test_hobo %>%
          group_by(pond_id, doy) %>%
          summarize(across(meta_top:thermocline, ~mean(., na.rm=T))) %>%
          ungroup() %>%
          filter(pond_id=="B")) +
   #
   geom_line(aes(x = doy, y = thermocline), color="black") +
   geom_line(aes(x = doy, y = meta_top), color="red") +
   geom_line(aes(x = doy, y = meta_bottom), color="blue") +
   #
   scale_x_continuous(name = "Day of year",
                      breaks = seq(140, 240, 10)) +
   scale_y_continuous(name = "Meta depths (m)",
                      trans = "reverse") +
   ggtitle("Pond B") +
   #
   theme_classic()

# ggsave(filename = "daily-meta-depths_Pond-B.png")


# Pond F
windows(height=6, width=10)
ggplot(test_hobo %>%
          group_by(pond_id, doy) %>%
          summarize(across(meta_top:thermocline, ~mean(., na.rm=T))) %>%
          ungroup() %>%
          filter(pond_id=="F")) +
   #
   geom_line(aes(x = doy, y = thermocline), color="black") +
   geom_line(aes(x = doy, y = meta_top), color="red") +
   geom_line(aes(x = doy, y = meta_bottom), color="blue") +
   #
   scale_x_continuous(name = "Day of year",
                      breaks = seq(140, 240, 10)) +
   scale_y_continuous(name = "Meta depths (m)",
                      trans = "reverse") +
   ggtitle("Pond F") +
   #
   theme_classic()

# ggsave(filename = "daily-meta-depths_Pond-F.png")


# Daily Buoyancy Frequency 
windows(height=6, width=10)
ggplot(test_hobo %>% 
          group_by(pond_id, doy) %>%
          summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
          ungroup() %>%
          mutate(pulse = case_when(pond_id=="B" ~ "yes",
                                   pond_id=="F" ~ "no")),
       aes(x = doy, y = buoy_freq)) +
   # reference
   # geom_line(data = ~filter(.x, pond_id=="F"),
   #           size=1, color="#0f3460", alpha=0.8) +
   # pulse
   # geom_line(data = ~filter(.x, pond_id=="B"),
   #           size=1, color="#e94560", alpha=0.8) +
   #
   geom_line(aes(group = pulse, color = pulse), size=1, alpha=0.8) +
   geom_vline(xintercept = c(176, 211), linetype=2, color = "gray60") +
   #
   scale_color_manual(breaks = nut_breaks, 
                      values = nut_color) +
   scale_x_continuous(name = "Day of year",
                      breaks = seq(140, 240, 10)) +
   scale_y_continuous(name = "median Buoyancy frequency", limits = c(0, 0.15)) +
   theme_classic() +
   theme(legend.position = c(0.85, 0.85),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))

# ggsave(filename = "daily-buoyancy-frequency.png")



##__High resolution stratification

# view thermocline "flipping" during times of turnover

# just thermocline depths with points
windows()
ggplot(test_hobo %>%
          mutate(thermocline = replace(thermocline, .$thermocline=="NaN", 1.75)) %>%
          mutate(thermocline = if_else(meta_top==0 & meta_bottom==2, 1.75, thermocline)) %>%
          filter(pond_id=="F"
                 , doy %in% c(210:211)
                 ),
       aes(x = date_time, y = thermocline)) +
   # all data points
   geom_line() +
   # when entire water column was metalimnion
   geom_point(
      data = ~filter(.x, meta_top==0 & meta_bottom==2),
      color = "firebrick2", shape=1, alpha=0.9) +
   # times when the metalimnion was not the whole water column
   geom_point(
      data = ~filter(.x, !(meta_top==0 & meta_bottom==2)),
      color = "cornflowerblue", shape=1, alpha=0.9) +
   #
   scale_y_continuous(limits = c(2,0), trans="reverse") +
   theme_classic()


# meta top, bottom, and thermo
windows()
ggplot(test_hobo %>%
          mutate(thermocline = replace(thermocline, .$thermocline=="NaN", 1.75)) %>%
          mutate(thermocline = if_else(meta_top==0 & meta_bottom==2, 1.75, thermocline)) %>%
          filter(pond_id=="F"
                 , doy %in% c(210:211)
                 )) +
   # all data points
   geom_line(aes(x = date_time, y = thermocline), color = "black") +
   geom_line(aes(x = date_time, y = meta_top), color = "cornflowerblue") +
   geom_line(aes(x = date_time, y = meta_bottom), color = "firebrick2") +
   scale_y_continuous(limits = c(2,0), trans="reverse") +
   theme_classic()
  



#---
#### Daily Sonde Profiles ####
#---

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



# windows()
# ggplot(test_sonde,
#        aes(x = doy, y = buoy_freq)) +
#    # reference
#    geom_line(data = ~filter(.x, pond_id=="F"),
#              size=1, color="#0f3460", alpha=0.8) +
#    # pulse
#    geom_line(data = ~filter(.x, pond_id=="B"),
#              size=1, color="#e94560", alpha=0.8) +
#    geom_vline(xintercept = c(176, 211), linetype=2, color = "gray60") +
#    labs(x = "Day of year",
#         y = "sonde Buoyancy Frequency") +
#    theme_classic()



# Pond maximum depth over time from sonde profiles (based on maximum 'vert_m' recorded by sonde each day; when sonde hit the sediment)

# 'sonde_strat' df from the 'data_stratification' script

windows(height=8, width=15)
ggplot(sonde_strat,
       aes(x = doy)) + 
   
   # pond depth
   geom_line(aes(y = pond_depth), color='black') +
   geom_smooth(aes(y = pond_depth), se=F, color='black') +
   
   # thermocline from sonde
   geom_line(aes(y = thermocline), color="red") +
   
   # z-mix from sonde (thermocline corrected for mixing times)
   # geom_line(aes(y = z_mix), color = "violet", linetype=2) +
   
   # thermo depth from hobos (not corrected for mixing times)
   geom_line(data = hobo_strat %>%
                mutate(doy = yday(date_time),
                       hour = hour(date_time)) %>%
                filter(hour %in% c(9:11)) %>%
                group_by(pond_id, doy) %>%
                summarize(across(meta_top:thermocline, ~mean(., na.rm=T))) %>%
                ungroup(),
             aes(x = doy, y = thermocline), color="blue") +
   
   # thermo depth from hobos (corrected for mixing times, from the metabolism script)
   geom_line(data = metab_data %>%
                mutate(doy = yday(date_time),
                       hour = hour(date_time)) %>%
                filter(hour %in% c(9:11)) %>%
                group_by(pond_id, doy) %>%
                summarize(z_mix = mean(z_mix)) %>%
                ungroup(),
             aes(x = doy, y = z_mix), color="seagreen3") +
   
   #
   scale_y_reverse(limits = c(2.5, 0)) + 
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()


# ggsave(filename = "thermoclines-and-pond-depth.png")



#---
# Viewing vertical temp profiles
#--

#- Using daily sonde profiles

# vertical temperature profiles for two representative days (one stratified, one mixed)
windows()
ggplot(sonde_int %>%
          filter(pond_id=="B") %>%
          filter(doy %in% c(189, 224)) %>%
          group_by(doy) %>%
          arrange(depth_int, .by_group=TRUE) %>%
          ungroup()) +
   geom_point(aes(x = temp, 
                  y = depth_int,
                  color = as.character(doy))) +
   geom_path(aes(x = temp,
                 y = depth_int,
                 color = as.character(doy),
                 group = as.character(doy))) +
   scale_y_reverse() +
   # coord_flip() +
   theme_classic() +
   theme(legend.position = "none")


#- Using Hobo t-chain data

# daily mean temperatures for two representative days (one stratified, one mixed)
windows()
ggplot(tdat %>%
          filter(pond_id=="B") %>%
          filter(doy %in% c(189, 224))) +
   geom_point(aes(x = temp, 
                  y = depth,
                  color = as.character(doy))) +
   geom_path(aes(x = temp,
                 y = depth,
                 color = as.character(doy),
                 group = as.character(doy))) +
   scale_y_reverse() +
   # coord_flip() +
   theme_classic() +
   theme(legend.position = "none")


# using mean temperature between 10:00 - 12:00 (to match sonde time)
windows(height=7/3, width=3.25)
ggplot(hobo_temp %>%
          mutate(hour = hour(date_time)) %>%
          # filter(pond_id=="B") %>%
          filter(doy %in% c(189, 224)) %>%
          filter(hour %in% c(10:12)) %>%
          group_by(pond_id, doy, depth) %>%
          summarize(temp = mean(temp, na.rm=T)) %>%
          ungroup() %>%
          left_join(fdat %>% select(pond_id, treatment) %>% unique),
       aes(x = temp, y = depth, color = treatment)) +
   geom_point(aes(shape = as.character(doy)), alpha=0.9) +
   geom_path(aes(linetype = as.character(doy),
                 group = interaction(treatment, as.character(doy))), alpha=0.9) +
   scale_x_continuous(name = expression(Temperature~(degree*C))) +
   scale_y_reverse(name = expression(Depth~(m))) +
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   theme_classic() +
   theme(legend.position = "none") %>%
   fig_theme()

# ggsave(filename = "temp-profiles_doy-189-224.png")


