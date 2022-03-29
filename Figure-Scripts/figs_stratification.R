#~~~
# Figures for water column stratification and thermal stability
# By: Robert Johnson
#~~~


library(viridis)

source("Figure-Scripts/figs_functions.R")


# color values for stratification heat maps
mycolors = magma(n=10)

# Format t-chain data for figures
tdat = hobo_temp %>%
   # DOY variable for grouping
   mutate(doy = yday(date_time)) %>%
   select(-date_time) %>%
   # remove beg/end dates
   filter(doy >= 143, doy <= 240) %>%
   group_by(pond_id, doy, depth) %>%
   # calculate daily mean temps
   summarize(temp = mean(temp, na.rm=T)) %>%
   mutate(depth = replace(depth, depth=="Anchor", "2.0"),
          depth = as.numeric(depth)) %>%
   ungroup() %>%
   #
   # add a 1.75m depth
   pivot_wider(id_cols = c(pond_id, doy),
               names_from = 'depth', 
               values_from = 'temp') %>%
   mutate(new = rep(-9999, nrow(.)),
          new = na_if(new, -9999),
          doy = as.character(doy)) %>%
   pivot_longer(cols = '0':new,
                names_to = "depth",
                values_to = "temp") %>%
   mutate(depth = replace(depth, depth=="new", 1.75) %>% as.numeric()) %>%
   #
   # interpolate missing temp data
   group_by(pond_id, doy) %>%
   arrange(depth, .by_group = TRUE) %>%
   mutate(temp = zoo::na.approx(temp)) %>%
   ungroup()


#--
# Using HOBO t-chain data
#--

## Stratification heat maps

# Pond B
windows(height=6, width=10)
ggplot(tdat %>% filter(pond_id=="B")) +
   #
   geom_tile(aes(x = doy, y = depth, fill = temp)) +
   #
   scale_fill_gradientn(name = "Temp", 
                        colors = mycolors,
                        breaks = seq(65, 85, 10)) +
   scale_x_discrete(name = "Day of year",
                    breaks = seq(140, 240, 10)) +
   scale_y_continuous(name = "Depth (m)",
                      trans = "reverse") +
   ggtitle("Pond B") +
   #
   theme_classic()

# ggsave(filename = "t-chain_heat-map_Pond-B.png")


# Pond F
windows(height=6, width=10)
ggplot(tdat %>% filter(pond_id=="F")) +
   #
   geom_tile(aes(x = doy, y = depth, fill = temp)) +
   #
   scale_fill_gradientn(name = "Temp", 
                        colors = mycolors,
                        breaks = seq(65, 85, 10)) +
   scale_x_discrete(name = "Day of year",
                    breaks = seq(140, 240, 10)) +
   scale_y_continuous(name = "Depth (m)",
                      trans = "reverse") +
   ggtitle("Pond F") +
   #
   theme_classic()

# ggsave(filename = "t-chain_heat-map_Pond-F.png")



## Daily stratification values

# Set up data
test_hobo = hobo_strat %>%
   mutate(doy = yday(date_time)) %>%
   filter(doy >= 145, doy <= 240)


# Daily thermocline and metalimnion depths

# Pond B
windows(height=6, width=10)
ggplot(test_hobo %>%
          group_by(pond_id, doy) %>%
          summarize(across(meta_top:buoy_freq, ~mean(., na.rm=T))) %>%
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
          summarize(across(meta_top:buoy_freq, ~mean(., na.rm=T))) %>%
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
             size=1, color="#0f3460", alpha=0.8) +
   # pulse
   geom_line(data = ~filter(.x, pond_id=="B"),
             size=1, color="#e94560", alpha=0.8) +
   geom_vline(xintercept = c(176, 211), linetype=2, color = "gray60") +
   labs(x = "Day of year",
        y = "sonde Buoyancy Frequency") +
   theme_classic()





