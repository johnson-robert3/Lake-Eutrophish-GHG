#~~~
# Script to create figures for the supplement
#
# By: RA Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)


# create the 'fdat' and 'pond_data' data sets from the "stats_model-data" script 
source("Analysis-Scripts/stats_model-data.R")

# get figure values, functions, and aesthetics from the 'figs_functions' script
source("Figure-Scripts/figs_functions.R")



#---
# Figure S2: TN and TP
#---

#-- S2a: TN --#
tn =
ggplot(fdat %>% filter(!(is.na(tn))),
       aes(x = doy, y = tn)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   #
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(tn, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TN~(mg~L^-1)), breaks = seq(0, 1, 0.2)) +
   coord_cartesian(ylim = c(0, 1), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 1 + ((1)*0.1), size=3) +
   # ggtitle("Total Nitrogen") +
   #
   theme_classic() +
   theme(legend.position = c(0.18, 0.87),
         legend.background = element_blank(),
         legend.key.size = unit(0.8, "lines"),
         # legend.position = 'none',
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()


#-- S2b: TP --# 
tp = 
ggplot(fdat %>% filter(!(is.na(tp))),
       aes(x = doy, y = tp)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   #
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(tp, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TP~(mu*g~L^-1)), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = c(0, 270), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 270 + ((270)*0.1), size=3) +
   # ggtitle("Total Phosphorus") +
   #
   theme_classic() +
   theme(# legend.position = c(0.82, 0.87),
         legend.position = "none",
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


#-- Build the complete Figure S2 --# 
windows(height=7/3*2, width=3.25)
plot_grid(tn, tp, ncol=1, align='v', labels="AUTO", label_size=11, label_y=c(0.99, 1.04), label_x=0.02)

# ggsave(file = "si_tn-tp.png", height=7/3*2, width=3.25, units='in')



#---
# Figure S3: N2O flux
#---

ylim_n = c(-4, 3)
windows(height=7/3, width=3.25)
ggplot(fdat %>%
          filter(!(is.na(n2o_flux))) %>%
          # convert from mmol to umol
          mutate(n2o_flux = n2o_flux * 1000), 
       aes(x = doy, y = n2o_flux)) %>%
   # add events and analysis windows
   # fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   geom_line(data = ~ .x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(n2o_flux)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), breaks = seq(-4, 3, 1)) +
   coord_cartesian(ylim = ylim_n, clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = (max(ylim_n) + (diff(ylim_n) * 0.1)), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(filename = "n2o-flux.png", height=7/3, width=3.25, units='in')



#---
# Figure S.DO: DO saturation
#---

#-- Surface DO Sat --#
sdo = 
# windows(height=7/3, width=3.25)
ggplot(fdat,
       aes(x = doy, y = do_sat)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=100, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   geom_line(data = ~.x %>% summarize(mean = mean(do_sat, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.8) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~DO~('%'~sat.)), breaks = seq(0, 200, 50)) +
   coord_cartesian(ylim = c(0, 200), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 200 + ((200)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()

# ggsave(file = "surface_DO_sat.png", height=7/3, width=3.25, units='in')


#-- Bottom DO Sat --#
bdo =
ggplot(fdat,
       aes(x = doy, y = bottom_do_sat)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=100, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   geom_line(data = ~.x %>% summarize(mean = mean(bottom_do_sat, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.8) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~DO~('%'~sat.)), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = c(0, 250), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 250 + ((250)*0.1), size=3) +
   # white box beneath legend
   annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 190, ymax = 250, fill="white", color="white") +
   #
   theme_classic() +
   theme(legend.position = c(0.79, 0.85),
         legend.background = element_blank(),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


#-- Build the complete Figure S.DO --# 
windows(height=7/3*2, width=3.25)
plot_grid(sdo, bdo, ncol=1, align='v', labels="AUTO", label_size=11, label_y=c(0.99, 1.04), label_x=0.02)

# ggsave(file = "si_DO-sat.png", height=7/3*2, width=3.25, units='in')



#---
# Figure S.T - Temperature
#---

#-- Surface temp --#
st = 
ggplot(fdat,
       aes(x = doy, y = temp)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   geom_line(data = ~.x %>% summarize(mean = mean(temp, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.8) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~temp.~(degree*C))) +
   coord_cartesian(ylim = c(15, 30), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 30 + ((30-15)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = 'none',
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()


#-- Bottom temp --#
bt =
ggplot(fdat,
       aes(x = doy, y = bottom_temp)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   geom_line(data = ~.x %>% summarize(mean = mean(bottom_temp, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.8) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~temp.~(degree*C))) +
   coord_cartesian(ylim = c(14, 28), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 28 + ((28-14)*0.1), size=3) +
   # white box beneath legend
   annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 14, ymax = 17, fill="white", color="white") +
   #
   theme_classic() +
   theme(legend.position = c(0.79, 0.15),
         # legend.position = 'none',
         legend.background = element_blank(),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


#-- Build the complete Figure S.T --# 
windows(height=7/3*2, width=3.25)
plot_grid(st, bt, ncol=1, align='v', labels="AUTO", label_size=11, label_y=c(0.99, 1.04), label_x=0.02)

# ggsave(file = "si_temperature.png", height=7/3*2, width=3.25, units='in')


#---
# Figure S.W - Max daily wind speed
#---

windows(height=7/3, width=3.25)
ggplot(weather_data %>%
          summarise(max_wind = max(gust_speed)*3.6, .by=doy),  # convert from m/s to km/h (*3.6)
       aes(x = doy, y = max_wind)) +
   geom_line() +
   geom_point() +
   labs(y = expression(Max.~wind~speed~(km~h^-1))) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(filename = "daily-max-wind-speed.png", height=7/3, width=3.25, units='in')



#---
# Figure S.K - Daily k600
#---

windows(height=7/3, width=3.25)
ggplot(weather_data %>%
          mutate(U10 = wind_speed * ((10 / wind_z)^(1/7)),
                 k_cole = LakeMetabolizer::k.cole.base(U10)) %>%
          summarise(k600 = mean(k_cole), .by=doy),
       aes(x = doy, y = k600)) +
   geom_line() +
   geom_point() +
   labs(y = expression(k[600]~(m~d^-1))) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(filename = "k600.png", height=7/3, width=3.25, units='in')



#---
# Figure S.P - Vertical temperature profiles
#---

# using mean temperature between 10:00 - 12:00 (to match sonde time)
windows(height=7/3, width=3.25)
ggplot(hobo_temp %>%
          mutate(hour = hour(date_time)) %>%
          # filter(pond_id=="B") %>%
          filter(doy %in% c(189, 224)) %>%  # DOY 189: heat event; DOY 224: day after derecho
          filter(hour %in% c(10:12)) %>%
          group_by(pond_id, doy, depth) %>%
          summarize(temp = mean(temp, na.rm=T)) %>%
          ungroup() %>%
          left_join(fdat %>% select(pond_id, treatment) %>% unique),
       aes(x = temp, y = depth, color = treatment)) +
   geom_point(aes(shape = as.character(doy)), alpha=0.9, show.legend=FALSE) +
   geom_path(aes(linetype = as.character(doy),
                 group = interaction(treatment, as.character(doy))), 
             alpha=0.9, show.legend = c(color=TRUE, shape=FALSE, linetype=FALSE)) +
   scale_x_continuous(name = expression(Temperature~(degree*C)), limits = c(19.5, 30.5)) +
   scale_y_reverse(name = expression(Depth~(m))) +
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   theme_classic() +
   theme(legend.position = c(0.79, 0.15),
         # legend.position = 'none',
         legend.background = element_blank(),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(filename = "temp-profiles_doy-189-224.png", height=7/3, width=3.25, units='in')



