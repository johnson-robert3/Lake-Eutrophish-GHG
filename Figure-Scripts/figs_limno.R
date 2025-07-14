#~~~
# Exploratory figs to view limno variables
# By: Robert Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)


# create the 'fdat' and 'pond_data' data sets from the "stats_model-data" script 
source("Analysis-Scripts/stats_model-data.R")

# get figure values, functions, and aesthetics from the 'figs_functions' script
source("Figure-Scripts/figs_functions.R")



#===
# Sonde Profile Data
#===

#--
# Dissolved Oxygen
#--

#- Surface DO concentration
windows(height=7/3, width=3.25)
sdo =
ggplot(fdat,
       aes(x = doy, y = do)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(do, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~DO~(mg~L^-1)), breaks = seq(0, 15, 5)) +
   coord_cartesian(ylim = c(0, 17), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 17 + ((17)*0.1), size=3) +
   # white box beneath legend
   annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 12, ymax = 17, fill="white", color="white") +
   #
   theme_classic() +
   theme(legend.position = c(0.79, 0.85),
         legend.background = element_blank(),
         legend.key.size = unit(0.5, "cm"),
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()


#- Bottom water DO concentration 
windows(height=7/3, width=3.25)
bdo =
ggplot(fdat,
       aes(x = doy, y = bottom_do)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(bottom_do, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~DO~(mg~L^-1)), breaks = seq(0, 20, 5)) +
   coord_cartesian(ylim = c(0, 20), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 20 + ((20)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none", 
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


# 2-panel, DO concentration
windows(height=7/3*2, width=3.25); plot_grid(sdo, bdo, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "pond_DO.png", height=7/3*2, width=3.25, units='in')



#- Surface DO saturation
windows(height=7/3, width=3.25)
# sdo =
ggplot(fdat,
       aes(x = doy, y = do_sat)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(do_sat, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~DO~('%'~sat.)), breaks = seq(0, 200, 50)) +
   coord_cartesian(ylim = c(0, 200), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 200 + ((200)*0.1), size=3) +
   # white box beneath legend
   # annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 180, ymax = 250, fill="white", color="white") +
   #
   theme_classic() +
   theme(#legend.position = c(0.79, 0.85),
         legend.position = "none",
         # legend.background = element_blank(),
         # legend.text = element_text(size=8),
         # legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(file = "surface_DO_sat.png", height=7/3, width=3.25, units='in')


#- Bottom DO saturation
windows(height=7/3, width=3.25)
bdo =
ggplot(fdat,
       aes(x = doy, y = bottom_do_sat)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(bottom_do_sat, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~DO~('%'~sat.)), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = c(0, 250), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 250 + ((250)*0.1), size=3) +
   # white box beneath legend
   annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 180, ymax = 250, fill="white", color="white") +
   #
   theme_classic() +
   theme(legend.position = c(0.79, 0.85),
         # legend.position = "none",
         legend.background = element_blank(),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


# 2-panel, DO saturation
windows(height=7/3*2, width=3.25); plot_grid(sdo, bdo, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "pond_DO_sat.png", height=7/3*2, width=3.25, units='in')



#--
# Temperature
#--

# Surface temp (data used in comparison analysis)
windows(height=7/3, width=3.25)
st = 
ggplot(fdat,
       aes(x = doy, y = temp)) %>%
   # add events 
   fig_events() +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(temp, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~temp.~(degree*C))) +
   coord_cartesian(ylim = c(15, 30), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 30 + ((30-15)*0.1), size=3) +
   # white box beneath legend
   # annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 12, ymax = 17, fill="white", color="white") +
   #
   theme_classic() +
   theme(# legend.position = c(0.79, 0.85),
         legend.position = 'none',
         # legend.background = element_blank(),
         # legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()

# ggsave(file = "surface temp.png", height=7/3, width=3.25, units='in')


# Bottom temp 
windows(height=7/3, width=3.25)
bt =
ggplot(fdat,
       aes(x = doy, y = bottom_temp)) %>%
   # add events 
   fig_events() +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(temp, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~temp.~(degree*C))) +
   coord_cartesian(ylim = c(14, 28), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 28 + ((28-14)*0.1), size=3) +
   # white box beneath legend
   # annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 14, ymax = 17, fill="white", color="white") +
   #
   theme_classic() +
   theme(#legend.position = c(0.79, 0.15),
         legend.position = 'none',
         # legend.background = element_blank(),
         # legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_mid, "lines")) %>%
   fig_theme()

# ggsave(file = "bottom temp.png", height=7/3, width=3.25, units='in')


# 2-panel, temperature
windows(height=7/3*2, width=3.25); plot_grid(st, bt, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "temperature.png", height=7/3*2, width=3.25, units='in')


##
# 3-panel figure - temperature and bottom DO sat. for manuscript
##

windows(height=7, width=3.25); plot_grid(st, bt, bdo, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "temperature_and_bottom-DO.png", height=7, width=3.25, units='in')



#--
# Chlorophyll
#--

# Surface Chlorophyll-a
windows(height=7/3, width=3.25)
ggplot(fdat,
       aes(x = doy, y = chla)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(chla, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~chl~italic("a")~(mu*g~L^-1)),
                      breaks = seq(0, 60, 10)) +
   coord_cartesian(ylim = c(0, 65), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 65 + ((65)*0.1), size=3) +
   # ggtitle(expression(Surface~Water~Chlorophyll~italic("a"))) +
   #
   theme_classic() +
   theme(legend.position = c(0.18, 0.88),
         legend.background = element_blank(),
         legend.text = element_text(size=8),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(filename = "surface-chla.png", height=7/3, width=3.25, units='in')


# Bottom Chlorophyll-a
windows(height=7/3, width=3.25)
ggplot(fdat,
       aes(x = doy, y = bottom_chla)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(bottom_chla, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~chl~italic("a")~(mu*g~L^-1)),
                      breaks = seq(0, 300, 50)) +
   coord_cartesian(ylim = c(0, 325), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 325 + ((325)*0.1), size=3) +
   # ggtitle(expression(Surface~Water~Chlorophyll~italic("a"))) +
   #
   theme_classic() +
   theme(legend.position = c(0.18, 0.88),
         legend.background = element_blank(),
         legend.text = element_text(size=8),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(filename = "bottom-chla.png", height=7/3, width=3.25, units='in')



#--
# Z-mix
#--

# Sonde Zmix depth (data used in comparison analysis)
windows(height=7/3, width=3.25)
ggplot(fdat,
       aes(x = doy, y = sonde_zmix)) %>%
   # add events 
   fig_events() +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(sonde_zmix, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Z[mix]~(m))) +
   coord_cartesian(ylim = c(0,2), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 2 + ((2)*0.1), size=3) +
   # white box beneath legend
   # annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 12, ymax = 17, fill="white", color="white") +
   #
   theme_classic() +
   theme(# legend.position = c(0.79, 0.85),
         legend.position = 'none',
         # legend.background = element_blank(),
         # legend.text = element_text(size=8),
         # legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(file = "z_mix.png", height=7/3, width=3.25, units='in')



#===
# Field Samples
#===

#--
# Nitrogen
#--

#- TN (mg/L)
windows(height=4, width=5.5)
tn =
ggplot(fdat %>% filter(!(is.na(tn))),
       aes(x = doy, y = tn)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   #
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean (loess smooth)
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=0.75, span=0.05) +
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
   theme(# legend.position = c(0.18, 0.87),
         # legend.background = element_blank(),
         legend.position = 'none',
         plot.margin = unit(c(1,0.5,0,0.5), "lines")) %>%
   fig_theme()


#- NOx (mg/L)
windows(height=4, width=5.5)
ggplot(fdat %>% filter(!(is.na(nox))) %>% left_join(pond_data),
       aes(x = doy, y = nox)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(NO[x]~(mg~L^-1)), limits = c(0, 0.4)) +
   #
   ggtitle("Nitrate") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.82, 0.87),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")))



#--
# Phosphorus
#--

#- TP (ug/L)
windows(height=4, width=5.5)
tp = 
ggplot(fdat %>% filter(!(is.na(tp))),
       aes(x = doy, y = tp)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   #
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean (loess smooth)
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=0.75, span=0.05) +
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
         plot.margin = unit(c(1,0.5,0.5,0.5), "lines")) %>%
   fig_theme()


# view TP outliers
# windows(height=4, width=5); ggplot(fdat) + geom_boxplot(aes(x=pond_id, y=tp, group=pond_id)) + theme_bw(); ggsave(file="tp_outliers_boxplot.png", height=4, width=5, units='in')


#- SRP (ug/L)
windows(height=4, width=5.5)
ggplot(fdat %>% filter(!(is.na(srp))),
       aes(x = date, y = srp)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(SRP~(mu*g~L^-1)), limits = c(-0.1, 30), breaks = seq(0, 30, 5)) +
   #
   ggtitle("Soluble Reactive Phosphorus") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.85, 0.87),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))


## TN/TP together
windows(height=7/3*2, width=3.25); plot_grid(tn, tp, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "tn-tp.png", height=7/3*2, width=3.25, units='in')



#--
# DOC
#--

#- 1-panel (blue & red)
windows(height=3.5, width=5)
ggplot(fdat %>% 
          filter(!(is.na(doc_ppm))) %>% 
          left_join(pond_data),
       aes(x = doy, y = doc_ppm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(DOC~(mg~L^-1)),
                      limits = c(0, 50), breaks = seq(0, 50, 10)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.85, 0.87),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")))


# doc over time in exp ponds
# windows(height=4, width=6)
p1 =
ggplot(fdat %>% 
          filter(pond_id %in% c('A', 'B', 'C')) %>%
          filter(!(is.na(doc_ppm)))) +
   #
   geom_point(aes(x = doy, y = doc_ppm, color = pond_id), size=2) +
   geom_line(aes(x = doy, y = doc_ppm, group = pond_id, color = pond_id), size=1, alpha=0.7) +
   #
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   #
   scale_color_manual(breaks = c('A', 'B', 'C'),
                      values = c('A' = "#3BB873", 'B' = '#51ADCF', 'C' = '#2D6187')) +
   lims(y=c(0, 50)) +
   labs(x = "day of year", y = "DOC (ppm)") +
   theme_classic()


# doc over time in ref ponds
# windows(height=4, width=6)
p2 =
ggplot(fdat %>% 
          filter(pond_id %in% c('D', 'E', 'F')) %>%
          filter(!(is.na(doc_ppm)))) +
   #
   geom_point(aes(x = doy, y = doc_ppm, color = pond_id), size=2) +
   geom_line(aes(x = doy, y = doc_ppm, group = pond_id, color = pond_id), size=1, alpha=0.7) +
   #
   # geom_smooth(aes(x = doy, y = doc_ppm, group = pond_id, fill = pond_id, color = pond_id), size=0, alpha=0.2) +
   # geom_smooth(aes(x = doy, y = doc_ppm, group = pond_id, fill = pond_id), alpha=0.2, color=NA) +
   #
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   #
   scale_color_manual(breaks = c('D', 'E', 'F'),
                      values = c('D' = "#3BB873", 'E' = '#51ADCF', 'F' = '#2D6187')) +
   scale_fill_manual(breaks = c('D', 'E', 'F'),
                      values = c('D' = "#3BB873", 'E' = '#51ADCF', 'F' = '#2D6187')) +
   #
   lims(y=c(0, 50)) +
   labs(x = "day of year", y = "DOC (ppm)") +
   theme_classic()


windows(height=8, width=12); p1 / p2

# ggsave("doc-by-np-trt.png")




