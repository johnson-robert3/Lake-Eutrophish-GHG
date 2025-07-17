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


#-- S2b: TP --# 
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


#-- Build the complete Figure S2 --# 
windows(height=7/3*2, width=3.25)
plot_grid(tn, tp, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "tn-tp.png", height=7/3*2, width=3.25, units='in')



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
# Figure S4: Surface DO saturation
#---

windows(height=7/3, width=3.25)
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
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~DO~('%'~sat.)), breaks = seq(0, 200, 50)) +
   coord_cartesian(ylim = c(0, 200), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 200 + ((200)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(mar_ind, "lines")) %>%
   fig_theme()

# ggsave(file = "surface_DO_sat.png", height=7/3, width=3.25, units='in')


