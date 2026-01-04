#~~~
# Script to create figures for the manuscript
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
# Figure 2: CH4 and CO2 diffusive flux
#---

# Function to calculate the mean difference between treatment and propagated standard error 
flux_diff = function(.dat, .var) {
   
   .dat %>%
      filter(!(is.na({{.var}}))) %>%  # enclose within {{}} to specify the argument supports <tidy-select>
      summarize(mean = mean({{.var}}), sd = sd({{.var}}), .by = c(trt_nutrients, doy)) %>%
      pivot_longer(cols = c(mean, sd), names_to = "stat", values_to = "value") %>%
      pivot_wider(id_cols = doy, names_from = c(trt_nutrients, stat), values_from = value) %>%
      mutate(diff = pulsed_mean - reference_mean,
             # propagated standard error
             diff_se = sqrt((pulsed_sd^2 + reference_sd^2) / 3))
   
}


#-- 2a: CH4 flux --#
ylim_m = c(0, 60)
m =
ggplot(fdat %>%
          filter(!(is.na(ch4_flux))), 
       aes(x = doy, y = ch4_flux)) %>%
   # add events and analysis windows
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   geom_line(data = ~ .x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(ch4_flux)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = " ", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), breaks = seq(0, 60, 10)) +
   coord_cartesian(ylim = ylim_m, clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = (max(ylim_m) + (diff(ylim_m) * 0.1)), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme() 


#-- 2b: CO2 flux --#
ylim_c = c(-20, 250)
c =
ggplot(fdat %>%
          filter(!(is.na(co2_flux))), 
       aes(x = doy, y = co2_flux)) %>%
   # add events and analysis windows
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   geom_line(data = ~ .x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(co2_flux)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = ylim_c, clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = (max(ylim_c) + (diff(ylim_c) * 0.1)), size=3) +
   #
   theme_classic() +
   theme(legend.position = c(0.18, 0.86),
         legend.background = element_blank(),
         legend.text = element_text(size=8),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


#-- 2c: CH4 difference --#
ylim_md = c(-10, 21)
md =
ggplot(fdat %>%
          flux_diff(ch4_flux),
       aes(x = doy, y = diff)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # error ribbon
   geom_ribbon(aes(ymin = pmax(diff - diff_se, (min(ylim_md) - (diff(ylim_md) * 0.05))), 
                   ymax = pmin(diff + diff_se, (max(ylim_md) + (diff(ylim_md) * 0.05)))), 
               fill="gray70", alpha=0.4) +
   # data
   geom_line() +
   geom_point(size=1) +
   #
   scale_x_continuous(name = " ", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CH[4]~diff*'.'~(mmol~m^-2~d^-1)), breaks = seq(-10, 20, 10)) +
   coord_cartesian(ylim = ylim_md, clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = (max(ylim_md) + (diff(ylim_md) * 0.1)), size=3) +
   #
   theme_classic() +
   theme(plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()


#-- 2d: CO2 difference --#
ylim_cd = c(-135, 65)
cd =
ggplot(fdat %>%
          flux_diff(co2_flux),
       aes(x = doy, y = diff)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # error ribbon
   geom_ribbon(aes(ymin = pmax(diff - diff_se, (min(ylim_cd) - (diff(ylim_cd) * 0.05))), 
                   ymax = pmin(diff + diff_se, (max(ylim_cd) + (diff(ylim_cd) * 0.05)))), 
               fill="gray70", alpha=0.4) +
   # data
   geom_line() +
   geom_point(size=1) +
   #
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CO[2]~diff*'.'~(mmol~m^-2~d^-1))) +
   coord_cartesian(ylim = ylim_cd, clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = (max(ylim_cd) + (diff(ylim_cd) * 0.1)), size=3) +
   #
   annotate(geom = "text", label = "Pulse > Ref", x = 142, y = 45, hjust=0, size=3) +
   annotate(geom = "text", label = "Ref > Pulse", x = 142, y = -45, hjust=0, size=3) +
   #
   theme_classic() +
   theme(plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


#-- Build the complete Figure 2 --#
windows(height = 7/3*2, width = 3.25*2)
plot_grid(m, c, md, cd, ncol=2, align="v", byrow=FALSE, labels=c('A', 'C', 'B', 'D'), label_size=11, label_y=c(0.98, 0.98, 1.03, 1.03), label_x=0.02)

# ggsave(file = "ms_ghg-flux.png", height = 7/3*2, width = 3.25*2, units='in')



#---
# Figure 1: Physicochemical parameters
#---

#-- 1a: Temp: treatment means only, surface/bottom together --#
# windows(height=7/3, width=3.25)
sbt =
ggplot(fdat) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   
   # surface treatment means (smoothed)
   # stat_smooth(aes(x=doy, y=temp, color = trt_nutrients), geom="line", linetype=1, linewidth=0.75, span=0.1, alpha=0.9) +
   # surface treatment means
   geom_line(data = ~.x %>% summarize(mean = mean(temp, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linetype=1, linewidth=0.75, alpha=0.8) +
   
   # bottom treatment means (smoothed)
   # stat_smooth(aes(x=doy, y=bottom_temp, color = trt_nutrients), geom="line", linetype=1, linewidth=0.5, span=0.1, alpha=0.25) +  # pale, solid
   # stat_smooth(aes(x=doy, y=bottom_temp, color = trt_nutrients), geom="line", linetype=3, linewidth=0.5, span=0.1, alpha=1) +  # dark, dashed
   # bottom treatment means
   geom_line(data = ~.x %>% summarize(mean = mean(bottom_temp, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linetype=1, linewidth=0.5, alpha=0.25) +  # pale, solid
   # bottom treatment means
   geom_line(data = ~ .x %>% summarize(mean = mean(bottom_temp, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linetype=3, linewidth=0.5, alpha=1) +  # dark, dashed
   
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Temperature~(degree*C))) +
   coord_cartesian(ylim = c(14, 30), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 30 + ((30-14)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = 'none',
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()



#-- 1b: DO sat: treatment means only, surface/bottom together --#
# windows(height=7/3, width=3.25)
sbdo =
ggplot(fdat) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=100, linetype=3, color="gray50", linewidth=0.8) +
   
   # surface treatment means
   # stat_smooth(aes(x=doy, y=do_sat, color = trt_nutrients), geom="line", linetype=1, linewidth=0.75, span=0.1, alpha=0.9) +
   # surface treatment means
   geom_line(data = ~.x %>% summarize(mean = mean(do_sat, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linetype=1, linewidth=0.75, alpha=0.8) +
   
   # bottom treatment means
   # stat_smooth(aes(x=doy, y=bottom_do_sat, color = trt_nutrients), geom="line", linetype=1, linewidth=0.5, span=0.1, alpha=0.25) +  # pale, solid
   # stat_smooth(aes(x=doy, y=bottom_do_sat, color = trt_nutrients), geom="line", linetype=3, linewidth=0.5, span=0.1, alpha=1) +  # dark, dashed
   # bottom treatment means
   geom_line(data = ~.x %>% summarize(mean = mean(bottom_do_sat, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linetype=1, linewidth=0.5, alpha=0.25) +  # pale, solid
   # bottom treatment means
   geom_line(data = ~ .x %>% summarize(mean = mean(bottom_do_sat, na.rm=T), .by=c(trt_nutrients, doy)),
             aes(x = doy, y = mean, color = trt_nutrients), linetype=3, linewidth=0.5, alpha=1) +  # dark, dashed
   
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(DO~saturation~('%'))) +
   coord_cartesian(ylim = c(0, 200), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 200 + ((200)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = 'none',
         plot.margin = unit(mar_mid, "lines")) %>%
   fig_theme()


#-- 1c: TP --# 
tp = 
ggplot(fdat %>% filter(!(is.na(tp))),
       aes(x = doy, y = tp)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean (loess smooth)
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, span=0.1, alpha=0.9) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(tp, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.75, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TP~(mu*g~L^-1)), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = c(0, 270), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 270 + ((270)*0.1), size=3) +
   #
   theme_classic() +
   theme(# legend.position = c(0.82, 0.87),
         legend.position = "none",
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


#-- Build the complete Figure 1A-C --# 
windows(height=7, width=3.25) 
plot_grid(sbt, sbdo, tp, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "physicochemical.png", height=7, width=3.25, units='in')



#---
# Figure 1: Metabolism
#---

#-- 1d: GPP --#
gpp =
ggplot(fdat,
       aes(x = doy, y = GPP)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, alpha=0.9, span=0.1) +
   geom_line(data = ~.x %>% summarize(mean = mean(GPP, na.rm=T), .by=c(trt_nutrients, doy)) %>% filter(!is.na(mean)),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.6, alpha=1) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(GPP~(mg~O[2]~L^-1~d^-1)), breaks = seq(0, 20, 5)) +
   coord_cartesian(ylim = c(0, 20), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 20 + ((20)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = c(0.17, 0.88),
         legend.background = element_blank(), 
         legend.text = element_text(size=8),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(mar_top, "lines")) %>%
   fig_theme()


#-- 1e: R --#
re =
ggplot(fdat,
       aes(x = doy, y = R)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, alpha=0.9, span=0.1) +
   geom_line(data = ~.x %>% summarize(mean = mean(R, na.rm=T), .by=c(trt_nutrients, doy)) %>% filter(!is.na(mean)),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.6, alpha=1) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)), breaks = seq(-20, 0, 5)) +
   coord_cartesian(ylim = c(-20, 0), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 0 + ((20)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none", 
         plot.margin = unit(mar_mid, "lines")) %>%
   fig_theme()


#-- 1f: NEP --# 
nep =
ggplot(fdat,
       aes(x = doy, y = NEP)) %>%
   # add events 
   fig_windows() %>%
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   # stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=0.75, alpha=0.9, span=0.1) +
   geom_line(data = ~.x %>% summarize(mean = mean(NEP, na.rm=T), .by=c(trt_nutrients, doy)) %>% filter(!is.na(mean)),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=0.6, alpha=1) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)), breaks = seq(-8, 8, 2)) +
   coord_cartesian(ylim = c(-8, 8), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 8 + ((8+8)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(mar_bot, "lines")) %>%
   fig_theme()


#-- Build the complete Figure 1D-F --#
windows(height=7, width=3.25)
plot_grid(gpp, re, nep, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "metabolism.png", height=7, width=3.25, units = "in")



#-- Build the complete Figure 1 (6 panel) --# 
windows(height=7, width=6.5)
plot_grid(sbt, sbdo, tp, gpp, re, nep, ncol=2, align="v", byrow=FALSE, 
          labels=c('A','D','B','E','C','F'), label_size=11, label_y=c(0.99, 0.99, 1.02, 1.02, 1.04, 1.04), label_x=0.02)

# ggsave(file = "ms_physico-metab.png", height=7, width=6.5, units = "in")



#---
# Figure 4: Methanogenesis and Ebullition
#---

#-- 4a: Methanogenesis potential --#
p =
ggplot(fdat %>% 
          filter(!(is.na(methanogenesis))) %>%
          # convert methano rate from umol to nmol/g/h
          mutate(methano = methanogenesis * 1000),
       aes(x = doy, y = methano)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # data
   geom_point(aes(color = trt_nutrients), size=1.5, shape=1, alpha=0.2) +  # just to make the point outlines a little darker
   geom_point(aes(color = trt_nutrients), size=1.5, shape=19, alpha=0.4) +
   # mean
   stat_smooth(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(methano)) %>% ungroup(),
               aes(y = mean, color = trt_nutrients), 
               geom='line', linewidth=0.75, linetype=1, span=0.4, alpha = 0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   # scale_y_continuous(name = expression(CH[4]~'potential'~(nmol~g^-1~h^-1)), breaks = seq(0, 15, 5)) +
   scale_y_continuous(name = "Methanogenesis potential<br>(nmol CH<sub>4</sub> g<sup>-1</sup>h<sup>-1</sup>)", breaks = seq(0, 15, 5)) +
   coord_cartesian(ylim = c(0, 15.5), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 15.5 + ((15.5)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         panel.border = element_rect(color = "black", fill = NA),
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         # axis.title.y = element_text(margin = margin(r=0.5, 'line')),
         axis.title.y = ggtext::element_markdown(),
         plot.margin = unit(mar_top, 'lines'))


#-- 4b: Ebullition --#
e =
ggplot(fdat %>% 
          filter(!(is.na(ch4_ebu_flux))),
       aes(x = doy, y = ch4_ebu_flux)) %>%
   # add events 
   fig_events() +
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # data
   geom_point(aes(color = trt_nutrients), size=1.5, shape=1, alpha=0.2) +  # just to make the point outlines a little darker
   geom_point(aes(color = trt_nutrients), size=1.5, shape=19, alpha=0.4) +
   # mean
   stat_smooth(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(ch4_ebu_flux)) %>% ungroup(),
               aes(y = mean, color = trt_nutrients), 
               geom='line', linewidth=0.75, linetype=1, span=0.4, alpha = 0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   # scale_y_continuous(name = expression(Ebullition~(mmol~m^2~d^-1)), breaks = seq(0, 15, 5)) +
   scale_y_continuous(name = "Ebullition<br>(mmol CH<sub>4</sub> m<sup>-2</sup>d<sup>-1</sup>)", breaks = seq(0, 15, 5)) +
   coord_cartesian(ylim = c(-0.5, 15), clip = "off") +
   # event labels
   annotate(geom = "text", label = event_labs, x = event_lab.x, y = 15 + ((0.5+15)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = c(0.17, 0.88),
         legend.background = element_blank(), 
         legend.text = element_text(size=8),
         legend.key.size = unit(0.8, "lines"),
         panel.border = element_rect(color = "black", fill = NA),
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         # axis.title.y = element_text(margin = margin(r=0.5, 'line')),
         axis.title.y = ggtext::element_markdown(),
         plot.margin = unit(mar_bot, 'lines'))


#-- Build the complete Figure 4 --#
windows(height = 7/3*2, width = 3.25)
plot_grid(p, e, ncol=1, align='v', labels="AUTO", label_size=11, label_y=c(0.99, 1.04), label_x=0.04)

# ggsave(file = "ms_ch4-production.png", height = 7/3*2, width = 3.25, units = "in")



#---
# PCA
#---

#- First, load data and packages, and create the 'pca_dat' df, from the 'stats_PCA' script


# after P1, but impute missing values
dat_exp_imp = pca_dat %>%
  filter(doy >= p1) %>%
  select(-doy) %>%
  drop_na(contains("flux"))

# impute for missing values
exp_imp_res = imputePCA(dat_exp_imp, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_exp_imp = PCA(exp_imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # create biplot from the factominer package from which to extract coord data
  p = fviz_pca_biplot(pca_res_exp_imp,
                  habillage  = as.factor(dat_exp_imp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95, 
                  geom.ind="point", col.var="black", label="none",
                  title = NULL)


# Data point coords
ind_coords <- as.data.frame(pca_res_exp_imp$ind$coord)
ind_coords$treatment <- dat_exp_imp$treatment

# Variable arrow coords
var_coords <- ggplot_build(p)$data[[6]]
var_coords$var <- rownames(pca_res_exp_imp$var$coord)
var_coords$var.num <- 1:nrow(var_coords)
var_coords$abbr <- c('GPP', 'R', 'NEP', 'DO', 'bDO', 'T', 'bT', 'TN', 'TP')

# GHG arrow coords
sup_quanti_coords <- ggplot_build(p)$data[[7]]
sup_quanti_coords$var <- rownames(pca_res_exp_imp$quanti.sup$coord)
sup_quanti_coords$abbr <- c('CH4', 'CO2')

# Ellipse coords
ellipse_coords <- ggplot_build(p)$data[[2]] %>%
  mutate(treatment = case_when(group==1 ~ "pulsed", group==2 ~ "reference"))


# PCA figure 
windows(width=5, height=3.5)
ggplot(ind_coords) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray20") +
  geom_vline(xintercept = 0, linetype="dashed", color="gray20") +
  # add data points
  geom_point(aes(x = Dim.1, y = Dim.2, color = treatment), shape=1, alpha=0.2) +
  geom_point(aes(x = Dim.1, y = Dim.2, color = treatment), shape=19, alpha=0.4) +
  # add ellipses
  geom_polygon(data = ellipse_coords,
               aes(x = x, y = y, fill = treatment, color=treatment),
               alpha=0.1) +
  # add arrows for variables
  geom_segment(data = var_coords,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")), color="black", alpha=0.9) +
  geom_label(data = var_coords,
             aes(x = xend, y = yend, label = abbr),
             color="black", size=2.5, nudge_x = c(var_coords$xend * 0.2), nudge_y = c(var_coords$yend * 0.1)) +
  # add arrows for GHG fluxes
  geom_segment(data = sup_quanti_coords,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")), color="blue", alpha=0.9) +
  geom_label(data = sup_quanti_coords,
             aes(x = xend, y = yend, label = abbr),
             color="blue", size=2.5, nudge_y = c(-0.4, 0.6)) +
  # aesthetics
  scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
  scale_fill_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
  lims(x = c(-5, 5), y = c(-5.5, 4.5)) +
  labs(x = "PC1 (26%)", y = "PC2 (21.3%)") +
  theme_minimal() #+
  # theme(legend.position = 'none')

# ggsave(file = "PCA_post-P1.png", width=5, height=3.5, units="in", dpi=300)



