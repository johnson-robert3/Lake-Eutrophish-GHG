#~~~
# Script for making diffusive gas flux figures
# 
# By: R. Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)

source("Figure-Scripts/figs_functions.R")


# Data

# create the 'fdat' and 'pond_data' data sets from the "stats_model-data" script 
source("Analysis-Scripts/stats_model-data.R")



#---
# Flux time-series, 3-panel, manuscript style
#---

## Components for all plots

# events and analysis windows
fig_events = function(.fig) {
   
   .fig +
   # add analysis windows
      # pulse windows (1-5 days after event)
      annotate(geom = 'rect', xmin = 176+1, xmax = 176+5, ymin = -Inf, ymax = Inf, fill='gray90') +
      annotate(geom = 'rect', xmin = 211+1, xmax = 211+5, ymin = -Inf, ymax = Inf, fill='gray90') +
      # pulse events, DOY 176 and 211 (after all sampling had occurred)
      geom_vline(xintercept = c(176.8, 211.8), linetype=1, color="gray40", linewidth=0.8) +
      # heat event, DOY 185-190 (July 3-8, 2020)
      annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill='gray75') +
      # derecho, DOY 223 (Aug. 10, 2020)
      annotate(geom = 'rect', xmin = 223+1, xmax = 223+5, ymin = -Inf, ymax = Inf, fill='gray90') +
      geom_vline(xintercept = 223.8, linetype=2, color='gray40', linewidth=0.8)
}

# panel and axis aesthetics
fig_theme = function(.fig) {
   .fig +
   theme(panel.border = element_rect(fill=NA, color='black'),
         # axis.ticks.length = unit(0.3, 'line'),
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=1, 'line')))
}


## CH4
# windows(height=3.5, width=5)
m =
ggplot(fdat %>%
          filter(!(is.na(ch4_flux))), 
       aes(x = doy, y = ch4_flux)) %>%
   
   # add analysis windows
   fig_events() +
   
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   geom_line(data = ~ .x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(ch4_flux)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = " ", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), breaks = seq(0, 60, 10)) +
   coord_cartesian(ylim = c(0, 60), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.8, 187.5, 211.8, 223.8), y = 60 + ((60)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(c(1,0.5,0,0.5), "lines")) %>%
   fig_theme() 


## N2O
# windows(height=3.5, width=5)
n =
ggplot(fdat %>%
          filter(!(is.na(n2o_flux))) %>%
          # convert from mmol to umol
          mutate(n2o_flux = n2o_flux * 1000), 
       aes(x = doy, y = n2o_flux)) %>%
   
   # add analysis windows
   fig_events() +
   
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   geom_line(data = ~ .x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(n2o_flux)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = " ", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), breaks = seq(-4, 3, 1)) +
   coord_cartesian(ylim = c(-4, 3), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.8, 187.5, 211.8, 223.8), y = 3 + ((4+3)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = "none",
         plot.margin = unit(c(1,0.5,0,0.5), "lines")) %>%
   fig_theme()


## CO2
# windows(height=3.5, width=5)
c =
ggplot(fdat %>%
          filter(!(is.na(co2_flux))), 
       aes(x = doy, y = co2_flux)) %>%
   
   # add analysis windows
   fig_events() +
   
   # zero line
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   geom_line(data = ~ .x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(co2_flux)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = c(-20, 250), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.8, 187.5, 211.8, 223.8), y = 250 + ((20+250)*0.1), size=3) +
   #
   theme_classic() +
   theme(legend.position = c(0.18, 0.86),
         legend.background = element_blank(),
         legend.text = element_text(size=8),
         legend.key.size = unit(0.8, "lines"),
         plot.margin = unit(c(1,0.5,0.5,0.5), "lines")) %>%
   fig_theme()


windows(height=7, width=3.25) #; m / n / c
plot_grid(m, n, c, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(filename = "diffusive-gas-flux.png", height=7, width=3.25, units='in')



#--
# Difference in flux between pulsed and reference treatments
#--

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


# Methane
# windows(height=7/3, width=3.25)
md =
ggplot(fdat %>%
          flux_diff(ch4_flux),
       aes(x = doy, y = diff)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", linewidth=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", linewidth=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # error ribbon
   geom_ribbon(aes(ymin = pmax(diff-diff_se, -10-1.5), ymax = pmin(diff+diff_se, 21+1.5)), fill="gray70", alpha=0.4) +
   # data
   geom_line() +
   geom_point() +
   #
   scale_x_continuous(name = " ", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CH[4]~diff*'.'~(mmol~m^-2~d^-1)), breaks = seq(-10, 20, 10)) +
   # scale_y_continuous(name = expression(CH[4]~diff*'.'*','~~Pulse~'\u2013'~Ref)) +
   coord_cartesian(ylim = c(-10, 21), clip = "off") +
   # event labels
   annotate(geom = "text", label = c("P1", "H", "P2", "D"), x = c(176.5, 187.5, 211.5, 223), y = 21 + ((10+21)*0.1), size=3) +
   #
   theme_classic() +
   theme(plot.margin = unit(c(1,0.5,0,0.5), "lines")) %>%
   fig_theme()


# Nitrous oxide
# windows(height=7/3, width=3.25)
nd =
ggplot(fdat %>%
          mutate(n2o_flux = n2o_flux * 1000) %>%
          flux_diff(n2o_flux),
       aes(x = doy, y = diff)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", linewidth=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", linewidth=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # error ribbon
   geom_ribbon(aes(ymin = pmax(diff-diff_se, -2.5-0.185), ymax = pmin(diff+diff_se, 1.2+0.185)), fill="gray70", alpha=0.4) +
   # data
   geom_line() +
   geom_point() +
   #
   scale_x_continuous(name = " ", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(N[2]*O~diff*'.'~(mu*mol~m^-2~d^-1))) +
   # scale_y_continuous(name = expression(N[2]*O~diff*'.'*','~~Pulse~'\u2013'~Ref)) +
   coord_cartesian(ylim = c(-2.5, 1.2), clip = "off") +
   # event labels
   annotate(geom = "text", label = c("P1", "H", "P2", "D"), x = c(176.5, 187.5, 211.5, 223), y = 1.2 + ((2.5+1.2)*0.1), size=3) +
   #
   theme_classic() +
   theme(plot.margin = unit(c(1,0.5,0,0.5), "lines")) %>%
   fig_theme()


# Carbon dioxide
# windows(height=4, width=5.5)
cd =
ggplot(fdat %>%
          flux_diff(co2_flux),
       aes(x = doy, y = diff)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", linewidth=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", linewidth=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # error ribbon
   geom_ribbon(aes(ymin = pmax(diff-diff_se, -135-10), ymax = pmin(diff+diff_se, 65+10)), fill="gray70", alpha=0.4) +
   # data
   geom_line() +
   geom_point() +
   #
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CO[2]~diff*'.'~(mmol~m^-2~d^-1))) +
   # scale_y_continuous(name = expression(CO[2]~diff*'.,'~~Pulse~'\u2013'~Ref)) +
   coord_cartesian(ylim = c(-135, 65), clip = "off") +
   # event labels
   annotate(geom = "text", label = c("P1", "H", "P2", "D"), x = c(176.5, 187.5, 211.5, 223), y = 65 + ((135+65)*0.1), size=3) +
   #
   annotate(geom = "text", label = "Pulse > Ref", x = 142, y = 45, hjust=0, size=3) +
   annotate(geom = "text", label = "Ref > Pulse", x = 142, y = -45, hjust=0, size=3) +
   #
   theme_classic() +
   theme(plot.margin = unit(c(1,0.5,0.5,0.5), "lines")) %>%
   fig_theme()


windows(height=7, width=3.25) #; md/nd/cd
plot_grid(md, nd, cd, ncol=1, align='v', labels=c('D', 'E', 'F'), label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "diffusive-flux_treatment-difference.png", height=7, width=3.25, units='in')



#-- Diffusive flux time-series and treatment difference together in 1 figure --#

windows(height = 7, width = 3.25*2)
plot_grid(m, n, c, md, nd, cd, ncol=2, align="v", byrow=FALSE, labels=c('A', 'D', 'B', 'E', 'C', 'F'), label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "diffusive-flux_ts-and-diff.png", height = 7, width = 3.25*2, units='in')



#---
#### Cumulative diffusive flux ####
#---

#-- Methane
windows(height=4, width=5.5)
ggplot(fdat %>%
          filter(!(is.na(ch4_flux))) %>%
          # add in blank rows for days without GHG sampling
          full_join(tibble('A' = c(146:240), 'B' = c(146:240), 'C' = c(146:240), 'D' = c(146:240), 'E' = c(146:240), 'F' = c(146:240)) %>%
                       pivot_longer(cols = everything(), names_to = "pond_id", values_to = "doy") %>%
                       mutate(doy = as.numeric(doy))) %>%
          group_by(pond_id) %>%
          arrange(doy, .by_group=TRUE) %>%
          # add Dates for the DOYs that have now been added
          mutate(date = as_date(doy, origin="2020-01-01"),
                 date = ymd(date)) %>%
          # linearly interpolate CH4 flux for days without measurements
          mutate(ch4_interp = zoo::na.approx(ch4_flux)) %>%
          # cumulative flux during experiment
          mutate(ch4_net = slider::slide_dbl(ch4_interp, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = date, y = ch4_net)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.8, size=1.1) +
   # treatment mean (loess smooth)
   # geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   # scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                # date_labels = "%b %Y",
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2)),
                      limits = c(0, 1000), breaks = seq(0, 1000, 250)) +
   #
   ggtitle(expression(Cumulative~Diffusive~CH[4]~Flux)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.16, 0.88),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         # axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(size=rel(1.1)))

# ggsave(filename="cumulative-diffusive-ch4-flux.png", height=4, width=5.5, units='in')


#-- Nitrous Oxide
windows(height=4, width=5.5)
ggplot(fdat %>%
          filter(!(is.na(n2o_flux))) %>%
          # convert from mmol to umol
          mutate(n2o_flux = n2o_flux * 1000) %>%
          # add in blank rows for days without GHG sampling
          full_join(tibble('A' = c(146:240), 'B' = c(146:240), 'C' = c(146:240), 'D' = c(146:240), 'E' = c(146:240), 'F' = c(146:240)) %>%
                       pivot_longer(cols = everything(), names_to = "pond_id", values_to = "doy") %>%
                       mutate(doy = as.numeric(doy))) %>%
          group_by(pond_id) %>%
          arrange(doy, .by_group=TRUE) %>%
          # add Dates for the DOYs that have now been added
          mutate(date = as_date(doy, origin="2020-01-01"),
                 date = ymd(date)) %>%
          # linearly interpolate CH4 flux for days without measurements
          mutate(n2o_interp = zoo::na.approx(n2o_flux)) %>%
          # cumulative flux during experiment
          mutate(n2o_net = slider::slide_dbl(n2o_interp, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = date, y = n2o_net)) +
   #
   geom_hline(yintercept=0, linetype=2, color="gray60", size=1) +
   # geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)),
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.8, size=1.1) +
   # treatment mean (loess smooth)
   # geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   # scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_x_date(name = NULL,
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')),
                # date_labels = "%b %Y",
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2)),
                      limits = c(-160, 25), breaks = seq(-150, 0, 50)) +
   #
   ggtitle(expression(Cumulative~Diffusive~N[2]*O~Flux)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.16, 0.16),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         # axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="cumulative-diffusive-n2o-flux.png", height=4, width=5.5, units='in')



#---
#### All Gases ####
#---

# CH4 - pulsed
# windows(height=4, width=5.5)
a =
ggplot(fdat %>%
          filter(!(is.na(ch4_flux)), treatment=="pulsed"), 
       aes(x = date, y = ch4_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)),
              aes(xintercept = date), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)),
                      limits = c(0, 60), breaks = seq(0, 60, 10)) +
   #
   ggtitle("CH4 - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")
   
# CH4 - reference
# windows(height=4, width=5.5)
d =
ggplot(fdat %>%
          filter(!(is.na(ch4_flux)), treatment=="reference"), 
       aes(x = date, y = ch4_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)),
              aes(xintercept = date), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = 'Food Web', breaks = fish_breaks, values = fish_color, 
                      labels = c('high' = 'High (C,E)', 'medium' = 'Med (A,D)', 'low' = 'Low (B,F)')) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)),
                      limits = c(0, 60), breaks = seq(0, 60, 10)) +
   #
   ggtitle("CH4 - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.17, 0.8))


# N2O - pulsed
# windows(height=4, width=5.5)
b = 
ggplot(fdat %>%
          filter(!(is.na(n2o_flux)), treatment=="pulsed") %>%
          # convert from mmol to umol
          mutate(n2o_flux = n2o_flux * 1000), 
       aes(x = date, y = n2o_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)),
              aes(xintercept = date), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')),
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)),
                      limits = c(-4, 3), breaks = seq(-4, 3, 1)) +
   #
   ggtitle("N2O - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")

# N2O - reference
# windows(height=4, width=5.5)
e = 
ggplot(fdat %>%
          filter(!(is.na(n2o_flux)), treatment=="reference") %>%
          # convert from mmol to umol
          mutate(n2o_flux = n2o_flux * 1000), 
       aes(x = date, y = n2o_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)),
              aes(xintercept = date), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')),
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)),
                      limits = c(-4, 3), breaks = seq(-4, 3, 1)) +
   #
   ggtitle("N2O - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# CO2 - pulsed
# windows(height=4, width=5.5)
c = 
ggplot(fdat %>%
          filter(!(is.na(co2_flux)), treatment=="pulsed"), 
       aes(x = date, y = co2_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)),
              aes(xintercept = date), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), limits = c(-20, 250), breaks = seq(0, 250, 50)) +
   #
   ggtitle("CO2 - pulsed") + 
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")

# CO2 - reference
# windows(height=4, width=5.5)
f =
ggplot(fdat %>%
          filter(!(is.na(co2_flux)), treatment=="reference"), 
       aes(x = date, y = co2_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)),
              aes(xintercept = date), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), limits = c(-20, 250), breaks = seq(0, 250, 50)) +
   #
   ggtitle("CO2 - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# 6-panel figure

windows(height=8, width=14)
(a + b + c) / (d + e + f)


# ggsave(file = '6-panel_flux_by-pulse-trt.png')


#--
# Histogram of Flux Rates
#-- 

# CH4

windows(); ggplot(fdat %>% filter(!(is.na(ch4_flux)))) +
   #
   # geom_histogram(aes(x = ch4_flux, fill = trt_nutrients), position = "dodge") +
   # geom_freqpoly(aes(x = ch4_flux, color = trt_nutrients)) +
   geom_area(aes(x = ch4_flux, fill = trt_nutrients), stat = "bin", alpha = 0.5) +
   #
   theme_classic()



