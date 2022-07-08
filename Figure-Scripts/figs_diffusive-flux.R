#~~~
# Script for making diffusive gas flux figures
# 
# By: R. Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)
library(slider)
library(lubridate)
library(viridis)

source("Figure-Scripts/figs_functions.R")


# Pond/Site Data
pond_data = read_csv("Data/R-Data/2020_pond-data.csv")

# Data for plotting
#  Need to run "stats_model-data.R" script first
pdat = left_join(mdat, pond_data %>% select(pond_id, starts_with("trt")))

# correct dates for fdat
fdat = fdat %>% mutate(date = ymd(date))


#---
#### Methane ####
#---

## Diffusive flux
windows(height=4, width=5.5)
ggplot(fdat %>%
          filter(!(is.na(ch4_flux))) %>%
          left_join(pond_data), 
       aes(x = date, y = ch4_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   # scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                # date_labels = "%b %Y",
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)),
                      limits = c(0, 60), breaks = seq(0, 60, 10)) +
   #
   ggtitle(expression(Diffusive~CH[4]~flux)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.82, 0.88),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         # axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="diffusive-ch4-flux.png", height=4, width=5.5, units='in')


## Cumulative diffusive flux
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
          mutate(ch4_net = slide_dbl(ch4_interp, ~sum(.), .before=Inf)) %>%
          ungroup() %>%
          left_join(pond_data),
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
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
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



#---
#### Nitrous Oxide ####
#---

## Diffusive flux
windows(height=4, width=5.5)
ggplot(fdat %>%
          filter(!(is.na(n2o_flux))) %>%
          # convert from mmol to umol
          mutate(n2o_flux = n2o_flux * 1000) %>%
          left_join(pond_data), 
       aes(x = date, y = n2o_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   # scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')),
                # date_labels = "%b %Y",
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)),
                      limits = c(-4, 3), breaks = seq(-4, 3, 1)) +
   #
   ggtitle(expression(Diffusive~N[2]*O~Flux)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.51, 0.88),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         # axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="diffusive-n2o-flux.png", height=4, width=5.5, units='in')


## Cumulative diffusive flux
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
          mutate(n2o_net = slide_dbl(n2o_interp, ~sum(.), .before=Inf)) %>%
          ungroup() %>%
          left_join(pond_data),
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
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
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
#### Carbon Dioxide ####
#---



#---
#### All Gases ####
#---

# CH4 - pulsed
# windows(height=4, width=5.5)
a =
ggplot(fdat %>%
          filter(!(is.na(ch4_flux)), treatment=="pulsed") %>%
          left_join(pond_data), 
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
          filter(!(is.na(ch4_flux)), treatment=="reference") %>%
          left_join(pond_data), 
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
          mutate(n2o_flux = n2o_flux * 1000) %>%
          left_join(pond_data), 
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
          mutate(n2o_flux = n2o_flux * 1000) %>%
          left_join(pond_data), 
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
          filter(!(is.na(co2_flux)), treatment=="pulsed") %>%
          left_join(pond_data), 
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
          filter(!(is.na(co2_flux)), treatment=="reference") %>%
          left_join(pond_data), 
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

