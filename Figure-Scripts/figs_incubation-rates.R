#~~~
# Script for making process rate figures (Methanogenesis, DEA, Ebullition)
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


# Data

# create the 'fdat' and 'pond_data' data sets from the "stats_model-data" script 



### 6-panel figure, all 3 process rates


## METHANOGENESIS

# Pulsed
# windows(height=4, width=5.5)
a = 
ggplot(fdat %>% 
          filter(!(is.na(methanogenesis)), treatment=='pulsed') %>%
          # convert methano rate from umol to nmol/g/h
          mutate(methano = methanogenesis * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = methano)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
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
   scale_y_continuous(name = expression(CH[4]~(nmol~g^-1~h^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Methanogenesis - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# Reference
# windows(height=4, width=5.5)
c = 
ggplot(fdat %>% 
          filter(!(is.na(methanogenesis)), treatment=='reference') %>%
          # convert methano rate from umol to nmol/g/h
          mutate(methano = methanogenesis * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = methano)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
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
   scale_y_continuous(name = expression(CH[4]~(nmol~g^-1~h^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Methanogenesis - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.17, 0.8))



## DEA

# Pulsed
# windows(height=4, width=5.5)
b = 
ggplot(fdat %>% 
          filter(!(is.na(DEA)), treatment=='pulsed') %>%
          # convert DEA rate from umol to nmol/g/h
          mutate(DEA = DEA * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = DEA)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
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
   scale_y_continuous(name = expression(N[2]*O~(nmol~g^-1~h^-1)),
                      limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25)) +
   #
   ggtitle("DEA - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# Reference
# windows(height=4, width=5.5)
d = 
ggplot(fdat %>% 
          filter(!(is.na(DEA)), treatment=='reference') %>%
          # convert DEA rate from umol to nmol/g/h
          mutate(DEA = DEA * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = DEA)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
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
   scale_y_continuous(name = expression(N[2]*O~(nmol~g^-1~h^-1)),
                      limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25)) +
   #
   ggtitle("DEA - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")



## EBULLITION

# Pulsed
# windows(height=4, width=5.5)
e = 
ggplot(fdat %>% 
          filter(!(is.na(ch4_ebu_flux)), treatment=="pulsed") %>%
          left_join(pond_data),
       aes(x = date, y = ch4_ebu_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
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
   scale_y_continuous(name = expression(CH[4]~(mmol~m^-2~d^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Ebullition - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# Reference
# windows(height=4, width=5.5)
f = 
ggplot(fdat %>% 
          filter(!(is.na(ch4_ebu_flux)), treatment=="reference") %>%
          left_join(pond_data),
       aes(x = date, y = ch4_ebu_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
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
   scale_y_continuous(name = expression(CH[4]~(mmol~m^-2~d^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Ebullition - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")



# Figure

windows(height=8, width=14)
(a + b + e) / (c + d + f)


# ggsave(file = '6-panel_methano-dea-ebu_by-pulse-trt.png')



#-
### Ebullition
#-

# 1 panel as points with error ribbons

windows(height=4, width=6); ggplot(fdat %>% 
                                      filter(!(is.na(ch4_ebu_flux))) %>%
                                      left_join(pond_data) %>%
                                      group_by(trt_nutrients, doy) %>%
                                      summarize(n = n(),
                                                se = sd(ch4_ebu_flux)/sqrt(n),
                                                mean = mean(ch4_ebu_flux)) %>%
                                      ungroup(),
                                   aes(x = doy, y = mean, group = trt_nutrients)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(xintercept = c(176, 211), linetype=1, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(xintercept = 223, linetype=2, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect',
            xmin = 186, xmax = 190,
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   # SE ribbons
   geom_ribbon(aes(x = doy, ymin = mean - se, ymax = mean + se, fill = trt_nutrients), alpha = 0.2, show.legend=F) +
   # data
   geom_point(aes(color = trt_nutrients), size=2, shape=19) +
   geom_line(aes(color = trt_nutrients), size=1.25, linetype=1) +
   #
   scale_x_continuous(name = "Day of year") +
   scale_y_continuous(name = expression(Ebullition~(mmol~CH[4]~m^2~d^-1))) +
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_fill_manual(name = NULL, breaks = nut_breaks, values = nut_color) +
   #
   theme_classic() +
   theme(legend.position = c(0.15, 0.88),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text = element_text(color = "black"))

# ggsave(file = "ebullition-time-series.png")


# 1 panel raw data points and smoothed means

windows(height=4, width=6); ggplot(fdat %>% 
                                      filter(!(is.na(ch4_ebu_flux))) %>%
                                      left_join(pond_data),
                                   aes(x = doy, y = ch4_ebu_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(xintercept = c(176, 211), linetype=1, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(xintercept = 223, linetype=2, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect',
            xmin = 186, xmax = 190,
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   # data
   geom_point(aes(color = trt_nutrients), size=2, shape=19, alpha = 0.4) +
   # mean
   geom_smooth(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(ch4_ebu_flux)) %>% ungroup(),
             aes(y = mean, color = trt_nutrients), 
             size=1.5, linetype=1, span=0.4, se=FALSE) +
   #
   scale_x_continuous(name = "Day of year") +
   scale_y_continuous(name = expression(Ebullition~(mmol~CH[4]~m^2~d^-1))) +
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_fill_manual(name = NULL, breaks = nut_breaks, values = nut_color) +
   #
   theme_classic() +
   theme(legend.position = c(0.15, 0.88),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text = element_text(color = "black"))

ggsave(file = "ebullition-time-series.png")



#-
### Methanogenesis
#- 

# 1 panel as points with error ribbons

windows(height=4, width=6); ggplot(fdat %>% 
                                      filter(!(is.na(methanogenesis))) %>%
                                      # convert methano rate from umol to nmol/g/h
                                      mutate(methano = methanogenesis * 1000) %>%
                                      left_join(pond_data) %>%
                                      group_by(trt_nutrients, doy) %>%
                                      summarize(n = n(),
                                                se = sd(methano)/sqrt(n),
                                                mean = mean(methano)) %>%
                                      ungroup(),
                                   aes(x = doy, y = mean, group = trt_nutrients)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(xintercept = c(176, 211), linetype=1, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(xintercept = 223, linetype=2, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect',
            xmin = 186, xmax = 190,
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   # SE ribbons
   geom_ribbon(aes(x = doy, ymin = mean - se, ymax = mean + se, fill = trt_nutrients), alpha = 0.2, show.legend=F) +
   # data
   geom_point(aes(color = trt_nutrients), size=2, shape=19) +
   geom_line(aes(color = trt_nutrients), size=1.25, linetype=1) +
   #
   scale_x_continuous(name = "Day of year") +
   scale_y_continuous(name = expression(Methanogenesis~potential~(nmol~g^-1~h^-1))) +
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_fill_manual(name = NULL, breaks = nut_breaks, values = nut_color) +
   #
   theme_classic() +
   theme(legend.position = c(0.15, 0.88),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text = element_text(color = "black"))

# ggsave(file = "methanogenesis-time-series.png")


# 1 panel raw data points and smoothed means

windows(height=4, width=6); ggplot(fdat %>% 
                                      filter(!(is.na(methanogenesis))) %>%
                                      # convert methano rate from umol to nmol/g/h
                                      mutate(methano = methanogenesis * 1000) %>%
                                      left_join(pond_data),
                                   aes(x = doy, y = methano)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(xintercept = c(176, 211), linetype=1, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(xintercept = 223, linetype=2, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect',
            xmin = 186, xmax = 190,
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   # data
   geom_point(aes(color = trt_nutrients), size=2, shape=19, alpha = 0.4) +
   # mean
   geom_smooth(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(methano)) %>% ungroup(),
             aes(y = mean, color = trt_nutrients), 
             size=1.5, linetype=1, span=0.4, se=FALSE) +
   #
   scale_x_continuous(name = "Day of year") +
   scale_y_continuous(name = expression(Methanogenesis~potential~(nmol~g^-1~h^-1))) +
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_fill_manual(name = NULL, breaks = nut_breaks, values = nut_color) +
   #
   theme_classic() +
   theme(legend.position = c(0.15, 0.88),
         legend.background = element_rect(fill = NA),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text = element_text(color = "black"))

ggsave(file = "methanogenesis-time-series.png")



#-
### DEA
#- 

# 1 panel as points with error ribbons

windows(height=4, width=6); ggplot(fdat %>% 
                                      filter(!(is.na(DEA))) %>%
                                      # convert DEA rate from umol to nmol/g/h
                                      mutate(DEA = DEA * 1000) %>%
                                      left_join(pond_data) %>%
                                      group_by(trt_nutrients, doy) %>%
                                      summarize(n = n(),
                                                se = sd(DEA)/sqrt(n),
                                                mean = mean(DEA)) %>%
                                      ungroup(),
                                   aes(x = doy, y = mean, group = trt_nutrients)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(xintercept = c(176, 211), linetype=1, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(xintercept = 223, linetype=2, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect',
            xmin = 186, xmax = 190,
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   # SE ribbons
   geom_ribbon(aes(x = doy, ymin = mean - se, ymax = mean + se, fill = trt_nutrients), alpha = 0.2, show.legend=F) +
   # data
   geom_point(aes(color = trt_nutrients), size=2, shape=19) +
   geom_line(aes(color = trt_nutrients), size=1.25, linetype=1) +
   #
   scale_x_continuous(name = "Day of year") +
   scale_y_continuous(name = expression(DEA~(nmol~g^-1~h^-1))) +
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_fill_manual(name = NULL, breaks = nut_breaks, values = nut_color) +
   #
   theme_classic() +
   theme(legend.position = c(0.23, 0.9),
         legend.background = element_rect(fill = NA),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text = element_text(color = "black"))

# ggsave(file = "DEA-time-series.png")


# 1 panel raw data points and smoothed means

windows(height=4, width=6); ggplot(fdat %>% 
                                      filter(!(is.na(DEA))) %>%
                                      # convert DEA rate from umol to nmol/g/h
                                      mutate(DEA = DEA * 1000) %>%
                                      left_join(pond_data),
                                   aes(x = doy, y = DEA)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(xintercept = c(176, 211), linetype=1, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(xintercept = 223, linetype=2, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect',
            xmin = 186, xmax = 190,
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   # data
   geom_point(aes(color = trt_nutrients), size=2, shape=19, alpha = 0.4) +
   # mean
   geom_smooth(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(DEA)) %>% ungroup(),
             aes(y = mean, color = trt_nutrients), 
             size=1.5, linetype=1, span=0.4, se=FALSE) +
   #
   scale_x_continuous(name = "Day of year") +
   scale_y_continuous(name = expression(DEA~(nmol~g^-1~h^-1))) +
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_fill_manual(name = NULL, breaks = nut_breaks, values = nut_color) +
   #
   theme_classic() +
   theme(legend.position = c(0.86, 0.88),
         legend.background = element_rect(fill = NA),
         panel.border = element_rect(color = "black", fill = NA),
         axis.text = element_text(color = "black"))

ggsave(file = "DEA-time-series.png")



