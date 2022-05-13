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

## Dissolved CH4 concentration
windows(height=4, width=5.5)
ggplot(fdat %>%
          filter(!(is.na(ch4_lake))) %>%
          left_join(pond_data), 
       aes(x = doy, y = ch4_lake)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CH[4]~(mu*M)),
                      limits = c(0, 86), breaks = seq(0, 80, 20)) +
   #
   ggtitle(expression(Dissolved~CH[4]~concentration)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.83, 0.88),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))

# ggsave(filename="ch4-concentration.png", height=4, width=5.5, units='in')


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



#---
#### Nitrous Oxide ####
#---

## Dissolved N2O concentration
windows(height=4, width=5.5)
ggplot(fdat %>%
          filter(!(is.na(n2o_lake))) %>%
          mutate(n2o_lake = n2o_lake * 1000,
                 n2o_lake = if_else(n2o_lake < 0, 0, n2o_lake)) %>%
          left_join(pond_data), 
       aes(x = doy, y = n2o_lake)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(N[2]*O~(nM)),
                      limits = c(0, 12.5), breaks = seq(0, 12.5, 2.5)) +
   #
   ggtitle(expression(Dissolved~N[2]*O~concentration)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.51, 0.88),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))

# ggsave(filename="n2o-concentration.png", height=4, width=5.5, units='in')


## Diffusive flux
windows(height=4, width=5.5)
ggplot(fdat %>%
          filter(!(is.na(n2o_flux))) %>%
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
   ggtitle(expression(Diffusive~N[2]*O~flux)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.51, 0.88),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         # axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="diffusive-n2o-flux.png", height=4, width=5.5, units='in')



#---
#### Carbon Dioxide ####
#---

## Dissolved CO2 concentration
windows(height=4, width=5)
ggplot(pdat, aes(x = doy, y = co2_lake)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = " ", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(Dissolved~CO[2]~(mu*M))) +
   #
   # ggtitle(expression(CO[2])) +
   theme_classic() +
   theme(legend.position = c(0.18, 0.9),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))



