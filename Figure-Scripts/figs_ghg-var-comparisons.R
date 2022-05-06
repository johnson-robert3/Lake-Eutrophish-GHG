#~~~
# Script to visualize comparisons between dissolved GHG concentrations and driver variables
#
# By: R. Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)
library(slider)

source("Figure-Scripts/figs_functions.R")


#===
#### CO2 - Carbon Dioxide ####
#===

# CO2 over time

windows(height=4, width=5)
ggplot(mdat_co2 %>%
          left_join(pond_data),
       aes(x = doy, y = co2_lake)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
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



#===
#### CH4 - Methane ####
#===

# CH4 over time

windows(height=4, width=5)
ggplot(mdat_ch4 %>%
          left_join(pond_data),
       aes(x = doy, y = ch4_lake)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = " ", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   #
   # ggtitle(expression(CH[4])) +
   theme_classic() +
   theme(legend.position = c(0.8, 0.9),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))



#===
#### N2O - Nitrous Oxide ####
#===

# N2O over time

windows(height=4, width=5)
ggplot(mdat_n2o %>%
          # n2o concentration data already in nM from model dataset
          left_join(pond_data),
       aes(x = doy, y = n2o_lake)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = " ", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   #
   # ggtitle(expression(N[2]*O)) +
   theme_classic() +
   theme(legend.position = c(0.8, 0.9),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))




