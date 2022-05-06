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


# Pond/Site Data
pond_data = read_csv("Data/R-Data/2020_pond-data.csv")

# Data for plotting
pdat = left_join(mdat, pond_data %>% select(pond_id, starts_with("trt")))


#===
#### CO2 - Carbon Dioxide ####
#===

# CO2 over time

windows(height=4, width=5)
ggplot(pdat, aes(x = doy, y = co2_lake)) +
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

## CH4 over time
windows(height=4, width=5)
ggplot(pdat, aes(x = doy, y = ch4_lake)) +
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


## CH4 vs SRP
windows(height=4, width=5)
ggplot(pdat, aes(x = srp, y = ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "SRP") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_srp, y = mean_ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "SRP") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()


## CH4 vs Bottom DO
windows(height=4, width=5)
ggplot(pdat, aes(x = bottom_do, y = ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DO in bottom 20cm") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_bottom_do, y = mean_ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DO in bottom 20cm") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()


## CH4 vs R
windows(height=4, width=5)
ggplot(pdat, aes(x = R, y = ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Respiration") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_R, y = mean_ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Respiration") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()


## CH4 vs Chla
windows(height=4, width=5)
ggplot(pdat, aes(x = chla, y = ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Surface Chla") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_chla, y = mean_ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Surface Chla") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()


## CH4 vs DOC
windows(height=4, width=5)
ggplot(pdat, aes(x = doc_ppm, y = ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOC") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_doc_ppm, y = mean_ch4_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOC") +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M))) +
   # 
   theme_classic()



#===
#### N2O - Nitrous Oxide ####
#===

## N2O over time
windows(height=4, width=5)
ggplot(pdat, aes(x = doy, y = n2o_lake)) +
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


## N2O vs NEP
windows(height=4, width=5)
ggplot(pdat, aes(x = NEP, y = n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "NEP") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(),
       aes(x = mean_NEP, y = mean_n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "NEP") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()


## N2O vs R
windows(height=4, width=5)
ggplot(pdat, aes(x = R, y = n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Respiration") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(),
       aes(x = mean_R, y = mean_n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Respiration") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()


## N2O vs Bottom DO
windows(height=4, width=5)
ggplot(pdat, aes(x = bottom_do, y = n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DO in bottom 20cm") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_bottom_do, y = mean_n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DO in bottom 20cm") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()


## N2O vs TN
windows(height=4, width=5)
ggplot(pdat, aes(x = tn, y = n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "TN") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_tn, y = mean_n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "TN") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()


## N2O vs NOx
windows(height=4, width=5)
ggplot(pdat, aes(x = nox, y = n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "NOx") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_nox, y = mean_n2o_lake)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "NOx") +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(nM))) +
   # 
   theme_classic()


