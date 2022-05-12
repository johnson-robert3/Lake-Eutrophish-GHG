#~~~
# Script to visualize comparisons between dissolved GHG concentrations and driver variables
#
# By: R. Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)
library(slider)
library(lubridate)

source("Figure-Scripts/figs_functions.R")


# Pond/Site Data
pond_data = read_csv("Data/R-Data/2020_pond-data.csv")

# Data for plotting
#  Need to run "stats_model-data.R" script first
pdat = left_join(mdat, pond_data %>% select(pond_id, starts_with("trt")))

# correct dates for fdat
fdat = fdat %>% mutate(date = ymd(date))


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


## Dissolved CH4 concentration over time
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
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
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


## LME Effect Sizes
windows(height=4, width=6)
ggplot(mtab_ch4 %>%
          rename(estimate = Value,
                 ci_lower = lower,
                 ci_upper = upper,
                 p = "p-value") %>%
          mutate(fixed.effect = as_factor(fixed.effect)) %>%
          filter(!(fixed.effect=="(Intercept)")),
       # fixed effects in order of model
       aes(x = estimate, y = fct_rev(fixed.effect))) +
       # fixed effects in order of effect size
       # aes(x = estimate, y = fct_reorder(fixed.effect, estimate, .fun=abs, .desc=F))) +
   #
   geom_vline(xintercept = 0, linetype=1, color="gray60") +
   # placeholder points to build the plot correctly
   geom_point(size=1) +
   #
   # data points for not significant
   geom_point(data = ~filter(.x, p > 0.05),
               size=3, color="gray60") +
   geom_errorbarh(data = ~filter(.x, p > 0.05), 
                  aes(xmin = ci_lower, xmax = ci_upper, height=0), size=0.6, color="gray60") +
   # data points for positive effect size
   geom_point(data = ~filter(.x, p <= 0.05 & estimate > 0),
               size=4, color="#28abb9") +
   geom_errorbarh(data = ~filter(.x, p <= 0.05 & estimate > 0),
                  aes(xmin = ci_lower, xmax = ci_upper, height=0), size=0.75, color="#28abb9") +
   # data points for negative effect size
   geom_point(data = ~filter(.x, p <= 0.05 & estimate < 0),
               size=4, color="#2d6187") +
   geom_errorbarh(data = ~filter(.x, p <= 0.05 & estimate < 0),
                  aes(xmin = ci_lower, xmax = ci_upper, height=0), size=0.75, color="#2d6187") +
   #
   scale_x_continuous(name = "Effect size", limits = c(-5, 25), breaks = seq(-5, 25, 5)) +
   # factors in order of model
   scale_y_discrete(name=NULL, 
                    labels = c("DOC × Treatment\n(Pulse)",
                               "SRP × Treatment\n(Pulse)",
                               "Bottom water DO", 
                               "Respiration", 
                               "Chlorophyll a",
                               "DOC", 
                               "SRP", 
                               "Treatment\n(Pulse)")) +
   # factors ordered by decreasing effect size
   # scale_y_discrete(name = NULL,
   #                  labels = c()) +
   ggtitle(expression(Effect~on~dissolved~CH[4])) +
   theme_classic() +
   theme(axis.title.x = element_text(margin = margin(t=0.5, unit="line")))

# ggsave(filename="ch4_effect-size.png", height=4, width=6, units='in')



#===
#### N2O - Nitrous Oxide ####
#===

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


## Dissolved N2O concentration over time
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
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
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


## LME Effect Sizes
windows(height=4, width=6)
ggplot(mtab_n2o %>%
          rename(estimate = Value,
                 ci_lower = lower,
                 ci_upper = upper,
                 p = "p-value") %>%
          mutate(fixed.effect = as_factor(fixed.effect)) %>%
          filter(!(fixed.effect=="(Intercept)")),
       # fixed effects in order of model
       aes(x = estimate, y = fct_rev(fixed.effect))) +
       # fixed effects in order of effect size
       # aes(x = estimate, y = fct_reorder(fixed.effect, estimate, .fun=abs, .desc=F))) +
   #
   geom_vline(xintercept = 0, linetype=1, color="gray60") +
   # placeholder points to build the plot correctly
   geom_point(size=1) +
   #
   # data points for not significant
   geom_point(data = ~filter(.x, p > 0.05),
               size=3, color="gray60") +
   geom_errorbarh(data = ~filter(.x, p > 0.05), 
                  aes(xmin = ci_lower, xmax = ci_upper, height=0), size=0.6, color="gray60") +
   # data points for positive effect size
   geom_point(data = ~filter(.x, p <= 0.05 & estimate > 0),
               size=4, color="#28abb9") +
   geom_errorbarh(data = ~filter(.x, p <= 0.05 & estimate > 0),
                  aes(xmin = ci_lower, xmax = ci_upper, height=0), size=0.75, color="#28abb9") +
   # data points for negative effect size
   geom_point(data = ~filter(.x, p <= 0.05 & estimate < 0),
               size=4, color="#2d6187") +
   geom_errorbarh(data = ~filter(.x, p <= 0.05 & estimate < 0),
                  aes(xmin = ci_lower, xmax = ci_upper, height=0), size=0.75, color="#2d6187") +
   #
   scale_x_continuous(name = "Effect size", limits = c(-3, 2), breaks = seq(-3, 2, 1)) +
   # scale_x_continuous(name = "Effect size", limits = c(-1.5,1.5), breaks = seq(-1.5, 1.5, 0.5)) +
   # factors in order of model
   scale_y_discrete(name=NULL,
                    labels = c("Resp. × Treatment\n(Pulse)",
                               "NEP × Treatment\n(Pulse)",
                               "Respiration",
                               "Net Ecosystem Production",
                               "Bottom water DO",
                               "SRP", 
                               "Nitrate",
                               "Total nitrogen",
                               "Time (day of year)",
                               "Treatment\n(Pulse)")) +
   # factors ordered by decreasing effect size
   # scale_y_discrete(name = NULL,
   #                  labels = c()) +
   ggtitle(expression(Effect~on~dissolved~N[2]*O)) +
   theme_classic() +
   theme(axis.title.x = element_text(margin = margin(t=0.5, unit="line")))

# ggsave(filename="n2o_effect-size.png", height=4, width=6, units='in')


