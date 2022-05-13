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



#===
#### CH4 - Methane ####
#===

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
ggplot(pdat, aes(x = log(chla+1), y = log(ch4_flux+1))) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   geom_smooth(aes(group = trt_nutrients, color=trt_nutrients), method = "lm") +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   # scale_x_continuous(name = "Surface Chla (ln-transformed)") +
   # scale_y_continuous(name = expression(CH[4]~flux~(ln-transformed))) +
   # 
   theme_classic() +
   theme(legend.position = "none")

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_chla, y = mean_ch4_flux)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Surface Chla") +
   scale_y_continuous(name = expression(CH[4]~flux)) +
   # 
   theme_classic() +
   theme(legend.position = "none")


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
windows(height=4, width=6.5)
ggplot(mtab_ch4 %>%
          rename(estimate = Value,
                 ci_lower = lower,
                 ci_upper = upper,
                 p = "p-value") %>%
          arrange(estimate) %>%  # to put factors in order of effect size (from neg. to pos.)
          mutate(fixed.effect = as_factor(fixed.effect)) %>%
          filter(!(fixed.effect=="(Intercept)")),
       # fixed effects in order of model
       aes(x = estimate, y = fct_rev(fixed.effect))) +
       # fixed effects in order of effect size
       # aes(x = estimate, y = fct_reorder(fixed.effect, estimate, .fun=abs, .desc=T))) +
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
   scale_x_continuous(name = "Estimate", limits = c(-2, 15), breaks = seq(0, 15, 5)) +
   # factors in order of model
   scale_y_discrete(name=NULL,
                    labels = c("Treatment (Pulse)", 
                               "SRP × Treat. (Pulse)",
                               "SRP",
                               "DOC",
                               "Chlorophyll a",
                               "Respiration",
                               "Bottom water DO",
                               "DOC × Treat. (Pulse)")) +
   # factors ordered by decreasing effect size
   # scale_y_discrete(name = NULL,
   #                  labels = c()) +
   ggtitle(expression(Effect~on~CH[4]~Flux)) +
   theme_classic() +
   theme(axis.text = element_text(color="black", size=rel(1)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="ch4_effect-size-decreasing.png")



#===
#### N2O - Nitrous Oxide ####
#===

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
ggplot(pdat, aes(x = log(bottom_do), y = log(n2o_flux+5))) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=1.5) +
   geom_smooth(aes(group = trt_nutrients, color = trt_nutrients), method="lm") +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   # scale_x_continuous(name = "bottom DO") +
   # scale_y_continuous(name = expression(N[2]*O~flux)) +
   # 
   theme_classic() +
   theme(legend.position = "none")

# treatment means
windows(height=4, width=5)
ggplot(pdat %>% 
          group_by(trt_nutrients, doy) %>%
          summarize(across(ch4_lake:np_ratio, list(mean=mean, sd=sd), .names="{.fn}_{.col}")) %>%
          ungroup(), 
       aes(x = mean_bottom_do, y = mean_n2o_flux)) +
   #
   geom_point(aes(color = trt_nutrients), alpha=0.8, size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "bottom DO") +
   scale_y_continuous(name = expression(N[2]*O~flux)) +
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
windows(height=4, width=6.5)
ggplot(mtab_n2o %>%
          rename(estimate = Value,
                 ci_lower = lower,
                 ci_upper = upper,
                 p = "p-value") %>%
          arrange(estimate) %>%  # to put factors in order of effect size (from neg. to pos.)
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
   # scale_x_continuous(name = "Effect size", limits = c(-3, 2), breaks = seq(-3, 2, 1)) +
   scale_x_continuous(name = "Estimate", limits = c(-1.5, 1), breaks = seq(-1.5, 1, 0.5)) +
   # factors in order of model
   scale_y_discrete(name=NULL,
                    labels = c("Total Nitrogen",
                               "NEP × Treat. (Pulse)",
                               "Bottom Water DO",
                               "Respiration",
                               "Time (day of year)",
                               "Net Ecosystem Production",
                               "SRP",
                               "Resp. × Treat. (Pulse)",
                               "Nitrate",
                               "Treatment (Pulse)")) +
   # factors ordered by decreasing effect size
   # scale_y_discrete(name = NULL,
   #                  labels = c()) +
   ggtitle(expression(Effect~on~N[2]*O~Flux)) +
   theme_classic() +
   theme(axis.text = element_text(color="black", size=rel(1)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="n2o_effect-size-decreasing.png")


