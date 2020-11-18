#~~~
# Figures from laboratory DEA assays
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(slider)

source("Figure-Scripts/figs_functions.R")


#---
#### Nitrous Oxide ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
n.high =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_fish=="high") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)),
                      limits = c(0, 1.2)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
n.int =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_fish=="medium") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)),
                      limits = c(0, 1.2)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
n.low =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_fish=="low") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)),
                      limits = c(0, 1.2)) +
   labs(title = "Low (B, F)")


# 3-panel
windows(height=10, width=6)

n.high / n.int / n.low

# ggsave(filename = "Figures/DEA-n2o_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/DEA-n2o_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
n.ref =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_nutrients=="no") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)), limits = c(0, 1.2)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
n.pul =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_nutrients=="yes") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)), limits = c(0, 1.2)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

n.ref / n.pul

# ggsave(filename = "Figures/DEA-n2o_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/DEA-n2o_np-trt.png", height=7, width=6, units="in")

}


#---
#### Carbon Dioxide ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
c.high =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="high") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = co2_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(CO[2]~production~(nmol~g^-1~h^-1)),
                      limits = c(0, 550),
                      breaks = seq(0, 500, 100)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
c.int =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="medium") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = co2_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(CO[2]~production~(nmol~g^-1~h^-1)),
                      limits = c(0, 550),
                      breaks = seq(0, 500, 100)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
c.low =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="low") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = co2_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(CO[2]~production~(nmol~g^-1~h^-1)),
                      limits = c(0, 550),
                      breaks = seq(0, 500, 100)) +
   labs(title = "Low (B, F)")


# 3-panel
windows(height=10, width=6)

c.high / c.int / c.low

# ggsave(filename = "Figures/DEA-co2_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/DEA-co2_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
c.ref =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_nutrients=="no") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = co2_rate)) +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(nmol~g^-1~h^-1)), limits = c(0, 550), breaks = seq(0, 500, 100)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
c.pul =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_nutrients=="yes") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = co2_rate)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(nmol~g^-1~h^-1)), limits = c(0, 550), breaks = seq(0, 500, 100)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

c.ref / c.pul

# ggsave(filename = "Figures/DEA-co2_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/DEA-co2_np-trt.png", height=7, width=6, units="in")

}


#---
#### N2O vs Buoyancy Frequency ####
#---

# REFERENCE
# windows()
n.ref =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_nutrients=="no") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue", show.legend=F) +
   #
   # buoy freq
   geom_line(data = test_hobo %>%
                filter(pond_id=="F") %>%
                group_by(doy) %>%
                summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
                ungroup() %>%
                mutate(
                   # regular
                   buoy_freq = buoy_freq * 8,
                   # 7-day lag
                   # buoy_freq = lag(buoy_freq, n=7),
                   # rolling window
                   roll_buoy = slide_dbl(buoy_freq, mean, .before=7)),
             aes(x = doy, y = roll_buoy), color="black", size=1) +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)), limits = c(0, 1.2),
                      sec.axis = sec_axis(~(./8), name = "Buoyancy Frequency \n(8-day rolling window)")) +
   labs(title = "Reference") +
   theme_classic() + 
   theme(axis.title.y.right = element_text(margin = margin(l=1, unit="line"))) +
   
   geom_smooth(method = "loess",
               se = T,
               span = 0.5,
               color = "royalblue")


# PULSED
# windows()
n.pul =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_nutrients=="yes") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen", show.legend=F) +
   #
   # buoy freq
   geom_line(data = test_hobo %>%
                filter(pond_id=="B") %>%
                group_by(doy) %>%
                summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
                ungroup() %>%
                mutate(
                   # regular
                   buoy_freq = buoy_freq * 8,
                   # 7-day lag
                   # buoy_freq = lag(buoy_freq, n=7),
                   # rolling window
                   roll_buoy = slide_dbl(buoy_freq, mean, .before=7)),
             aes(x = doy, y = roll_buoy), color="black", size=1) +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)), limits = c(0, 1.2),
                      sec.axis = sec_axis(~(./8), name = "Buoyancy Frequency \n(8-day rolling window)")) +
   labs(title = "Pulsed") +
   theme_classic() + 
   theme(axis.title.y.right = element_text(margin = margin(l=1, unit="line"))) +
   
   geom_smooth(method = "loess",
               se = T,
               span = 0.5,
               color = "seagreen")


# 2-panel
windows(height=7, width=6)

n.ref / n.pul

# ggsave(filename = "Figures/new-figs/DEA_vs_buoy-freq.png", height=7, width=6, units="in")


#---
#### Mean + CI ####
#---

## Nitrous Oxide
# windows(height=4, width=6)

a =
ggplot(dea_rates %>% left_join(pond_data) %>% mutate(n2o_rate = n2o_rate * 1000) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(n2o_rate),
                    se = sd(n2o_rate)/sqrt(n())) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   # CI
   # geom_smooth(aes(group = trt_nutrients,
   #                 fill = trt_nutrients),
   #             method = "loess", se = T, span = 0.5, alpha = 0.15, show.legend=F) +
   # mean
   # geom_smooth(aes(group = trt_nutrients,
   #                 color = trt_nutrients),
   #             method = "loess", se = F, span = 0.5) +
   # mean+se
   geom_errorbar(aes(x = doy, ymin = mean - se, ymax = mean + se), width=1, color="gray60") +
   geom_line(aes(color = trt_nutrients), size=1.5, alpha=0.7) +
   #
   scale_color_manual(name = NULL,
                      values = nut_color,
                      labels = c("no" = "Reference", "yes" = "Pulsed")) +
   # scale_fill_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(DEA~N[2]*O~(nmol~g^-1~h^-1)), limits = c(0, 1.2)) +
   theme_classic()


## Carbon Dioxide
# windows(height=4, width=6)

b =
ggplot(dea_rates %>% left_join(pond_data) %>% mutate(co2_rate = co2_rate * 1000) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(co2_rate),
                    se = sd(co2_rate)/sqrt(n())) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   # CI
   # geom_smooth(aes(group = trt_nutrients,
   #                 fill = trt_nutrients),
   #             method = "loess", se = T, span = 0.5, alpha = 0.15, show.legend=F) +
   # mean
   # geom_smooth(aes(group = trt_nutrients,
   #                 color = trt_nutrients),
   #             method = "loess", se = F, span = 0.5) +
   # mean+se
   geom_errorbar(aes(x = doy, ymin = mean - se, ymax = mean + se), width=1, color="gray60") +
   geom_line(aes(color = trt_nutrients), size=1.5, alpha=0.7) +
   #
   scale_color_manual(name = NULL,
                      values = nut_color,
                      labels = c("no" = "Reference", "yes" = "Pulsed")) +
   scale_fill_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(DEA~CO[2]~(nmol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   theme_classic()


# Figure
windows(height=7, width=6)

a / b

ggsave(filename = "Figures/new-figs/DEA_all.png", height=7, width=6, units="in")


