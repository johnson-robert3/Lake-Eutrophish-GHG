#~~~
# Figures from laboratory DEA assays
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(PNWColors)

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


# Mean + CI by nutrient treatment
windows(height=4, width=6)
ggplot(dea_rates %>% left_join(pond_data) %>% mutate(n2o_rate = n2o_rate * 1000),
       aes(x = doy, y = n2o_rate)) +
   geom_smooth(aes(group = trt_nutrients,
                   color = trt_nutrients),
               method = "loess", se = T, span = 0.5) +
   #
   scale_color_manual(name = "Treatment",
                      breaks = c("no", "yes"),
                      values = c("no" = "royalblue", "yes" = "seagreen"),
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      guide = guide_legend(override.aes = list(fill = NA))) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   theme_classic()


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


# Mean + CI by nutrient treatment
windows(height=4, width=6)
ggplot(dea_rates %>% left_join(pond_data) %>% mutate(co2_rate = co2_rate * 1000),
       aes(x = doy, y = co2_rate)) +
   geom_smooth(aes(group = trt_nutrients,
                   color = trt_nutrients),
               method = "loess", se = T, span = 0.5) +
   #
   scale_color_manual(name = "Treatment",
                      breaks = c("no", "yes"),
                      values = c("no" = "royalblue", "yes" = "seagreen"),
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      guide = guide_legend(override.aes = list(fill = NA))) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(nmol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   theme_classic()

