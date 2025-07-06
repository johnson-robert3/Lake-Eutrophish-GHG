#~~~
# Script for figures from Methanogenesis Potential laboratory assays
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(slider)

source("Figure-Scripts/figs_functions.R")


#---
#### Methane ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
m.high =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = ch4_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)),
                      limits = c(0, 0.016)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
m.int =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = ch4_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)),
                      limits = c(0, 0.016)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
m.low =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = ch4_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)),
                      limits = c(0, 0.016)) +
   labs(title = "Low (B, F)")


# 3-panel
windows(height=10, width=6)

m.high / m.int / m.low

# ggsave(filename = "Figures/methanogenesis-ch4_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/methanogenesis-ch4_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
m.ref =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = ch4_rate)) +
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
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), limits = c(0, 0.016)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
m.pul =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = ch4_rate)) +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), limits = c(0, 0.016)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

m.ref / m.pul

# ggsave(filename = "Figures/methanogenesis-ch4_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/methanogenesis-ch4_np-trt.png", height=7, width=6, units="in")

}


## Mean + CI by nutrient treatment
windows(height=4, width=6)

a = 
ggplot(methano_rates %>% left_join(pond_data),
       aes(x = doy, y = ch4_rate)) +
   # CI
   geom_smooth(aes(group = trt_nutrients,
                   fill = trt_nutrients),
               method = "loess", se = T, span = 0.5, alpha = 0.15, show.legend=F) +
   # mean
   geom_smooth(aes(group = trt_nutrients,
                   color = trt_nutrients),
               method = "loess", se = F, span = 0.5) +
   #
   scale_color_manual(name = NULL,
                      values = c("no" = "royalblue", "yes" = "seagreen"),
                      labels = c("no" = "Reference", "yes" = "Pulsed")) +
   scale_fill_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(methanogenesis~CH[4]~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.05)) +
   theme_classic()


#---
#### Carbon Dioxide ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
c.high =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = co2_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)),
                      limits = c(0, 0.1)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
c.int =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = co2_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)),
                      limits = c(0, 0.1)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
c.low =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = co2_rate)) %>%
   fig_aes_fw() +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)),
                      limits = c(0, 0.1)) +
   labs(title = "Low (B, F)")


# 3-panel
windows(height=10, width=6)

c.high / c.int / c.low

# ggsave(filename = "Figures/methanogenesis-co2_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/methanogenesis-co2_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
c.ref =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
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
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)), limits = c(0, 0.1)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
c.pul =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = co2_rate)) +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)), limits = c(0, 0.1)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

c.ref / c.pul

# ggsave(filename = "Figures/methanogenesis-co2_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/methanogenesis-co2_np-trt.png", height=7, width=6, units="in")

}


## Mean + CI by nutrient treatment
windows(height=4, width=6)

b = 
ggplot(methano_rates %>% left_join(pond_data),
       aes(x = doy, y = co2_rate)) +
   # CI
   geom_smooth(aes(group = trt_nutrients,
                   fill = trt_nutrients),
               method = "loess", se = T, span = 0.5, alpha = 0.15, show.legend=F) +
   # mean
   geom_smooth(aes(group = trt_nutrients,
                   color = trt_nutrients),
               method = "loess", se = F, span = 0.5) +
   #
   scale_color_manual(name = NULL,
                      values = c("no" = "royalblue", "yes" = "seagreen"),
                      labels = c("no" = "Reference", "yes" = "Pulsed")) +
   scale_fill_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(methanogenesis~CO[2]~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.05)) +
   theme_classic()


#---
#### CH4 vs Buoyancy Frequency ####
#---

# REFERENCE
# windows()
m.ref =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = ch4_rate)) +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue", show.legend=F) +
   # buoy freq
   geom_line(data = test_hobo %>%
                filter(pond_id=="F") %>%
                group_by(doy) %>%
                summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
                ungroup() %>%
                mutate(
                   # regular
                   buoy_freq = buoy_freq / 10,
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
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), limits = c(0, 0.016), 
                      sec.axis = sec_axis(~(.*10), name = "Buoyancy Frequency \n(8-day rolling window)")) +
   labs(title = "Reference") +
   theme_classic() + 
   theme(axis.title.y.right = element_text(margin = margin(l=1, unit="line"))) +
   
   geom_smooth(method = "loess",
               se = T,
               span = 0.5,
               color = "royalblue")


# PULSED
# windows()
m.pul =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = ch4_rate)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen", show.legend=F) +
   # buoy freq
   geom_line(data = test_hobo %>%
                filter(pond_id=="B") %>%
                group_by(doy) %>%
                summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
                ungroup() %>%
                mutate(
                   # regular
                   buoy_freq = buoy_freq / 10,
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
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), limits = c(0, 0.016),
                      sec.axis = sec_axis(~(.*10), name = "Buoyancy Frequency \n(8-day rolling window)")) +
   labs(title = "Pulsed") +
   theme_classic() + 
   theme(axis.title.y.right = element_text(margin = margin(l=1, unit="line"))) +
   
   geom_smooth(method = "loess",
               se = T,
               span = 0.5,
               color = "seagreen")


# 2-panel
windows(height=7, width=6)

m.ref / m.pul

# ggsave(filename = "Figures/new-figs/methanogenesis_vs_buoy-freq.png", height=7, width=6, units="in")


#---
#### Mean + CI ####
#---

## Methane
# windows(height=4, width=6)

a =
ggplot(methano_rates %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(ch4_rate),
                    se = sd(ch4_rate)/sqrt(n())) %>%
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
   scale_y_continuous(name = expression(methanogenesis~CH[4]~(mu*mol~g^-1~h^-1)), limits = c(0, 0.0127)) +
   ggtitle("Methane") +
   theme_classic()


## Carbon Dioxide
# windows(height=4, width=6)

b =
ggplot(methano_rates %>% left_join(pond_data) %>%
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
   # scale_fill_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(methanogenesis~CO[2]~(mu*mol~g^-1~h^-1)), limits = c(0, 0.09)) +
   ggtitle("Carbon Dioxide") +
   theme_classic()


# Figure
windows(height=7, width=6)

a / b

ggsave(filename = "Figures/new-figs/methanogenesis_all.png", height=7, width=6, units="in")


