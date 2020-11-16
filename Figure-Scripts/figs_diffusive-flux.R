#~~~
# Script for figures of Diffusive Gas Flux from lake dissolved GHG concentrations
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)

source("Figure-Scripts/figs_functions.R")


#---
#### Methane ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
m.high =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="high"),
       aes(x = doy, y = ch4_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(limits = c(0, 60)) +
   labs(title = "High (C, E)",
        y = expression(CH[4]~flux~(mmol~m^-2~d^-1)))


# INTERMEDIATE B-P
# windows()
m.int =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = ch4_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(limits = c(0, 60)) +
   labs(title = "Intermediate (A, D)",
        y = expression(CH[4]~flux~(mmol~m^-2~d^-1)))


# LOW B-P
# windows()
m.low =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="low"),
       aes(x = doy, y = ch4_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(limits = c(0, 60)) +
   labs(title = "Low (B, F)",
        y = expression(CH[4]~flux~(mmol~m^-2~d^-1)))


# 3-panel
windows(height=10, width=6)

m.high / m.int / m.low

# ggsave(filename = "Figures/flux-ch4_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/flux-ch4_fw-trt.png", height=10, width=6, units="in")

}


## 3-panel: colors by pulse
{# test multi-panel figure with different colors for each step of the nutrient addition

# test figure data
tdat = lake_flux %>%
   left_join(pond_data) %>%
   mutate(pulse = case_when(.$trt_nutrients=="no" ~ "no",
                            .$trt_nutrients=="yes" & .$doy<=176 ~ "no",
                            .$trt_nutrients=="yes" & .$doy %in% c(177:211) ~ "p1",
                            .$trt_nutrients=="yes" & .$doy>=212 ~ "p2"))


# HIGH B-P
# windows()
m.high =
ggplot(tdat %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(shape = trt_nutrients, size = trt_nutrients), 
              color="white", fill="white") +
   geom_point(aes(shape = trt_nutrients, size = trt_nutrients, fill = pulse), 
              alpha=0.7) +
   # scales
   scale_shape_manual(name = NULL,
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      values = c("no" = 22, "yes" = 21)) +
   scale_size_manual(name = NULL,
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      values = c("no" = 3, "yes" = 4)) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "None", "p1" = "Pulse 1", "p2" = "Pulse 2"),
                     values = c("no" = "cornflowerblue", "p1" = "seagreen3", "p2" = "seagreen4"),
                     guide = guide_legend(override.aes = list(size=3, 
                                                              shape=21, 
                                                              fill=c("cornflowerblue", "seagreen3", "seagreen4")))) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# INTERMEDIATE B-P
# windows()
m.int =
ggplot(tdat %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(shape = trt_nutrients, size = trt_nutrients), 
              color="white", fill="white") +
   geom_point(aes(shape = trt_nutrients, size = trt_nutrients, fill = pulse), 
              alpha=0.7) +
   # scales
   scale_shape_manual(name = NULL,
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      values = c("no" = 22, "yes" = 21)) +
   scale_size_manual(name = NULL,
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      values = c("no" = 3, "yes" = 4)) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "None", "p1" = "Pulse 1", "p2" = "Pulse 2"),
                     values = c("no" = "cornflowerblue", "p1" = "seagreen3", "p2" = "seagreen4"),
                     guide = guide_legend(override.aes = list(size=3, 
                                                              shape=21, 
                                                              fill=c("cornflowerblue", "seagreen3", "seagreen4")))) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Intermediate (A, D)") +
   theme_classic()


# LOW B-P
# windows()
m.low =
ggplot(tdat %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(shape = trt_nutrients, size = trt_nutrients), 
              color="white", fill="white") +
   geom_point(aes(shape = trt_nutrients, size = trt_nutrients, fill = pulse), 
              alpha=0.7) +
   # scales
   scale_shape_manual(name = NULL,
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      values = c("no" = 22, "yes" = 21)) +
   scale_size_manual(name = NULL,
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      values = c("no" = 3, "yes" = 4)) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "None", "p1" = "Pulse 1", "p2" = "Pulse 2"),
                     values = c("no" = "cornflowerblue", "p1" = "seagreen3", "p2" = "seagreen4"),
                     guide = guide_legend(override.aes = list(size=3, 
                                                              shape=21, 
                                                              fill=c("cornflowerblue", "seagreen3", "seagreen4")))) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

m.high / m.int / m.low

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
m.ref = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = ch4_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
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
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), limits = c(0, 60)) +
   labs(title = "Reference") +
   theme_classic()

# PULSED
# windows()
m.pul = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = ch4_flux)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
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
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), limits = c(0, 60)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

m.ref / m.pul

# ggsave(filename = "Figures/flux-ch4_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/flux-ch4_np-trt.png", height=7, width=6, units="in")

}


## 6-panel: by pond ----
{# viewing all ponds

windows(width=12, height=7)
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))),
       aes(x = doy, y = ch4_flux)) +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   geom_hline(yintercept = 0, linetype=3, color="gray60") +
   # data
   geom_line() +
   geom_point(shape=19, size=3, color="white") +
   geom_point(aes(fill = trt_nutrients, alpha = trt_fish), shape=21, size=3, show.legend=F) +
   # scales
   scale_fill_manual(values = c("yes" = "seagreen3", "no" = "cornflowerblue")) +
   scale_alpha_manual(values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   #
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()

}


## Mean + CI by nutrient treatment
windows(height=4, width=6)

b=
ggplot(lake_flux %>% left_join(pond_data),
       aes(x = doy, y = ch4_flux)) +
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
                      breaks = c("no", "yes"),
                      values = c("no" = "royalblue", "yes" = "seagreen"),
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      guide = guide_legend(override.aes = list(fill = NA))) +
   scale_fill_manual(breaks = c("no", "yes"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   theme_classic()


#---
#### Nitrous Oxide ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
n.high =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="high") %>%
          mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   lims(y = c(-4, 4)) +
   labs(title = "High (C, E)",
        y = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)))


# INTERMEDIATE B-P
# windows()
n.int =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="medium") %>%
          mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   lims(y = c(-4, 4)) +
   labs(title = "Intermediate (A, D)",
        y = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)))


# LOW B-P
# windows()
n.low =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="low") %>%
          mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   lims(y = c(-4, 4)) +
   labs(title = "Low (B, F)",
        y = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)))


# 3-panel
windows(height=10, width=6)

n.high / n.int / n.low

# ggsave(filename = "Figures/flux-n2o_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/flux-n2o_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
n.ref = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="no") %>%
          mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
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
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), limits = c(-4, 4)) +
   labs(title = "Reference") +
   theme_classic()

# PULSED
# windows()
n.pul = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="yes") %>%
          mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
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
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), limits = c(-4, 4)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

n.ref / n.pul

# ggsave(filename = "Figures/flux-n2o_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/flux-n2o_np-trt.png", height=7, width=6, units="in")

}


## 6-panel: by pond ----
{# viewing all ponds

windows(width=12, height=7)
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))),
       aes(x = doy, y = n2o_flux)) +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   geom_hline(yintercept = 0, linetype=3, color="gray60") +
   # data
   geom_line() +
   geom_point(shape=19, size=3, color="white") +
   geom_point(aes(fill = trt_nutrients, alpha = trt_fish), shape=21, size=3, show.legend=F) +
   # scales
   scale_fill_manual(values = c("yes" = "seagreen3", "no" = "cornflowerblue")) +
   scale_alpha_manual(values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   #
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()

}


## Mean + CI by nutrient treatment
windows(height=4, width=6)

c=
ggplot(lake_flux %>% left_join(pond_data)%>% mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) +
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
                      breaks = c("no", "yes"),
                      values = c("no" = "royalblue", "yes" = "seagreen"),
                      labels = c("no" = "Reference", "yes" = "Pulsed"),
                      guide = guide_legend(override.aes = list(fill = NA))) +
   scale_fill_manual(breaks = c("no", "yes"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   theme_classic()


#---
#### Carbon Dioxide ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
c.high =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="high"),
       aes(x = doy, y = co2_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   lims(y = c(-25, 250)) +
   labs(title = "High (C, E)",
        y = expression(CO[2]~flux~(mmol~m^-2~d^-1)))


# INTERMEDIATE B-P
# windows()
c.int =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = co2_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   lims(y = c(-25, 250)) +
   labs(title = "Intermediate (A, D)",
        y = expression(CO[2]~flux~(mmol~m^-2~d^-1)))


# LOW B-P
# windows()
c.low =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_fish=="low"),
       aes(x = doy, y = co2_flux)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   lims(y = c(-25, 250)) +
   labs(title = "Low (B, F)",
        y = expression(CO[2]~flux~(mmol~m^-2~d^-1)))


# 3-panel
windows(height=10, width=6)

c.high / c.int / c.low

# ggsave(filename = "Figures/flux-co2_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/flux-co2_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
c.ref =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = co2_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
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
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), limits = c(-25, 250)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
c.pul =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = co2_flux)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
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
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), limits = c(-25, 250)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

c.ref / c.pul

# ggsave(filename = "Figures/flux-co2_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/flux-co2_np-trt.png", height=7, width=6, units="in")

}


## 6-panel: by pond ----
{# viewing all ponds

windows(width=12, height=7)
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(co2_ppm))),
       aes(x = doy, y = co2_flux)) +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   geom_hline(yintercept = 0, linetype=3, color="gray60") +
   # data
   geom_line() +
   geom_point(shape=19, size=3, color="white") +
   geom_point(aes(fill = trt_nutrients, alpha = trt_fish), shape=21, size=3, show.legend=F) +
   # scales
   scale_fill_manual(values = c("yes" = "seagreen3", "no" = "cornflowerblue")) +
   scale_alpha_manual(values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)) +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   #
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()

}


## Mean + CI by nutrient treatment
windows(height=4, width=6)

a=
ggplot(lake_flux %>% left_join(pond_data),
       aes(x = doy, y = co2_flux)) +
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
                      breaks = c("no", "yes"),
                      values = c("no" = "royalblue", "yes" = "seagreen"),
                      labels = c("no" = "Reference", "yes" = "Pulsed")) +
   scale_fill_manual(breaks = c("no", "yes"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), expand = expansion(mult=0.1), breaks = seq(-25, 100, 25)) +
   theme_classic()



#---
# All gases, mean + CI
#---

windows(height=10, width=6)

a / b / c

ggsave(filename = "Figures/new-figs/diffusive-flux_all.png", height=10, width=6, units="in")


