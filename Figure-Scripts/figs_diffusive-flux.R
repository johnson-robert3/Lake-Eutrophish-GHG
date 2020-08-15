#~~~
# Script for figures of Diffusive Gas Flux from lake dissolved GHG concentrations
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(PNWColors)


mycolors = c("high" = inferno(n=1, begin=0.25),
             "medium" = inferno(n=1, begin=0.5),
             "low" = magma(n=1, begin=0.8))


#---
#### Methane ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
m.high =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# INTERMEDIATE B-P
# windows()
m.int =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Intermediate (A, D)") +
   theme_classic()


# LOW B-P
# windows()
m.low =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

m.high / m.int / m.low

ggsave(filename = "Figures/flux-ch4_3panel.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
m.ref = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = ch4_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Reference") +
   theme_classic()

# PULSED
# windows()
m.pul = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = ch4_flux)) +
   geom_vline(xintercept = 176, color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

m.ref / m.pul

ggsave(filename = "Figures/flux-ch4_2panel.png", height=7, width=6, units="in")

}



#---
#### Nitrous Oxide ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
n.high =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = n2o_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# INTERMEDIATE B-P
# windows()
n.int =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = n2o_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Intermediate (A, D)") +
   theme_classic()


# LOW B-P
# windows()
n.low =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = n2o_flux)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

n.high / n.int / n.low

ggsave(filename = "Figures/flux-n2o_3panel.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
n.ref = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = n2o_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Reference") +
   theme_classic()

# PULSED
# windows()
n.pul = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = n2o_flux)) +
   geom_vline(xintercept = 176, color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

n.ref / n.pul

ggsave(filename = "Figures/flux-n2o_2panel.png", height=7, width=6, units="in")

}



#---
#### Carbon Dioxide ####
#---

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
# c.high =


# INTERMEDIATE B-P
# windows()
# c.int =


# LOW B-P
# windows()
# c.low =


# 3-panel
windows(height=10, width=6)

c.high / c.int / c.low

ggsave(filename = "Figures/flux-co2_3panel.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
# c.ref = 


# PULSED
# windows()
# c.pul = 


# 2-panel
windows(height=7, width=6)

c.ref / c.pul

ggsave(filename = "Figures/flux-co2_2panel.png", height=7, width=6, units="in")

}



