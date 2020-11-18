#~~~
# Exploratory figs to view limno variables
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(slider)

source("Figure-Scripts/figs_functions.R")


#===
# Sonde Profile Data
#===

# daily surface limno values from sonde profiles
ldat = left_join(sonde_surface, pond_data)


# rolling window data (3-day)
rdat = sonde_surface %>%
   mutate(across(temp:salinity, ~slide_dbl(., mean, .before=2, .complete=T))) %>%
   left_join(pond_data)


#--
# Chlorophyll
#--

# 3 panel
{
# HIGH B-P
# windows()
l.high =
ggplot(rdat %>% filter(trt_fish=="high"), aes(x = doy, y = chla)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="high"), aes(x = doy, y = chla_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Chlorophyll~alpha~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# INTERMEDIATE B-P
# windows()
l.int =
ggplot(rdat %>% filter(trt_fish=="medium"), aes(x = doy, y = chla)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="medium"), aes(x = doy, y = chla_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Chlorophyll~alpha~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Intermediate (A, D)") +
   theme_classic()


# LOW B-P
# windows()
l.low =
ggplot(rdat %>% filter(trt_fish=="low"), aes(x = doy, y = chla)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="low"), aes(x = doy, y = chla_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Chlorophyll~alpha~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

l.high / l.int / l.low

}


# 2 panel
{
# REFERENCE
# windows()
a =
ggplot(rdat %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = chla)) +
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
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), limits = c(0, 35)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
b =
ggplot(rdat %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = chla)) +
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
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), limits = c(0, 35)) +
   labs(title = "Pulsed") +
   theme_classic()


# figure
windows(height=7, width=6); a / b

}


#--
# Phycocyanin
#--

# 3 panel
{
# HIGH B-P
# windows()
l.high =
ggplot(rdat %>% filter(trt_fish=="high"), aes(x = doy, y = phyco)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="high"), aes(x = doy, y = phyco_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Phycocyanin~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# INTERMEDIATE B-P
# windows()
l.int =
ggplot(rdat %>% filter(trt_fish=="medium"), aes(x = doy, y = phyco)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="medium"), aes(x = doy, y = phyco_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Phycocyanin~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Intermediate (A, D)") +
   theme_classic()


# LOW B-P
# windows()
l.low =
ggplot(rdat %>% filter(trt_fish=="low"), aes(x = doy, y = phyco)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="low"), aes(x = doy, y = phyco_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Phycocyanin~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

l.high / l.int / l.low

}


# 2 panel
{
# REFERENCE
# windows()
a =
ggplot(rdat %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = phyco)) +
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
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), limits = c(-0.5, 1)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
b =
ggplot(rdat %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = phyco)) +
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
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), limits = c(-0.5, 1)) +
   labs(title = "Pulsed") +
   theme_classic()


# figure
windows(height=7, width=6); a / b

}


#===
# Field Samples
#===


#--
# Nitrogen
#--

## TN (mg/L)
windows()
ggplot(limno_field_data %>% filter(!(is.na(tn))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(tn = mean(tn, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = tn)) +
   geom_line(aes(color = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   lims(y = c(0,1)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()

# Reference
a = 
ggplot(limno_field_data %>% filter(!(is.na(tn))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = tn)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(TN~(mg~L^-1)), limits = c(0, 1)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Reference") +
   theme_classic()

# Pulsed
b = 
ggplot(limno_field_data %>% filter(!(is.na(tn))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = tn)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(TN~(mg~L^-1)), limits = c(0, 1)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Pulsed") +
   theme_classic()


# TN fig
windows(height=7, width=6); a / b

ggsave(filename = "Figures/new-figs/TN.png", height=7, width=6, units="in")


## NOx (mg/L)
windows()
ggplot(limno_field_data %>% filter(!(is.na(nox))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(nox = mean(nox, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = nox)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   lims(y = c(0, 0.5)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()

# Reference
a = 
ggplot(limno_field_data %>% filter(!(is.na(nox))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = nox)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(NO[x]~(mg~L^-1)), limits = c(0, 0.5)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Reference") +
   theme_classic()

# Pulsed
b = 
ggplot(limno_field_data %>% filter(!(is.na(nox))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = nox)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(NO[x]~(mg~L^-1)), limits = c(0, 0.5)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Pulsed") +
   theme_classic()


# NOx fig
windows(height=7, width=6); a / b

ggsave(filename = "Figures/new-figs/NOx.png", height=7, width=6, units="in")


## NHx (mg/L)
windows()
ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(nhx = mean(nhx, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = nhx)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   lims(y = c(0, 0.1)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()

# Reference
a = 
ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = nhx)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(NH[x]~(mg~L^-1)), limits = c(0, 0.075)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Reference") +
   theme_classic()

# Pulsed
b = 
ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = nhx)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(NH[x]~(mg~L^-1)), limits = c(0, 0.075)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Pulsed") +
   theme_classic()


# NHx fig
windows(height=7, width=6); a / b

ggsave(filename = "Figures/new-figs/NHx.png", height=7, width=6, units="in")


#--
# Phosphorus
#--

## TP (ug/L)
windows()
ggplot(limno_field_data %>% filter(!(is.na(tp))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(tp, na.rm=T),
                    se = sd(tp, na.rm=T)/sqrt(n())) %>%
          ungroup() %>%
          mutate(doy = if_else(trt_nutrients=="yes", doy+1, doy)),
       aes(x = doy, y = mean)) +
   geom_errorbar(aes(x = doy, ymin = mean - se, ymax = mean + se), width = 0.75) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()

# Reference
a = 
ggplot(limno_field_data %>% filter(!(is.na(tp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = tp)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(TP~(mu*g~L^-1)), limits = c(0, 250)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Reference") +
   theme_classic()

# Pulsed
b = 
ggplot(limno_field_data %>% filter(!(is.na(tp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = tp)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(TP~(mu*g~L^-1)), limits = c(0, 250)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Pulsed") +
   theme_classic()


# TP fig
windows(height=7, width=6); a / b

ggsave(filename = "Figures/new-figs/TP.png", height=7, width=6, units="in")


## SRP (ug/L)
windows()
ggplot(limno_field_data %>% filter(!(is.na(srp))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(srp = mean(srp, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = srp)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()

# Reference
a = 
ggplot(limno_field_data %>% filter(!(is.na(srp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = srp)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(SRP~(mu*g~L^-1)), limits = c(0, 30)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Reference") +
   theme_classic()

# Pulsed
b = 
ggplot(limno_field_data %>% filter(!(is.na(srp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = srp)) +
   #
   geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = fish_breaks,
                      values = fish_alpha,
                      labels = fish_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 242)) +
   scale_y_continuous(name = expression(SRP~(mu*g~L^-1)), limits = c(0, 30)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   ggtitle("Pulsed") +
   theme_classic()


# SRP fig
windows(height=7, width=6); a / b

ggsave(filename = "Figures/new-figs/SRP.png", height=7, width=6, units="in")


