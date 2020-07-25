#~~~
# Script for making GHG figures
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(PNWColors)


mycolors = c("high" = inferno(n=1, begin=0.25),
             "medium" = inferno(n=1, begin=0.5),
             "low" = magma(n=1, begin=0.8))

# from PNWColors palette "Anemone"
# mycolors = c("high" = "#009474", "medium" = "#72e1e1", "low" = "#dcbe9b")


#---
#### Dissolved Methane Concentrations ####
#---

# ponds distinguished by alpha
{
windows(height=5, width=7)
ggplot(lake_conc, aes(x = doy, y = ch4_lake)) +
   # data
   geom_line(aes(alpha = pond_id), size=1.5, color="seagreen3") +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = pond_id), size=4, color="seagreen3") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~(mu*M)),
                      expand = expansion(mult=0.1)) +
   # aesthetics
   theme_classic()
}

# ponds distinguished by treatments
windows(height=5, width=7)
ggplot(lake_conc %>%
          left_join(pond_data), 
       aes(x = doy, y = ch4_lake)) +
   # data lines
   geom_line(aes(group = pond_id, color = trt_fish, alpha = trt_nutrients), 
             size=1.25) +
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # white points to cover lines
   geom_point(aes(shape = trt_nutrients), 
              size=4, color="white", fill="white") +
   # data points
   geom_point(aes(shape = trt_nutrients, fill = trt_fish, alpha = trt_nutrients), 
              size=4) +
   # scales
   scale_color_manual(breaks = c("high", "medium", "low"),
                      values = mycolors,
                      guide=NULL) +
   scale_fill_manual(name = "Food Web",
                     breaks = c("high", "medium", "low"),
                     values = mycolors,
                     guide = guide_legend(override.aes = list(shape=21, alpha=0.7))) +
   scale_shape_manual(name = "Nutrients",
                      breaks = c("yes", "no"),
                      values = c("yes" = 22, "no" = 21),
                      guide = guide_legend(override.aes = list(size=3))) +
   scale_alpha_manual(breaks = c("yes", "no"),
                      values = c("yes" = 0.7, "no" = 0.5),
                      guide=NULL) +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~(mu*M)),
                      expand = expansion(mult=0.1)
                      # set limits, instead of expansion
                      # limits = c(0,20)
                      ) +
   # aesthetics
   theme_classic()


ggsave(filename = "Figures/lake-dissolved-ch4.png", height=5, width=7, units="in")


# log scale (to view dynamics in lower concentrations)
{
windows(height=5, width=7)
ggplot(lake_conc %>%
          left_join(pond_data), 
       aes(x = doy, y = ch4_lake)) +
   # data lines
   geom_line(aes(group = pond_id, color = trt_fish, alpha = trt_nutrients), 
             size=1.25) +
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # white points to cover lines
   geom_point(aes(shape = trt_nutrients), 
              size=4, color="white", fill="white") +
   # data points
   geom_point(aes(shape = trt_nutrients, fill = trt_fish, alpha = trt_nutrients), 
              size=4) +
   # scales
   scale_color_manual(breaks = c("high", "medium", "low"),
                      values = mycolors,
                      guide=NULL) +
   scale_fill_manual(name = "Food Web",
                     breaks = c("high", "medium", "low"),
                     values = mycolors,
                     guide = guide_legend(override.aes = list(shape=21, alpha=0.7))) +
   scale_shape_manual(name = "Nutrients",
                      breaks = c("yes", "no"),
                      values = c("yes" = 22, "no" = 21),
                      guide = guide_legend(override.aes = list(size=3))) +
   scale_alpha_manual(breaks = c("yes", "no"),
                      values = c("yes" = 0.7, "no" = 0.5),
                      guide=NULL) +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~(mu*M)),
                      expand = expansion(mult=0.1),
                      trans = "log10") +
   # aesthetics
   theme_classic()


ggsave(filename = "Figures/lake-dissolved-ch4_logY.png", height=5, width=7, units="in")
}


##_Comparing between nutrient treatments (by fish); 3-panel
{
# HIGH B-P
# windows()
mhigh =
ggplot(lake_conc %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = ch4_lake)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # atm
   geom_line(aes(x = doy, y = ch4_atm), linetype=2, color="gray0") +
   geom_point(aes(x = doy, y = ch4_atm), size=1.5, shape=21, fill="gray70") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# MEDIUM B-P
# windows()
mmed =
ggplot(lake_conc %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = ch4_lake)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # atm
   geom_line(aes(x = doy, y = ch4_atm), linetype=2, color="gray0") +
   geom_point(aes(x = doy, y = ch4_atm), size=1.5, shape=21, fill="gray70") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M)), expand = expansion(mult=0.1)) +
   labs(title = "Medium (A, D)") +
   theme_classic()


# LOW B-P
# windows()
mlow =
ggplot(lake_conc %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = ch4_lake)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # atm
   geom_line(aes(x = doy, y = ch4_atm), linetype=2, color="gray0") +
   geom_point(aes(x = doy, y = ch4_atm), size=1.5, shape=21, fill="gray70") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Dissolved~CH[4]~(mu*M)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

mhigh / mmed / mlow

ggsave(filename = "Figures/lake-ch4_3panel.png", height=10, width=6, units="in")

}


#---
#### Dissolved Nitrous Oxide Concentrations ####
#---

# ponds distinguished by alpha
{
windows(height=5, width=7)
ggplot(lake_conc, aes(x = doy, y = n2o_lake)) +
   # data
   geom_line(aes(alpha = pond_id), size=1.5, color="dodgerblue3") +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = pond_id), size=4, color="dodgerblue3") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Nitrous~oxide~(mu*M)),
                      expand = expansion(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()
}

# ponds distinguished by treatment
windows(height=5, width=7)
ggplot(lake_conc %>%
          left_join(pond_data), 
       aes(x = doy, y = n2o_lake)) +
   # data lines
   geom_line(aes(group = pond_id, color = trt_fish, alpha = trt_nutrients), 
             size=1.25) +
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # white points to cover lines
   geom_point(aes(shape = trt_nutrients), 
              size=4, color="white", fill="white") +
   # data points
   geom_point(aes(shape = trt_nutrients, fill = trt_fish, alpha = trt_nutrients), 
              size=4) +
   # scales
   scale_color_manual(breaks = c("high", "medium", "low"),
                      values = mycolors,
                      guide=NULL) +
   scale_fill_manual(name = "Food Web",
                     breaks = c("high", "medium", "low"),
                     values = mycolors,
                     guide = guide_legend(override.aes = list(shape=21, alpha=0.7))) +
   scale_shape_manual(name = "Nutrients",
                      breaks = c("yes", "no"),
                      values = c("yes" = 22, "no" = 21),
                      guide = guide_legend(override.aes = list(size=3))) +
   scale_alpha_manual(breaks = c("yes", "no"),
                      values = c("yes" = 0.7, "no" = 0.5),
                      guide=NULL) +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Nitrous~oxide~(mu*M)),
                      expand = expansion(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()


ggsave(filename = "Figures/lake-dissolved-n2o.png", height=5, width=7, units="in")


##_Comparing between nutrient treatments (by fish); 3-panel
{
# HIGH B-P
# windows()
nhigh =
ggplot(lake_conc %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = n2o_lake)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # atm
   geom_line(aes(x = doy, y = n2o_atm), linetype=2, color="gray40") +
   geom_point(aes(x = doy, y = n2o_atm), size=1.5, shape=21, fill="gray70") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(mu*M)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# MEDIUM B-P
# windows()
nmed =
ggplot(lake_conc %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = n2o_lake)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # atm
   geom_line(aes(x = doy, y = n2o_atm), linetype=2, color="gray40") +
   geom_point(aes(x = doy, y = n2o_atm), size=1.5, shape=21, fill="gray70") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(mu*M)), expand = expansion(mult=0.1)) +
   labs(title = "Medium (A, D)") +
   theme_classic()


# LOW B-P
# windows()
nlow =
ggplot(lake_conc %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = n2o_lake)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # atm
   geom_line(aes(x = doy, y = n2o_atm), linetype=2, color="gray40") +
   geom_point(aes(x = doy, y = n2o_atm), size=1.5, shape=21, fill="gray70") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Dissolved~N[2]*O~(mu*M)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

nhigh / nmed / nlow

ggsave(filename = "Figures/lake-n2o_3panel.png", height=10, width=6, units="in")

}


#---
#### Dissolved Carbon Dioxide Concentrations ####
#---


#---
#### Diffusive Methane Flux ####
#---

##_Comparing between nutrient treatments (by fish); 3-panel
{
# HIGH B-P
# windows()
mhigh =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# MEDIUM B-P
# windows()
mmed =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Medium (A, D)") +
   theme_classic()


# LOW B-P
# windows()
mlow =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = ch4_flux)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

mhigh / mmed / mlow

ggsave(filename = "Figures/flux-ch4_3panel.png", height=10, width=6, units="in")

}


##_Comparing among B-P fish treatments (by nutrients); 2-panel
{
# AMBIENT
# windows()
m.amb = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = ch4_flux)) +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=3, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Fish",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   theme_classic()

# PULSED
# windows()
m.pul = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(ch4_ppm))) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = ch4_flux)) +
   geom_vline(xintercept=176, color="gray40", linetype=2) +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=3, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Fish",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   theme_classic()


# 2-panel
windows(height=7, width=6)

m.amb / m.pul

ggsave(filename = "Figures/flux-ch4_2panel.png", height=7, width=6, units="in")

}


#---
#### Diffusive Nitrous Oxide Flux ####
#---

##_Comparing between nutrient treatments (by fish); 3-panel
{
# HIGH B-P
# windows()
nhigh =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = n2o_flux)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# MEDIUM B-P
# windows()
nmed =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = n2o_flux)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Medium (A, D)") +
   theme_classic()


# LOW B-P
# windows()
nlow =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = n2o_flux)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

nhigh / nmed / nlow

ggsave(filename = "Figures/flux-n2o_3panel.png", height=10, width=6, units="in")

}


##_Comparing among B-P fish treatments (by nutrients); 2-panel
{
# AMBIENT
# windows()
n.amb = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = n2o_flux)) +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=3, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Fish",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   theme_classic()

# PULSED
# windows()
n.pul = 
ggplot(lake_flux %>% left_join(pond_data) %>% filter(!(is.na(n2o_ppm))) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = n2o_flux)) +
   geom_vline(xintercept=176, color="gray40", linetype=2) +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=3, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Fish",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), expand = expansion(mult=0.1)) +
   theme_classic()


# 2-panel
windows(height=7, width=6)

n.amb / n.pul

ggsave(filename = "Figures/flux-n2o_2panel.png", height=7, width=6, units="in")

}


#---
#### Methanogenesis Potential ####
#---

## METHANE

# ponds distinguished by alpha
{
windows(height=5, width=7)
ggplot(methano_rates %>%
          mutate(doy = case_when(.$pond_id=="A" ~ doy-0.2,
                                 .$pond_id=="B" ~ doy-0.1,
                                 .$pond_id=="C" ~ doy,
                                 .$pond_id=="D" ~ doy+0.1,
                                 .$pond_id=="E" ~ doy+0.2,
                                 .$pond_id=="F" ~ doy+0.3)),
       aes(x = doy, y = ch4_rate)) +
   # data
   geom_line(aes(alpha = pond_id), size=1.5, color="firebrick2") +
   geom_errorbar(aes(x = doy, ymin = ch4_rate - sd_ch4_rate, ymax = ch4_rate + sd_ch4_rate), 
                 color="gray75", width=0) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = pond_id), size=4, color="firebrick2") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~production~(mu*mol~ml^-1*(s+w)~h^-1)),
                      expand = expansion(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()
}

# ponds distinguished by treatment
windows(height=5, width=7)
ggplot(methano_rates %>%
          mutate(doy = case_when(.$pond_id=="A" ~ doy-0.2,
                                 .$pond_id=="B" ~ doy-0.1,
                                 .$pond_id=="C" ~ doy,
                                 .$pond_id=="D" ~ doy+0.1,
                                 .$pond_id=="E" ~ doy+0.2,
                                 .$pond_id=="F" ~ doy+0.3)) %>%
          left_join(pond_data), 
       aes(x = doy, y = ch4_rate)) +
   # data lines
   geom_line(aes(group = pond_id, color = trt_fish, alpha = trt_nutrients), 
             size=1.25) +
   geom_errorbar(aes(ymin = ch4_rate - sd_ch4_rate, ymax = ch4_rate + sd_ch4_rate), 
                 color="gray75", width=0) +
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # white points to cover lines
   geom_point(aes(shape = trt_nutrients), 
              size=4, color="white", fill="white") +
   # data points
   geom_point(aes(shape = trt_nutrients, fill = trt_fish, alpha = trt_nutrients), 
              size=4) +
   # scales
   scale_color_manual(breaks = c("high", "medium", "low"),
                      values = mycolors,
                      guide=NULL) +
   scale_fill_manual(name = "Food Web",
                     breaks = c("high", "medium", "low"),
                     values = mycolors,
                     guide = guide_legend(override.aes = list(shape=21, alpha=0.7))) +
   scale_shape_manual(name = "Nutrients",
                      breaks = c("yes", "no"),
                      values = c("yes" = 22, "no" = 21),
                      guide = guide_legend(override.aes = list(size=3))) +
   scale_alpha_manual(breaks = c("yes", "no"),
                      values = c("yes" = 0.7, "no" = 0.5),
                      guide=NULL) +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~production~(mu*mol~g^-1~h^-1)),
                      expand = expansion(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()

ggsave(filename = "Figures/methano-rates.png", height=5, width=7, units="in")


##_Comparing between nutrient treatments (by fish); 3-panel
{
# HIGH B-P
# windows()
mhigh =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = ch4_rate)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_errorbar(aes(ymin = ch4_rate - sd_ch4_rate, ymax = ch4_rate + sd_ch4_rate),
                 color="gray75", width=0) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~production~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# MEDIUM B-P
# windows()
mmed =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = ch4_rate)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_errorbar(aes(ymin = ch4_rate - sd_ch4_rate, ymax = ch4_rate + sd_ch4_rate),
                 color="gray75", width=0) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~production~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Medium (A, D)") +
   theme_classic()


# LOW B-P
# windows()
mlow =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = ch4_rate)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_errorbar(aes(ymin = ch4_rate - sd_ch4_rate, ymax = ch4_rate + sd_ch4_rate),
                 color="gray75", width=0) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~production~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

mhigh / mmed / mlow

ggsave(filename = "Figures/methanogenesis-ch4_3panel.png", height=10, width=6, units="in")

}


## CARBON DIOXIDE

# ponds distinguished by treatment
windows(height=5, width=7)
ggplot(methano_rates %>%
          mutate(doy = case_when(.$pond_id=="A" ~ doy-0.2,
                                 .$pond_id=="B" ~ doy-0.1,
                                 .$pond_id=="C" ~ doy,
                                 .$pond_id=="D" ~ doy+0.1,
                                 .$pond_id=="E" ~ doy+0.2,
                                 .$pond_id=="F" ~ doy+0.3)) %>%
          left_join(pond_data), 
       aes(x = doy, y = co2_rate)) +
   # data lines
   geom_line(aes(group = pond_id, color = trt_fish, alpha = trt_nutrients), 
             size=1.25) +
   geom_errorbar(aes(ymin = co2_rate - sd_co2_rate, ymax = co2_rate + sd_co2_rate), 
                 color="gray75", width=0) +
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # white points to cover lines
   geom_point(aes(shape = trt_nutrients), 
              size=4, color="white", fill="white") +
   # data points
   geom_point(aes(shape = trt_nutrients, fill = trt_fish, alpha = trt_nutrients), 
              size=4) +
   # scales
   scale_color_manual(breaks = c("high", "medium", "low"),
                      values = mycolors,
                      guide=NULL) +
   scale_fill_manual(name = "Food Web",
                     breaks = c("high", "medium", "low"),
                     values = mycolors,
                     guide = guide_legend(override.aes = list(shape=21, alpha=0.7))) +
   scale_shape_manual(name = "Nutrients",
                      breaks = c("yes", "no"),
                      values = c("yes" = 22, "no" = 21),
                      guide = guide_legend(override.aes = list(size=3))) +
   scale_alpha_manual(breaks = c("yes", "no"),
                      values = c("yes" = 0.7, "no" = 0.5),
                      guide=NULL) +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)),
                      expand = expansion(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()

# ggsave(filename = "Figures/methano-rates_co2.png", height=5, width=7, units="in")


##_Comparing between nutrient treatments (by fish); 3-panel
{
# HIGH B-P
# windows()
chigh =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="high"),
       aes(x = doy, y = co2_rate)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_errorbar(aes(ymin = co2_rate - sd_co2_rate, ymax = co2_rate + sd_co2_rate),
                 color="gray75", width=0) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# MEDIUM B-P
# windows()
cmed =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = co2_rate)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_errorbar(aes(ymin = co2_rate - sd_co2_rate, ymax = co2_rate + sd_co2_rate),
                 color="gray75", width=0) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Medium (A, D)") +
   theme_classic()


# LOW B-P
# windows()
clow =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(co2_rate))) %>% filter(trt_fish=="low"),
       aes(x = doy, y = co2_rate)) +
   # pulse
   geom_vline(xintercept = 176, linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_errorbar(aes(ymin = co2_rate - sd_co2_rate, ymax = co2_rate + sd_co2_rate),
                 color="gray75", width=0) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Ambient", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~production~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

chigh / cmed / clow

ggsave(filename = "Figures/methanogenesis-co2_3panel.png", height=10, width=6, units="in")

}

