#~~~
# Figures for AGU poster presentation
# Dec. 2020
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(slider)

source("Figure-Scripts/figs_functions.R")


#--
# Diffusive flux
#--

# Carbon Dioxide
windows()
a =
ggplot(lake_flux %>%
          left_join(pond_data),
       aes(x = doy, y = co2_flux)) +
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
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1))) +
   #
   ggtitle(expression(CO[2])) +
   theme_classic()


# Methane
windows()
b =
ggplot(lake_flux %>%
          left_join(pond_data),
       aes(x = doy, y = ch4_flux)) +
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
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1))) +
   #
   ggtitle(expression(CH[4])) +
   theme_classic()


# Nitrous Oxide
windows()
c =
ggplot(lake_flux %>%
          left_join(pond_data) %>%
          mutate(n2o_flux = n2o_flux * 1000), 
       aes(x = doy, y = n2o_flux)) +
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
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)),
                      limits = c(-4, 3),
                      breaks = seq(-4, 3, 1)) +
   #
   ggtitle(expression(N[2]*O)) +
   theme_classic()


# Combined diffusive flux figure
windows(height=10, width=6); a / b / c

ggsave(filename = "Figures/AGU-poster/diffusive-flux_all.png", height=10, width=6, units="in")


## INDIVIDUAL GASES

### CO2

# PULSED
# windows()
d =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = co2_flux)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(group=pond_id), color="seagreen3", alpha=0.4, size=1) +
   geom_point(aes(shape = pond_id), color="seagreen3", size=2.25, alpha=0.4) +
   geom_smooth(se=F, color="seagreen", size=1.25, alpha=0.8, span = 0.2) +
   #
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C'),
                      values = c("A" = 1, "B" = 2, "C" = 0),
                      guide = guide_legend(override.aes = list(size=3))) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1), breaks=seq(145,240,15)) +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), limits = c(-25, 250)) +
   labs(title = "Pulsed") +
   theme_classic()


# REFERENCE
# windows()
e =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = co2_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(group=pond_id), color="cornflowerblue", alpha=0.6, size=1) +
   geom_point(aes(shape = pond_id), color="cornflowerblue", size=2.25, alpha=0.6) +
   geom_smooth(se=F, color="royalblue3", size=1.25, alpha=0.8, span = 0.2) +
   #
   scale_shape_manual(name = "Pond",
                      breaks = c('D', 'E', 'F'),
                      values = c("D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1)), limits = c(-25, 250)) +
   labs(title = "Reference") +
   theme_classic()


# CO2 figure
windows(height=7, width=6); d / e

ggsave(filename = "Figures/AGU-poster/co2_flux.png", height=7, width=6, units="in")


### CH4

# PULSED
# windows()
d =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = ch4_flux)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(group=pond_id), color="seagreen3", alpha=0.6, size=1.25) +
   geom_point(aes(shape = pond_id), color="seagreen3", size=2, alpha=0.6) +
   #
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C'),
                      values = c("A" = 1, "B" = 2, "C" = 0)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), limits = c(0, 60)) +
   labs(title = "Pulsed") +
   theme_classic()


# REFERENCE
# windows()
e =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = ch4_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(group=pond_id), color="cornflowerblue", alpha=0.6, size=1.25) +
   geom_point(aes(shape = pond_id), color="cornflowerblue", size=2, alpha=0.6) +
   #
   scale_shape_manual(name = "Pond",
                      breaks = c('D', 'E', 'F'),
                      values = c("D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), limits = c(0, 60)) +
   labs(title = "Reference") +
   theme_classic()


# CH4 figure
windows(height=7, width=6); d / e

ggsave(filename = "Figures/AGU-poster/ch4_flux.png", height=7, width=6, units="in")


### N2O

# PULSED
# windows()
d =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="yes") %>%
          mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(group=pond_id), color="seagreen3", alpha=0.6, size=1.25) +
   geom_point(aes(shape = pond_id), color="seagreen3", size=2, alpha=0.6) +
   #
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C'),
                      values = c("A" = 1, "B" = 2, "C" = 0)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), limits = c(-4, 4)) +
   labs(title = "Pulsed") +
   theme_classic()


# REFERENCE
# windows()
e =
ggplot(lake_flux %>% left_join(pond_data) %>% filter(trt_nutrients=="no") %>%
          mutate(n2o_flux = n2o_flux * 1000),
       aes(x = doy, y = n2o_flux)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(group=pond_id), color="cornflowerblue", alpha=0.6, size=1.25) +
   geom_point(aes(shape = pond_id), color="cornflowerblue", size=2, alpha=0.6) +
   #
   scale_shape_manual(name = "Pond",
                      breaks = c('D', 'E', 'F'),
                      values = c("D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)), limits = c(-4, 4)) +
   labs(title = "Reference") +
   theme_classic()


# N2O figure
windows(height=7, width=6); d / e

ggsave(filename = "Figures/AGU-poster/n2o_flux.png", height=7, width=6, units="in")


#--
# Cumulative Diffusive Flux
#--

# Carbon Dioxide
windows(height=4, width=6)
a =
ggplot(lake_flux %>%
          left_join(pond_data) %>%
          group_by(pond_id) %>%
          mutate(cumm = slide_dbl(co2_flux, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1) +
   geom_point(aes(color = trt_nutrients, shape = pond_id), size=1.75, alpha=0.6) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C', 'D', 'E', 'F'),
                      values = c("A" = 1, "B" = 2, "C" = 0, "D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(Cumulative~flux~(mmol~m^-2))) +
   #
   ggtitle(expression(CO[2])) +
   theme_classic() +
   theme(legend.position = "none")


# Methane
windows(height=4, width=6)
b =
ggplot(lake_flux %>%
          left_join(pond_data) %>%
          group_by(pond_id) %>%
          mutate(cumm = slide_dbl(ch4_flux, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1) +
   geom_point(aes(color = trt_nutrients, shape = pond_id), size=1.75, alpha=0.6) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C', 'D', 'E', 'F'),
                      values = c("A" = 1, "B" = 2, "C" = 0, "D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(Cumulative~flux~(mmol~m^-2))) +
   #
   ggtitle(expression(CH[4])) +
   theme_classic() +
   theme(legend.position = "none")


# Nitrous Oxide
windows(height=4, width=6)
c =
ggplot(lake_flux %>%
          left_join(pond_data) %>%
          mutate(n2o_flux = n2o_flux * 1000) %>%
          group_by(pond_id) %>%
          mutate(cumm = slide_dbl(n2o_flux, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1) +
   geom_point(aes(color = trt_nutrients, shape = pond_id), size=1.75, alpha=0.6, show.legend=c(shape=T, color=F)) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C', 'D', 'E', 'F'),
                      values = c("A" = 1, "B" = 2, "C" = 0, "D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(Cumulative~flux~(mu*mol~m^-2))) +
   #
   ggtitle(expression(N[2]*O)) +
   theme_classic() +
   theme(legend.position = "none")


# legend

l = get_legend(c)


# Combined cumulative diffusive flux figure
windows(height=10, width=6)#; a / b / c

ggdraw(plot_grid(
   plot_grid(a, b, c, ncol=1, align = "v"),
   plot_grid(NULL, l, NULL, ncol=1, align = "v"),
   rel_widths = c(1, 0.2)))


ggsave(filename = "Figures/AGU-poster/cumulative-diffusive-flux_all.png", height=10, width=6, units="in")


