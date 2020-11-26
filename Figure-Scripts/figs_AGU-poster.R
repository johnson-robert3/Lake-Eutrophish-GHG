#~~~
# Figures for AGU poster presentation
# Dec. 2020
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(slider)

source("Figure-Scripts/figs_functions.R")


#--
# Diffusive flux
#--

# Carbon Dioxide
windows(height=4, width=5)
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
   scale_x_continuous(name = " ", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CO[2]~flux~(mmol~m^-2~d^-1))) +
   #
   ggtitle(expression(CO[2])) +
   theme_classic() +
   theme(legend.position = c(0.18, 0.85),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))


# Methane
windows(height=4, width=5)
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
   scale_x_continuous(name = " ", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CH[4]~flux~(mmol~m^-2~d^-1)), limits = c(0, 60), breaks = seq(0,60,10)) +
   #
   ggtitle(expression(CH[4])) +
   theme_classic() +
   theme(legend.position = "none",
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))


# Nitrous Oxide
windows(height=4, width=5)
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
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)),
                      limits = c(-4, 3),
                      breaks = seq(-4, 3, 1)) +
   #
   ggtitle(expression(N[2]*O)) +
   theme_classic() +
   theme(legend.position = "none",
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))


# Combined diffusive flux figure
windows(height=10, width=5); a / b / c

ggsave(filename = "Figures/AGU-poster/diffusive-flux_all.png", height=10, width=5, units="in")



#--
# Cumulative Diffusive Flux
#--

# Tibble of all DOYs used for interpolating diffusive flux to estimate cumulative
all_doys = tibble(doy = 146:240,
                  pond_id = rep("A", length(doy))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("B", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("C", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("D", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("E", length(doy)))) %>%
   full_join(tibble(doy = 146:240,
                    pond_id = rep("F", length(doy))))


# Carbon Dioxide
windows(height=4, width=5)
a =
ggplot(lake_flux %>%
          # add blank non-measurement days
          full_join(all_doys) %>%
          left_join(pond_data) %>%
          arrange(pond_id, doy) %>%
          group_by(pond_id) %>%
          # interpolate flux values for non-measurement days
          mutate(co2_interp = zoo::na.approx(co2_flux)) %>%
          # cumulative flux over summer
          mutate(cumm = slide_dbl(co2_interp, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # line for cumulative flux (all days)
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1) +
   # add data points only for measurement days
   geom_point(data = ~filter(.x, !(is.na(co2_flux))),
              aes(color = trt_nutrients, shape = pond_id), size=1.75, alpha=0.6, show.legend=c(shape=T, color=F)) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C', 'D', 'E', 'F'),
                      values = c("A" = 1, "B" = 2, "C" = 0, "D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = " ", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(Cumulative~flux~(mmol~m^-2)), limits = c(-1000, 5000), breaks = seq(-1000,5000,1000)) +
   #
   ggtitle(expression(CO[2])) +
   theme_classic() +
   theme(legend.position = "none")


# Methane
windows(height=4, width=6)
b =
ggplot(lake_flux %>%
          # add blank non-measurement days
          full_join(all_doys) %>%
          left_join(pond_data) %>%
          arrange(pond_id, doy) %>%
          group_by(pond_id) %>%
          # interpolate flux values for non-measurement days
          mutate(ch4_interp = zoo::na.approx(ch4_flux)) %>%
          # cumulative flux over summer
          mutate(cumm = slide_dbl(ch4_interp, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # line for cumulative flux (all days)
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1) +
   # add data points only for measurement days
   geom_point(data = ~filter(.x, !(is.na(ch4_flux))),
              aes(color = trt_nutrients, shape = pond_id), size=1.75, alpha=0.6, show.legend=c(shape=T, color=F)) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C', 'D', 'E', 'F'),
                      values = c("A" = 1, "B" = 2, "C" = 0, "D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = " ", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(Cumulative~flux~(mmol~m^-2))) +
   #
   ggtitle(expression(CH[4])) +
   theme_classic() +
   theme(legend.position = "none")


# Nitrous Oxide
windows(height=4, width=6)
c =
ggplot(lake_flux %>%
          # add blank non-measurement days
          full_join(all_doys) %>%
          left_join(pond_data) %>%
          arrange(pond_id, doy) %>%
          # convert to umol
          mutate(n2o_flux = n2o_flux * 1000) %>%
          group_by(pond_id) %>%
          # interpolate flux values for non-measurement days
          mutate(n2o_interp = zoo::na.approx(n2o_flux)) %>%
          # cumulative flux over summer
          mutate(cumm = slide_dbl(n2o_interp, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # line for cumulative flux (all days)
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1) +
   # add data points only for measurement days
   geom_point(data = ~filter(.x, !(is.na(n2o_flux))),
              aes(color = trt_nutrients, shape = pond_id), size=1.75, alpha=0.6, show.legend=c(shape=T, color=F)) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_shape_manual(name = "Pond",
                      breaks = c('A', 'B', 'C', 'D', 'E', 'F'),
                      values = c("A" = 1, "B" = 2, "C" = 0, "D" = 1, "E" = 2, "F" = 0)) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(Cumulative~flux~(mu*mol~m^-2))) +
   #
   ggtitle(expression(N[2]*O)) +
   # force the shape guide (pond_id) on the left, and the color guide (treatment) on the right
   guides(shape = guide_legend(order=1), color = guide_legend(order=2)) +
   theme_classic() +
   # position the shape and color guides inside the plot
   theme(legend.box = "horizontal",
         legend.box.just = "bottom",
         # legend.spacing = unit(3.5, "line"),
         legend.position = c(0.25, 0.42),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")))


# extract a figure legend to plot separately using 'cowplot'
# legend
# l = get_legend(c)

# c = c + theme(legend.position = "none")


# Combined cumulative diffusive flux figure
windows(height=10, width=5); a / b / c

# plot multi-panel figure with single outer legend using 'cowplot'
# ggdraw(plot_grid(
#    plot_grid(a, b, c, ncol=1, align = "v"),
#    plot_grid(NULL, l, NULL, ncol=1, align = "v"),
#    rel_widths = c(1, 0.2)))


ggsave(filename = "Figures/AGU-poster/cumulative-diffusive-flux_all.png", height=10, width=5, units="in")



#--
# TN & TP
#--

# TN (mg/L)
a = 
ggplot(limno_field_data %>% filter(!(is.na(tn))) %>% left_join(pond_data),
       aes(x = doy, y = tn)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TN~(mg~L^-1)), limits = c(0, 1)) +
   ggtitle("Total Nitrogen") +
   theme_classic()


# TP (ug/L)
b = 
ggplot(limno_field_data %>% filter(!(is.na(tp))) %>% left_join(pond_data),
       aes(x = doy, y = tp)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TP~(mu*g~L^-1)), limits = c(0, 250)) +
   ggtitle("Total Phosphorus") +
   theme_classic()


# Combined total nutrient figure
windows(height=7, width=6); a/b

ggsave(filename = "Figures/AGU-poster/tn-tp.png", height=7, width=6, units="in")



#--
# NOx, NHx, SRP
#--

# NOx
a = 
ggplot(limno_field_data %>% filter(!(is.na(nox))) %>% left_join(pond_data),
       aes(x = doy, y = nox)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(NO[x]~(mg~L^-1)), limits = c(0, 0.4)) +
   #
   ggtitle(expression(NO[x])) +
   theme_classic()


# NHx
b = 
ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data),
       aes(x = doy, y = nhx)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(NH[x]~(mg~L^-1)), limits = c(0, 0.075)) +
   #
   ggtitle(expression(NH[x])) +
   theme_classic()


# SRP
c = 
ggplot(limno_field_data %>% filter(!(is.na(srp))) %>% left_join(pond_data),
       aes(x = doy, y = srp)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(SRP~(mu*g~L^-1)), limits = c(0, 30)) +
   #
   ggtitle(expression(SRP)) +
   theme_classic()


# Combined dissolved nutrient figure
windows(height=10, width=6); a / b / c

ggsave(filename = "Figures/AGU-poster/nutrient-concentration.png", height=10, width=6, units="in")



#--
# Methanogenesis Potential
#--

windows(height=4, width=6)
ggplot(methano_rates %>% left_join(pond_data),
       aes(x = doy, y = ch4_rate)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(CH[4]~(mu*mol~g^-1~h^-1)), limits = c(0, 0.016), breaks = seq(0, 0.016, 0.004)) +
   #
   ggtitle("Methanogenesis Potential") +
   theme_classic()
   

ggsave(filename = "Figures/AGU-poster/methanogenesis.png", height=4, width=6, units="in")



#--
# DEA
#--

windows(height=4, width=6)
ggplot(dea_rates %>% left_join(pond_data) %>% mutate(n2o_rate = n2o_rate * 1000),
       aes(x = doy, y = n2o_rate)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(N[2]*O~(nmol~g^-1~h^-1)), limits = c(0, 1.2)) +
   #
   ggtitle("Denitrification Enzyme Activity") +
   theme_classic()


ggsave(filename = "Figures/AGU-poster/DEA.png", height=4, width=6, units="in")



#--
# N:P Ratio
#--

windows(height=4, width=6)
ggplot(limno_field_data %>% mutate(np_ratio = (tn/14.001) / ((tp/1000)/30.97)) %>% filter(!(is.na(np_ratio))) %>% left_join(pond_data),
       aes(x = doy, y = np_ratio)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TN:TP), limits = c(0, 60)) +
   theme_classic()



#--
# Mixed-effects Model Outputs
#--

# CO2
windows(height=4, width=6)
a =
ggplot(mod_co2 %>%
          rename(estimate = Estimate,
                 se = "Std. Error") %>%
          mutate(ci = se * 1.96,
                 fixed.effect = as_factor(fixed.effect)) %>%
          filter(!(fixed.effect=="(Intercept)")),
       aes(x = estimate, y = fct_rev(fixed.effect))) +
       # aes(x = estimate, y = fct_reorder(fixed.effect, estimate, .fun=abs, .desc=F))) +
   #
   geom_vline(xintercept = 0, linetype=1, color="gray60") +
   # placeholder points to build the plot correctly
   geom_point(size=1) +
   #
   # data points for positive effect size
   geom_point(data = ~filter(.x, estimate > 0),
               size=4, color="#28abb9") +
   geom_errorbarh(data = ~filter(.x, estimate > 0), 
                  aes(y = fixed.effect, xmin = estimate-ci, xmax = estimate+ci, height=0), size=0.75, color="#28abb9") +
   # data points for negative effect size
   geom_point(data = ~filter(.x, estimate < 0),
               size=4, color="#2d6187") +
   geom_errorbarh(data = ~filter(.x, estimate < 0),
                  aes(y = fixed.effect, xmin = estimate-ci, xmax = estimate+ci, height=0), size=0.75, color="#2d6187") +
   #
   scale_x_continuous(name = "Estimate", limits = c(-400,200), breaks = seq(-400, 200, 100)) +
   # factors in order of model
   scale_y_discrete(name=NULL, labels = c("Treatment * Period \n(Pulse2)", "Treatment * Period \n(Pulse1)", "log(TP)",
                                          "Buoyancy \nFrequency", "Period \n(Pulse2)", "Period \n(Pulse1)", "Treatment")) +
   # factors ordered by decreasing effect size
   # scale_y_discrete(name = NULL,
   #                  labels = c("Treatment", "Treatment * Period \n(Pulse1)", "Period \n(Pulse1)", "log(TP)",
   #                             "Treatment * Period \n(Pulse2)", "Period \n(Pulse2)", "Buoyancy \nFrequency")) +
   ggtitle(expression(Effect~on~CO[2])) +
   theme_classic() +
   theme(axis.title.x = element_text(margin = margin(t=0.5, unit="line")))


# CH4
windows(height=4, width=6)
b =
ggplot(mod_ch4 %>%
          rename(estimate = Estimate,
                 se = "Std. Error") %>%
          mutate(ci = se * 1.96,
                 fixed.effect = as_factor(fixed.effect)) %>%
          filter(!(fixed.effect=="(Intercept)")),
       aes(x = estimate, y = fct_rev(fixed.effect))) +
       # aes(x = estimate, y = fct_reorder(fixed.effect, estimate, .fun=abs, .desc=F))) +
   #
   geom_vline(xintercept = 0, linetype=1, color="gray60") +
   # placeholder points to build the plot correctly
   geom_point(size=1) +
   #
   # data points for positive effect size
   geom_point(data = ~filter(.x, estimate > 0),
               size=4, color="#28abb9") +
   geom_errorbarh(data = ~filter(.x, estimate > 0), 
                  aes(y = fixed.effect, xmin = estimate-ci, xmax = estimate+ci, height=0), size=0.75, color="#28abb9") +
   # data points for negative effect size
   geom_point(data = ~filter(.x, estimate < 0),
               size=4, color="#2d6187") +
   geom_errorbarh(data = ~filter(.x, estimate < 0),
                  aes(y = fixed.effect, xmin = estimate-ci, xmax = estimate+ci, height=0), size=0.75, color="#2d6187") +
   #
   scale_x_continuous(name = "Estimate", limits = c(-20,20), breaks = seq(-20, 20, 5)) +
   # factors in order of model
   scale_y_discrete(name=NULL, labels = c("Treatment * Period \n(Pulse2)", "Treatment * Period \n(Pulse1)", "Bottom  \nWater DO",
                                          "Period \n(Pulse2)", "Period \n(Pulse1)", "Treatment")) +
   # factors ordered by decreasing effect size
   # scale_y_discrete(name = NULL,
   #                  labels = c("Bottom  \nWater DO", "Period \n(Pulse2)", "Treatment * Period \n(Pulse1)",
   #                             "Period \n(Pulse1)", "Treatment", "Treatment * Period \n(Pulse2)")) +
   ggtitle(expression(Effect~on~CH[4])) +
   theme_classic() +
   theme(axis.title.x = element_text(margin = margin(t=0.5, unit="line")))


# N2O
windows(height=4, width=6)
c =
ggplot(mod_n2o %>%
          rename(estimate = Estimate,
                 se = "Std. Error") %>%
          mutate(ci = se * 1.96,
                 fixed.effect = as_factor(fixed.effect)) %>%
          filter(!(fixed.effect=="(Intercept)")),
       aes(x = estimate, y = fct_rev(fixed.effect))) +
       # aes(x = estimate, y = fct_reorder(fixed.effect, estimate, .fun=abs, .desc=F))) +
   #
   geom_vline(xintercept = 0, linetype=1, color="gray60") +
   # placeholder points to build the plot correctly
   geom_point(size=1) +
   #
   # data points for positive effect size
   geom_point(data = ~filter(.x, estimate > 0),
               size=4, color="#28abb9") +
   geom_errorbarh(data = ~filter(.x, estimate > 0), 
                  aes(y = fixed.effect, xmin = estimate-ci, xmax = estimate+ci, height=0), size=0.75, color="#28abb9") +
   # data points for negative effect size
   geom_point(data = ~filter(.x, estimate < 0),
               size=4, color="#2d6187") +
   geom_errorbarh(data = ~filter(.x, estimate < 0),
                  aes(y = fixed.effect, xmin = estimate-ci, xmax = estimate+ci, height=0), size=0.75, color="#2d6187") +
   #
   scale_x_continuous(name = "Estimate", limits = c(-8, 2), breaks = seq(-8, 2, 2)) +
   # factors in order of model
   scale_y_discrete(name=NULL, labels = c("Treatment * Period \n(Pulse2)", "Treatment * Period \n(Pulse1)", "Bottom  \nWater DO",
                                          "NOx", "Period \n(Pulse2)", "Period \n(Pulse1)", "Treatment")) +
   # factors ordered by decreasing effect size
   # scale_y_discrete(name = NULL,
   #                  labels = c("Treatment * Period \n(Pulse2)", "Bottom  \nWater DO", "Treatment",
   #                             "Treatment * Period \n(Pulse1)", "Period \n(Pulse1)", "Period \n(Pulse2)", "NOx")) +
   ggtitle(expression(Effect~on~N[2]*O)) +
   theme_classic() +
   theme(axis.title.x = element_text(margin = margin(t=0.5, unit="line")))


# model output figure
windows(height=10, width=5); a / b / c

# plot_grid(a, b, c, ncol=1, align="v")

ggsave(filename = "Figures/AGU-poster/lme-effect-sizes.png", height=10, width=5, units="in")


