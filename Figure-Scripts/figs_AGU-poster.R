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


