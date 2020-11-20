#~~~
# Figures of CO2-equivalent flux and Global Warming Potential
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(slider)

source("Figure-Scripts/figs_functions.R")


# data
gdat = lake_flux %>%
   # convert from mmol to mg
   mutate(co2_flux_g = co2_flux * 44.01,
          ch4_flux_g = ch4_flux * 16.04,
          n2o_flux_g = n2o_flux * 44.013) %>%
   # CO2 equivalents (100-year GWP) (units = mg/m2/d)
   mutate(co2_flux_eqv = co2_flux_g,
          ch4_flux_eqv = ch4_flux_g * 25,
          n2o_flux_eqv = n2o_flux_g * 298) %>%
   # 100-year GWP (flux, CO2-equivalents, mg/m2/d)
   mutate(gwp = co2_flux_eqv + ch4_flux_eqv + n2o_flux_eqv)


#--
# Flux in CO2-equivalents
#--

##__Raw
{
# Carbon Dioxide
windows()
a =
ggplot(gdat %>%
          left_join(pond_data) %>%
          mutate(co2_flux_eqv = co2_flux_eqv / 1000) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(co2_flux_eqv),
                    se = sd(co2_flux_eqv)/sqrt(n())) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_errorbar(aes(x = doy, ymin = mean-se, ymax = mean+se), width=1, color="gray70") +
   geom_line(aes(color = trt_nutrients), alpha=0.7, size=1.5) +
   # geom_point(aes(color = trt_nutrients), size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(g~CO[2]*"-eq"~m^-2~d^-1)) +
   #
   ggtitle("Carbon Dioxide") +
   theme_classic()


# Methane
windows()
b =
ggplot(gdat %>%
          left_join(pond_data) %>%
          mutate(ch4_flux_eqv = ch4_flux_eqv / 1000) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(ch4_flux_eqv),
                    se = sd(ch4_flux_eqv)/sqrt(n())) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_errorbar(aes(x = doy, ymin = mean-se, ymax = mean+se), width=1, color="gray70") +
   geom_line(aes(color = trt_nutrients), alpha=0.7, size=1.5) +
   # geom_point(aes(color = trt_nutrients), size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(g~CO[2]*"-eq"~m^-2~d^-1)) +
   #
   ggtitle("Methane") +
   theme_classic()


# Nitrous Oxide
windows()
c =
ggplot(gdat %>%
          left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(n2o_flux_eqv),
                    se = sd(n2o_flux_eqv)/sqrt(n())) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_errorbar(aes(x = doy, ymin = mean-se, ymax = mean+se), width=1, color="gray70") +
   geom_line(aes(color = trt_nutrients), alpha=0.7, size=1.5) +
   # geom_point(aes(color = trt_nutrients), size=2.5) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(mg~CO[2]*"-eq"~m^-2~d^-1)) +
   #
   ggtitle("Nitrous Oxide") +
   theme_classic()


# Total CO2-eq figure
windows(height=10, width=6); a / b / c

ggsave(filename = "Figures/new-figs/diffusive-flux_co2-equivalent.png", height=10, width=6, units="in")

}


##__Cumulative
{
# Carbon Dioxide
windows()
a =
ggplot(gdat %>%
          left_join(pond_data) %>%
          mutate(co2_flux_eqv = co2_flux_eqv / 1000) %>%
          # group_by(trt_nutrients, doy) %>%
          # summarize(mean = mean(co2_flux_eqv),
          #           se = sd(co2_flux_eqv)/sqrt(n())) %>%
          # ungroup() %>%
          group_by(pond_id) %>%
          mutate(cumm = slide_dbl(co2_flux_eqv, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1.25) +
   geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.6) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(Cumulative~flux~(g~CO[2]*"-eq"~m^-2))) +
   #
   ggtitle(expression(CO[2])) +
   theme_classic()


# Methane
windows()
b =
ggplot(gdat %>%
          left_join(pond_data) %>%
          mutate(ch4_flux_eqv = ch4_flux_eqv / 1000) %>%
          # group_by(trt_nutrients, doy) %>%
          # summarize(mean = mean(ch4_flux_eqv),
          #           se = sd(ch4_flux_eqv)/sqrt(n())) %>%
          # ungroup() %>%
          group_by(pond_id) %>%
          mutate(cumm = slide_dbl(ch4_flux_eqv, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1.25) +
   geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.6) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(Cumulative~flux~(g~CO[2]*"-eq"~m^-2))) +
   #
   ggtitle(expression(CH[4])) +
   theme_classic()


# Nitrous Oxide
windows()
c =
ggplot(gdat %>%
          left_join(pond_data) %>%
          # group_by(trt_nutrients, doy) %>%
          # summarize(mean = mean(n2o_flux_eqv),
          #           se = sd(n2o_flux_eqv)/sqrt(n())) %>%
          # ungroup() %>%
          group_by(pond_id) %>%
          mutate(cumm = slide_dbl(n2o_flux_eqv, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_line(aes(color = trt_nutrients, group=pond_id), alpha=0.6, size=1.25) +
   geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.6) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(Cumulative~flux~(mg~CO[2]*"-eq"~m^-2))) +
   #
   ggtitle(expression(N[2]*O)) +
   theme_classic()


# Total CO2-eq figure
windows(height=10, width=6); a / b / c

ggsave(filename = "Figures/new-figs/diffusive-flux-cumulative_co2-equivalent.png", height=10, width=6, units="in")

}


#--
# Total Diffusive Flux in CO2-equivalents
#--

##__Raw
{
windows(height=4, width=6)
ggplot(gdat %>%
          left_join(pond_data) %>%
          mutate(gwp = gwp / 1000) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(mean = mean(gwp),
                    se = sd(gwp)/sqrt(n())) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_errorbar(aes(x = doy, ymin = mean-se, ymax = mean+se), width=1, color="gray70") +
   geom_line(aes(color = trt_nutrients), alpha=0.7, size=1.5) +
   # geom_point(aes(color = trt_nutrients), size=2) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(g~CO[2]*"-eq"~m^-2~d^-1)) +
   #
   ggtitle("Total diffusive GHG Flux (100-yr GWP)") +
   theme_classic()

ggsave(filename = "Figures/new-figs/total-diffusive-flux.png", height=4, width=6, units="in")

}


##__Cumulative
{
windows(height=4, width=6)
ggplot(gdat %>%
          left_join(pond_data) %>%
          mutate(gwp = gwp / 1000) %>%
          # group_by(trt_nutrients, doy) %>%
          # summarize(mean = mean(gwp),
          #           se = sd(gwp)/sqrt(n())) %>%
          # ungroup() %>%
          group_by(pond_id) %>%
          mutate(cumm = slide_dbl(gwp, ~sum(.), .before=Inf)) %>%
          ungroup(),
       aes(x = doy, y = cumm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   #
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.6, size=1.25) +
   geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.6) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(Cumulative~flux~(g~CO[2]*"-eq"~m^-2))) +
   #
   ggtitle("Total diffusive GHG Flux (100-yr GWP)") +
   theme_classic()

ggsave(filename = "Figures/new-figs/total-diffusive-flux-cumulative.png", height=4, width=6, units="in")

}


#--
# total gwp flux as bars
#--

windows()
ggplot(gdat %>%
          left_join(pond_data) %>%
          select(pond_id, trt_nutrients, doy, ends_with("_eqv")) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(across(ends_with("_eqv"), mean)) %>%
          ungroup() %>%
          pivot_longer(cols = ends_with("_eqv"),
                       names_to = "gas",
                       values_to = "flux",
                       values_drop_na = T) %>%
          mutate(gas = str_remove(gas, pattern="_flux_eqv")),
       aes(x = doy, y = flux)) +
   #
   geom_col(data = ~filter(.x, trt_nutrients=="yes"), 
            aes(fill = gas)) +
   geom_hline(yintercept=0) +
   theme_classic()


