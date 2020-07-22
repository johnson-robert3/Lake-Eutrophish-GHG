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


##__Lake GHG Concentrations

# METHANE

# ponds distinguished by alpha
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


ggsave(filename = "Figures/2020_lake-dissolved-ch4.png", height=5, width=7, units="in")


# log scale (to view dynamics in lower concentrations
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


ggsave(filename = "Figures/2020_lake-dissolved-ch4_logY.png", height=5, width=7, units="in")
}


# NITROUS OXIDE

# ponds distinguished by alpha
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


ggsave(filename = "Figures/2020_lake-dissolved-n2o.png", height=5, width=7, units="in")


##__Methanogenesis Potential

# METHANE

# ponds distinguished by alpha
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


ggsave(filename = "Figures/2020_methano-rates.png", height=5, width=7, units="in")


