#~~~
# Ebullition figures
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)

source("Figure-Scripts/figs_functions.R")


# dataset
ebu_flux_pond = read_csv("Data/ebullition_total.csv")


# bars
windows()
ggplot(ebu_flux_pond %>%
          left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(n = n(),
                    se_ebu = sd(ch4_ebu_flux)/sqrt(n),
                    mean_ebu = mean(ch4_ebu_flux)) %>%
          ungroup(),
       aes(x = doy, y = mean_ebu, group = trt_nutrients)) +
   #
   geom_errorbar(aes(ymin = mean_ebu - se_ebu,
                     ymax = mean_ebu + se_ebu),
                 position = position_dodge(width = 0.9*6),
                 width=2, color="gray50") +
   #
   geom_col(aes(fill = trt_nutrients),
            position = position_dodge()) +
   #
   #
   scale_y_continuous(name = expression(Ebullitive~flux~(mmol~m^-2~d^-1))) +
   theme_classic()


## 2 panel points

# Reference
r =
# windows()
ggplot(ebu_flux_pond %>%
          left_join(pond_data) %>%
          filter(trt_nutrients=="no") %>%
          group_by(doy) %>%
          summarize(n = n(),
                    se = sd(ch4_ebu_flux)/sqrt(n),
                    mean = mean(ch4_ebu_flux)) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   geom_hline(yintercept=0, linetype=2) +
   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0, color="gray60") +
   geom_point(color = "cornflowerblue", size=3) +
   # geom_line(color = "cornflowerblue") +
   lims(y = c(0,12)) +
   theme_classic()

# Pulsed
p =
# windows()
ggplot(ebu_flux_pond %>%
          left_join(pond_data) %>%
          filter(trt_nutrients=="yes") %>%
          group_by(doy) %>%
          summarize(n = n(),
                    se = sd(ch4_ebu_flux)/sqrt(n),
                    mean = mean(ch4_ebu_flux)) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   geom_hline(yintercept=0, linetype=2) +
   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0, color="gray60") +
   geom_point(color = "seagreen3", size=3) +
   # geom_line(color = "seagreen3") +
   lims(y = c(0,12)) +
   theme_classic()


windows(); r / p



# 2 panel bars

# reference 
r = 
ggplot(ebu_flux_pond %>%
          left_join(pond_data) %>%
          filter(trt_nutrients=="no") %>%
          group_by(doy) %>%
          summarize(n = n(),
                    se = sd(ch4_ebu_flux)/sqrt(n),
                    mean = mean(ch4_ebu_flux)) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   geom_errorbar(aes(ymin = mean - se, 
                     ymax = mean + se),
                 color="gray50", width=0) +
   #
   geom_col(fill = "cornflowerblue", color = "gray50") +
   # 
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(CH[4]~ebullition~(mmol~m^-2~d^-1)), limits = c(0, 12)) +
   ggtitle("Reference") +
   theme_classic()


# pulse
p = 
ggplot(ebu_flux_pond %>%
          left_join(pond_data) %>%
          filter(trt_nutrients=="yes") %>%
          group_by(doy) %>%
          summarize(n = n(),
                    se = sd(ch4_ebu_flux)/sqrt(n),
                    mean = mean(ch4_ebu_flux)) %>%
          ungroup(),
       aes(x = doy, y = mean)) +
   geom_errorbar(aes(ymin = mean - se, 
                     ymax = mean + se),
                 color="gray50", width=0) +
   #
   geom_col(fill = "seagreen3", color = "gray50") +
   # 
   scale_x_continuous(name = "DOY") +
   scale_y_continuous(name = expression(CH[4]~ebullition~(mmol~m^-2~d^-1)), limits = c(0, 12)) +
   ggtitle("Pulsed") +
   theme_classic()


windows(); r / p

# ggsave(filename = "Figures/new-figs/ebullition_ch4.png", height=7, width=6, units="in")


# 1 panel as points

windows(height=4, width=6); ggplot(ebu_flux_pond %>% 
                     left_join(pond_data) %>%
                     group_by(trt_nutrients, doy) %>%
                     summarize(n = n(),
                               se = sd(ch4_ebu_flux)/sqrt(n),
                               mean = mean(ch4_ebu_flux)) %>%
                     ungroup(),
                  aes(x = doy, y = mean, group = trt_nutrients)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days
   geom_vline(xintercept = c(176, 211), linetype=1, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(xintercept = 223, linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect',
            xmin = 186, xmax = 190,
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_point(aes(color = trt_nutrients), size=3, shape=19) +
   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0, color="gray60") +
   geom_line(aes(color = trt_nutrients), size=0.6, linetype=2) +
   #
   scale_y_continuous(name = expression(Ebullition)) +
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   #
   theme_classic()







