

library(slider)


#--
# Change in diffusive flux from previous measurement
#--

source("Analysis-Scripts/stats_model-data.R")
source("Figure-Scripts/figs_functions.R")

# treatment means
windows(height=4, width=7)
ggplot(fdat %>% filter(!(is.na(n2o_flux))) %>%
          left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>% 
          summarize(mean = mean(n2o_flux)) %>%
          ungroup() %>% 
          group_by(trt_nutrients) %>% arrange(doy, .by_group=T) %>%
          mutate(change = mean - lag(mean)) %>%
          ungroup()) +
   #
   geom_point(aes(x = doy, y = change, color = trt_nutrients)) +
   geom_line(aes(x = doy, y = change, group = trt_nutrients, color = trt_nutrients)) +
   
   # geom_col(aes(x = doy, y = change, fill = trt_nutrients), position = 'dodge') +
   
   #
   geom_hline(yintercept = 0, linetype=2, color="gray50") +
   #
   # scale_fill_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   theme_classic() +
   theme(legend.position = "none")



#--
# Methanogenesis vs Buoyancy Frequency
#--

# REFERENCE
# windows()
m.ref =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = ch4_rate)) +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue", show.legend=F) +
   # buoy freq
   geom_line(data = test_hobo %>%
                group_by(pond_id, doy) %>%
                summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
                ungroup() %>%
                mutate(
                   # regular
                   buoy_freq = buoy_freq / 10,
                   # 7-day lag
                   # buoy_freq = lag(buoy_freq, n=7),
                   # rolling window
                   roll_buoy = slide_dbl(buoy_freq, mean, .before=7)) %>%
                filter(pond_id=="F"),
             aes(x = doy, y = roll_buoy), color="black", size=1) +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), limits = c(0, 0.016), 
                      sec.axis = sec_axis(~(.*10), name = "Buoyancy Frequency \n(8-day rolling window)")) +
   labs(title = "Reference") +
   theme_classic() + 
   theme(axis.title.y.right = element_text(margin = margin(l=1, unit="line")))


# PULSED
# windows()
m.pul =
ggplot(methano_rates %>% left_join(pond_data) %>% filter(!(is.na(ch4_rate))) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = ch4_rate)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen", show.legend=F) +
   # buoy freq
   geom_line(data = test_hobo %>%
                group_by(pond_id, doy) %>%
                summarize(buoy_freq = median(buoy_freq, na.rm=T)) %>%
                ungroup() %>%
                mutate(
                   # regular
                   buoy_freq = buoy_freq / 10,
                   # 7-day lag
                   # buoy_freq = lag(buoy_freq, n=7),
                   # rolling window
                   roll_buoy = slide_dbl(buoy_freq, mean, .before=7)) %>%
                filter(pond_id=="B"),
             aes(x = doy, y = roll_buoy), color="black", size=1) +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), limits = c(0, 0.016),
                      sec.axis = sec_axis(~(.*10), name = "Buoyancy Frequency \n(8-day rolling window)")) +
   labs(title = "Pulsed") +
   theme_classic() + 
   theme(axis.title.y.right = element_text(margin = margin(l=1, unit="line")))


# 2-panel
windows(height=7, width=6)

m.ref / m.pul

# ggsave(filename = "Figures/new-figs/methanogenesis_vs_buoy-freq.png", height=7, width=6, units="in")


#--
# DEA vs Buoyancy Frequency
#--


# REFERENCE
# windows()
n.ref =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_nutrients=="no") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)), limits = c(0, 1.2)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
n.pul =
ggplot(dea_rates %>% left_join(pond_data) %>% filter(!(is.na(n2o_rate))) %>% filter(trt_nutrients=="yes") %>%
          mutate(across(ends_with("rate"), ~(.*1000))),
       aes(x = doy, y = n2o_rate)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(N[2]*O~production~(nmol~g^-1~h^-1)), limits = c(0, 1.2)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

n.ref / n.pul


