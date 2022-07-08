

library(cowplot)
library(patchwork)

source("Figure-Scripts/figs_functions.R")


# Data

# DEA and Methano (create 'fdat' dataset from 'stats_model-data' script)
fdat = fdat %>% mutate(date = ymd(date))

# Ebullition
edat = read_csv("Data/ebullition_total.csv") %>%
   # add date
   mutate(date = as_date(doy, origin="2019-12-31"))



## METHANOGENESIS

# Pulsed
# windows(height=4, width=5.5)
a = 
ggplot(fdat %>% 
          filter(!(is.na(methanogenesis)), treatment=='pulsed') %>%
          # convert methano rate from umol to nmol/g/h
          mutate(methano = methanogenesis * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = methano)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~(nmol~g^-1~h^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Methanogenesis - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# Reference
# windows(height=4, width=5.5)
c = 
ggplot(fdat %>% 
          filter(!(is.na(methanogenesis)), treatment=='reference') %>%
          # convert methano rate from umol to nmol/g/h
          mutate(methano = methanogenesis * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = methano)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = 'Food Web', breaks = fish_breaks, values = fish_color, 
                      labels = c('high' = 'High (C,E)', 'medium' = 'Med (A,D)', 'low' = 'Low (B,F)')) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~(nmol~g^-1~h^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Methanogenesis - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = c(0.17, 0.8))



## DEA

# Pulsed
# windows(height=4, width=5.5)
b = 
ggplot(fdat %>% 
          filter(!(is.na(DEA)), treatment=='pulsed') %>%
          # convert DEA rate from umol to nmol/g/h
          mutate(DEA = DEA * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = DEA)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(N[2]*O~(nmol~g^-1~h^-1)),
                      limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25)) +
   #
   ggtitle("DEA - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# Reference
# windows(height=4, width=5.5)
d = 
ggplot(fdat %>% 
          filter(!(is.na(DEA)), treatment=='reference') %>%
          # convert DEA rate from umol to nmol/g/h
          mutate(DEA = DEA * 1000) %>%
          left_join(pond_data),
       aes(x = date, y = DEA)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(N[2]*O~(nmol~g^-1~h^-1)),
                      limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25)) +
   #
   ggtitle("DEA - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")



## EBULLITION

# Pulsed
# windows(height=4, width=5.5)
e = 
ggplot(edat %>% 
          left_join(pond_data) %>%
          filter(trt_nutrients=='yes'),
       aes(x = date, y = ch4_ebu_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~(mmol~m^-2~d^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Ebullition - pulsed") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")


# Reference
# windows(height=4, width=5.5)
f = 
ggplot(edat %>% 
          left_join(pond_data) %>%
          filter(trt_nutrients=='no'),
       aes(x = date, y = ch4_ebu_flux)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pulse days, DOY 176, 211
   geom_vline(xintercept = c(as_date(176, origin='2019-12-31'), as_date(211, origin='2019-12-31')), linetype=2, color="gray60") +
   # derecho, DOY 223 (Aug. 10, 2020)
   geom_vline(aes(xintercept = as_date('2020-08-10')), linetype=1, color='gray60') +
   # heat wave, DOY 186-190 (July 4-8, 2020)
   annotate(geom = 'rect', 
            xmin = as_date(186, origin='2019-12-31'), xmax = as_date(190, origin='2019-12-31'),
            ymin = -Inf, ymax = Inf,
            fill = 'gray90') +
   #
   geom_line(aes(group = pond_id, color = trt_fish), size=1) +
   geom_point(aes(color = trt_fish), shape=1, size=2) +
   #
   scale_color_manual(name = NULL, breaks = fish_breaks, values = fish_color, labels = fish_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(CH[4]~(mmol~m^-2~d^-1)),
                      limits = c(0, 15), breaks = seq(0, 15, 3)) +
   #
   ggtitle("Ebullition - reference") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color='black'),
         legend.position = "none")



# Figure

windows(height=8, width=14)
(a + b + e) / (c + d + f)


# ggsave(file = '6-panel_methano-dea-ebu_by-pulse-trt.png')
