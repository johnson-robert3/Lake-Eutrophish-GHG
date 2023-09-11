#~~~
# Exploratory figs to view limno variables
# By: Robert Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)
# library(viridis)
# library(slider)

source("Figure-Scripts/figs_functions.R")


# Data for plotting
source("Analysis-Scripts/stats_model-data.R")

pdat = fdat 



#===
# Sonde Profile Data
#===

# daily surface limno values from sonde profiles
# ldat = left_join(sonde_surface, pond_data)  # no longer needed, the full dataset (fdat) now directly uses the sonde_surface values


# rolling window data (3-day)
# rdat = sonde_surface %>%
#    mutate(across(temp:salinity, ~slide_dbl(., mean, .before=2, .complete=T))) %>%
#    left_join(pond_data)


#--
# Chlorophyll
#--

# 1 panel (blue & red)
windows(height=4, width=5.5)
ggplot(pdat,
       aes(x = date, y = chla)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(Chl~italic("a")~(mu*g~L^-1)),
                      limits = c(0, 65), breaks = seq(0, 60, 10)) +
   ggtitle(expression(Surface~Water~Chlorophyll~italic("a"))) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.18, 0.86),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename = "surface-chla.png")



#--
# Dissolved Oxygen
#--

## Surface DO concentration
windows(height=7/3, width=3.25)
sdo =
ggplot(pdat,
       aes(x = doy, y = do)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", size=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", size=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", size=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(do, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~DO~(mg~L^-1)), breaks = seq(0, 15, 5)) +
   coord_cartesian(ylim = c(0, 17), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.5, 187.5, 211.5, 223), y = 17 + ((17)*0.1), size=3) +
   # white box beneath legend
   annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 12, ymax = 17, fill="white", color="white") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.79, 0.85),
         legend.background = element_blank(),
         legend.key.size = unit(0.5, "cm"),
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=1, 'line')),
         plot.margin = unit(c(1,0.5,0,0.5), "lines"))

# ggsave(filename = "surface-water-DO.png")


## Bottom water DO concentration 
windows(height=7/3, width=3.25)
bdo =
ggplot(pdat,
       aes(x = doy, y = bottom_do)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", size=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", size=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", size=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean (loess smooth)
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(bottom_do, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~DO~(mg~L^-1)), breaks = seq(0, 20, 5)) +
   coord_cartesian(ylim = c(0, 20), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.5, 187.5, 211.5, 223), y = 20 + ((20)*0.1), size=3) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = "none", 
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=1, 'line')),
         plot.margin = unit(c(1,0.5,0.5,0.5), "lines"))

# ggsave(filename = "bottom-water-DO.png")


# 2-panel, DO concentration
windows(height=7/3*2, width=3.25); plot_grid(sdo, bdo, ncol=1, align='v', rel_heights = c(0.95, 1), labels="AUTO", label_size=11, label_y=0.99, label_x=0.01)

# ggsave(file = "pond_DO.png", height=7/3*2, width=3.25, units='in')



## Surface DO saturation
windows(height=7/3, width=3.25)
sdo =
ggplot(pdat,
       aes(x = doy, y = do_sat)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", size=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", size=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", size=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(do_sat, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Surface~DO~('%')), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = c(0, 250), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.5, 187.5, 211.5, 223), y = 250 + ((250)*0.1), size=3) +
   # white box beneath legend
   # annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 140, ymax = 200, fill="white", color="white") +
   annotate(geom = "rect", xmin = 205, xmax = 241, ymin = 180, ymax = 250, fill="white", color="white") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.79, 0.85),
         legend.background = element_blank(),
         legend.key.size = unit(0.5, "cm"),
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=1, 'line')),
         plot.margin = unit(c(1,0.5,0,0.5), "lines"))

# ggsave(filename = "surface-water-DO-sat.png")


## Bottom DO saturation
windows(height=7/3, width=3.25)
bdo =
ggplot(pdat,
       aes(x = doy, y = bottom_do_sat)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", size=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", size=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", size=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean (loess smooth)
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(bottom_do_sat, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(Bottom~DO~('%')), breaks = seq(0, 250, 50)) +
   coord_cartesian(ylim = c(0, 250), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.5, 187.5, 211.5, 223), y = 250 + ((250)*0.1), size=3) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = "none", 
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=1, 'line')),
         plot.margin = unit(c(1,0.5,0.5,0.5), "lines"))

# ggsave(filename = "bottom-water-DO-sat.png")


# 2-panel, DO saturation
windows(height=7/3*2, width=3.25); plot_grid(sdo, bdo, ncol=1, align='v', rel_heights = c(0.95, 1), labels="AUTO", label_size=11, label_y=0.99, label_x=0.01)

# ggsave(file = "pond_DO_sat.png", height=7/3*2, width=3.25, units='in')



#===
# Field Samples
#===

#--
# DOC
#--

## 1-panel (blue & red)
windows(height=3.5, width=5)
ggplot(fdat %>% 
          filter(!(is.na(doc_ppm))) %>% 
          left_join(pond_data),
       aes(x = doy, y = doc_ppm)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(DOC~(mg~L^-1)),
                      limits = c(0, 50), breaks = seq(0, 50, 10)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.85, 0.87),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")))

# ggsave(filename = "doc.png", height=3.5, width=5, units="in")


# doc over time in exp ponds
# windows(height=4, width=6)
p1 =
ggplot(fdat %>% 
          filter(pond_id %in% c('A', 'B', 'C')) %>%
          filter(!(is.na(doc_ppm)))) +
   #
   geom_point(aes(x = doy, y = doc_ppm, color = pond_id), size=2) +
   geom_line(aes(x = doy, y = doc_ppm, group = pond_id, color = pond_id), size=1, alpha=0.7) +
   #
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   #
   scale_color_manual(breaks = c('A', 'B', 'C'),
                      values = c('A' = "#3BB873", 'B' = '#51ADCF', 'C' = '#2D6187')) +
   lims(y=c(0, 50)) +
   labs(x = "day of year", y = "DOC (ppm)") +
   theme_classic()


# doc over time in ref ponds
# windows(height=4, width=6)
p2 =
ggplot(fdat %>% 
          filter(pond_id %in% c('D', 'E', 'F')) %>%
          filter(!(is.na(doc_ppm)))) +
   #
   geom_point(aes(x = doy, y = doc_ppm, color = pond_id), size=2) +
   geom_line(aes(x = doy, y = doc_ppm, group = pond_id, color = pond_id), size=1, alpha=0.7) +
   #
   # geom_smooth(aes(x = doy, y = doc_ppm, group = pond_id, fill = pond_id, color = pond_id), size=0, alpha=0.2) +
   # geom_smooth(aes(x = doy, y = doc_ppm, group = pond_id, fill = pond_id), alpha=0.2, color=NA) +
   #
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   #
   scale_color_manual(breaks = c('D', 'E', 'F'),
                      values = c('D' = "#3BB873", 'E' = '#51ADCF', 'F' = '#2D6187')) +
   scale_fill_manual(breaks = c('D', 'E', 'F'),
                      values = c('D' = "#3BB873", 'E' = '#51ADCF', 'F' = '#2D6187')) +
   #
   lims(y=c(0, 50)) +
   labs(x = "day of year", y = "DOC (ppm)") +
   theme_classic()


windows(height=8, width=12); p1 / p2

# ggsave("doc-by-np-trt.png")


#--
# Nitrogen
#--

## TN (mg/L)
windows(height=4, width=5.5)
tn =
# ggplot(fdat %>% filter(!(is.na(tn))) %>% left_join(pond_data),
#        aes(x = doy, y = tn)) +
ggplot(pdat %>% filter(!(is.na(tn))),
       aes(x = doy, y = tn)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40") +
   geom_vline(xintercept = 223, linetype=2, color="gray40") +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   #
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, size=0.5) +
   # treatment mean (loess smooth)
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(tn, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), size=1.3, alpha=1) + 
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TN~(mg~L^-1)), limits = c(0, 1)) +
   # ggtitle("Total Nitrogen") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.18, 0.87),
         legend.background = element_blank(), 
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line"), size=rel(1.1)),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="total-nitrogen.png", height=4, width=5.5, units='in')


## NOx (mg/L)
windows(height=4, width=5.5)
ggplot(fdat %>% filter(!(is.na(nox))) %>% left_join(pond_data),
       aes(x = doy, y = nox)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(NO[x]~(mg~L^-1)), limits = c(0, 0.4)) +
   #
   ggtitle("Nitrate") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.82, 0.87),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")))

# ggsave(filename="nitrate.png", height=4, width=5.5, units='in')



#--
# Phosphorus
#--

## TP (ug/L)
windows(height=4, width=5.5)
tp = 
# ggplot(fdat %>% filter(!(is.na(tp))) %>% left_join(pond_data),
#        aes(x = doy, y = tp)) +
ggplot(pdat %>% filter(!(is.na(tp))),
       aes(x = doy, y = tp)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40") +
   geom_vline(xintercept = 223, linetype=2, color="gray40") +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   #
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, size=0.5) +
   # treatment mean (loess smooth)
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(tp, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), size=1.3, alpha=1) + 
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TP~(mu*g~L^-1)), limits = c(0, 250)) +
   # ggtitle("Total Phosphorus") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         # legend.position = c(0.82, 0.87),
         legend.position = "none",
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line"), size=rel(1.1)),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="total-phosphorus.png", height=4, width=5.5, units='in')


## SRP (ug/L)
windows(height=4, width=5.5)
ggplot(pdat %>% filter(!(is.na(srp))),
       aes(x = date, y = srp)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(SRP~(mu*g~L^-1)), limits = c(-0.1, 30), breaks = seq(0, 30, 5)) +
   #
   ggtitle("Soluble Reactive Phosphorus") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.85, 0.87),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename="SRP.png")


## TN/TP together
windows(height=3.5*2, width=5); plot_grid(tn, tp, ncol=1, align='v', labels="AUTO", label_size=13, label_y=0.99, label_x=0.01)

# ggsave(file = "tn-tp.png", height=3.5*2, width=5, units='in')


