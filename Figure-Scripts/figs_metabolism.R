#~~~
# Figures of pond ecosystem metabolism
# By: Robert Johnson
#~~~


library(tidyverse)
library(cowplot)
library(patchwork)
# library(viridis)
# library(slider)


source("Figure-Scripts/figs_functions.R")


# Data

# create the 'fdat' and 'pond_data' data sets from the "stats_model-data" script 
source("Analysis-Scripts/stats_model-data.R")



#--
# NEP
#--

# 1 panel (blue & red)
windows(height=7/3, width=3.25)
nep =
ggplot(fdat,
       aes(x = doy, y = NEP)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", linewidth=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", linewidth=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean 
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=1, alpha=0.8, span=0.1) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(NEP, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140, 240, 20)) +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)), breaks = seq(-8, 8, 2)) +
   coord_cartesian(ylim = c(-8, 8), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.5, 187.5, 211.5, 223), y = 8 + ((8+8)*0.1), size=3) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         # legend.position = c(0.16, 0.16),
         # legend.background = element_blank(), 
         legend.position = "none",
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=2, 'line')),
         plot.margin = unit(c(0.75,0.25,0.25,0.25), "lines"))

# ggsave(file = "NEP.png")



#--
# Re
#--

# 1 panel (blue & red)
windows(height=7/3, width=3.25)
re =
ggplot(fdat,
       aes(x = doy, y = R)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", linewidth=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", linewidth=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=1, alpha=0.8, span=0.1) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(R, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)), breaks = seq(-20, 0, 5)) +
   coord_cartesian(ylim = c(-20, 0), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.5, 187.5, 211.5, 223), y = 0 + ((20)*0.1), size=3) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         # legend.position = c(0.16, 0.16),
         legend.position = "none", 
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=2, 'line')),
         plot.margin = unit(c(1,0.25,0,0.25), "lines"))

# ggsave(file = "Re.png")



#--
# GPP
#--

# 1 panel (blue & red)
windows(height=7/3, width=3.25)
gpp =
ggplot(fdat,
       aes(x = doy, y = GPP)) +
   #
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40", linewidth=0.8) +
   geom_vline(xintercept = 223, linetype=2, color="gray40", linewidth=0.8) +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray50", linewidth=0.8) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, linewidth=0.33) +
   # treatment mean
   stat_smooth(aes(color = trt_nutrients), geom="line", linewidth=1, alpha=0.8, span=0.1) +
   # geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(GPP, na.rm=T)) %>% ungroup(),
   #           aes(x = doy, y = mean, color = trt_nutrients), linewidth=1, alpha=0.9) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(GPP~(mg~O[2]~L^-1~d^-1)), breaks = seq(0, 20, 5)) +
   coord_cartesian(ylim = c(0, 20), clip = "off") +
   # event labels
   annotate(geom = "text", label = c('P1', 'H', 'P2', 'D'), x = c(176.5, 187.5, 211.5, 223), y = 20 + ((20)*0.1), size=3) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.17, 0.88),
         legend.background = element_blank(), 
         legend.text = element_text(size=8),
         legend.key.size = unit(0.8, "lines"),
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=2, 'line')),
         plot.margin = unit(c(1,0.25,0,0.25), "lines"))

# ggsave(file = "GPP.png")



## 3-panel all metabolism together
windows(height=7, width=3.25); plot_grid(gpp, re, nep, ncol=1, align='v', labels="AUTO", label_size=11, label_y=0.99, label_x=0.02)

# ggsave(file = "metabolism.png", height=7, width=3.25, units = "in")



## Bar Charts

# All metab vars by pond

windows(width=12)
ggplot(metabolism %>%
          # filter(!(GPP<0) & !(R>0)) %>%
          pivot_longer(cols = GPP:NEP,
                       names_to = "variable",
                       values_to = "rate",
                       values_drop_na = T)) +
   geom_col(aes(x = doy, y = rate), fill="gray80", color="black") +
   # geom_hline(yintercept=0, linetype=2) +
   facet_grid(rows = vars(variable), cols = vars(pond_id)) +
   theme_classic()



# NEP by pond

windows(width=12)
ggplot(metabolism) + # %>%
          # filter(!(GPP<0) & !(R>0)) %>%
   geom_col(aes(x = doy, y = NEP), fill="gray80", color="black") +
   # geom_hline(yintercept=0, linetype=2) +
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()


