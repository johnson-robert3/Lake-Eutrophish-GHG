#~~~
# Figures of pond ecosystem metabolism
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)
library(viridis)
library(slider)

source("Figure-Scripts/figs_functions.R")


# Pond/Site Data
pond_data = read_csv("Data/R-Data/2020_pond-data.csv")

# Data for plotting
#  Need to run "stats_model-data.R" script first
pdat = fdat %>% 
   mutate(date = ymd(date)) %>%
   left_join(pond_data %>% 
                select(pond_id, starts_with("trt")))



#--
# NEP
#--

# 1 panel (blue & red)
windows(height=4, width=5.5)
nep = 
ggplot(pdat,
       aes(x = doy, y = NEP)) +
   #
   # geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
   #            aes(xintercept = date), linetype=2, color="gray60") +
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40") +
   geom_vline(xintercept = 223, linetype=2, color="gray40") +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, size=0.5) +
   # treatment mean 
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, alpha=0.8, span=0.05) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(NEP, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), size=1.3, alpha=1) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   # scale_x_date(name = NULL, 
   #              breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
   #              labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_x_continuous(name = "Day of year", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)), limits = c(-8, 8), breaks = seq(-8, 8, 2)) +
   # ggtitle("Net Ecosystem Production") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         # legend.position = c(0.16, 0.16),
         legend.position = "none",
         legend.background = element_blank(), 
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line"), size=rel(1.1)),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(file = "NEP.png")



#--
# Re
#--

# 1 panel (blue & red)
windows(height=4, width=5.5)
re = 
ggplot(pdat,
       aes(x = doy, y = R)) +
   #
   # geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
   #            aes(xintercept = date), linetype=2, color="gray60") +
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40") +
   geom_vline(xintercept = 223, linetype=2, color="gray40") +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, size=0.5) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, alpha=0.8, span=0.15) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(R, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), size=1.3, alpha=1) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   # scale_x_date(name = NULL, 
   #              breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
   #              labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)), limits = c(-20, 0), breaks = seq(-20, 0, 5)) +
   # ggtitle("Ecosystem Respiration") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         # legend.position = c(0.16, 0.16),
         legend.position = "none", 
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line"), size=rel(1.1)),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(file = "Re.png")



#--
# GPP
#--

# 1 panel (blue & red)
windows(height=4, width=5.5)
gpp = 
ggplot(pdat,
       aes(x = doy, y = GPP)) +
   #
   # geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
   #            aes(xintercept = date), linetype=2, color="gray60") +
   geom_vline(xintercept = c(176.5, 211.5), linetype=1, color="gray40") +
   geom_vline(xintercept = 223, linetype=2, color="gray40") +
   annotate(geom = 'rect', xmin = 185, xmax = 190, ymin = -Inf, ymax = Inf, fill = 'gray75') +
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.4, size=0.5) +
   # treatment mean
   # stat_smooth(aes(color = trt_nutrients), geom="line", size=1.5, alpha=0.8, span=0.15) +
   geom_line(data = ~.x %>% group_by(trt_nutrients, doy) %>% summarize(mean = mean(GPP, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = mean, color = trt_nutrients), size=1.3, alpha=1) +
   #
   scale_color_manual(name = NULL, breaks = pulse_breaks, values = pulse_color, labels = pulse_labs) +
   # scale_x_date(name = NULL, 
   #              breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
   #              labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_x_continuous(name = "", limits = c(142, 242), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(GPP~(mg~O[2]~L^-1~d^-1)), limits = c(0, 20), breaks = seq(0, 20, 5)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.15, 0.88),
         legend.background = element_blank(), 
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line"), size=rel(1.1)),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(file = "GPP.png")



## 3-panel all metabolism together
windows(height=3.5*3, width=5); plot_grid(gpp, re, nep, ncol=1, align='v', labels="AUTO", label_size=13, label_y=0.99, label_x=0.01)

# ggsave(file = "metabolism.png", height=3.5*3, width=5, units = "in")



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


