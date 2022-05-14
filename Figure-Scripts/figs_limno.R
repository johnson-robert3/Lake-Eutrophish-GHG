#~~~
# Exploratory figs to view limno variables
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

# 3 panel
{
# HIGH B-P
# windows()
l.high =
ggplot(rdat %>% filter(trt_fish=="high"), aes(x = doy, y = chla)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="high"), aes(x = doy, y = chla_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Chlorophyll~alpha~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# INTERMEDIATE B-P
# windows()
l.int =
ggplot(rdat %>% filter(trt_fish=="medium"), aes(x = doy, y = chla)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="medium"), aes(x = doy, y = chla_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Chlorophyll~alpha~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Intermediate (A, D)") +
   theme_classic()


# LOW B-P
# windows()
l.low =
ggplot(rdat %>% filter(trt_fish=="low"), aes(x = doy, y = chla)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="low"), aes(x = doy, y = chla_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Chlorophyll~alpha~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

l.high / l.int / l.low

}


# 2 panel
{
# REFERENCE
# windows()
a =
ggplot(rdat %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = chla)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), limits = c(0, 35)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
b =
ggplot(rdat %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = chla)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Chlorophyll~alpha~(mu*g~L^-1)), limits = c(0, 35)) +
   labs(title = "Pulsed") +
   theme_classic()


# figure
windows(height=7, width=6); a / b

}


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
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
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
# Phycocyanin
#--

# 3 panel
{
# HIGH B-P
# windows()
l.high =
ggplot(rdat %>% filter(trt_fish=="high"), aes(x = doy, y = phyco)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="high"), aes(x = doy, y = phyco_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Phycocyanin~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "High (C, E)") +
   theme_classic()


# INTERMEDIATE B-P
# windows()
l.int =
ggplot(rdat %>% filter(trt_fish=="medium"), aes(x = doy, y = phyco)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="medium"), aes(x = doy, y = phyco_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Phycocyanin~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Intermediate (A, D)") +
   theme_classic()


# LOW B-P
# windows()
l.low =
ggplot(rdat %>% filter(trt_fish=="low"), aes(x = doy, y = phyco)) +
# ggplot(limno_field_data %>% left_join(pond_data) %>% filter(trt_fish=="low"), aes(x = doy, y = phyco_rfu)) +
   # pulse
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   # lake
   geom_line(aes(group = trt_nutrients), size=0.5) +
   geom_point(color="white", fill="white", shape=21, size=3) +
   geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
   scale_fill_manual(name = NULL,
                     labels = c("no" = "Reference", "yes" = "Pulsed"),
                     values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), expand = expansion(mult=0.1)) +
   # scale_y_continuous(name = expression(Phycocyanin~(RFU)), expand = expansion(mult=0.1)) +
   labs(title = "Low (B, F)") +
   theme_classic()


# 3-panel
windows(height=10, width=6)

l.high / l.int / l.low

}


# 2 panel
{
# REFERENCE
# windows()
a =
ggplot(rdat %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = phyco)) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), limits = c(-0.5, 1)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
b =
ggplot(rdat %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = phyco)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   #
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   #
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Phycocyanin~(mu*g~L^-1)), limits = c(-0.5, 1)) +
   labs(title = "Pulsed") +
   theme_classic()


# figure
windows(height=7, width=6); a / b

}


#--
# Dissolved Oxygen
#--

## Surface DO  # doesn't work currently, surface DO not included in fdat dataset

# 1 panel (blue & red)
windows(height=4, width=5.5)
ggplot(pdat,
       aes(x = date, y = do)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(DO~(mg~L^-1))) +
   #
   ggtitle(expression(Surface~Water~Dissolved~O[2])) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.85, 0.85),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename = "surface-water-DO.png")


## Bottom water DO
windows(height=4, width=5.5)
ggplot(pdat,
       aes(x = date, y = bottom_do)) +
   #
   geom_hline(yintercept=0, linetype=3, color="gray60") +
   geom_vline(data = ~filter(.x, doy %in% c(176, 211)), 
              aes(xintercept = date), linetype=2, color="gray60") +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # geom_point(aes(color = trt_nutrients), size=1.5, alpha=0.4) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_date(name = NULL, 
                breaks = as_date(c('2020-06-01', '2020-06-15', '2020-07-01', '2020-07-15', '2020-08-01', '2020-08-15', '2020-09-01')), 
                labels = c('Jun 1', '', 'Jul 1', '', 'Aug 1', '', " ")) + 
   scale_y_continuous(name = expression(DO~(mg~L^-1))) +
   #
   ggtitle(expression(Bottom~Water~Dissolved~O[2])) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.85, 0.86),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename = "bottom-water-DO.png")


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
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
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
ggplot(fdat %>% filter(!(is.na(tn))) %>% left_join(pond_data),
       aes(x = doy, y = tn)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TN~(mg~L^-1)), limits = c(0, 1)) +
   ggtitle("Total Nitrogen") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.18, 0.87),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")))

# ggsave(filename="total-nitrogen.png", height=4, width=5.5, units='in')


# Reference
# a = 
# ggplot(limno_field_data %>% filter(!(is.na(tn))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
#        aes(x = doy, y = tn)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(TN~(mg~L^-1)), limits = c(0, 1)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Reference") +
#    theme_classic()

# Pulsed
# b = 
# ggplot(limno_field_data %>% filter(!(is.na(tn))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
#        aes(x = doy, y = tn)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(TN~(mg~L^-1)), limits = c(0, 1)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Pulsed") +
#    theme_classic()


# TN fig
# windows(height=7, width=6); a / b

# ggsave(filename = "Figures/new-figs/TN.png", height=7, width=6, units="in")


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
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
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


# Reference
# a = 
# ggplot(limno_field_data %>% filter(!(is.na(nox))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
#        aes(x = doy, y = nox)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(NO[x]~(mg~L^-1)), limits = c(0, 0.5)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Reference") +
#    theme_classic()

# Pulsed
# b = 
# ggplot(limno_field_data %>% filter(!(is.na(nox))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
#        aes(x = doy, y = nox)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(NO[x]~(mg~L^-1)), limits = c(0, 0.5)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Pulsed") +
#    theme_classic()


# NOx fig
# windows(height=7, width=6); a / b

# ggsave(filename = "Figures/new-figs/NOx.png", height=7, width=6, units="in")


## NHx (mg/L)
# windows()
# ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data) %>%
#           group_by(trt_nutrients, doy) %>%
#           summarize(nhx = mean(nhx, na.rm=T)) %>%
#           ungroup(),
#        aes(x = doy, y = nhx)) +
#    geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
#    geom_point(aes(color = trt_nutrients), shape=16, size=2) +
#    scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
#                       labels = c("no" = "Ref", "yes" = "Pulse")) +
#    scale_x_continuous(limits = c(140, 242)) +
#    lims(y = c(0, 0.1)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    theme_classic()

# Reference
# a = 
# ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
#        aes(x = doy, y = nhx)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(NH[x]~(mg~L^-1)), limits = c(0, 0.075)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Reference") +
#    theme_classic()

# Pulsed
# b = 
# ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
#        aes(x = doy, y = nhx)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(NH[x]~(mg~L^-1)), limits = c(0, 0.075)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Pulsed") +
#    theme_classic()


# NHx fig
# windows(height=7, width=6); a / b

# ggsave(filename = "Figures/new-figs/NHx.png", height=7, width=6, units="in")


#--
# Phosphorus
#--

## TP (ug/L)
windows(height=4, width=5.5)
ggplot(fdat %>% filter(!(is.na(tp))) %>% left_join(pond_data),
       aes(x = doy, y = tp)) +
   #
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   # pond data
   geom_line(aes(color = trt_nutrients, group = pond_id), alpha=0.3, size=1) +
   # treatment mean (loess smooth)
   geom_smooth(aes(color = trt_nutrients), size=1.5, alpha=0.8, se=F, span=0.15) +
   #
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
   scale_x_continuous(name = "Day of year", limits = c(140, 245), breaks = seq(140,240,20)) +
   scale_y_continuous(name = expression(TP~(mu*g~L^-1)), limits = c(0, 250)) +
   ggtitle("Total Phosphorus") +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.82, 0.87),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")))

# ggsave(filename="total-phosphorus.png", height=4, width=5.5, units='in')


# Reference
# a = 
# ggplot(limno_field_data %>% filter(!(is.na(tp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
#        aes(x = doy, y = tp)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(TP~(mu*g~L^-1)), limits = c(0, 250)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Reference") +
#    theme_classic()

# Pulsed
# b = 
# ggplot(limno_field_data %>% filter(!(is.na(tp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
#        aes(x = doy, y = tp)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(TP~(mu*g~L^-1)), limits = c(0, 250)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Pulsed") +
#    theme_classic()


# TP fig
# windows(height=7, width=6); a / b

# ggsave(filename = "Figures/new-figs/TP.png", height=7, width=6, units="in")


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
   scale_color_manual(name = NULL, breaks = nut_breaks, values = nut_color, labels = nut_labs) +
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



# Reference
# a = 
# ggplot(limno_field_data %>% filter(!(is.na(srp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="no"),
#        aes(x = doy, y = srp)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="cornflowerblue") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(SRP~(mu*g~L^-1)), limits = c(0, 30)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Reference") +
#    theme_classic()

# Pulsed
# b = 
# ggplot(limno_field_data %>% filter(!(is.na(srp))) %>% left_join(pond_data) %>% filter(trt_nutrients=="yes"),
#        aes(x = doy, y = srp)) +
#    #
#    geom_line(aes(alpha = trt_fish, group = pond_id), size=1.25, color="seagreen3") +
#    #
#    scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
#                       breaks = fish_breaks,
#                       values = fish_alpha,
#                       labels = fish_labs) +
#    scale_x_continuous(name = "DOY", limits = c(140, 242)) +
#    scale_y_continuous(name = expression(SRP~(mu*g~L^-1)), limits = c(0, 30)) +
#    geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
#    ggtitle("Pulsed") +
#    theme_classic()


# SRP fig
# windows(height=7, width=6); a / b
# 
# ggsave(filename = "Figures/new-figs/SRP.png", height=7, width=6, units="in")


