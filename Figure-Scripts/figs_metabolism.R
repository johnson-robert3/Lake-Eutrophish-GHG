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

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
n.high =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="high"),
       aes(x = doy, y = NEP)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
n.int =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = NEP)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
n.low =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="low"),
       aes(x = doy, y = NEP)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "Low (B, F)")


# 3-panel
windows(height=10, width=6)

n.high / n.int / n.low

# ggsave(filename = "Figures/NEP_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/NEP_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
n.ref =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="no") %>%
          group_by(pond_id) %>%
          mutate(roll_nep = slide_dbl(NEP, mean, .before=2)) %>%
          ungroup(),
       aes(x = doy, y = roll_nep)) +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
n.pul =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="yes") %>%
          group_by(pond_id) %>%
          mutate(roll_nep = slide_dbl(NEP, mean, .before=2)) %>%
          ungroup(),
       aes(x = doy, y = roll_nep)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

n.ref / n.pul

# ggsave(filename = "Figures/NEP_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/roll-NEP_np-trt.png", height=7, width=6, units="in")

}


# 1 panel (blue & red)
windows(height=4, width=5.5)
ggplot(pdat,
       aes(x = date, y = NEP)) +
   #
   geom_hline(yintercept=0, linetype=2, color="gray60", size=1) +
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
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-8, 8), breaks = seq(-8, 8, 2)) +
   ggtitle("Net Ecosystem Production") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.16, 0.16),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename = "NEP.png")



#--
# Re
#--

## 3-panel: by food web ----
{# Comparing between nutrient treatments within each food web treatment

# HIGH B-P
# windows()
r.high =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="high"),
       aes(x = doy, y = R)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
r.int =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = R)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
r.low =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="low"),
       aes(x = doy, y = R)) %>%
   fig_aes_fw() +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "Low (B, F)")


# 3-panel
windows(height=10, width=6)

r.high / r.int / r.low

# ggsave(filename = "Figures/R_3panel.png", height=10, width=6, units="in")
ggsave(filename = "Figures/new-figs/R_fw-trt.png", height=10, width=6, units="in")

}


## 2-panel: by nutrients ----
{# Comparing between food web treatments within each nutrient treatment

# REFERENCE
# windows()
r.ref =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="no") %>%
          group_by(pond_id) %>%
          mutate(roll_re = slide_dbl(R, mean, .before=2)) %>%
          ungroup(),
       aes(x = doy, y = roll_re)) +
   geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue") +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="cornflowerblue", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="cornflowerblue", color="royalblue") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
r.pul =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="yes") %>%
          group_by(pond_id) %>%
          mutate(roll_re = slide_dbl(R, mean, .before=2)) %>%
          ungroup(),
       aes(x = doy, y = roll_re)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   geom_hline(yintercept = 0, linetype=3, color="gray40") +
   geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3") +
   # geom_line(aes(alpha = trt_fish), size=1.25, color="seagreen3", show.legend=F) +
   # geom_point(size=4, color="white") +
   # geom_point(aes(alpha = trt_fish), shape=21, size=4, fill="seagreen3", color="seagreen") +
   #
   scale_alpha_manual(name = "Benthic-Pelagic \nCoupling",
                      breaks = c("high", "medium", "low"),
                      values = c("high" = 0.9, "medium" = 0.6, "low" = 0.3),
                      labels = c("high" = "High", "medium" = "Intermediate", "low" = "Low")) +
   scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

r.ref / r.pul

# ggsave(filename = "Figures/R_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/roll-R_np-trt.png", height=7, width=6, units="in")

}


# 1 panel (blue & red)
windows(height=4, width=5.5)
ggplot(pdat,
       aes(x = date, y = R)) +
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
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-20, 0), breaks = seq(-20, 0, 5)) +
   ggtitle("Ecosystem Respiration") +
   #
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         legend.position = c(0.16, 0.16),
         axis.ticks.length = unit(0.3, 'line'),
         axis.text = element_text(color='black', size=rel(1)),
         axis.text.x = element_text(hjust=0.2, margin = margin(t=0.5, unit='line')),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line"), size=rel(1.1)))

# ggsave(filename = "Re.png")




# all ponds and variables
# windows(width=12)
# ggplot(metab_mle %>%
#           filter(!(GPP<0) & !(R>0)) %>%
#           pivot_longer(cols = GPP:NEP,
#                        names_to = "variable",
#                        values_to = "rate",
#                        values_drop_na = T)) +
#    geom_col(aes(x = doy, y = rate)) +
#    geom_hline(yintercept=0, linetype=2) +
#    facet_grid(rows = vars(variable), cols = vars(pond_id), scales="free_y") +
#    theme_classic()


# just NEP
# windows(width=12)
# ggplot(metab_mle %>%
#           filter(!(GPP<0) & !(R>0)) %>%
#           pivot_longer(cols = GPP:NEP,
#                        names_to = "variable",
#                        values_to = "rate",
#                        values_drop_na = T) %>%
#           filter(variable=="NEP")) +
#    geom_col(aes(x = doy, y = rate)) +
#    geom_hline(yintercept=0, linetype=2) +
#    facet_wrap(facets = vars(pond_id), ncol=3) +
#    theme_classic()

