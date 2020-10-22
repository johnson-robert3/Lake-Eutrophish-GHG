#~~~
# Figures of pond ecosystem metabolism
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)


# Function for 3-panel aesthetics
metab_3panel = function(.fig) {
   
   .fig +
      # pulse
      geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
      geom_hline(yintercept = 0, linetype=3, color="gray40") +
      # data as points
      # geom_line(aes(group = trt_nutrients), size=0.5) +
      # geom_point(color="white", fill="white", shape=21, size=3) +
      # geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
      # scale_fill_manual(name = NULL,
      #                   labels = c("no" = "Ambient", "yes" = "Pulsed"),
      #                   values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
      # data as lines
      geom_line(aes(group = trt_nutrients, color = trt_nutrients), size=1.2, alpha=0.7) +
      scale_color_manual(name = NULL,
                         labels = c("no" = "Reference", "yes" = "Pulsed"),
                         values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
      scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
      theme_classic()
   
}


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
   metab_3panel() +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
n.int =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = NEP)) %>%
   metab_3panel() +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
n.low =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="low"),
       aes(x = doy, y = NEP)) %>%
   metab_3panel() +
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
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = NEP)) +
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
   # scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
n.pul =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = NEP)) +
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
   # scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(NEP~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-12, 15),
                      breaks = seq(-10, 15, 5)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

n.ref / n.pul

# ggsave(filename = "Figures/NEP_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/NEP_np-trt.png", height=7, width=6, units="in")

}


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
   metab_3panel() +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "High (C, E)")


# INTERMEDIATE B-P
# windows()
r.int =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="medium"),
       aes(x = doy, y = R)) %>%
   metab_3panel() +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "Intermediate (A, D)")


# LOW B-P
# windows()
r.low =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_fish=="low"),
       aes(x = doy, y = R)) %>%
   metab_3panel() +
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
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="no"),
       aes(x = doy, y = R)) +
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
   # scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "Reference") +
   theme_classic()


# PULSED
# windows()
r.pul =
ggplot(metab_mle %>% left_join(pond_data) %>% filter(GPP>0 & R<0) %>% filter(trt_nutrients=="yes"),
       aes(x = doy, y = R)) +
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
   # scale_y_continuous(name = expression(Methanogenesis~potential~(mu*mol~g^-1~h^-1)), expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(R~(mg~O[2]~L^-1~d^-1)),
                      limits = c(-25, 0)) +
   labs(title = "Pulsed") +
   theme_classic()


# 2-panel
windows(height=7, width=6)

r.ref / r.pul

# ggsave(filename = "Figures/R_2panel.png", height=7, width=6, units="in")
ggsave(filename = "Figures/new-figs/R_np-trt.png", height=7, width=6, units="in")

}






# all ponds and variables
windows(width=12)
ggplot(metab_mle %>%
          filter(!(GPP<0) & !(R>0)) %>%
          pivot_longer(cols = GPP:NEP,
                       names_to = "variable",
                       values_to = "rate",
                       values_drop_na = T)) +
   geom_col(aes(x = doy, y = rate)) +
   geom_hline(yintercept=0, linetype=2) +
   facet_grid(rows = vars(variable), cols = vars(pond_id), scales="free_y") +
   theme_classic()


# just NEP
windows(width=12)
ggplot(metab_mle %>%
          filter(!(GPP<0) & !(R>0)) %>%
          pivot_longer(cols = GPP:NEP,
                       names_to = "variable",
                       values_to = "rate",
                       values_drop_na = T) %>%
          filter(variable=="NEP")) +
   geom_col(aes(x = doy, y = rate)) +
   geom_hline(yintercept=0, linetype=2) +
   facet_wrap(facets = vars(pond_id), ncol=3) +
   theme_classic()

