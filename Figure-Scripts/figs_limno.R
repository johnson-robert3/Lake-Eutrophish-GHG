#~~~
# Exploratory figs to view limno variables
# By: Robert Johnson
#~~~


library(slider)


# daily surface limno values from sonde profiles
ldat = left_join(sonde_surface, pond_data)


# rolling window data (3-day)
rdat = sonde_surface %>%
   mutate(across(temp:salinity, ~slide_dbl(., mean, .before=2, .complete=T))) %>%
   left_join(pond_data)



#--
# Chlorophyll
#--
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


#--
# Phycocyanin
#--
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





