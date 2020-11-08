#~~~
# Exploratory figs to view limno variables
# By: Robert Johnson
#~~~


library(slider)


#===
# Sonde Profile Data
#===

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



#===
# Field Samples
#===


#--
# Nitrogen
#--

## TN

ggplot(limno_field_data %>% filter(!(is.na(tn))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(tn = mean(tn, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = tn)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()


## NOx

ggplot(limno_field_data %>% filter(!(is.na(nox))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(nox = mean(nox, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = nox)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()


## NHx

ggplot(limno_field_data %>% filter(!(is.na(nhx))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(nhx = mean(nhx, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = nhx)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()


#--
# Phosphorus
#--

## TP

ggplot(limno_field_data %>% filter(!(is.na(tp))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(tp = mean(tp, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = tp)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()


## SRP

ggplot(limno_field_data %>% filter(!(is.na(srp))) %>% left_join(pond_data) %>%
          group_by(trt_nutrients, doy) %>%
          summarize(srp = mean(srp, na.rm=T)) %>%
          ungroup(),
       aes(x = doy, y = srp)) +
   geom_line(aes(color = trt_nutrients, group = trt_nutrients), size=1.25) +
   geom_point(aes(color = trt_nutrients), shape=16, size=2) +
   scale_color_manual(values = c("no" = "cornflowerblue", "yes" = "seagreen3"),
                      labels = c("no" = "Ref", "yes" = "Pulse")) +
   scale_x_continuous(limits = c(140, 242)) +
   geom_vline(xintercept = c(176, 211), color="gray40", linetype=2) +
   theme_classic()


