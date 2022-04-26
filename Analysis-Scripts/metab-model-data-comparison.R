
library(viridis)

# compare results from Kalman filter metabolism model using different inputs of DO data


# kalman models

# raw data
kalman.raw = metab_data %>%
   filter(pond_id=="F") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup()

# rolling window on raw data
kalman.roll = metab_data %>%
   filter(pond_id=="F") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$roll_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup()

# corrected/interpolated data 
kalman.corr = metab_data %>%
   filter(pond_id=="F") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup()

# rolling window on corrected/interpolated data
kalman.roll.corr = metab_data %>%
   filter(pond_id=="F") %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.kalman(do.obs = .$roll_corr_do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 3, "GPP")),
          R = map(metab, ~pluck(., 3, "R")),
          NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup()


# combine all
test = kalman.raw %>% mutate(dat = rep("Raw", nrow(.))) %>%
   bind_rows(kalman.roll %>% mutate(dat = rep("Raw-roll", nrow(.)))) %>%
   bind_rows(kalman.corr %>% mutate(dat = rep("Corr", nrow(.)))) %>%
   bind_rows(kalman.roll.corr %>% mutate(dat = rep("Corr-roll", nrow(.))))



# compare model estimates

mybreaks = c('Raw', 'Raw-roll', 'Corr', 'Corr-roll')
myfill = c('Raw'="#CD1818", 
           'Raw-roll'="#F3950D",
           'Corr'="#0f3460", 
           'Corr-roll'='#51c2d5')

# GPP
# windows()
gpp =
ggplot(test) +
   
   # geom_line(aes(x = doy, y = GPP, group = dat, color = dat)) +
   # scale_color_manual(breaks = mybreaks,
   #                    values = myfill) +
   
   geom_col(aes(x = doy, y = GPP, group = dat, fill = dat), position = position_dodge(preserve="single")) +
   scale_fill_manual(breaks = mybreaks,
                     values = myfill) +
   
   scale_x_continuous(breaks = seq(140, 240, 10)) +
   # labs(title = "Pond F",
   #      subtitle = "1.0 mg/l cutoff") +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# R
# windows()
re =
ggplot(test) +
   
   # geom_line(aes(x = doy, y = R, group = dat, color = dat)) +
   # scale_color_manual(breaks = mybreaks,
   #                    values = myfill) +
   
   geom_col(aes(x = doy, y = R, group = dat, fill = dat), position = position_dodge(preserve="single")) +
   scale_fill_manual(breaks = mybreaks,
                     values = myfill) +
   
   scale_x_continuous(breaks = seq(140, 240, 10)) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# NEP
# windows()
nep =
ggplot(test) +
   
   # geom_line(aes(x = doy, y = NEP, group = dat, color = dat)) +
   # scale_color_manual(breaks = mybreaks,
   #                    values = myfill) +
   
   geom_col(aes(x = doy, y = NEP, group = dat, fill = dat), position = position_dodge(preserve="single")) +
   scale_fill_manual(breaks = mybreaks,
                     values = myfill) +
   
   scale_x_continuous(breaks = seq(140, 240, 10)) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# all

windows(height=8, width=12)
cowplot::plot_grid(gpp, re, nep, align = "v", ncol=1, rel_heights=c(1.2, 1, 1))


# ggsave(file="F_metab_1.0.png")

