
library(slider)

# compare results from Kalman filter metabolism model using different inputs of DO data


# kalman models

# raw data
kalman.raw = metab_data %>%
   filter(pond_id=="A") %>%
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
   filter(pond_id=="A") %>%
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
   filter(pond_id=="A") %>%
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
   filter(pond_id=="A") %>%
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
   bind_rows(kalman.roll %>% mutate(dat = rep("3-hr_win", nrow(.)))) %>%
   bind_rows(kalman.corr %>% mutate(dat = rep("Interp", nrow(.)))) %>%
   bind_rows(kalman.roll.corr %>% mutate(dat = rep("3-hr_interp", nrow(.))))



# compare model estimates

# GPP
# windows()
gpp =
ggplot(test) +
   
   # geom_line(aes(x = doy, y = GPP, group = dat, color = dat)) +
   # scale_color_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'),
   #                    values = c('Raw'="cornflowerblue", '3-hr_win'="seagreen2", 'Interp'="firebrick2", '3-hr_interp'='darkorchid1')) +
   
   geom_col(aes(x = doy, y = GPP, group = dat, fill = dat), position = position_dodge(preserve="single")) +
   scale_fill_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'), 
                      values = c('Raw'="#940A37", '3-hr_win'="#E26241", 'Interp'="#4CA1A3", '3-hr_interp'='#A5E1AD')) +
   
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# R
# windows()
re =
ggplot(test) +
   
   # geom_line(aes(x = doy, y = R, group = dat, color = dat)) +
   # scale_color_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'),
   #                    values = c('Raw'="cornflowerblue", '3-hr_win'="seagreen2", 'Interp'="firebrick2", '3-hr_interp'='darkorchid1')) +
   
   geom_col(aes(x = doy, y = R, group = dat, fill = dat), position = position_dodge(preserve="single")) +
   scale_fill_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'), 
                      values = c('Raw'="#940A37", '3-hr_win'="#E26241", 'Interp'="#4CA1A3", '3-hr_interp'='#A5E1AD')) +
   
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# NEP
# windows()
nep =
ggplot(test) +
   
   # geom_line(aes(x = doy, y = NEP, group = dat, color = dat)) +
   # scale_color_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'),
   #                    values = c('Raw'="cornflowerblue", '3-hr_win'="seagreen2", 'Interp'="firebrick2", '3-hr_interp'='darkorchid1')) +
   
   geom_col(aes(x = doy, y = NEP, group = dat, fill = dat), position = position_dodge(preserve="single")) +
   scale_fill_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'),
                      values = c('Raw'="#940A37", '3-hr_win'="#E26241", 'Interp'="#4CA1A3", '3-hr_interp'='#A5E1AD')) +
   
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# all

windows(height=10, width=8)
cowplot::plot_grid(gpp, re, nep, align = "v", ncol=1)


