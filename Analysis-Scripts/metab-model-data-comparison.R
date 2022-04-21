

# compare results from Kalman filter metabolism model using different inputs of DO data


# Process the minidot DO data
# run this first, then create the metab_data dataframe from the metabolism-calcs script

mdat = minidot %>% 
   group_by(pond_id) %>% 
   arrange(date_time, .by_group=TRUE) %>%
   # add change in DO concentration from previous point
   mutate(delta_do = do - lag(do)) %>%
   # remove the offending point along with the next 3 hours (5 points) of data any time DO drops more than 2.0 mg/l
   mutate(drop_pt = case_when(delta_do <= -2 ~ 1,
                              lag(delta_do, n=1)  <= -2 ~ 1,
                              lag(delta_do, n=2)  <= -2 ~ 1,
                              lag(delta_do, n=3)  <= -2 ~ 1,
                              lag(delta_do, n=4)  <= -2 ~ 1,
                              lag(delta_do, n=5)  <= -2 ~ 1,
                              TRUE ~ 0),
          new_do = case_when(drop_pt == 1 ~ -9999,
                             drop_pt == 0 ~ do),
          new_do = na_if(new_do, -9999),
          # linearly interpolate across removed points
          new_do = zoo::na.approx(new_do, na.rm=FALSE)) %>%
   # rolling window data (3-hour)
   mutate(
      # rolling window on original/raw data
      roll_do = slide_dbl(do, ~mean(.), .before=3, .after=2, .complete=F),
      # rolling window on new, corrected DO data with large drops removed/interpolated
      roll_new_do = slide_dbl(new_do, ~mean(.), .before=3, .after=2, .complete=F)) %>%
   ungroup()


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
   mutate(metab = map(data, ~metab.kalman(do.obs = .$new_do,
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
   mutate(metab = map(data, ~metab.kalman(do.obs = .$roll_new_do,
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
   geom_line(aes(x = doy, y = GPP, group = dat, color = dat)) +
   scale_color_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'), 
                      values = c('Raw'="cornflowerblue", '3-hr_win'="seagreen2", 'Interp'="firebrick2", '3-hr_interp'='darkorchid1')) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# R
# windows()
re = 
ggplot(test) +
   geom_line(aes(x = doy, y = R, group = dat, color = dat)) +
   scale_color_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'), 
                      values = c('Raw'="cornflowerblue", '3-hr_win'="seagreen2", 'Interp'="firebrick2", '3-hr_interp'='darkorchid1')) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# NEP
# windows()
nep =
ggplot(test) +
   geom_line(aes(x = doy, y = NEP, group = dat, color = dat)) +
   scale_color_manual(breaks = c('Raw', '3-hr_win', 'Interp', '3-hr_interp'), 
                      values = c('Raw'="cornflowerblue", '3-hr_win'="seagreen2", 'Interp'="firebrick2", '3-hr_interp'='darkorchid1')) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# all

windows(height=10, width=8)
cowplot::plot_grid(gpp, re, nep, align = "v", ncol=1)


