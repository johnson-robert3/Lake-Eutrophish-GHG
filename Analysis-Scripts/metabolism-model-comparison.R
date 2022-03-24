

# comparing metabolism results between metab.mle (process error and observation error) and metab.ols and metab.kalman

# models
{

# metab.mle using Observation Error (default)
test.oe = metab_data %>%
   filter(pond_id=="C") %>%
   filter(!(doy==231)) %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "OE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup()

# metab.mle using Process Error
test.pe = metab_data %>%
   filter(pond_id=="C") %>%
   filter(!(doy==231)) %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.mle(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       error.type = "PE",
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup()

# metab.ols
test.ols = metab_data %>%
   filter(pond_id=="C") %>%
   filter(!(doy==231)) %>%
   filter(n == max(n)) %>%
   group_by(doy) %>%
   nest() %>%
   mutate(metab = map(data, ~metab.ols(do.obs = .$do,
                                       do.sat = .$o2_eq_sat,
                                       k.gas = .$k_gas,
                                       z.mix = .$z_mix,
                                       irr = .$par,
                                       wtr = .$temp,
                                       datetime = .$date_time)),
          GPP = map(metab, ~pluck(., 2, "GPP")),
          R = map(metab, ~pluck(., 2, "R")),
          NEP = map(metab, ~pluck(., 2, "NEP"))) %>%
   select(doy, GPP, R, NEP) %>%
   unnest(cols = c(GPP, R, NEP)) %>%
   ungroup()

# metab.kalman
test.kal = metab_data %>%
   filter(pond_id=="C") %>%
   filter(!(doy==231)) %>%
   filter(n == max(n)) %>%
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

# metab.bayesian  [need JAGS installed]
# test.bay = metab_data %>%
#    filter(pond_id=="B") %>%
#    filter(!(doy==151)) %>%
#    filter(n == max(n)) %>%
#    group_by(doy) %>%
#    nest() %>%
#    mutate(metab = map(data, ~metab.bayesian(do.obs = .$do,
#                                        do.sat = .$o2_eq_sat,
#                                        k.gas = .$k_gas,
#                                        z.mix = .$z_mix,
#                                        irr = .$par,
#                                        wtr = .$temp,
#                                        datetime = .$date_time)),
#           GPP = map(metab, ~pluck(., 3, "GPP")),
#           R = map(metab, ~pluck(., 3, "R")),
#           NEP = map(metab, ~pluck(., 3, "NEP"))) %>%
#    select(doy, GPP, R, NEP) %>%
#    unnest(cols = c(GPP, R, NEP)) %>%
#    ungroup() %>%
#    mutate(pond_id = rep_len("B", n())) %>%
#    relocate(pond_id)

}


# combine all
test = test.oe %>% mutate(mod = rep("mle.OE", nrow(.))) %>%
   bind_rows(test.pe %>% mutate(mod = rep("mle.PE", nrow(.)))) %>%
   bind_rows(test.ols %>% mutate(mod = rep("OLS", nrow(.)))) %>%
   bind_rows(test.kal %>% mutate(mod = rep("Kalman", nrow(.))))


# how many negative GPP days?
test %>% filter(GPP < 0) %>% count(mod)

# how many positive R days?
test %>% filter(R > 0) %>% count(mod)

# are they the same days?
test %>% filter(GPP < 0 | R > 0) %>% View  # not all same


# compare model estimates

# GPP
# windows()
gpp =
ggplot(test) +
   geom_line(aes(x = doy, y = GPP, group = mod, color = mod)) +
   scale_color_manual(breaks = c('mle.OE', 'mle.PE', 'OLS', 'Kalman'), 
                      values = c('mle.OE'="cornflowerblue", 'mle.PE'="seagreen2", 'OLS'="firebrick2", 'Kalman'='darkorchid1')) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# R
# windows()
re = 
ggplot(test) +
   geom_line(aes(x = doy, y = R, group = mod, color = mod)) +
   scale_color_manual(breaks = c('mle.OE', 'mle.PE', 'OLS', 'Kalman'), 
                      values = c('mle.OE'="cornflowerblue", 'mle.PE'="seagreen2", 'OLS'="firebrick2", 'Kalman'='darkorchid1')) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# NEP
# windows()
nep = 
ggplot(test) +
   geom_line(aes(x = doy, y = NEP, group = mod, color = mod)) +
   scale_color_manual(breaks = c('mle.OE', 'mle.PE', 'OLS', 'Kalman'), 
                      values = c('mle.OE'="cornflowerblue", 'mle.PE'="seagreen2", 'OLS'="firebrick2", 'Kalman'='darkorchid1')) +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()


# all

windows(height=10, width=8)
cowplot::plot_grid(gpp, re, nep, align = "v", ncol=1)

# ggsave(filename = "Figures/metabolism-model-estimate-comparison_C.png", height=10, width=8, units="in")



