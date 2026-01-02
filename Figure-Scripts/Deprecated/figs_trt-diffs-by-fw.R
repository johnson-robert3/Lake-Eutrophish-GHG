
### Viewing differences between pulsed and reference ponds based on food web treatment


# function to calculate difference between pulsed and reference for each food web treatment

fw_diff = function(.dat, .var) {
   
   .dat %>%
      drop_na({{.var}}) %>%  # enclose within {{}} to specify the argument supports <tidy-select>
      select(pond_id, trt_nutrients, trt_fish, doy, {{.var}}) %>%
      pivot_wider(id_cols = c(trt_fish, doy),
                  names_from = trt_nutrients,
                  values_from = {{.var}}) %>%
      mutate(diff = pulsed - reference)
}



# CH4 flux
windows(width=5, height=3)
ggplot(fdat %>% fw_diff(ch4_flux)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_point(aes(x = doy, y = diff, color = trt_fish)) +
   geom_line(aes(x = doy, y = diff, color = trt_fish)) +
   labs(y = expression(Diff.~CH[4]~flux~(mmol~m^{-2}~d^{-1})),
        x = "Day of year",
        color = "FW") +
   scale_color_manual(breaks = fish_breaks, values = fish_color) +
   theme_bw()


# CO2 flux
windows(width=5, height=3)
ggplot(fdat %>% fw_diff(co2_flux)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_point(aes(x = doy, y = diff, color = trt_fish)) +
   geom_line(aes(x = doy, y = diff, color = trt_fish)) +
   labs(y = expression(Diff.~CO[2]~flux~(mmol~m^{-2}~d^{-1})),
        x = "Day of year",
        color = "FW") +
   scale_color_manual(breaks = fish_breaks, values = fish_color) +
   theme_bw()


# Bottom DO saturation
windows(width=5, height=3)
ggplot(fdat %>% fw_diff(bottom_do_sat)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_point(aes(x = doy, y = diff, color = trt_fish)) +
   geom_line(aes(x = doy, y = diff, color = trt_fish)) +
   labs(y = expression(Diff.~bottom~DO~('%')),
        x = "Day of year",
        color = "FW") +
   scale_color_manual(breaks = fish_breaks, values = fish_color) +
   theme_bw()


# Bottom temperature
windows(width=5, height=3)
ggplot(fdat %>% fw_diff(bottom_temp)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_point(aes(x = doy, y = diff, color = trt_fish)) +
   geom_line(aes(x = doy, y = diff, color = trt_fish)) +
   labs(y = expression(Diff.~bottom~temp~(degree*C)),
        x = "Day of year",
        color = "FW") +
   scale_color_manual(breaks = fish_breaks, values = fish_color) +
   theme_bw()


# TN
windows(width=5, height=3)
ggplot(fdat %>% fw_diff(tn)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_point(aes(x = doy, y = diff, color = trt_fish)) +
   geom_line(aes(x = doy, y = diff, color = trt_fish)) +
   labs(y = expression(TN~(mg~L^{-1})),
        x = "Day of year",
        color = "FW") +
   scale_color_manual(breaks = fish_breaks, values = fish_color) +
   theme_bw()


# TP
windows(width=5, height=3)
ggplot(fdat %>% fw_diff(tp)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_point(aes(x = doy, y = diff, color = trt_fish)) +
   geom_line(aes(x = doy, y = diff, color = trt_fish)) +
   labs(y = expression(TP~(mu*g~L^{-1})),
        x = "Day of year",
        color = "FW") +
   scale_color_manual(breaks = fish_breaks, values = fish_color) +
   theme_bw()


# NEP
windows(width=5, height=3)
ggplot(fdat %>% fw_diff(NEP)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
   geom_point(aes(x = doy, y = diff, color = trt_fish)) +
   geom_line(aes(x = doy, y = diff, color = trt_fish)) +
   labs(y = expression(NEP~(mg~O[2]~L^{-1}~d^{-1})),
        x = "Day of year",
        color = "FW") +
   scale_color_manual(breaks = fish_breaks, values = fish_color) +
   theme_bw()


