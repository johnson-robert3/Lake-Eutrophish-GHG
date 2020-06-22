#~~~
# Script for making GHG figures
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)


##__Lake GHG Concentrations

# METHANE
windows(height=5, width=7)
ggplot(lake_conc, aes(x = doy, y = ch4_lake)) +
   # data
   geom_line(aes(alpha = pond_id), size=1.5, color="seagreen3") +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = pond_id), size=4, color="seagreen3") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~(mu*M)),
                      expand = expansion(mult=0.1)) +
   # aesthetics
   theme_classic()


# NITROUS OXIDE
windows(height=5, width=7)
ggplot(lake_conc, aes(x = doy, y = n2o_lake)) +
   # data
   geom_line(aes(alpha = pond_id), size=1.5, color="dodgerblue3") +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = pond_id), size=4, color="dodgerblue3") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Nitrous~oxide~(mu*M)),
                      expand = expansion(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()


##__Methanogenesis Potential

# METHANE
windows(height=5, width=7)
ggplot(methano_rates %>%
          mutate(doy = case_when(.$pond_id=="A" ~ doy-0.2,
                                 .$pond_id=="B" ~ doy-0.1,
                                 .$pond_id=="C" ~ doy,
                                 .$pond_id=="D" ~ doy+0.1,
                                 .$pond_id=="E" ~ doy+0.2,
                                 .$pond_id=="F" ~ doy+0.3)),
       aes(x = doy, y = ch4_rate)) +
   # data
   geom_line(aes(alpha = pond_id), size=1.5, color="firebrick2") +
   geom_errorbar(aes(x = doy, ymin = ch4_rate - sd_ch4_rate, ymax = ch4_rate + sd_ch4_rate), 
                 color="gray60", width=0) +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = pond_id), size=4, color="firebrick2") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expansion(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~production~(mu*mol~ml^-1*(s+w)~h^-1)),
                      expand = expansion(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()


