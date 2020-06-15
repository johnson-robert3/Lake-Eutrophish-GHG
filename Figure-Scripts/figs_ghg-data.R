#~~~
# Script for making GHG figures
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)


##__Lake GHG Concentrations

# METHANE
windows(height=5, width=7)
ggplot(lake_conc) +
   geom_line(aes(x = doy, y = ch4_lake, alpha = pond_id), size=1.5, color="seagreen3") +
   geom_point(aes(x = doy, y = ch4_lake), size=4, color="white") +
   geom_point(aes(x = doy, y = ch4_lake, alpha = pond_id), size=4, color="seagreen3") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expand_scale(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~(mu*M)),
                      expand = expand_scale(mult=0.1)) +
   # aesthetics
   theme_classic()


# NITROUS OXIDE
windows(height=5, width=7)
ggplot(lake_conc, aes(x = doy, y = n2o_lake)) +
   geom_line(aes(alpha = pond_id), size=1.5, color="dodgerblue3") +
   geom_point(size=4, color="white") +
   geom_point(aes(alpha = pond_id), size=4, color="dodgerblue3") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expand_scale(mult=0.1)) +
   scale_y_continuous(name = expression(Nitrous~oxide~(mu*M)),
                      expand = expand_scale(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()


##__Methanogenesis Potential

# METHANE
windows(height=5, width=7)
ggplot(methano_rates) +
   geom_line(aes(x = doy, y = ch4_rate, alpha = pond_id), size=1.5, color="firebrick2") +
   geom_point(aes(x = doy, y = ch4_rate), size=4, color="white") +
   geom_point(aes(x = doy, y = ch4_rate, alpha = pond_id), size=4, color="firebrick2") +
   # scales and axes
   scale_alpha_manual(breaks = c("A", "B", "C", "D", "E", "F"),
                      values = c(0.25, 0.4, 0.55, 0.7, 0.85, 1),
                      name = "Pond") +
   scale_x_continuous(name = "DOY",
                      expand = expand_scale(mult=0.1)) +
   scale_y_continuous(name = expression(Methane~production~(mu*mol~ml^-1*(s+w)~h^-1)),
                      expand = expand_scale(mult=0.1)) +
   expand_limits(y = 0) +
   # aesthetics
   theme_classic()


