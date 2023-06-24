
# Figures of variability


library(ggbeeswarm)



## CO2 flux

windows()
ggplot(lake_flux) +
   geom_beeswarm(aes(pond_id, co2_flux), size=2, priority="random") +
   theme_classic()


windows()
a =
ggplot(lake_flux) +
   geom_boxplot(aes(x = pond_id, y = co2_flux)) +
   theme_classic()



## CH4 flux

windows()
ggplot(lake_flux) +
   geom_beeswarm(aes(pond_id, ch4_flux), size=2, priority="random") +
   theme_classic()


windows()
b =
ggplot(lake_flux) +
   geom_boxplot(aes(x = pond_id, y = ch4_flux)) +
   theme_classic()



## N2O flux

windows()
ggplot(lake_flux) +
   geom_beeswarm(aes(pond_id, n2o_flux), size=2, priority="random") +
   theme_classic()


windows()
c =
ggplot(lake_flux) +
   geom_boxplot(aes(x = pond_id, y = n2o_flux)) +
   theme_classic()


windows(height=10, width=6); a/b/c




# CV by pulse period

windows()
ggplot(cv_period) +
   geom_path(aes(x = period, y = n2o_flux, color = pond_id)) +
   geom_point(aes(x = period, y = n2o_flux, color = pond_id)) +
   theme_classic()




