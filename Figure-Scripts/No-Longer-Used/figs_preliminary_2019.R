# Preliminary figures to view Hort Farm pond data


# select the Hort Farm data
farm_data = lake_conc %>%
   # add factor columns nutrient and fish additions
   mutate(add_nuts = c("reference", "reference", "reference", "+ nuts", "+ nuts", "+ nuts"),
          add_fish = c("none", "high", "low", "none", "low", "high"))


# Methane

png(file="Figures/2019_CH4-conc_prelim.png")
# windows()

ggplot(farm_data) +
   geom_col(aes(x = factor(add_nuts),
                y = ch4_lake,
                fill = add_fish),
            position="dodge2") +
   scale_x_discrete(limits = c("reference", "+ nuts"),
                    labels = c("Reference", "+ Nutrients")) +
   scale_fill_manual(limits = c("none", "low", "high"),
                     values = c("gray60", "seagreen3", "seagreen4"),
                     name = "Fish density",
                     labels = c("None", "Low", "High")) +
   xlab("") +
   ylab("Methane (uM)") +
   theme_classic() +
   theme(axis.text.y = element_text(size=11),
         axis.text.x = element_text(size=12, face="bold"),
         axis.title.y = element_text(size=12, face="bold"))

dev.off()


# Nitrous oxide

png(file="Figures/2019_N2O-conc_prelim.png")
# windows()

ggplot(farm_data) +
   geom_col(aes(x = factor(add_nuts),
                y = n2o_lake,
                fill = add_fish),
            position="dodge2") +
   scale_x_discrete(limits = c("reference", "+ nuts"),
                    labels = c("Reference", "+ Nutrients")) +
   scale_fill_manual(limits = c("none", "low", "high"),
                     values = c("gray60", "seagreen3", "seagreen4"),
                     name = "Fish density",
                     labels = c("None", "Low", "High")) +
   xlab("") +
   ylab("Nitrous Oxide (uM)") +
   theme_classic() +
   theme(axis.text.y = element_text(size=11),
         axis.text.x = element_text(size=12, face="bold"),
         axis.title.y = element_text(size=12, face="bold"))

dev.off()


