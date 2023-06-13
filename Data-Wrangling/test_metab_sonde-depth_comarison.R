

# comparing daily metabolism estimates when using Salinity from 10-30cm vs 5-50cm

metab_10 = read_csv("metabolism_10_30.csv")

metab_50 = read_csv("metabolism_5_50.csv")


# combine

test = full_join(metab_10 %>% rename_with(.fn = ~paste(., "10", sep="_"), .cols = GPP:NEP),
                 metab_50 %>% rename_with(.fn = ~paste(., "50", sep="_"), .cols = GPP:NEP))

# correlation
cor.test(test$GPP_10, test$GPP_50)
cor.test(test$R_10, test$R_50)
cor.test(test$NEP_10, test$NEP_50)


# without erroneous estimates

test2 = test %>% 
   filter(!(GPP_10 < 0 | GPP_50 < 0)) %>%
   filter(!(R_10 > 0 | R_50 > 0))

# correlation
cor.test(test2$GPP_10, test2$GPP_50)
cor.test(test2$R_10, test2$R_50)
cor.test(test2$NEP_10, test2$NEP_50)


# fig

# gpp
windows(); ggplot(test2) +
   #
   geom_point(aes(x = GPP_10, y = GPP_50), size=1.5) +
   geom_abline(intercept = 0, slope = 1, linetype=2, color="gray50") +
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()


# r
windows(); ggplot(test2) +
   #
   geom_point(aes(x = R_10, y = R_50), size=1.5) +
   geom_abline(intercept = 0, slope = 1, linetype=2, color="gray50") +
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()


# nep 
windows(); ggplot(test2) +
   #
   geom_point(aes(x = NEP_10, y = NEP_50), size=1.5) +
   geom_abline(intercept = 0, slope = 1, linetype=2, color="gray50") +
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()





