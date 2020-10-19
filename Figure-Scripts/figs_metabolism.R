#~~~
# Figures of pond ecosystem metabolism
# By: Robert Johnson
#~~~


library(cowplot)
library(patchwork)


# all ponds and variables
windows(width=12)
ggplot(metab_mle %>%
          filter(!(GPP<0) & !(R>0)) %>%
          pivot_longer(cols = GPP:NEP,
                       names_to = "variable",
                       values_to = "rate",
                       values_drop_na = T)) +
   geom_col(aes(x = doy, y = rate)) +
   geom_hline(yintercept=0, linetype=2) +
   facet_grid(rows = vars(variable), cols = vars(pond_id), scales="free_y") +
   theme_classic()


# just NEP
windows(width=12)
ggplot(metab_mle %>%
          filter(!(GPP<0) & !(R>0)) %>%
          pivot_longer(cols = GPP:NEP,
                       names_to = "variable",
                       values_to = "rate",
                       values_drop_na = T) %>%
          filter(variable=="NEP")) +
   geom_col(aes(x = doy, y = rate)) +
   geom_hline(yintercept=0, linetype=2) +
   facet_wrap(facets = vars(pond_id), ncol=3) +
   theme_classic()


