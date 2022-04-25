
## Script for visualizing times of rapid drops in DO concentration

library(slider)


#-- View one pond --#

windows(width=12, height=8)
ggplot(minidot %>% 
          # select pond
          filter(pond_id=="E") %>% 
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(200:209))) +
   
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   # anoxia (0 DO)
   geom_hline(yintercept=0, linetype=3, color="gray60") + 
   
   # raw/measured DO data
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   
   # outline points when DO drops by more than 2.0 mg/l
   geom_point(data = ~filter(.x, delta_do <= -2),
              aes(x = date_time, y = do), shape=1, color="red", size=3) +
   
   # corrected/interpolated raw DO as lines
   geom_line(aes(x = date_time, y = corr_do), color = "seagreen3") +
   geom_point(aes(x = date_time, y = corr_do), color = "seagreen3") +
   
   # rolling window on new/corrected DO data
   geom_line(#data = ~mutate(.x, date_time = date_time - minutes(60)),
             aes(x = date_time, y = roll_corr_do), color = "firebrick2") +
   geom_point(#data = ~mutate(.x, date_time = date_time - minutes(60)),
              aes(x = date_time, y = roll_corr_do), color = "firebrick2") +
   
   theme_classic()




#-- 6-panel, view all ponds at same time --#

# pre-processed minidot data from 'metab-model-data-comparison' script

windows()
ggplot(minidot %>%
          # select days to view
          filter(doy %in% c(200:219), pond_id=="D"),
       aes(x = date_time)) +
   
   # lines to denote days (midnight)
   geom_vline(data = minidot %>%
                 mutate(hour = hour(date_time), minute = minute(date_time)) %>%
                 filter(hour==0 & minute==0),
              aes(xintercept = date_time), linetype=2, color="gray60") +
   # anoxia (0 DO)
   geom_hline(yintercept=0, linetype=3, color="gray60") + 
   
   # raw/measured DO data
   # geom_line(aes(y = do), color="cornflowerblue") +
   # geom_point(aes(y = do), color="cornflowerblue", alpha=0.6) +

      # # outline points when DO drops by more than 2.0 mg/l
      # geom_point(data = ~filter(.x, delta_do <= -2),
      #            aes(x = date_time, y = do), shape=1, color="red", size=3) +
      # # outline points when DO drops by more than 1.5 mg/l
      # geom_point(data = ~filter(.x, delta_do <= -1.5),
      #            aes(x = date_time, y = do), shape=1, color="#34BE82", size=5) +
      # # outline points when DO drops by more than 1.0 mg/l
      # geom_point(data = ~filter(.x, delta_do <= -1),
      #            aes(x = date_time, y = do), shape=1, color="black", size=4) +
   
   # rolling window on raw data
   # geom_line(aes(y = roll_do), color="firebrick2") +
   # geom_point(aes(y = roll_do), color="firebrick2", alpha=0.6) +

   # corrected/interpolated raw DO as lines
   geom_line(aes(y = corr_do), color = "seagreen3") +
   geom_point(aes(y = corr_do), color = "seagreen3", alpha=0.6) +

   # rolling window on new/corrected DO data
   geom_line(aes(y = roll_corr_do), color = "darkorchid1") +
   geom_point(aes(y = roll_corr_do), color = "darkorchid1", alpha=0.6) +
   
   # facet by pond
   # facet_wrap(facets = vars(pond_id)) +
   
   labs(title = "Corrected data in green, rolling window (corrected) in violet",
        subtitle = "Pond D", 
        x = "Date_time",
        y = expression(Dissolved~oxygen~(mg~L^-1))) +
   theme_classic()


