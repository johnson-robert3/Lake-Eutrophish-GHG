
## Script for visualizing times of rapid drops in DO concentration

library(slider)

#-- MiniDOT DO data --#

windows(width=12, height=8)
ggplot(minidot %>% 
          filter(pond_id=="E") %>% 
          arrange(date_time) %>%
          
          # add change in DO concentration from previous point
          mutate(delta_do = do - lag(do)) %>%
          
          # remove the offending point along with the next 3 hours (5 points) of data any time DO drops more than 2.0 mg/l
          mutate(drop_do = case_when(delta_do <= -2 ~ 1,
                                     lag(delta_do, n=1)  <= -2 ~ 1,
                                     lag(delta_do, n=2)  <= -2 ~ 1,
                                     lag(delta_do, n=3)  <= -2 ~ 1,
                                     lag(delta_do, n=4)  <= -2 ~ 1,
                                     lag(delta_do, n=5)  <= -2 ~ 1,
                                     TRUE ~ 0),
                 new_do = case_when(drop_do == 1 ~ -9999,
                                    drop_do == 0 ~ do),
                 new_do = na_if(new_do, -9999),
                 new_do = zoo::na.approx(new_do, na.rm=FALSE)) %>%
          
          # rolling window data (3-hour)
          mutate(
             # rolling window on original/raw data
             roll_do = slide_dbl(do, ~mean(.), .before=3, .after=2, .complete=F),
             # rolling window on new, corrected DO data with large drops removed/interpolated
             roll_new_do = slide_dbl(new_do, ~mean(.), .before=3, .after=2, .complete=F)) %>%
          
          # shift time by 1 hour to match when ups and downs naturally occur in the time series
          #  this is better than trying to shift date_time later on
          #  Nevermind, this is exactly the same as using a rolling window of 3 points back and 2 points forward
          # group_by(pond_id) %>%
          # mutate(corr_new_do = lead(roll_new_do, n=2)) %>%
          # ungroup() %>%
          
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
   geom_line(aes(x = date_time, y = new_do), color = "seagreen3") +
   geom_point(aes(x = date_time, y = new_do), color = "seagreen3") +
   
   # rolling window on new/corrected DO data
   geom_line(#data = ~mutate(.x, date_time = date_time - minutes(60)),
             aes(x = date_time, y = roll_new_do), color = "firebrick2") +
   geom_point(#data = ~mutate(.x, date_time = date_time - minutes(60)),
              aes(x = date_time, y = roll_new_do), color = "firebrick2") +
   
   # rolling without needing to correct times
   # geom_line(aes(x = date_time, y = corr_new_do), color = "firebrick2") +
   # geom_point(aes(x = date_time, y = corr_new_do), color = "firebrick2") +
   
   theme_classic()


