
## Script for visualizing times of rapid drops in DO concentration


#-- MiniDOT DO data --#

windows(width=12, height=8)
ggplot(minidot %>% 
          filter(pond_id=="E") %>% 
          
          # add DO difference from previous point
          mutate(do_diff = do - lag(do)) %>%
          
          # remove offending point and the next 3 hours (5 points) any time DO drops more than 2.0 mg/l
          mutate(drop_do = case_when(do_diff <= -2 ~ 1,
                                     lag(do_diff, n=1)  <= -2 ~ 1,
                                     lag(do_diff, n=2)  <= -2 ~ 1,
                                     lag(do_diff, n=3)  <= -2 ~ 1,
                                     lag(do_diff, n=4)  <= -2 ~ 1,
                                     lag(do_diff, n=5)  <= -2 ~ 1,
                                     TRUE ~ 0),
                 new_do = case_when(drop_do == 1 ~ -9999,
                                    drop_do == 0 ~ do),
                 new_do = na_if(new_do, -9999),
                 new_do = zoo::na.approx(new_do)) %>%
          
          # rolling window data (3-hour)
          mutate(
             # rolling window on original/raw data
             roll_do = slide_dbl(do, ~mean(.), .before=5, .complete=F),
             # rolling window on new, corrected DO data with large drops removed/interpolated
             roll_new_do = slide_dbl(new_do, ~mean(.), .before=5, .complete=F)) %>%
          
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(200:209))) +
   
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   # raw/measured DO data
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   
   # outline points when DO drops by more than 2.0 mg/l
   geom_point(data = ~filter(.x, do_diff <= -2),
              aes(x = date_time, y = do), shape=1, color="red", size=2) +
   
   # corrected/interpolated DO as lines
   geom_line(aes(x = date_time, y = new_do), color = "seagreen3") +
   geom_point(aes(x = date_time, y = new_do), color = "seagreen3") +
   
   # rolling window on new/corrected DO data
   geom_line(data = ~mutate(.x, date_time = date_time - minutes(90)), 
             aes(x = date_time, y = roll_new_do), color = "firebrick2") +
   geom_point(data = ~mutate(.x, date_time = date_time - minutes(90)), 
              aes(x = date_time, y = roll_new_do), color = "firebrick2") +

   theme_classic()






