#~~~
# Exploratory figures to view miniDOT data and compare to sonde profiles
# By: Robert Johnson
#~~~


library(slider)
library(patchwork)


### View MiniDOT raw data

# Pond A
windows()
ggplot(minidot %>% 
          filter(pond_id=="A") %>% 
          
          # add rolling window of do_sat data
          mutate(rolldo = slide_dbl(do, ~mean(.), .before=5, .complete=F)) %>%
          
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(162:165))) +
   
   # data as lines
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   # data as points
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   # rolling window data
   geom_line(aes(x = date_time, y = rolldo), color="firebrick2") +
   geom_point(aes(x = date_time, y = rolldo), color="firebrick2") +
   
   theme_classic()


# Pond B
windows()
ggplot(minidot %>% 
          filter(pond_id=="B") %>% 
          
          # add rolling window of do_sat data
          # mutate(rolldo = slide_dbl(do_sat, ~mean(.), .before=4, .complete=F)) %>%
          
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(146))) +
   
   # data as lines
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   # data as points
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   theme_classic()


# Pond C
windows()
ggplot(minidot %>% 
          filter(pond_id=="C") %>% 
          
          # add rolling window of do_sat data
          # mutate(rolldo = slide_dbl(do_sat, ~mean(.), .before=4, .complete=F)) %>%
          
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(210:214))) +
   
   # data as lines
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   # data as points
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   theme_classic()


# Pond D
windows()
ggplot(minidot %>% 
          filter(pond_id=="D") %>% 
          
          # add rolling window of do_sat data
          # mutate(rolldo = slide_dbl(do_sat, ~mean(.), .before=4, .complete=F)) %>%
          
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(210:214))) +
   
   # data as lines
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   # data as points
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   theme_classic()


# Pond E
windows()
ggplot(minidot %>% 
          filter(pond_id=="E") %>% 
          
          # add rolling window of do_sat data
          # mutate(rolldo = slide_dbl(do_sat, ~mean(.), .before=4, .complete=F)) %>%
          
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(210:214))) +
   
   # data as lines
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   # data as points
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   theme_classic()


# Pond F
windows()
ggplot(minidot %>% 
          filter(pond_id=="F") %>% 
          
          # add rolling window of do_sat data
          # mutate(rolldo = slide_dbl(do_sat, ~mean(.), .before=4, .complete=F)) %>%
          
          # add day-start for vertical lines
          mutate(hour = hour(date_time),
                 minute = minute(date_time)) %>%
          # select which days to view
          filter(doy %in% c(210:214))) +
   
   # data as lines
   geom_line(aes(x = date_time, y = do), color="cornflowerblue") +
   # data as points
   geom_point(aes(x = date_time, y = do), color="cornflowerblue") +
   # denote days
   geom_vline(data = ~filter(.x, hour==0 & minute==0), aes(xintercept = date_time), linetype=2, color="gray60") +
   
   theme_classic()










# six panel, miniDOT vs sonde profile DO

windows()
ggplot(sonde_surface) +
   # daily sonde
   geom_line(aes(x = doy, y = do_sat), color="firebrick2") +
   # morning minidot (between 9am and 11am)
   geom_line(data = minidot %>% 
                mutate(hour = hour(date_time)) %>% 
                filter(hour>=9, hour<=11) %>% 
                group_by(pond_id, doy) %>%
                summarize(across(temp:do_sat, ~mean(., na.rm=T))) %>%
                ungroup(),
             aes(x = doy, y = do_sat), color="cornflowerblue") +
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()





