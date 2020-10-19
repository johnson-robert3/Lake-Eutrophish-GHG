#~~~
# Exploratory figures to view miniDOT data and compare to sonde profiles
# By: Robert Johnson
#~~~


library(slider)
library(patchwork)



# miniDOT

windows()
ggplot(minidot %>% filter(pond_id=="A", doy %in% c(180:191)) %>% 
          mutate(newdo = slide_dbl(do_sat, ~mean(.), .before=4, .complete=F))) +
   # geom_line(aes(x = date_time, y = temp), color="firebrick2") +
   geom_line(aes(x = date_time, y = do_sat), color="cornflowerblue") +
   theme_classic()




# Sonde, top 15cm

windows()
ggplot(sonde_surface %>% filter(pond_id=="A")) +
   geom_line(aes(x = doy, y = do_sat), color="firebrick2") +
   geom_line(data = minidot %>% filter(pond_id=="A") %>% group_by(doy) %>% summarize(do_sat = mean(do_sat, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = do_sat), color="cornflowerblue") +
   theme_classic()



# six panel, minidot vs sonde DO


# windows()
# a =
ggplot(sonde_surface %>% filter(pond_id=="A")) +
   geom_line(aes(x = doy, y = do_sat), color="firebrick2") +
   geom_line(data = minidot %>% filter(pond_id=="A") %>% group_by(doy) %>% summarize(do_sat = mean(do_sat, na.rm=T)) %>% ungroup(),
             aes(x = doy, y = do_sat), color="cornflowerblue") +
   theme_classic()


# windows(height=6, width=10)
# 
# (a + b + c) / (d + e + f)



# wrapped

windows()
ggplot(sonde_surface) +
   # daily sonde
   geom_line(aes(x = doy, y = do_sat), color="firebrick2") +
   # morning minidot (between 9am and noon)
   geom_line(data = minidot %>% 
                mutate(hour = hour(date_time)) %>% 
                filter(hour>=9, hour<=11) %>% 
                group_by(pond_id, doy) %>%
                summarize(across(temp:do_sat, ~mean(., na.rm=T))) %>%
                ungroup(),
             aes(x = doy, y = do_sat), color="cornflowerblue") +
   facet_wrap(facets = vars(pond_id), nrow=2) +
   theme_classic()





