#~~~
# Viewing and checking DO data cleaning methods. How many data points are removed across pond-days with our method?
# 
# By: R. Johnson
#~~~



### STEP 1: Create the "minidot" dataset from the 'data_import-and-process' script and then the 'metabolism-calcs' script


### info about dropped points
do_cleaning_pts = 
minidot %>%
   # view just days that were part of the experiment
   filter(doy >= 145, doy <= 240) %>%
   # how many points were flagged to be dropped for each day for each pond
   mutate(points_interp = case_when(drop_pt == 1 | 
                                 lag(drop_pt, n=1) == 1 | 
                                 lag(drop_pt, n=2) == 1 | 
                                 lag(drop_pt, n=3) == 1 | 
                                 lag(drop_pt, n=4) == 1 | 
                                 lag(drop_pt, n=5) == 1 ~ 1,
                             TRUE ~ 0)) %>%
   group_by(pond_id, doy) %>%
   summarize(total = n(),
             drop = sum(drop_pt),
             interp = sum(points_interp)) %>%
   ungroup() %>%
   mutate(perc_drop = drop / total * 100,
          perc_interp = interp / total * 100) #%>%
   # group_by(pond_id) %>%
   # summarize(min_perc_drop = min(perc_drop),
   #           max_perc_drop = max(perc_drop),
   #           mean_perc_drop = mean(perc_drop),
   #           min_perc_interp = min(perc_interp),
   #           max_perc_interp = max(perc_interp),
   #           mean_perc_interp = mean(perc_interp)) %>%
   # ungroup() %>%
   # View

# percent of points removed and backfilled
summarize(do_cleaning_pts, mean(perc_interp))   # mean = 7.1 %
summarize(do_cleaning_pts, range(perc_interp))   # range = 0 - 54.2 %
summarize(do_cleaning_pts, median(perc_interp))   # median = 0 %


# next: view a distribution of number of points dropped across ponds and days, are there days where too much of the data is being removed with this 
# method (w/ interpolation) and the entire day should be removed prior to metabolism calcs?

# view histograms of number of points dropped within days by pond
windows(); hist(do_cleaning_pts %>% filter(pond_id=="A") %>% .$drop)

# number of points flagged and dropped (points that dropped by > 2.0 mg/l)
windows(); ggplot(do_cleaning_pts) +
   geom_histogram(aes(x = drop)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of points flagged and dropped") +
   theme_classic()

# view histograms of number of points dropped and backfilled via linear interpolation within days by pond
windows(); hist(do_cleaning_pts %>% filter(pond_id=="A") %>% .$interp)

# number of points dropped and backfilled via linear interpolation
windows(); ggplot(do_cleaning_pts) +
   geom_histogram(aes(x = interp)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of points removed and backfilled") +
   theme_classic()

# time series of number of points dropped per day
windows(); ggplot(do_cleaning_pts) +
   geom_line(aes(x=doy, y = drop)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of points flagged and dropped") +
   theme_classic()

# time series of number of points dropped and backfilled via linear interpolation per day
windows(); ggplot(do_cleaning_pts) +
   geom_line(aes(x=doy, y = interp)) +
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("number of points removed and backfilled") +
   theme_classic()

# rank plot of percentage of points removed and interpolated for each pond
windows(); ggplot(do_cleaning_pts %>%
                     group_by(pond_id) %>%
                     arrange(perc_interp, .by_group=TRUE) %>%
                     mutate(rank = row_number()) %>%
                     ungroup()) +
   geom_point(aes(x = rank, y = perc_interp), shape=21) +
   
   # thresholds (for viewing)
   geom_hline(yintercept = 33, color = "blue") +
   # geom_hline(yintercept = 50, color = "red") +
   # geom_hline(yintercept = 25, color = "green") +
   
   facet_wrap(facets = vars(pond_id), nrow=3) +
   ylab("Percent of data points each day removed and backfilled via linear interpolation") +
   theme_classic()

# how many days in each pond over a threshold percentage for number of data points removed and backfilled
do_cleaning_pts %>% filter(perc_interp > 50) %>% count(pond_id)
do_cleaning_pts %>% filter(perc_interp > 40) %>% count(pond_id)
do_cleaning_pts %>% filter(perc_interp > 33) %>% count(pond_id) # n = 10  # 1.7% of pond-days
do_cleaning_pts %>% filter(perc_interp > 25) %>% count(pond_id) # n = 20  # 3.5% of pond-days



### STEP 2: Call in the metabolism dataset, if not already created through the 'metabolism-calcs' script

   metabolism = read_csv("Data/metabolism_total.csv")


# number of days per pond with erroneous metabolism estimates (metabolism calculated w/o removing any days beforehand due to DO cleaning >33%)
metabolism %>% filter(GPP<0 | R>0) %>% count(pond_id)
metabolism %>% filter(GPP<0 | R>0) %>% View
# 62 days total

# percent range of days removed per pond (experiment was 96 days)
metabolism %>% filter(GPP<0 | R>0) %>% count(pond_id) %>% mutate(perc = n / 96 * 100)


# view number of DO points removed and backfilled on days with erroneous estimates
metabolism %>% filter(GPP<0 | R>0) %>% left_join(do_cleaning_pts) %>% count(drop)
metabolism %>% filter(GPP<0 | R>0) %>% left_join(do_cleaning_pts) %>% arrange(drop) %>% View
# not as much overlap as I expected. 
# none of the days with >25% of points removed and backfilled overlapped with days producing erroneous metabolism estimates (perhaps b/c we "fixed" these?)
# of 62 days with erroneous estimates, 40 were on days that did not have any DO data points dropped

# 62 days with erroneous estimates using corrected DO data
# 71 days with erroneous estimates using raw DO data
# 16 days with >=3 flagged DO points
# 20 days with >25% of points removed and backfilled
# 10 days with >33% of points removed and backfilled


# combined dataset of metabolism and DO cleaning
test = full_join(metabolism, do_cleaning_pts) %>%
   rename(n_total = total, n_drop = drop, n_interp = interp) %>%
   mutate(flag_33 = case_when(perc_interp > 33 ~ 1,
                              TRUE ~ 0))


# number of pond-days with >33% of DO data points removed and backfilled
test %>% filter(flag_33 == 1) %>% count(pond_id)


# view metabolism around days when a higher percent of DO data points were cleaned
windows(); ggplot(test,
                  aes(x = doy, y = NEP)) +
   # all metabolism
   geom_line() +
   # circle days in red where >33% of DO points have been cleaned
   geom_point(data = ~filter(.x, flag_33==1), shape = 21, color ="red", size=3) +
   # circle days in blue that have erroneous metabolism estimates
   geom_point(data = ~filter(.x, GPP<0 | R>0), shape = 21, color ="blue", size=3) +
   # 0 line
   geom_hline(yintercept = 0, linetype = 2) +
   #
   facet_wrap(facets = vars(pond_id)) +
   theme_classic()

ggsave(filename = "NEP-highlight-cleaning-pts.png", height=5, width=8, units ="in")


