#~~~
# Import and process the DOC data
# By: R. Johnson
#~~~


library(tidyverse)
library(janitor)
library(lubridate)
library(patchwork)


# Run 0, December mini-run
run0_raw = read_csv("Data/R-Data/2020_DOC/DOC_Run-0.csv", skip=6)

run0_dat = run0_raw %>%
   remove_empty(c("rows", "cols")) %>%
   clean_names() %>%
   filter(!(is.na(toc_average_ppb)))

# make output match other run files
run0 = run0_dat %>%
   rename(vial = sample_name) %>%
   mutate(vial = str_replace(vial, "blk", "Blank"),
          sample_id = vial,
          sample_id = na_if(sample_id, "Blank")) %>%
   mutate(sample_type = case_when(vial == "Blank" ~ "Blank",
                                  TRUE ~ "Sample")) %>%
   relocate(sample_id) %>%
   mutate(run = 0) %>%
   relocate(run, .before = start_time)


# Run 1
run1_raw = read_csv("Data/R-Data/2020_DOC/DOC_Run-1.csv", skip=6)

run1_dat = run1_raw %>%
   remove_empty(c("rows", "cols")) %>%
   clean_names() %>%
   filter(!(is.na(toc_average_ppb))) %>%
   # correct times when SD < 0.03 (i.e., SD = 0) (i.e., all measurements were the same)
   #  only occurs within columns toc_standard_deviation_ppb and toc_rsd_percent
   mutate(across(contains("toc"), ~replace(., str_detect(., "<"), 0))) %>%
   mutate(across(toc_average_ppb:last_col(), ~as.numeric(.)))

# make output match other run files
run1 = run1_dat %>%
   rename(vial = sample_name) %>%
   unite(sample_id, lot_number, vial, sep="", remove=FALSE) %>%
   mutate(sample_id = na_if(sample_id, "blkBlank")) %>%
   mutate(run = 1) %>%
   relocate(run, .before = start_time)


# Run 2
run2_raw = read_csv("Data/R-Data/2020_DOC/DOC_Run-2.csv", skip=6)
run2_ids = read_csv("Data/R-Data/2020_DOC/doc_run-2_vial_IDs.csv")

run2_dat = run2_raw %>%
   remove_empty(c("rows", "cols")) %>%
   clean_names() %>%
   filter(!(is.na(toc_average_ppb))) %>%
   # correct times when SD < 0.03 (i.e., SD = 0) (i.e., all measurements were the same)
   #  only occurs within columns toc_standard_deviation_ppb and toc_rsd_percent
   mutate(across(contains("toc"), ~replace(., str_detect(., "<"), 0))) %>%
   mutate(across(toc_average_ppb:last_col(), ~as.numeric(.)))

# add vial IDs to data
run2 = run2_dat %>%
   rename(vial = sample_name) %>%
   left_join(run2_ids %>%
                filter(!(sample_id=="Blank")) %>%
                select(-run) %>%
                mutate(vial = as.character(vial))) %>%
   relocate(sample_id) %>%
   mutate(run = 2) %>%
   relocate(run, .before = start_time)


# Run 3
run3_raw = read_csv("Data/R-Data/2020_DOC/DOC_Run-3.csv", skip=6)
run3_ids = read_csv("Data/R-Data/2020_DOC/doc_run-3_vial_IDs.csv")

run3_dat = run3_raw %>%
   remove_empty(c("rows", "cols")) %>%
   clean_names() %>%
   # remove all empty spots from when run was aborted
   filter(!(sample_name=="No Results")) %>%
   filter(!(toc_average_ppb=="---")) %>%
   # correct times when SD < 0.03 (i.e., SD = 0) (i.e., all measurements were the same)
   #  only occurs within columns toc_standard_deviation_ppb and toc_rsd_percent
   mutate(across(contains("toc"), ~replace(., str_detect(., "<"), 0))) %>%
   mutate(across(toc_average_ppb:last_col(), ~as.numeric(.)))

# add vial IDs to data
run3 = run3_dat %>%
   rename(vial = sample_name) %>%
   left_join(run3_ids %>%
                filter(!(sample_id=="Blank")) %>%
                select(-run) %>%
                mutate(vial = as.character(vial))) %>%
   relocate(sample_id) %>%
   mutate(run = 3) %>%
   relocate(run, .before = start_time)


# Run 4
run4_raw = read_csv("Data/R-Data/2020_DOC/DOC_Run-4.csv", skip=6)
run4_ids = read_csv("Data/R-Data/2020_DOC/doc_run-4_vial_IDs.csv")

run4_dat = run4_raw %>%
   remove_empty(c("rows", "cols")) %>%
   clean_names() %>%
   filter(!(is.na(toc_average_ppb))) %>%
   # correct times when SD < 0.03 (i.e., SD = 0) (i.e., all measurements were the same)
   #  only occurs within columns toc_standard_deviation_ppb and toc_rsd_percent
   mutate(across(contains("toc"), ~replace(., str_detect(., "<"), 0))) %>%
   mutate(across(toc_average_ppb:last_col(), ~as.numeric(.)))

# add vial IDs to data
run4 = run4_dat %>%
   rename(vial = sample_name) %>%
   left_join(run4_ids %>%
                filter(!(sample_id=="Blank")) %>%
                select(-run) %>%
                mutate(vial = as.character(vial))) %>%
   relocate(sample_id) %>%
   mutate(run = 4) %>%
   relocate(run, .before = start_time)


# Combine data sets
doc_all = bind_rows(run0, run1, run2, run3, run4) %>%
   rename(toc_ppb = toc_average_ppb,
          sd_toc = toc_standard_deviation_ppb,
          ic_ppb = ic_average_ppb,
          sd_ic = ic_standard_deviation_ppb,
          tc_ppb = tc_average_ppb,
          sd_tc = tc_standard_deviation_ppb) %>%
   mutate(start_time = mdy_hms(start_time),
          end_time = mdy_hms(end_time)) %>%
   select(-vial_number:-flush_time_sec)

# just blanks
blank_dat = doc_all %>% filter(vial=="Blank") %>%
   mutate(date = date(start_time),
          doy = yday(date),
          run = case_when(doy == 342 ~ "Dec. 8",
                          doy %in% c(21:22) ~ "Jan. 21",
                          doy %in% c(23:25) ~ "Jan. 23",
                          doy == 26 ~ "Jan. 26",
                          doy %in% c(34:36) ~ "Feb. 3"),
          run = fct_inorder(run))

# just samples, add Pond ID and DOY
doc_dat = doc_all %>%
   filter(!(is.na(sample_id))) %>%
   mutate(sample_id = str_remove(sample_id, "E20")) %>%
   separate(sample_id, into=c("pond_id", "doy"), sep=1, remove=FALSE) %>%
   mutate(doy = as.numeric(doy))
   


# view change in blank values between runs / over time

windows(height=4, width=6)
ggplot(blank_dat %>% filter(doy %in% c(21:36))) +
   # TOC
   geom_point(aes(x = start_time, y = toc_ppb, color = run), size=1.5) +
   # geom_line(aes(x = start_time, y = toc_ppb, group = run, color = run), size=0.5) +
   guides(color=guide_legend(title="Run Start")) +
   labs(x = "Injection start time", y = "TOC (ppb)") +
   # IC
   # geom_point(aes(x = start_time, y = ic_ppb, color = run), size=1.5) +
   # # geom_line(aes(x = start_time, y = ic_ppb, group = run, color = run), size=0.5) +
   # guides(color=guide_legend(title="Run Start")) +
   # labs(x = "Injection start time", y = "IC (ppb)") +
   # TC
   # geom_point(aes(x = start_time, y = tc_ppb, color = run), size=1.5) +
   # # geom_line(aes(x = start_time, y = tc_ppb, group = run, color = run), size=0.5) +
   # guides(color=guide_legend(title="Run Start")) +
   # labs(x = "Injection start time", y = "TC (ppb)") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# ggsave(filename = "Figures/DOC data/Johnson_blanks_TOC.png", height=4, width=6, units="in")
# ggsave(filename = "Figures/DOC data/Johnson_blanks_IC.png", height=4, width=6, units="in")
# ggsave(filename = "Figures/DOC data/Johnson_blanks_TC.png", height=4, width=6, units="in")


# doc over time in exp ponds
# windows(height=4, width=6)
p1 =
ggplot(doc_dat %>% 
          filter(pond_id %in% c('A', 'B', 'C')) %>%
          mutate(toc_ppm = toc_ppb/1000)) +
   #
   geom_point(aes(x = doy, y = toc_ppm, color = pond_id), size=2) +
   geom_line(aes(x = doy, y = toc_ppm, group = pond_id, color = pond_id), size=1, alpha=0.7) +
   #
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   #
   scale_color_manual(breaks = c('A', 'B', 'C'),
                      values = c('A' = "#3BB873", 'B' = '#51ADCF', 'C' = '#2D6187')) +
   lims(y=c(0, 50)) +
   theme_classic()


# doc over time in ref ponds
# windows(height=4, width=6)
p2 =
ggplot(doc_dat %>% 
          filter(pond_id %in% c('D', 'E', 'F')) %>%
          mutate(toc_ppm = toc_ppb/1000)) +
   #
   geom_point(aes(x = doy, y = toc_ppm, color = pond_id), size=2) +
   geom_line(aes(x = doy, y = toc_ppm, group = pond_id, color = pond_id), size=1, alpha=0.7) +
   #
   # geom_smooth(aes(x = doy, y = toc_ppm, group = pond_id, fill = pond_id, color = pond_id), size=0, alpha=0.2) +
   # geom_smooth(aes(x = doy, y = toc_ppm, group = pond_id, fill = pond_id), alpha=0.2, color=NA) +
   #
   geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
   #
   scale_color_manual(breaks = c('D', 'E', 'F'),
                      values = c('D' = "#3BB873", 'E' = '#51ADCF', 'F' = '#2D6187')) +
   scale_fill_manual(breaks = c('D', 'E', 'F'),
                      values = c('D' = "#3BB873", 'E' = '#51ADCF', 'F' = '#2D6187')) +
   #
   lims(y=c(0, 50)) +
   theme_classic()


windows(height=8, width=6); p1 / p2






