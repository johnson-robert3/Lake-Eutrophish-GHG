# estimate CH4 and CO2 flux from pulsed ponds as a result of P1 
# (using difference in CH4 flux between pulsed and reference ponds,
#  using the difference accounts for the heat event, which affected all ponds)


# Function to calculate the mean difference between treatment and propagated standard error 
flux_diff = function(.dat, .var) {
   
   .dat %>%
      filter(!(is.na({{.var}}))) %>%  # enclose within {{}} to specify the argument supports <tidy-select>
      summarize(mean = mean({{.var}}), sd = sd({{.var}}), .by = c(trt_nutrients, doy)) %>%
      pivot_longer(cols = c(mean, sd), names_to = "stat", values_to = "value") %>%
      pivot_wider(id_cols = doy, names_from = c(trt_nutrients, stat), values_from = value) %>%
      mutate(diff = pulsed_mean - reference_mean,
             # propagated standard error
             diff_se = sqrt((pulsed_sd^2 + reference_sd^2) / 3))
}


# Standard error
se = function(.x) { sd(.x) / sqrt(length(.x)) }


# days between P1 and P2
p1.days = 177:211
h.spike = 191:195
p1 = 176
p2 = 211
H = 185:190
D = 223


# methane difference
mdiff = flux_diff(fdat, ch4_flux) %>%
  # filter to days between P1 and P2
  filter(doy %in% p1.days) %>%
  # add days GHG wasn't measured
  right_join(tibble('doy' = c(p1.days)) %>% mutate(doy = as.numeric(doy))) %>% 
  arrange(doy) %>%
  mutate(across(pulsed_mean:diff_se, ~zoo::na.approx(.)))

# methane emission from pulsed ponds above that from reference between P1 and P2
mdiff %>% summarize(sum(diff))


# co2 difference
cdiff = flux_diff(fdat, co2_flux) %>%
  # filter to days between P1 and P2
  filter(doy %in% p1.days) %>%
  # add days GHG wasn't measured
  right_join(tibble('doy' = c(p1.days)) %>% mutate(doy = as.numeric(doy))) %>% 
  arrange(doy) %>%
  mutate(across(pulsed_mean:diff_se, ~zoo::na.approx(.)))

# co2 emission from pulsed ponds above that from reference between P1 and P2
cdiff %>% summarize(sum(diff))



##-- CH4 and CO2 emissions by ponds during experiment (interpolating for days GHGs not measured)
flux = fdat %>%
  filter(doy %in% 146:240) %>%
  # interpolate for days GHGs not measured
  mutate(across(c(ch4_flux, co2_flux), ~zoo::na.approx(.)), .by = pond_id)



### total emissions during experiment

# by pond
flux %>% 
  # filter(doy %in% p1.days) %>%
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id)

# by treatment
emit.tot = flux %>% 
  # filter(doy %in% h.spike) %>% 
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id) %>%
  mutate(treatment = c('pulsed', 'pulsed', 'pulsed', 'reference', 'reference', 'reference')) %>%
  summarize(across(c(ch4, co2), list(mean=mean, se=se)), .by=treatment) %>%
  mutate(emission_period = "total")



### total CH4 and CO2 emitted by ponds between P1 and P2

# total by pond
flux %>% 
  filter(doy %in% p1.days) %>%
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id)

# total by treatment
emit.p1p2 = flux %>% 
  filter(doy %in% p1.days) %>%
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id) %>%
  mutate(treatment = c('pulsed', 'pulsed', 'pulsed', 'reference', 'reference', 'reference')) %>%
  summarize(across(c(ch4, co2), list(mean=mean, se=se)), .by=treatment) %>%
  mutate(emission_period = "P1 - P2")



### total CH4 and CO2 emissions as a result of the spike following the heat event

# total by pond
flux %>% 
  filter(doy %in% h.spike) %>% 
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id)

# total by treatment
emit.hspike = flux %>% 
  filter(doy %in% h.spike) %>% 
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id) %>%
  mutate(treatment = c('pulsed', 'pulsed', 'pulsed', 'reference', 'reference', 'reference')) %>%
  summarize(across(c(ch4, co2), list(mean=mean, se=se)), .by=treatment) %>%
  mutate(emission_period = "post-heat spike")



### total emissions after P1 (i.e., not including the pre-pulse period)

# by pond
flux %>% 
  filter(doy > p1) %>%
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id)

# by treatment
emit.afterp1 = flux %>% 
  filter(doy > p1) %>%
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id) %>%
  mutate(treatment = c('pulsed', 'pulsed', 'pulsed', 'reference', 'reference', 'reference')) %>%
  summarize(across(c(ch4, co2), list(mean=mean, se=se)), .by=treatment) %>%
  mutate(emission_period = "total after P1")


### total emissions after P2 

# by pond
flux %>% 
  filter(doy > p2) %>%
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id)

# by treatment
emit.afterp2 = flux %>% 
  filter(doy > p2) %>%
  summarize(ch4 = sum(ch4_flux), co2=sum(co2_flux), .by = pond_id) %>%
  mutate(treatment = c('pulsed', 'pulsed', 'pulsed', 'reference', 'reference', 'reference')) %>%
  summarize(across(c(ch4, co2), list(mean=mean, se=se)), .by=treatment) %>%
  mutate(emission_period = "total after P2")



# 
emissions = bind_rows(emit.tot, emit.p1p2, emit.hspike, emit.afterp1, emit.afterp2)


# magnitude difference in CH4 flux across experiment
fdat %>% 
  filter(!if_any(c(ch4_flux, co2_flux), is.na)) %>%
  summarize(across(c(ch4_flux, co2_flux), list(min=min, max=max)), .by=pond_id)




