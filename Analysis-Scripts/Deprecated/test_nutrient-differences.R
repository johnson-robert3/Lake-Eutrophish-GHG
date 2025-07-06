

# are there actually differences in nutrient variables between treatments or across temporal periods?


ndat = fdat %>%
   select(pond_id:period2, tp:nox, alkalinity, doc_ppm) %>%
   filter(!(if_any(tp:last_col(), is.na))) %>%
   group_by(pond_id) %>%
   arrange(doy, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup()



# TN

tn1 = lme(tn ~ treatment * period, random = ~1|pond_id, data = ndat, method="REML")
tn1 = update(tn1, correlation = corAR1(form = ~ time|pond_id, value = ACF(tn1, form = ~ time|pond_id)[2,2]))
summary(tn1)

summary(aov(tn ~ treatment * period, ndat))


# TP

tp1 = lme(tp ~ treatment * period, random = ~1|pond_id, data = ndat, method="REML")
tp1 = update(tp1, correlation = corAR1(form = ~ time|pond_id, value = ACF(tp1, form = ~ time|pond_id)[2,2]))
summary(tp1)

summary(aov(tp ~ treatment * period, ndat))


# srp

srp1 = lme(srp ~ treatment * period, random = ~1|pond_id, data = ndat, method="REML")
srp1 = update(srp1, correlation = corAR1(form = ~ time|pond_id, value = ACF(srp1, form = ~ time|pond_id)[2,2]))
summary(srp1)

summary(aov(srp ~ treatment * period, ndat))


# nox

nox1 = lme(nox ~ treatment * period, random = ~1|pond_id, data = ndat, method="REML")
nox1 = update(nox1, correlation = corAR1(form = ~ time|pond_id, value = ACF(nox1, form = ~ time|pond_id)[2,2]))
summary(nox1)

summary(aov(nox ~ treatment * period, ndat))


# doc

doc1 = lme(doc_ppm ~ treatment * period, random = ~1|pond_id, data = ndat, method="REML")
doc1 = update(doc1, correlation = corAR1(form = ~ time|pond_id, value = ACF(doc1, form = ~ time|pond_id)[2,2]))
summary(doc1)

summary(aov(doc_ppm ~ treatment * period, ndat))


