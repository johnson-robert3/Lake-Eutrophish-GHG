#~~
# Script to run a PCA and view variable relationships
#
#~~


#-- Load data and packages --#

# create the 'fdat' and 'pond_data' data sets from the "stats_model-data" script 
source("Analysis-Scripts/stats_model-data.R")

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(missMDA)

set.seed(1234)



#-- Select data for PCA --#

# variables used in comparison analysis
pca_dat = fdat %>%
  select(pond_id, doy, treatment, 
         ch4_flux, co2_flux,
         GPP, R, NEP,
         do_sat, bottom_do_sat,
         temp, bottom_temp,
         tn, tp) %>%
  arrange(pond_id, doy)



#-- Run PCAs and view biplots on various data subsets --#

#- Full experiment duration, remove any rows with missing data
dat_sub = pca_dat %>%
  # remove day of year
  select(-doy) %>%
  # remove any row that contains NAs
  drop_na()

# run PCA
pca_res = PCA(dat_sub,
              quali.sup = 1:2,  # pond_id and treatment are supplementary qualitative variables
              quanti.sup = 3:4,  # fluxes are supplementary quantitative variables
              graph = FALSE)

  # view the scree plot for important PCs
  fviz_screeplot(pca_res, addlabels = TRUE, ylim = c(0, 50))
  
  # PCA biplot by treatment
  fviz_pca_biplot(pca_res,
                  habillage  = as.factor(dat_sub$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "entire experiment")


# full experiment duration, but impute for missing values
dat_imp = pca_dat %>%
  select(-doy) %>%
  # remove rows without Flux data
  drop_na(contains("flux"))
  
# impute for missing values
imp_res = imputePCA(dat_imp, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_imp = PCA(imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot on imputed data by treatment
  fviz_pca_biplot(pca_res_imp,
                  habillage  = as.factor(dat_imp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "entire experiment (impute)")



#- Only using days before first nutrient pulse (anything correlate with the pond A CH4 spike?)
dat_base = pca_dat %>%
  # keep only pre-pulse days
  filter(doy < 177) %>%
  select(-doy) %>%
  drop_na()

# run PCA, pre-pulse period only
pca_res_base = PCA(dat_base, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot by treatment for base period only
  fviz_pca_biplot(pca_res_base,
                  habillage  = as.factor(dat_base$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "pre-pulse period")
  
    # by pond
    fviz_pca_biplot(pca_res_base,
                    habillage  = as.factor(dat_base$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var",
                    title = "pre-pulse period (by pond)")


# before P1, but impute missing values
dat_base_imp = pca_dat %>%
  filter(doy < 177) %>%
  select(-doy) %>%
  drop_na(contains("flux"))

# impute for missing values
base_imp_res = imputePCA(dat_base_imp, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_base_imp = PCA(base_imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot on imputed base period data
  fviz_pca_biplot(pca_res_base_imp,
                  habillage  = as.factor(dat_base_imp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "pre-pulse period (impute)")
  
    # by pond
    fviz_pca_biplot(pca_res_base_imp,
                    habillage  = as.factor(dat_base_imp$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var",
                    title = "pre-pulse period (impute, by pond)")


# just pond A before P1, but impute missing values
dat_base_A = pca_dat %>%
  filter(doy < 177 & pond_id == 'A') %>%
  select(-doy) %>%
  drop_na(contains("flux"))

# impute for missing values
base_A_imp_res = imputePCA(dat_base_A, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_base_A_imp = PCA(base_A_imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot on imputed base period data for pond A only
  fviz_pca_biplot(pca_res_base_A_imp,
                  # habillage  = as.factor(dat_base_A$treatment),  # specify the variable by which to split and color data
                  # addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "pre-pulse period, pond A (impute)")

  
    
#- Full experiment duration, but remove pond A
dat_noA = pca_dat %>%
  # remove pond A
  filter(pond_id != 'A') %>%
  select(-doy) %>%
  drop_na()

# run PCA, without pond A
pca_res_noA = PCA(dat_noA, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot without pond A
  fviz_pca_biplot(pca_res_noA, 
                  habillage  = as.factor(dat_noA$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "without pond A")
  
    # by pond
    fviz_pca_biplot(pca_res_noA,
                    habillage  = as.factor(dat_noA$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var")


# without pond A, but impute for missing values
dat_noA_imp = pca_dat %>%
  filter(pond_id != 'A') %>%
  select(-doy) %>%
  drop_na(contains("flux"))

# impute for missing values
noA_imp_res = imputePCA(dat_noA_imp, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_noA_imp = PCA(noA_imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot on imputed data without pond A
  fviz_pca_biplot(pca_res_noA_imp,
                  habillage  = as.factor(dat_noA_imp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "without pond A (impute)")
  
    # by pond
    fviz_pca_biplot(pca_res_noA_imp,
                    habillage  = as.factor(dat_noA_imp$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var",
                    title = "without pond A (impute, by pond)")


    
#- Only using days between P1 and P2
dat_pulse = pca_dat %>%
  # keep only between P1 and P2
  filter(doy >= 177 & doy <= 211) %>%
  select(-doy) %>%
  drop_na()

# run PCA, between P1 and P2 only
pca_res_pulse = PCA(dat_pulse, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot, between P1 and P2
  fviz_pca_biplot(pca_res_pulse,
                  habillage  = as.factor(dat_pulse$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "Between P1 and P2")
  
    # by pond
    fviz_pca_biplot(pca_res_pulse,
                    habillage  = as.factor(dat_pulse$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var")


# between P1 and P2, but impute missing values
dat_pulse_imp = pca_dat %>%
  filter(doy >= 177 & doy <= 211) %>%
  select(-doy) %>%
  drop_na(contains("flux"))

# impute for missing values
pulse_imp_res = imputePCA(dat_pulse_imp, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_pulse_imp = PCA(pulse_imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot, imputed between P1 and P2
  fviz_pca_biplot(pca_res_pulse_imp,
                  habillage  = as.factor(dat_pulse_imp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "Between P1 and P2 (impute)")
  
    # by pond
    fviz_pca_biplot(pca_res_pulse_imp,
                    habillage  = as.factor(dat_pulse_imp$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var")



#- Using all days after P1 (remove the pre-pulse period)
dat_exp = pca_dat %>%
  # keep only after P1
  filter(doy >= 177) %>%
  select(-doy) %>%
  drop_na()

# run PCA, after P1
pca_res_exp = PCA(dat_exp, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot, after P1
  fviz_pca_biplot(pca_res_exp,
                  habillage  = as.factor(dat_exp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "w/o pre-pulse period")
  
    # by pond
    fviz_pca_biplot(pca_res_exp,
                    habillage  = as.factor(dat_exp$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var")

    
# after P1, but impute missing values
dat_exp_imp = pca_dat %>%
  filter(doy >= 177) %>%
  select(-doy) %>%
  drop_na(contains("flux"))

# impute for missing values
exp_imp_res = imputePCA(dat_exp_imp, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_exp_imp = PCA(exp_imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot, imputed after P1
  windows(width=5, height=4)
  fviz_pca_biplot(pca_res_exp_imp,
                  habillage  = as.factor(dat_exp_imp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind="point", col.var="black", label="var",
                  title = "after P1 (impute)")
  
  # ggsave(file = "PCA_post-P1.png", width=5, height=4, units="in", dpi=300)
  
    # by pond
    fviz_pca_biplot(pca_res_exp_imp,
                    habillage  = as.factor(dat_exp_imp$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var")


    
#- Only using days after P2
dat_post = pca_dat %>%
  # keep only after P2
  filter(doy > 211) %>%
  select(-doy) %>%
  drop_na()

# run PCA, post P2 only
pca_res_post = PCA(dat_post, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot, post P2 only
  fviz_pca_biplot(pca_res_post,
                  habillage  = as.factor(dat_post$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "post P2 period")
  
    # by pond
    fviz_pca_biplot(pca_res_post,
                    habillage  = as.factor(dat_post$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var")


# post P2 only, but impute missing values
dat_post_imp = pca_dat %>%
  filter(doy > 211) %>%
  select(-doy) %>%
  drop_na(contains("flux"))

# impute for missing values
post_imp_res = imputePCA(dat_post_imp, quali.sup = 1:2, quanti.sup = 3:4, ncp=5)

# run PCA on imputed dataset
pca_res_post_imp = PCA(post_imp_res$completeObs, quali.sup = 1:2, quanti.sup = 3:4, graph = FALSE)

  # PCA biplot, imputed post P2 only
  fviz_pca_biplot(pca_res_post_imp,
                  habillage  = as.factor(dat_post_imp$treatment),  # specify the variable by which to split and color data
                  addEllipses = TRUE, ellipse.level = 0.95,
                  geom.ind = "point", col.var="black", label="var",
                  title = "post P2 period (impute)")
  
    # by pond
    fviz_pca_biplot(pca_res_post_imp,
                    habillage  = as.factor(dat_post_imp$pond_id),  # specify the variable by which to split and color data
                    addEllipses = TRUE, ellipse.level = 0.95,
                    geom.ind = "point", col.var="black", label="var")



