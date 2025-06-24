## Explore hort farm pond GHG dataset from Robert Johnson & Grace Wilkinson 
## and begin prototyping new analyses

rm(list=ls())

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# data
dat.raw <- read.csv("Data/ghg-model-dataset_2024-07-26.csv")

# absolute value of ecosystem respiration values (lower values of R denote higher rates of respiration)
dat.raw <- dat.raw |> dplyr::mutate(R = abs(R))


## basic exploration of data contents
str(dat.raw)
unique(dat.raw$pond_id)
unique(dat.raw$doy)
unique(dat.raw$treatment)

## add food web treatment
dat.raw$foodweb <- NA
dat.raw$foodweb[dat.raw$pond_id %in% c("B","F")] <- "low"
dat.raw$foodweb[dat.raw$pond_id %in% c("A","D")] <- "int"
dat.raw$foodweb[dat.raw$pond_id %in% c("C","E")] <- "hi"

dat.raw = dat.raw |> dplyr::mutate(treatment = dplyr::case_when(treatment=="reference" ~ "ref",
                                                                treatment=="pulsed" ~ "puls"))

# define some time periods of interest
wwidth <- 5 #days: width of windows, e.g. following nutrient pulses
pulse1 <- 177: (177 + wwidth - 1)#doy
pulse2 <- 212: (212 + wwidth - 1)
derecho <- 224: (224 + wwidth - 1)
heatwave <- 185:190


varlist <- colnames(dat.raw)
# varlist <- varlist[!varlist %in% c("pond_id","treatment","date","doy","period","week","flag","foodweb",
#                                    "sonde_strat","sonde_strat","sonde_zmix","salinity", "bottom_salinity",
#                                    "wind_speed","wind_U10","ph","bottom_ph","nox","phyco","bottom_phyco")]

varlist <- varlist[varlist %in% c("ch4_flux","n2o_flux","co2_flux",
                                  "GPP","R","NEP",
                                  "do_sat","bottom_do_sat",
                                  "tn","tp",
                                  "temp","sonde_zmix")]


# cols <- c("green","yellow","red")
# cols <- c("red","yellow","green")  # make green the highest (rank 3)
cols <- c("#B9E5E8","#7AB2D3","#4A628A")  # blue ramp, light blue is lowest (rank 1) and dark blue is highest (rank 3)


pdf("foodweb_consistency.pdf", onefile=TRUE)

par(mfrow=c(4,2), mar=c(2.5,2.5,2.5,0.5))

for(var in varlist){
  
  tmp <- dat.raw
  tmp$treatment = factor(tmp$treatment, levels=c("ref", "puls"))
  tmp$foodweb = factor(tmp$foodweb, levels=c("low","int","hi"))
  colnames(tmp)[colnames(tmp)==var] <- "var"
  boxplot(var ~ treatment + foodweb, data=tmp)
  mtext(paste0(var), line=1.1, cex=2/3)

  pondmean.ref <- aggregate(dat.raw[dat.raw$treatment=="ref", var], 
                            by=list(dat.raw[dat.raw$treatment=="ref", "foodweb"]), FUN="mean", na.rm=TRUE)
  pondmean.pulse <- aggregate(dat.raw[dat.raw$treatment=="puls", var], 
                            by=list(dat.raw[dat.raw$treatment=="puls", "foodweb"]), FUN="mean", na.rm=TRUE)
  
  plotmat <- cbind(rank(pondmean.ref$x), rank(pondmean.pulse$x))[c(3,2,1),]  # need to update this order [c(3,2,1)] if names (and alphabetical order) of fw levels change
  image(x=1:3, y=1:2, z=plotmat, col=cols, xaxt="n", yaxt="n", xlab="", ylab="")
  axis(1, at=1:3, labels=c("low","intermediate","high"))
  axis(2, at=1:2, labels=c("ref","pulse"))
  mtext(paste0("Spearman correlation = ", cor(pondmean.ref$x, pondmean.pulse$x, method="spearman", 
                                              use="pairwise.complete.obs")), line=0.1, cex=2/3)
  mtext(paste0(var), line=1.1, cex=2/3)

  
}

dev.off()



nreps <- 1000
nvars <- length(varlist)


nMatched <- rep(NA, nreps)

for(ii in 1:nreps){
  
  tmpCount <- 0
  
  for(jj in 1:nvars){
    xx <- sample(1:3, 3)
    yy <- sample(1:3, 3)
    
    if(all(xx==yy)){tmpCount <- tmpCount + 1}
    
  }
  nMatched[ii] <- tmpCount
  
}

hist(nMatched)

quantile(nMatched, 0.95)
