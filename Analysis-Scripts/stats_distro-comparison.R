## Explore hort farm pond GHG dataset from Robert Johnson & Grace Wilkinson 
## and begin prototyping new analyses

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dat.raw <- read.csv("../data/ghg-model-dataset_2022-07-26.csv")

## basic exploration of data contents

str(dat.raw)
unique(dat.raw$pond_id)
unique(dat.raw$doy)
unique(dat.raw$treatment)


## define some time periods & units of comparison/grouping

wwidth <- 5 #days: width of windows, e.g. following nutrient pulses
pulse1 <- 176: (176 + wwidth - 1)#doy
pulse2 <- 211: (211 + wwidth - 1)
derecho <- 220: (220 + wwidth - 1)
heatwave <- 190:195

pond_id <- unique(dat.raw$ pond_id)
time_doy <- min(dat.raw$doy):max(dat.raw$doy)
ref_ponds <- pond_id %in% unique(dat.raw$pond_id[dat.raw$treatment=="reference"])
pulse_ponds <- pond_id %in% unique(dat.raw$pond_id[dat.raw$treatment=="pulsed"])


## turn data into matrices --  mostly because Jon likes things this way


varlist <- colnames(dat.raw)
varlist <- varlist[!varlist %in% c("pond_id","treatment","date","doy","period","period2","week","sonde_strat")]

data_matrices <- list()

for(var in varlist){
  
  tmp <- matrix(NA, nrow=length(pond_id), ncol=length(time_doy))
  
  for(ii in 1:length(pond_id)){
    for(jj in 1:length(time_doy)){
      
      tmp[ii,jj] <- mean(dat.raw[,paste0(var)][dat.raw$pond_id==pond_id[ii] & dat.raw$doy==time_doy[jj]], na.rm=TRUE)

      
    }
  }
  data_matrices[[paste0(var)]] <- tmp
}



pdf("../distro_comparison_rough.pdf", onefile=TRUE)


for(var in varlist){

  tmp <- data_matrices[[paste0(var)]]
  
  pulsetrt.pulse1 <- tmp[pulse_ponds, time_doy %in% pulse1]
  pulsetrt.pulse2 <- tmp[pulse_ponds, time_doy %in% pulse2]
  heatwaveobs <- tmp[pulse_ponds, time_doy %in% heatwave]
  derechoobs <- tmp[pulse_ponds, time_doy %in% derecho]
  
  ## comparison 1: pulses to other n-day windows in pulsed ponds
  comparison1 <- NULL
  
  for(ii in 1:length(time_doy)){
    
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    if(time_doy[ii] %in% pulse1){next} #ignore obs in pulse 1
    if(time_doy[ii] %in% pulse2){next} #ignore obs in pulse 2
    
    comparison1 <- cbind(comparison1, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
    
  }
  
  ecdf.ref1<- ecdf(colMeans(comparison1))
  
  hist(colMeans(comparison1), main="Comparison vs. other time periods", xlab=paste0(var))
  abline(v=mean(pulsetrt.pulse1, na.rm=T), col="red")
  abline(v=mean(pulsetrt.pulse2, na.rm=T), col="blue")
  abline(v=mean(heatwaveobs, na.rm=T), col="magenta")
  abline(v=mean(derechoobs, na.rm=T), col="green")
  legend("topright", lty=1, col=c("red","blue","magenta","green"),legend=c("Pulse 1", "Pulse 2","heatwave","derecho"))
  text(mean(pulsetrt.pulse1, na.rm=T), 0, round(ecdf.ref1(mean(pulsetrt.pulse1, na.rm=T)), 2), pos=1, col="red")
  text(mean(pulsetrt.pulse2, na.rm=T), 0, round(ecdf.ref1(mean(pulsetrt.pulse2, na.rm=T)), 2), pos=1, col="blue")
  
  ## comparison 2: pulsed ponds to same time in reference ponds
  comparison2 <- NULL
  
  for(ii in 1:length(time_doy)){
    
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% pulse1) & !any(ww %in% pulse2)){next} #ignore windows that don't overlap pulses
    comparison2 <- cbind(comparison2, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
    
  }
  
  ecdf.ref2<- ecdf(colMeans(comparison2))
  
  hist(colMeans(comparison2), main="Comparison vs. reference ponds", xlab=paste0(var))
  abline(v=mean(pulsetrt.pulse1, na.rm=T), col="red")
  abline(v=mean(pulsetrt.pulse2, na.rm=T), col="blue")
  abline(v=mean(heatwaveobs, na.rm=T), col="magenta")
  abline(v=mean(derechoobs, na.rm=T), col="green")
  legend("topright", lty=1, col=c("red","blue","magenta","green"),legend=c("Pulse 1", "Pulse 2","heatwave","derecho"))
  text(mean(pulsetrt.pulse1, na.rm=T), 0, round(ecdf.ref2(mean(pulsetrt.pulse1, na.rm=T)), 2), pos=1, col="red")
  text(mean(pulsetrt.pulse2, na.rm=T), 0, round(ecdf.ref2(mean(pulsetrt.pulse2, na.rm=T)), 2), pos=1, col="blue")
  
  
}



dev.off()


