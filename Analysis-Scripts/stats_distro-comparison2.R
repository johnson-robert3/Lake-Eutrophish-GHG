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
varlist.main <- c("ch4_flux","co2_flux","n2o_flux","chla","GPP","R",
                                       "NEP","do_sat","bottom_do_sat","srp","nox","tn","tp","doc_ppb","temp",
                                       "sp_cond","sonde_zmix")

data_matrices <- list()

for(var in varlist.main){
  
  tmp <- matrix(NA, nrow=length(pond_id), ncol=length(time_doy))
  
  for(ii in 1:length(pond_id)){
    for(jj in 1:length(time_doy)){
      
      tmp[ii,jj] <- mean(dat.raw[,paste0(var)][dat.raw$pond_id==pond_id[ii] & dat.raw$doy==time_doy[jj]], na.rm=TRUE)

      
    }
  }
  data_matrices[[paste0(var)]] <- tmp
}


t_col <- function(color, percent = 50) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100)
  
  return(t.col)
}


## plot time series of focal variables

png("../flux_timeseries.png", res=300, units="in", height=6.5/3, width=6.5)

par(mfrow=c(1,3), mar=c(3.3,3.3,1.3,0.9), mgp=c(2,0.8,0))

tmp <- data_matrices[["ch4_flux"]]
tmppulsemean <- colMeans(tmp[pulse_ponds,])
tmppulsemin <- apply(tmp[pulse_ponds,], 2, FUN="min")
tmppulsemax <- apply(tmp[pulse_ponds,], 2, FUN="max")
tmprefmean <- colMeans(tmp[ref_ponds,])
tmprefmin <- apply(tmp[ref_ponds,], 2, FUN="min")
tmprefmax <- apply(tmp[ref_ponds,], 2, FUN="max")

plot(NA, NA, ylab=expression('CH'['4']~'flux'), xlab="DOY", col=NA, xlim=range(time_doy), ylim=range(tmp, na.rm=T))
polygon(x=c(time_doy[!is.na(tmprefmin)], rev(time_doy[!is.na(tmprefmax)]), time_doy[!is.na(tmprefmin)][1]),
        y=c(tmprefmin[!is.na(tmprefmin)], rev(tmprefmax[!is.na(tmprefmax)]), tmprefmin[!is.na(tmprefmin)][1]),
        col=t_col("orange"), border=NA)
lines(time_doy[!is.na(colMeans(tmp))], colMeans(tmp[ref_ponds,])[!is.na(colMeans(tmp))], lwd=2, col="orange")
polygon(x=c(time_doy[!is.na(tmppulsemin)], rev(time_doy[!is.na(tmppulsemax)]), time_doy[!is.na(tmppulsemin)][1]),
        y=c(tmppulsemin[!is.na(tmppulsemin)], rev(tmppulsemax[!is.na(tmppulsemax)]), tmppulsemin[!is.na(tmppulsemin)][1]),
        col=t_col("purple"), border=NA)
lines(time_doy[!is.na(colMeans(tmp))], colMeans(tmp[pulse_ponds,])[!is.na(colMeans(tmp))], lwd=2, col="purple")
rect(xleft=min(pulse1), xright=max(pulse1), ybottom=par("usr")[3], ytop=par("usr")[4])#pulse 1
rect(xleft=min(heatwave), xright=max(heatwave), ybottom=par("usr")[3], ytop=par("usr")[4])#heatwave
rect(xleft=min(pulse2), xright=max(pulse2), ybottom=par("usr")[3], ytop=par("usr")[4])#pulse 1
rect(xleft=min(derecho), xright=max(derecho), ybottom=par("usr")[3], ytop=par("usr")[4])#derecho
axis(side=3, at=c(178,192.5,213,222), tick=FALSE, labels=c("P1","H","P2","D"), line=-0.7, cex.axis=0.9, gap.axis=0)

tmp <- data_matrices[["co2_flux"]]
tmppulsemean <- colMeans(tmp[pulse_ponds,])
tmppulsemin <- apply(tmp[pulse_ponds,], 2, FUN="min")
tmppulsemax <- apply(tmp[pulse_ponds,], 2, FUN="max")
tmprefmean <- colMeans(tmp[ref_ponds,])
tmprefmin <- apply(tmp[ref_ponds,], 2, FUN="min")
tmprefmax <- apply(tmp[ref_ponds,], 2, FUN="max")

plot(NA, NA, ylab=expression('CO'['2']~'flux'), xlab="DOY", col=NA, xlim=range(time_doy), ylim=range(tmp, na.rm=T))
polygon(x=c(time_doy[!is.na(tmprefmin)], rev(time_doy[!is.na(tmprefmax)]), time_doy[!is.na(tmprefmin)][1]),
        y=c(tmprefmin[!is.na(tmprefmin)], rev(tmprefmax[!is.na(tmprefmax)]), tmprefmin[!is.na(tmprefmin)][1]),
        col=t_col("orange"), border=NA)
lines(time_doy[!is.na(colMeans(tmp))], colMeans(tmp[ref_ponds,])[!is.na(colMeans(tmp))], lwd=2, col="orange")
polygon(x=c(time_doy[!is.na(tmppulsemin)], rev(time_doy[!is.na(tmppulsemax)]), time_doy[!is.na(tmppulsemin)][1]),
        y=c(tmppulsemin[!is.na(tmppulsemin)], rev(tmppulsemax[!is.na(tmppulsemax)]), tmppulsemin[!is.na(tmppulsemin)][1]),
        col=t_col("purple"), border=NA)
lines(time_doy[!is.na(colMeans(tmp))], colMeans(tmp[pulse_ponds,])[!is.na(colMeans(tmp))], lwd=2, col="purple")
rect(xleft=min(pulse1), xright=max(pulse1), ybottom=par("usr")[3], ytop=par("usr")[4])#pulse 1
rect(xleft=min(heatwave), xright=max(heatwave), ybottom=par("usr")[3], ytop=par("usr")[4])#heatwave
rect(xleft=min(pulse2), xright=max(pulse2), ybottom=par("usr")[3], ytop=par("usr")[4])#pulse 1
rect(xleft=min(derecho), xright=max(derecho), ybottom=par("usr")[3], ytop=par("usr")[4])#derecho
axis(side=3, at=c(178,192.5,213,222), tick=FALSE, labels=c("P1","H","P2","D"), line=-0.7, cex.axis=0.9, gap.axis=0)
legend("topleft", lwd=2, col=c("orange","purple"), legend=c("Reference","Pulsed"), box.lwd=0, seg.len=1.5, inset=0.01)

tmp <- data_matrices[["n2o_flux"]]
tmppulsemean <- colMeans(tmp[pulse_ponds,])
tmppulsemin <- apply(tmp[pulse_ponds,], 2, FUN="min")
tmppulsemax <- apply(tmp[pulse_ponds,], 2, FUN="max")
tmprefmean <- colMeans(tmp[ref_ponds,])
tmprefmin <- apply(tmp[ref_ponds,], 2, FUN="min")
tmprefmax <- apply(tmp[ref_ponds,], 2, FUN="max")


plot(NA, NA, ylab=expression('N'['2']*'O'~'flux'), xlab="DOY", col=NA, xlim=range(time_doy), ylim=range(tmp, na.rm=T))
polygon(x=c(time_doy[!is.na(tmprefmin)], rev(time_doy[!is.na(tmprefmax)]), time_doy[!is.na(tmprefmin)][1]),
        y=c(tmprefmin[!is.na(tmprefmin)], rev(tmprefmax[!is.na(tmprefmax)]), tmprefmin[!is.na(tmprefmin)][1]),
        col=t_col("orange"), border=NA)
lines(time_doy[!is.na(colMeans(tmp))], colMeans(tmp[ref_ponds,])[!is.na(colMeans(tmp))], lwd=2, col="orange")
polygon(x=c(time_doy[!is.na(tmppulsemin)], rev(time_doy[!is.na(tmppulsemax)]), time_doy[!is.na(tmppulsemin)][1]),
        y=c(tmppulsemin[!is.na(tmppulsemin)], rev(tmppulsemax[!is.na(tmppulsemax)]), tmppulsemin[!is.na(tmppulsemin)][1]),
        col=t_col("purple"), border=NA)
lines(time_doy[!is.na(colMeans(tmp))], colMeans(tmp[pulse_ponds,])[!is.na(colMeans(tmp))], lwd=2, col="purple")
rect(xleft=min(pulse1), xright=max(pulse1), ybottom=par("usr")[3], ytop=par("usr")[4])#pulse 1
rect(xleft=min(heatwave), xright=max(heatwave), ybottom=par("usr")[3], ytop=par("usr")[4])#heatwave
rect(xleft=min(pulse2), xright=max(pulse2), ybottom=par("usr")[3], ytop=par("usr")[4])#pulse 1
rect(xleft=min(derecho), xright=max(derecho), ybottom=par("usr")[3], ytop=par("usr")[4])#derecho
axis(side=3, at=c(178,192.5,213,222), tick=FALSE, labels=c("P1","H","P2","D"), line=-0.7, cex.axis=0.9, gap.axis=0)

dev.off()


## Look at quantiles of pulse ponds relative to focal comparisons

result_otherTimes <- matrix(NA, nrow=length(varlist.main), ncol=4)
result_refPonds <- matrix(NA, nrow=length(varlist.main), ncol=4)


for(var in varlist.main){

  tmp <- data_matrices[[paste0(var)]]
  
  ## comparison 1: pulses to other n-day windows in pulsed ponds
  pulsetrt.pulse1 <- rowMeans(tmp[pulse_ponds, time_doy %in% pulse1], na.rm=TRUE)
  comp1 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    if(time_doy[ii] %in% pulse1){next} #ignore obs in pulse 1
    comp1 <- cbind(comp1, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp1<- ecdf(comp1)
  result_otherTimes[varlist.main==var, 1] <- mean(ecdf.comp1(pulsetrt.pulse1))
  
  pulsetrt.pulse2 <- rowMeans(tmp[pulse_ponds, time_doy %in% pulse2], na.rm=TRUE)
  comp1 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    if(time_doy[ii] %in% pulse2){next} #ignore obs in pulse 1
    comp1 <- cbind(comp1, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp1<- ecdf(comp1)
  result_otherTimes[varlist.main==var, 3] <- mean(ecdf.comp1(pulsetrt.pulse2))
  
  heatwaveobs <- rowMeans(tmp[pulse_ponds, time_doy %in% heatwave], na.rm=TRUE)
  comp1 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    if(time_doy[ii] %in% heatwave){next} #ignore obs in pulse 1
    comp1 <- cbind(comp1, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp1<- ecdf(comp1)
  result_otherTimes[varlist.main==var, 2] <- mean(ecdf.comp1(heatwaveobs))
  
  derechoobs <- rowMeans(tmp[pulse_ponds, time_doy %in% derecho], na.rm=TRUE)
  comp1 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    if(time_doy[ii] %in% derecho){next} #ignore obs in pulse 1
    comp1 <- cbind(comp1, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp1<- ecdf(comp1)
  result_otherTimes[varlist.main==var, 4] <- mean(ecdf.comp1(derechoobs))
  
 
  
  ## comparison 2: pulsed ponds to same time in reference ponds
  comp2 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% pulse1)){next} #ignore windows that don't overlap pulses
    comp2 <- cbind(comp2, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2<- ecdf(comp2)
  result_refPonds[varlist.main==var, 1] <- mean(ecdf.comp2(pulsetrt.pulse1))
  
  comp2 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% pulse2)){next} #ignore windows that don't overlap pulses
    comp2 <- cbind(comp2, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2<- ecdf(comp2)
  result_refPonds[varlist.main==var, 3] <- mean(ecdf.comp2(pulsetrt.pulse2))
  
  comp2 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% heatwave)){next} #ignore windows that don't overlap pulses
    comp2 <- cbind(comp2, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2<- ecdf(comp2)
  result_refPonds[varlist.main==var, 2] <- mean(ecdf.comp2(heatwaveobs))
  
  comp2 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% derecho)){next} #ignore windows that don't overlap pulses
    comp2 <- cbind(comp2, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2<- ecdf(comp2)
  result_refPonds[varlist.main==var, 4] <- mean(ecdf.comp2(derechoobs))
  
}




pal <- colorRampPalette(colors=c("red","white","blue"))
prettyNames <- c(expression('CH'['4']~'flux'),
                 expression('CO'['2']~'flux'),
                 expression('N'['2']*'O'~'flux'),
                 'Chlorophyll-a',
                 'GPP',
                 'R',
                 'NEP',
                 'Surface DO',
                 'Bottom DO',
                 'SRP',
                 expression('NO'['2']+'NO'['3']),
                 'Total N',
                 'Total P',
                 'DOC',
                 'Temperature',
                 'Spec. cond.',
                 expression('Z'['mix'])
                 )


png("../event_quantiles.png", res=300, units="in", width=6.5, height=8)

layout(matrix(1:3, ncol=3), widths=c(0.45,0.45,0.15))
par(mar=c(6.1,2.1,2.1,0.6), oma=c(0,5,0,0), mgp=c(2.5,1,0))

image(x=1:4, y=1:length(varlist.main), z=t(result_otherTimes)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("Pulse 1", "Heatwave", "Pulse 2", "Derecho"), las=2)
axis(2, at=1:length(varlist.main), labels=rev(prettyNames), las=2)
mtext("Comparison v. other times", cex=3/4, line=0.2)
mtext("A)", cex=3/4, at=0.5, line=0.2)
abline(h=c(3,10,14)+0.5)

image(x=1:4, y=1:length(varlist.main), z=t(result_refPonds)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("Pulse 1", "Heatwave", "Pulse 2", "Derecho"), las=2)
mtext("Comparison v. reference ponds", cex=3/4, line=0.2)
mtext("B)", cex=3/4, at=0.5, line=0.2)
abline(h=c(3,10,14)+0.5)

par(mar=c(6.1,3.6,2.1,1.1))
image(t(matrix(1:21)), col=pal(21), xaxt="n", ylab="Quantile")


dev.off()


