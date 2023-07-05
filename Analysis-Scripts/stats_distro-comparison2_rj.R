## Explore hort farm pond GHG dataset from Robert Johnson & Grace Wilkinson 
## and begin prototyping new analyses


# Robert playing around


rm(list=ls())

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dat.raw <- read.csv("Data/ghg-model-dataset_2023-03-21.csv")

# absolute value of ecosystem respiration values (lower values of R denote higher rates of respiration)
dat.raw <- dat.raw |> dplyr::mutate(R = abs(R))

## basic exploration of data contents

str(dat.raw)
unique(dat.raw$pond_id)
unique(dat.raw$doy)
unique(dat.raw$treatment)


## define some time periods & units of comparison/grouping

# nutrients pulses were DOYs 176 and 211; nutrients added after all sampling occurred 
# heatwave was DOYs 185-190
# Derecho on DOY 223 (Aug. 10, 2020)

wwidth <- 5 #days: width of windows, e.g. following nutrient pulses
pulse1 <- 177: (177 + wwidth - 1)#doy
pulse2 <- 212: (212 + wwidth - 1)
derecho <- 224: (224 + wwidth - 1)
heatwave <- 185:190

pond_id <- unique(dat.raw$ pond_id)
time_doy <- min(dat.raw$doy):max(dat.raw$doy)
ref_ponds <- pond_id %in% unique(dat.raw$pond_id[dat.raw$treatment=="reference"])
pulse_ponds <- pond_id %in% unique(dat.raw$pond_id[dat.raw$treatment=="pulsed"])


## turn data into matrices --  mostly because Jon likes things this way


# varlist <- colnames(dat.raw)
# varlist <- varlist[!varlist %in% c("pond_id","treatment","date","doy","period","period2","week","sonde_strat")]
varlist.main <- c("ch4_flux","n2o_flux","co2_flux",
                  "GPP","R","NEP",
                  "do_sat","bottom_do_sat",
                  "tn","tp",
                  "temp","sonde_zmix")


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
{
png("flux_timeseries_rj.png", res=300, units="in", height=6.5/3, width=6.5)

par(mfrow=c(1,3), mar=c(3.3,3.3,1.3,0.9), mgp=c(2,0.8,0))

# CH4
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
rect(xleft=min(pulse1), xright=max(pulse1), ybottom=par("usr")[3], ytop=par("usr")[4]) # pulse 1
rect(xleft=min(heatwave), xright=max(heatwave), ybottom=par("usr")[3], ytop=par("usr")[4]) # heatwave
rect(xleft=min(pulse2), xright=max(pulse2), ybottom=par("usr")[3], ytop=par("usr")[4]) # pulse 2
rect(xleft=min(derecho), xright=max(derecho), ybottom=par("usr")[3], ytop=par("usr")[4]) # derecho
axis(side=3, at=c(178,192.5,213,222), tick=FALSE, labels=c("P1","H","P2","D"), line=-0.7, cex.axis=0.9, gap.axis=0)

# N2O
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
rect(xleft=min(pulse1), xright=max(pulse1), ybottom=par("usr")[3], ytop=par("usr")[4]) # pulse 1
rect(xleft=min(heatwave), xright=max(heatwave), ybottom=par("usr")[3], ytop=par("usr")[4]) # heatwave
rect(xleft=min(pulse2), xright=max(pulse2), ybottom=par("usr")[3], ytop=par("usr")[4]) # pulse 2
rect(xleft=min(derecho), xright=max(derecho), ybottom=par("usr")[3], ytop=par("usr")[4]) # derecho
axis(side=3, at=c(178,192.5,213,222), tick=FALSE, labels=c("P1","H","P2","D"), line=-0.7, cex.axis=0.9, gap.axis=0)

# CO2
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
rect(xleft=min(pulse1), xright=max(pulse1), ybottom=par("usr")[3], ytop=par("usr")[4]) # pulse 1
rect(xleft=min(heatwave), xright=max(heatwave), ybottom=par("usr")[3], ytop=par("usr")[4]) # heatwave
rect(xleft=min(pulse2), xright=max(pulse2), ybottom=par("usr")[3], ytop=par("usr")[4]) # pulse 2
rect(xleft=min(derecho), xright=max(derecho), ybottom=par("usr")[3], ytop=par("usr")[4]) # derecho
axis(side=3, at=c(178,192.5,213,222), tick=FALSE, labels=c("P1","H","P2","D"), line=-0.7, cex.axis=0.9, gap.axis=0)
legend("topleft", lwd=2, col=c("orange","purple"), legend=c("Reference","Pulsed"), box.lwd=0, seg.len=1.5, inset=0.01)

dev.off()
}

## Look at quantiles of pulse ponds relative to focal comparisons

result_otherTimes <- matrix(NA, nrow=length(varlist.main), ncol=4)
result_refPonds <- matrix(NA, nrow=length(varlist.main), ncol=4)


for(var in varlist.main){

  tmp <- data_matrices[[paste0(var)]]
  
  ## comparison 1: pulses to other n-day windows in pulsed ponds
  
  #- P1
  pulsetrt.pulse1 <- rowMeans(tmp[pulse_ponds, time_doy %in% pulse1], na.rm=TRUE)
  
  comp1.p1 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)) {break} #stop when we overlap the end of the time series
     
    # if(time_doy[ii] %in% pulse1) {next} #ignore obs in pulse 1  # RAJ: I think this still allows overlap w/ the focal window, it only skips if the 1st day of the window overlaps the focal window
    
    # RAJ update to window overlap
    # ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1) # borrowed syntax from comp2
    # if(any(ww %in% pulse1)) {next} # ignore windows that have any overlap with the focal event window
    
    # only use windows prior to the focal event to build the reference distribution
    if(time_doy[ii]+(wwidth-1) >= min(pulse1)) {break} #stop when we reach the focal event window
    
    comp1.p1 <- cbind(comp1.p1, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.p1 <- NULL
  for(ii in 1:nrow(comp1.p1)){
    ecdf.comp1.p1<- ecdf(comp1.p1[ii,])  # quantile for each pond
    res.p1 <- c(res.p1, ecdf.comp1.p1(pulsetrt.pulse1[ii]))  # vector of quantiles for the three ponds
  }
  result_otherTimes[varlist.main==var, 1] <- mean(res.p1)  # mean(res) = the mean quantile for the focal event window
  
  #- P2
  pulsetrt.pulse2 <- rowMeans(tmp[pulse_ponds, time_doy %in% pulse2], na.rm=TRUE)
  
  comp1.p2 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    # if(time_doy[ii] %in% pulse2){next} #ignore obs in pulse 2
    
    # ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1) # borrowed syntax from comp2
    # if(any(ww %in% pulse2)) {next} # ignore windows that have any overlap with the focal event window
    
    if(time_doy[ii]+(wwidth-1) >= min(pulse2)){break} #stop when we reach the focal event window
     
    comp1.p2 <- cbind(comp1.p2, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.p2 <- NULL
  for(ii in 1:nrow(comp1.p2)){
    ecdf.comp1.p2<- ecdf(comp1.p2[ii,])
    res.p2 <- c(res.p2, ecdf.comp1.p2(pulsetrt.pulse2[ii]))
  }
  result_otherTimes[varlist.main==var, 3] <- mean(res.p2)
  
  #- Heatwave
  heatwaveobs <- rowMeans(tmp[pulse_ponds, time_doy %in% heatwave], na.rm=TRUE)
  
  comp1.h <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    # if(time_doy[ii] %in% heatwave){next} #ignore obs in heatwave
    
    # ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1) # borrowed syntax from comp2
    # if(any(ww %in% heatwave)) {next} # ignore windows that have any overlap with the focal event window
    
    if(time_doy[ii]+(wwidth-1) >= min(heatwave)){break} #stop when we reach the focal event window
     
    comp1.h <- cbind(comp1.h, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.h <- NULL
  for(ii in 1:nrow(comp1.h)){
    ecdf.comp1.h<- ecdf(comp1.h[ii,])
    res.h <- c(res.h, ecdf.comp1.h(heatwaveobs[ii]))
  }
  result_otherTimes[varlist.main==var, 2] <- mean(res.h)
  
  #- Derecho
  derechoobs <- rowMeans(tmp[pulse_ponds, time_doy %in% derecho], na.rm=TRUE)
  
  comp1.d <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    # if(time_doy[ii] %in% derecho){next} #ignore obs in derecho
    
    # ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1) # borrowed syntax from comp2
    # if(any(ww %in% derecho)) {next} # ignore windows that have any overlap with the focal event window
    
    if(time_doy[ii]+(wwidth-1) >= min(derecho)){break} #stop when we reach the focal event window
     
    comp1.d <- cbind(comp1.d, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.d <- NULL
  for(ii in 1:nrow(comp1.d)){
    ecdf.comp1.d<- ecdf(comp1.d[ii,])
    res.d <- c(res.d, ecdf.comp1.d(derechoobs[ii]))
  }
  result_otherTimes[varlist.main==var, 4] <- mean(res.d)
 
  
  ## comparison 2: pulsed ponds to same time in reference ponds
  
  #- P1
  comp2.p1 <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% pulse1)){next} #ignore windows that don't overlap pulses
    
    comp2.p1 <- cbind(comp2.p1, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.p1<- ecdf(comp2.p1)
  result_refPonds[varlist.main==var, 1] <- mean(ecdf.comp2.p1(pulsetrt.pulse1))
  
  #- P2
  comp2.p2 <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% pulse2)){next} #ignore windows that don't overlap pulses
    
    comp2.p2 <- cbind(comp2.p2, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.p2<- ecdf(comp2.p2)
  result_refPonds[varlist.main==var, 3] <- mean(ecdf.comp2.p2(pulsetrt.pulse2))
  
  #- Heatwave
  comp2.h <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% heatwave)){next} #ignore windows that don't overlap pulses
    
    comp2.h <- cbind(comp2.h, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.h<- ecdf(comp2.h)
  result_refPonds[varlist.main==var, 2] <- mean(ecdf.comp2.h(heatwaveobs))
  
  #- Derecho
  comp2.d <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% derecho)){next} #ignore windows that don't overlap pulses
    
    comp2.d <- cbind(comp2.d, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.d<- ecdf(comp2.d)
  result_refPonds[varlist.main==var, 4] <- mean(ecdf.comp2.d(derechoobs))
  
}



# pal <- colorRampPalette(colors=c("red","white","blue"))
# pal <- colorRampPalette(colors=c('#009392', '#72aaa1', '#b1c7b3', '#f1eac8', '#e5b9ad', '#d98994', '#d0587e'))  # TealRose
# pal <- colorRampPalette(colors=c('#009B9E','#42B7B9','#A7D3D4','#F1F1F1','#E4C1D9','#D691C1','#C75DAB'))  # Tropic
# pal <- colorRampPalette(colors=c('#A16928','#bd925a','#d6bd8d','#edeac2','#b5c8b8','#79a7ac','#2887a1'))  # Earth

pal <- colorRampPalette(colors=c('#009392', '#fdfbe4', '#d0587e')) 

prettyNames <- c(expression('CH'['4']~'flux'),
                 expression('N'['2']*'O'~'flux'),
                 expression('CO'['2']~'flux'),
                 #'Chlorophyll-a',
                 'GPP',
                 'R',
                 'NEP',
                 'Surface DO',
                 'Bottom DO',
                 #'SRP',
                 #expression('NO'['2']+'NO'['3']),
                 'Total N',
                 'Total P',
                 #'DOC',
                 'Temperature',
                 #'Spec. cond.',
                 expression('Z'['mix'])
                 )



atx <- rep(1:4, each=length(varlist.main))
aty <- rep(length(varlist.main):1, times=4)
aty2 <- rep(1:length(varlist.main), times=4)

# png("event_quantiles_rj_absR.png", res=300, units="in", width=6.5, height=6.5)
# png("event_quantiles_rj_absR_comp1-window-update.png", res=300, units="in", width=6.5, height=6.5)
png("event_quantiles_rj_absR_comp1-prev-times-only.png", res=300, units="in", width=6.5, height=4.5)

# windows(height = 4.5, width = 6.5)

layout(matrix(1:3, ncol=3), widths=c(0.45,0.45,0.15))
par(mar=c(6.1,1.1,3.1,0.6), oma=c(0,6,0,2), mgp=c(2.5,1,0))

# Comparison 1
image(x=1:4, y=1:length(varlist.main), z=t(result_otherTimes)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("Pulse 1", "Heat event", "Pulse 2", "Derecho"), las=2)
axis(2, at=1:length(varlist.main), labels=rev(prettyNames), las=2)
mtext("Comparison to prior \nwindows in pulsed ponds", cex=3/4, line=0.2)
mtext(expression(bold("A)")), cex=3/4, at=0.35, line=0.2)
abline(h=c(2,6,9)+0.5)
text(atx, aty, round(c(result_otherTimes), 2))

# Comparison 2
image(x=1:4, y=1:length(varlist.main), z=t(result_refPonds)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("Pulse 1", "Heat event", "Pulse 2", "Derecho"), las=2)
axis(2, at=1:length(varlist.main), labels=NA, las=2)
mtext("Comparison to overlapping \nwindows in reference ponds", cex=3/4, line=0.2)
mtext(expression(bold("B)")), cex=3/4, at=0.35, line=0.2)
abline(h=c(2,6,9)+0.5)
text(atx, aty, round(c(result_refPonds), 2))

par(mar=c(6.1,3.6,2.1,1.1))
image(t(matrix(1:21)), col=pal(21), xaxt="n", ylab="Quantile of pulsed pond event windows versus comparison", cex.lab=1.1)
mtext("Values > comparison", side=4, at=0.9, line=0.75, cex=3/4)
mtext("Values < comparison", side=4, at=0.1, line=0.75, cex=3/4)

dev.off()


# Create boxplot and histogram examples as demonstrations to accompany comparison quantile figure





## plot all time series
# pdf("all_timeseries.pdf", onefile=TRUE)
# 
# for(var in varlist.main){
#   
#   tmp <- data_matrices[[var]]
#   
#   plot(NA, NA , xlim=range(time_doy), ylim=range(tmp, na.rm=T),
#        xlab="doy", ylab=var)
#   
#   rect(xleft=min(pulse1), xright=max(pulse1), ybottom=par("usr")[3], ytop=par("usr")[4], col="grey", border=NA) # pulse 1
#   rect(xleft=min(heatwave), xright=max(heatwave), ybottom=par("usr")[3], ytop=par("usr")[4], col="grey", border=NA) # heatwave
#   rect(xleft=min(pulse2), xright=max(pulse2), ybottom=par("usr")[3], ytop=par("usr")[4], col="grey", border=NA) # pulse 2
#   rect(xleft=min(derecho), xright=max(derecho), ybottom=par("usr")[3], ytop=par("usr")[4], col="grey", border=NA) # derecho
#   axis(side=3, at=c(mean(pulse1),mean(heatwave),mean(pulse2),mean(derecho)), tick=FALSE, labels=c("P1","H","P2","D"), line=-0.7, cex.axis=0.9, gap.axis=0)
#   
#   
#   lines(time_doy[!is.na(tmp[1,])], tmp[1,!is.na(tmp[1,])], col=t_col("purple"))
#   lines(time_doy[!is.na(tmp[2,])], tmp[2,!is.na(tmp[2,])], col=t_col("purple"))
#   lines(time_doy[!is.na(tmp[3,])], tmp[3,!is.na(tmp[3,])], col=t_col("purple"))
#   lines(time_doy[!is.na(tmp[4,])], tmp[4,!is.na(tmp[4,])], col=t_col("orange"))
#   lines(time_doy[!is.na(tmp[5,])], tmp[5,!is.na(tmp[5,])], col=t_col("orange"))
#   lines(time_doy[!is.na(tmp[6,])], tmp[6,!is.na(tmp[6,])], col=t_col("orange"))
#   lines(time_doy[!is.na(colMeans(tmp[pulse_ponds,]))], colMeans(tmp[pulse_ponds,])[!is.na(colMeans(tmp[pulse_ponds,]))], col="purple", lwd=2)
#   lines(time_doy[!is.na(colMeans(tmp[ref_ponds,]))], colMeans(tmp[ref_ponds,])[!is.na(colMeans(tmp[ref_ponds,]))], col="orange", lwd=2)
#   
#   
#   mtext(var, line=2)
#   
# }
# 
# dev.off()


