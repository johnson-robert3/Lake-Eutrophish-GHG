#--
# Update arrangement of comparison figure. Group by event type (nutrient pulses vs meteorological events) instead of by comparison (1 vs 2)
#  (easier to visually evaluate comparison results for a given variable and event)
#--



#- Nutrient pulses (comp 1 and 2)
result_nutrients <- matrix(NA, nrow=length(varlist.main), ncol=4)

#- Meteorological events (comp 1 and 2)
result_meteor <- matrix(NA, nrow=length(varlist.main), ncol=4)


# Run the comparison analyses using the matrices of focal variable mean daily values created above and fill into the empty 'result_' matrices
for(var in varlist.main){

  tmp <- data_matrices[[paste0(var)]]
  
  ## Pulse 1
  
  #- P1 comp 1
  pulsetrt.pulse1 <- rowMeans(tmp[pulse_ponds, time_doy %in% pulse1], na.rm=TRUE)
  
  comp1.p1 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)) {break} #stop when we overlap the end of the time series
    
    # only use windows prior to the focal event to build the reference distribution
    if(time_doy[ii]+(wwidth-1) >= min(pulse1)) {break} #stop when we reach the focal event window
    
    comp1.p1 <- cbind(comp1.p1, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.p1 <- NULL
  for(ii in 1:nrow(comp1.p1)){
    ecdf.comp1.p1<- ecdf(comp1.p1[ii,])  # quantile for each pond
    res.p1 <- c(res.p1, ecdf.comp1.p1(pulsetrt.pulse1[ii]))  # vector of quantiles for the three ponds
  }
  result_nutrients[varlist.main==var, 1] <- mean(res.p1)  # mean(res) = the mean quantile for the focal event window
  
  #- P1 comp 2 
  comp2.p1 <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% pulse1)){next} #ignore windows that don't overlap pulses
    
    comp2.p1 <- cbind(comp2.p1, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.p1<- ecdf(comp2.p1)
  result_nutrients[varlist.main==var, 2] <- mean(ecdf.comp2.p1(pulsetrt.pulse1))
  
  
  ## Pulse 2

  #- P2 comp 1
  pulsetrt.pulse2 <- rowMeans(tmp[pulse_ponds, time_doy %in% pulse2], na.rm=TRUE)
  
  comp1.p2 <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    
    if(time_doy[ii]+(wwidth-1) >= min(pulse2)){break} #stop when we reach the focal event window
     
    comp1.p2 <- cbind(comp1.p2, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.p2 <- NULL
  for(ii in 1:nrow(comp1.p2)){
    ecdf.comp1.p2<- ecdf(comp1.p2[ii,])
    res.p2 <- c(res.p2, ecdf.comp1.p2(pulsetrt.pulse2[ii]))
  }
  result_nutrients[varlist.main==var, 3] <- mean(res.p2)

  #- P2 comp 2
  comp2.p2 <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% pulse2)){next} #ignore windows that don't overlap pulses
    
    comp2.p2 <- cbind(comp2.p2, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.p2<- ecdf(comp2.p2)
  result_nutrients[varlist.main==var, 4] <- mean(ecdf.comp2.p2(pulsetrt.pulse2))
    

  ## Heat event
  
  #- H comp 1
  heatwaveobs <- rowMeans(tmp[pulse_ponds, time_doy %in% heatwave], na.rm=TRUE)
  
  comp1.h <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    
    if(time_doy[ii]+(wwidth-1) >= min(heatwave)){break} #stop when we reach the focal event window
     
    comp1.h <- cbind(comp1.h, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.h <- NULL
  for(ii in 1:nrow(comp1.h)){
    ecdf.comp1.h<- ecdf(comp1.h[ii,])
    res.h <- c(res.h, ecdf.comp1.h(heatwaveobs[ii]))
  }
  result_meteor[varlist.main==var, 1] <- mean(res.h)
  
  #- H comp 2
  comp2.h <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% heatwave)){next} #ignore windows that don't overlap pulses
    
    comp2.h <- cbind(comp2.h, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.h<- ecdf(comp2.h)
  result_meteor[varlist.main==var, 2] <- mean(ecdf.comp2.h(heatwaveobs))

  
  ## Derecho 
  
  #- D comp 1
  derechoobs <- rowMeans(tmp[pulse_ponds, time_doy %in% derecho], na.rm=TRUE)
  
  comp1.d <- NULL
  for(ii in 1:length(time_doy)){
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    
    if(time_doy[ii]+(wwidth-1) >= min(derecho)){break} #stop when we reach the focal event window
     
    comp1.d <- cbind(comp1.d, rowMeans(tmp[pulse_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  res.d <- NULL
  for(ii in 1:nrow(comp1.d)){
    ecdf.comp1.d<- ecdf(comp1.d[ii,])
    res.d <- c(res.d, ecdf.comp1.d(derechoobs[ii]))
  }
  result_meteor[varlist.main==var, 3] <- mean(res.d)
  
  #- D comp 2
  comp2.d <- NULL
  for(ii in 1:length(time_doy)){
     
    if(time_doy[ii]+(wwidth-1) > max(time_doy)){break} #stop when we overlap the end of the time series
    ww <- time_doy[ii]:(time_doy[ii] + wwidth - 1)
    if(!any(ww %in% derecho)){next} #ignore windows that don't overlap pulses
    
    comp2.d <- cbind(comp2.d, rowMeans(tmp[ref_ponds, ii:(ii+wwidth-1)], na.rm=TRUE))
  }
  ecdf.comp2.d<- ecdf(comp2.d)
  result_meteor[varlist.main==var, 4] <- mean(ecdf.comp2.d(derechoobs))
  
}
  
  
#--
# Figures
#--

pal <- colorRampPalette(colors=c('#009392', '#fdfbe4', '#d0587e')) 

prettyNames <- c(expression('CH'['4']~'flux'),
                 # expression('N'['2']*'O'~'flux'),
                 expression('CO'['2']~'flux'),
                 #'Chlorophyll-a',
                 'GPP',
                 'R',
                 'NEP',
                 'Surface DO',
                 'Bottom DO',
                 'Surface Temp.',
                 'Bottom Temp.',
                 #'SRP',
                 #expression('NO'['2']+'NO'['3']),
                 'Total N',
                 'Total P'
                 #'DOC',
                 # 'Temperature',
                 #'Spec. cond.',
                 # expression('Z'['mix'])
                 )


atx <- rep(1:4, each=length(varlist.main))
aty <- rep(length(varlist.main):1, times=4)
aty2 <- rep(1:length(varlist.main), times=4)


# png("event_quantiles_rj_var-update.png", res=300, units="in", width=6.5, height=4.5)
pdf("event_quantiles_rj_var-update_test.pdf", width=6.5, height=4.5)

# windows(height = 4.5, width = 6.5)

layout(matrix(1:3, ncol=3), widths=c(0.45,0.45,0.15))
par(mar=c(6.1,1.1,3.1,0.6), oma=c(0,6,0,2), mgp=c(2.5,1,0))

# P1 and P2
image(x=1:4, y=1:length(varlist.main), z=t(result_nutrients)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("P1 Comp 1", "P1 Comp 2", "P2 Comp 1", "P2 Comp 2"), las=2)
axis(2, at=1:length(varlist.main), labels=rev(prettyNames), las=2)
mtext("Nutrient Pulses", cex=3/4, line=0.2)
mtext(expression(bold("B")), cex=3/4, at=0.3, line=0.3)
abline(h=c(2,6,9)+0.5)
text(atx, aty, round(c(result_nutrients), 2))

# H and D
image(x=1:4, y=1:length(varlist.main), z=t(result_meteor)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("H Comp 1", "H Comp 2", "D Comp 1", "D Comp 2"), las=2)
axis(2, at=1:length(varlist.main), labels=NA, las=2)
mtext("Meteorological Events", cex=3/4, line=0.2)
mtext(expression(bold("C")), cex=3/4, at=0.3, line=0.3)
abline(h=c(2,6,9)+0.5)
text(atx, aty, round(c(result_meteor), 2))

par(mar=c(6.1,3.6,2.1,1.1))
image(t(matrix(1:21)), col=pal(21), xaxt="n", ylab="Quantile of pulsed pond event windows versus comparison", cex.lab=1.1)
mtext("Values > comparison", side=4, at=0.9, line=0.75, cex=3/4)
mtext("Values < comparison", side=4, at=0.1, line=0.75, cex=3/4)

dev.off()



 
  
