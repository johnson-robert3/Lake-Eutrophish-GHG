#~~~
# Script to run the comparison analyses used in the manuscript
#
# By: JA Walter (w/ minor updates by R. Johnson)
#~~~


rm(list=ls())

# data
dat.raw <- read.csv("Data/ghg-model-dataset_ms-data.csv")  # created from the 'data_model-dataset' script


# absolute value of ecosystem respiration values (lower values of R denote higher rates of respiration)
dat.raw <- dat.raw |> dplyr::mutate(R = abs(R))

## basic exploration of data contents
str(dat.raw)
unique(dat.raw$pond_id)
unique(dat.raw$doy)
unique(dat.raw$treatment)


## define some time periods & units of comparison/grouping

# Nutrients pulses were DOYs 176 and 211; nutrients added after all sampling occurred 
# Heat event was DOYs 185-190
# Derecho on DOY 223 (Aug. 10, 2020)

wwidth <- 5  # days: width of windows, e.g. following nutrient pulses
pulse1 <- 177: (177 + wwidth - 1)  # doy
pulse2 <- 212: (212 + wwidth - 1)
derecho <- 224: (224 + wwidth - 1)
heatwave <- 185:190

pond_id <- unique(dat.raw$ pond_id)
time_doy <- min(dat.raw$doy):max(dat.raw$doy)
ref_ponds <- pond_id %in% unique(dat.raw$pond_id[dat.raw$treatment=="reference"])
pulse_ponds <- pond_id %in% unique(dat.raw$pond_id[dat.raw$treatment=="pulsed"])


# focal variables
varlist.main <- c("ch4_flux","co2_flux",
                  "GPP","R","NEP",
                  "do_sat","bottom_do_sat",
                  "temp", "bottom_temp",
                  "tn","tp")

# set the variable list used by the function to only the example variable (for creating visualization example (manuscript Fig. 3a))
# varlist.main = c("ch4_flux")


## turn data into matrices 

# Create a matrix of all mean daily values for each pond for each focal variable
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


##- Comparison Analyses -##

# Look at quantiles of pulse ponds relative to focal comparisons

#- Comparison 1: comparison to previous times in same pulsed pond
result_otherTimes <- matrix(NA, nrow=length(varlist.main), ncol=4)
#- Comparison 2: comparison to same time in reference ponds
result_refPonds <- matrix(NA, nrow=length(varlist.main), ncol=4)


# Run the comparison analyses using the matrices of focal variable mean daily values created above and fill into the empty 'result_' matrices
for(var in varlist.main){

  tmp <- data_matrices[[paste0(var)]]
  
  ## comparison 1: pulses to other n-day windows in pulsed ponds
  
  #- P1
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
  result_otherTimes[varlist.main==var, 1] <- mean(res.p1)  # mean(res) = the mean quantile for the focal event window
  
  #- P2
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
  result_otherTimes[varlist.main==var, 3] <- mean(res.p2)
  
  #- Heat event
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
  result_otherTimes[varlist.main==var, 2] <- mean(res.h)
  
  #- Derecho
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



#--
# Figures
#--

pal <- colorRampPalette(colors=c('#009392', '#fdfbe4', '#d0587e')) 

prettyNames <- c(expression('CH'['4']~'flux'),
                 expression('CO'['2']~'flux'),
                 'GPP',
                 'R',
                 'NEP',
                 'Surface DO',
                 'Bottom DO',
                 'Surface Temp.',
                 'Bottom Temp.',
                 'Total N',
                 'Total P'
                 )


atx <- rep(1:4, each=length(varlist.main))
aty <- rep(length(varlist.main):1, times=4)
aty2 <- rep(1:length(varlist.main), times=4)


# png("event_quantiles_rj_var-update.png", res=300, units="in", width=6.5, height=4.5)
pdf("event_quantiles_rj_var-update.pdf", width=6.5, height=4.5)

# windows(height = 4.5, width = 6.5)

layout(matrix(1:3, ncol=3), widths=c(0.45,0.45,0.15))
par(mar=c(6.1,1.1,3.1,0.6), oma=c(0,6,0,2), mgp=c(2.5,1,0))

# Comparison 1
image(x=1:4, y=1:length(varlist.main), z=t(result_otherTimes)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("Pulse 1", "Heat event", "Pulse 2", "Derecho"), las=2)
axis(2, at=1:length(varlist.main), labels=rev(prettyNames), las=2)
mtext("Comparison to prior \nwindows in pulsed ponds", cex=3/4, line=0.2)
mtext(expression(bold("B")), cex=3/4, at=0.3, line=0.3)
abline(h=c(2,6,9)+0.5)
text(atx, aty, round(c(result_otherTimes), 2))

# Comparison 2
image(x=1:4, y=1:length(varlist.main), z=t(result_refPonds)[,rev(1:length(varlist.main))], col=pal(21), zlim=c(0,1),
      xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at=1:4, labels=c("Pulse 1", "Heat event", "Pulse 2", "Derecho"), las=2)
axis(2, at=1:length(varlist.main), labels=NA, las=2)
mtext("Comparison to overlapping \nwindows in reference ponds", cex=3/4, line=0.2)
mtext(expression(bold("C")), cex=3/4, at=0.3, line=0.3)
abline(h=c(2,6,9)+0.5)
text(atx, aty, round(c(result_refPonds), 2))

par(mar=c(6.1,3.6,2.1,1.1))
image(t(matrix(1:21)), col=pal(21), xaxt="n", ylab="Quantile of pulsed pond event windows versus comparison", cex.lab=1.1)
mtext("Values > comparison", side=4, at=0.9, line=0.75, cex=3/4)
mtext("Values < comparison", side=4, at=0.1, line=0.75, cex=3/4)

dev.off()



#-- Create boxplot and histogram examples as demonstrations to accompany comparison quantile figure

# base
windows(height = 3, width = 4)

hist(colMeans(comp1.p2), main = NULL,
     breaks=10,
     col = pal(10))



# ggplot
windows(height = 3, width = 4)

ggplot(comp1.p2 %>% as.data.frame() %>%
          summarize(across(where(is.numeric), mean)) %>%
          pivot_longer(cols = where(is.numeric), values_to = 'value', names_to = 'window'),
       aes(x = value)) +
   geom_histogram(bins = 10, fill = pal(10), color = 'black') +
   labs(x = expression(N[2]*O~flux~(mu*mol~m^-2~d^-1)),
        y = "Frequency") +
   # lims(y = c(0, 15)) +
   #
   theme_classic() +
   theme(axis.text = element_text(color='black', size=unit(9, 'pt')),
         axis.title = element_text(color='black', size=unit(10, 'pt')))



