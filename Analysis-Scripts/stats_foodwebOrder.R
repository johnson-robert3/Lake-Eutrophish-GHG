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
dat.raw$foodweb <- NA
dat.raw$foodweb[dat.raw$pond_id %in% c("B","F")] <- "low"
dat.raw$foodweb[dat.raw$pond_id %in% c("A","D")] <- "mid"
dat.raw$foodweb[dat.raw$pond_id %in% c("C","E")] <- "high"

# define some time periods of interest
wwidth=5
pulse1 <- 176: (176 + wwidth - 1)#doy
pulse2 <- 211: (211 + wwidth - 1)
derecho <- 220: (220 + wwidth - 1)
heatwave <- 190:195


varlist <- colnames(dat.raw)
varlist <- varlist[!varlist %in% c("pond_id","treatment","date","doy","period","period2","week","sonde_strat","foodweb"
                                   ,"wind_speed","wind_U10","ph","alkalinity","NOx","phyco","doc_ppb")]


cols <- c("green","yellow","red")

pdf("../foodweb_consistency.pdf", onefile=TRUE)

par(mfrow=c(4,2), mar=c(2.5,2.5,2.5,0.5))

for(var in varlist){
  
  tmp <- dat.raw
  tmp$treatment = factor(tmp$treatment, levels=c("reference","pulsed"))
  tmp$foodweb = factor(tmp$foodweb, levels=c("low","mid","high"))
  colnames(tmp)[colnames(tmp)==var] <- "var"
  boxplot(var ~ treatment + foodweb, data=tmp)
  mtext(paste0(var), line=1.1, cex=2/3)

  pondmean.ref <- aggregate(dat.raw[dat.raw$treatment=="reference",var], 
                            by=list(dat.raw[dat.raw$treatment=="reference","foodweb"]), FUN="mean", na.rm=TRUE)
  pondmean.pulse <- aggregate(dat.raw[dat.raw$treatment=="pulsed",var], 
                            by=list(dat.raw[dat.raw$treatment=="pulsed","foodweb"]), FUN="mean", na.rm=TRUE)
  
  plotmat <- cbind(rank(pondmean.ref$x), rank(pondmean.pulse$x))[c(2,3,1),]
  image(x=1:3, y=1:2, z=plotmat, col=cols, xaxt="n", yaxt="n", xlab="", ylab="")
  axis(1, at=1:3, labels=c("lo","int","hi"))
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
