### ------------------------------------------------------------------------ ###
### analyse by Marta Cousido ####
### ------------------------------------------------------------------------ ###

rm(list=ls())
### Read packages and data ---------------------------------------------------
req_pckgs <- c("doParallel", "doRNG", "mse", "GA", "ggplot2", "cowplot", 
               "scales", "Cairo", "tidyr", "dplyr", "FLCore", "FLash", "FLBRP")
for (i in req_pckgs) library(package = i, character.only = TRUE)

res_mp=readRDS(paste0("output/500_50/catch_rule/random/", "sol8c9a", "_mp.rds"))
stocks <- read.csv("input/stock_solea.csv", stringsAsFactors = FALSE)
pol_ow <- readRDS("input/500_50/OM_1_hist/random/sol8c9a.rds")$stk
pol_brp <- readRDS("input/sol8c9a.rds")
refpts <- pol_brp@refpts
Fmsy=(refpts["msy", "harvest"])
ssbmsy=refpts["msy", "ssb"]

### Create the data sets------------------------------------------------------
  
Fbar=0
SSB=0
Fbar_rel=0
SSB_rel=0
replicate=0
years=0

Fbar.matrix=matrix(0,ncol=500,nrow=151)
SSB.matrix=Fbar_rel.matrix=SSB_rel.matrix=rec.matrix=catch.matrix=Fbar.matrix

for (i in 1:500){
aux_Fbar=c( fbar(pol_ow)[,1:100,,,,i],fbar(res_mp[[1]]@stock)[,,,,,i])
aux_SSB = c(ssb(pol_ow)[,1:100,,,,i],ssb(res_mp[[1]]@stock)[,,,,,i])
aux_Fbar_rel=aux_Fbar/Fmsy
aux_SSB_rel=aux_SSB/ssbmsy
Fbar=c(Fbar,aux_Fbar)
Fbar_rel=c(Fbar_rel,aux_Fbar_rel)
SSB=c(SSB,aux_SSB)
SSB_rel=c(SSB_rel,aux_SSB_rel)
replicate=c(replicate, rep(i,length(aux_SSB)))
years=c(years,0:150)
Fbar.matrix[,i]=aux_Fbar
SSB.matrix[,i]=aux_SSB
Fbar_rel.matrix[,i]=aux_Fbar_rel
SSB_rel.matrix[,i]=aux_SSB_rel

aux_catch = c(catch(pol_ow)[,1:100,,,,i],catch(res_mp[[1]]@stock)[,,,,,i])
aux_rec = c(rec(pol_ow)[,1:100,,,,i],rec(res_mp[[1]]@stock)[,,,,,i])
rec.matrix[,i]=aux_rec 
catch.matrix[,i]=aux_catch
}

### Alternative data set that I don't use now

data=data.frame(Fbar=Fbar[-1],Fbar_rel=Fbar_rel[-1],SSB=SSB[-1],SSB_rel=SSB_rel[-1],
         replicate=replicate[-1],years=years[-1])
 
### Plots --------------------------------------------------------------------

SSB.matrix=SSB.matrix[-1,]
Fbar_rel.matrix=Fbar_rel.matrix[-1,]
SSB_rel.matrix=SSB_rel.matrix[-1,]
rec.matrix=rec.matrix[-1,]
catch.matrix=catch.matrix[-1,]
Fbar.matrix=Fbar.matrix[-1,]

par(mfcol=c(3,2))
### Fbar abs ----------------------------------------------------------------

median=apply(Fbar.matrix, 1, quantile, probs=0.5)
high=apply(Fbar.matrix, 1, quantile, probs=0.9)
low=apply(Fbar.matrix, 1, quantile, probs=0.10)

plot(1:150, median,type="l", ylim=c(0,max(high)),
     main="F abs",xlab="F",ylab="years",lwd=2)
lines(1:150,high, type="l",lty=2,col="red",lwd=1.5)
lines(1:150,low, type="l",lty=2,col="red",lwd=1.5)
abline(h=Fmsy,col="green",lty=2)

### Fbar rel ----------------------------------------------------------------

median=apply(Fbar_rel.matrix, 1, quantile, probs=0.5)
high=apply(Fbar_rel.matrix, 1, quantile, probs=0.9)
low=apply(Fbar_rel.matrix, 1, quantile, probs=0.10)

plot(1:150, median,type="l", ylim=c(0,max(high)),
     main="F rel",xlab="F",ylab="years",lwd=2)
lines(1:150,high, type="l",lty=2,col="red",lwd=1.5)
lines(1:150,low, type="l",lty=2,col="red",lwd=1.5)
abline(h=1,col="green",lty=2)

### SSB abs ----------------------------------------------------------------

median=apply(SSB.matrix, 1, quantile, probs=0.5)
high=apply(SSB.matrix, 1, quantile, probs=0.9)
low=apply(SSB.matrix, 1, quantile, probs=0.10)

plot(1:150, median,type="l", ylim=c(0,max(high)),
     main="SSB abs",xlab="SSB",ylab="years",lwd=2)
lines(1:150,high, type="l",lty=2,col="red",lwd=1.5)
lines(1:150,low, type="l",lty=2,col="red",lwd=1.5)
abline(h=ssbmsy,col="green",lty=2)

### SSB rel ----------------------------------------------------------------

median=apply(SSB_rel.matrix, 1, quantile, probs=0.5)
high=apply(SSB_rel.matrix, 1, quantile, probs=0.9)
low=apply(SSB_rel.matrix, 1, quantile, probs=0.10)

plot(1:150, median,type="l", ylim=c(0,max(high)),
     main="SSB rel",xlab="SSB",ylab="years",lwd=2)
lines(1:150,high, type="l",lty=2,col="red",lwd=1.5)
lines(1:150,low, type="l",lty=2,col="red",lwd=1.5)
abline(h=1,col="green",lty=2)

### catch  ----------------------------------------------------------------

median=apply(catch.matrix, 1, quantile, probs=0.5)
high=apply(catch.matrix, 1, quantile, probs=0.9)
low=apply(catch.matrix, 1, quantile, probs=0.10)

plot(1:150, median,type="l", ylim=c(0,max(high)),
     main="catch",xlab="catch",ylab="years",lwd=2)
lines(1:150,high, type="l",lty=2,col="red",lwd=1.5)
lines(1:150,low, type="l",lty=2,col="red",lwd=1.5)

### rec  ----------------------------------------------------------------

median=apply(rec.matrix, 1, quantile, probs=0.5)
high=apply(rec.matrix, 1, quantile, probs=0.9)
low=apply(rec.matrix, 1, quantile, probs=0.10)

plot(1:150, median,type="l", ylim=c(0,max(high)),
     main="rec",xlab="rec",ylab="years",lwd=2)
lines(1:150,high, type="l",lty=2,col="red",lwd=1.5)
lines(1:150,low, type="l",lty=2,col="red",lwd=1.5)