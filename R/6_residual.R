#Alexander Dunhill   19th September 2024
#Code to calculate residual diversity through time

#setwd("#####")

#Load packages
library(earth)
library(nlme)
library(paleoTS)
library(plotrix)

#Source code from Graeme Lloyd
source("http://www.graemetlloyd.com/pubdata/functions_2.r")

#run this bit of code as well
#(one of the functions in sourced code is incompatible with current R versions)
akaike.wts<- function(aa)
{
  okset<- !is.na(aa)
  aas<- aa[okset]
  
  ma<- min(aas)
  delt<- aas - ma
  denom<- sum(exp(-delt/2))
  ww<- exp(-delt/2)/denom
  names(ww)<- names(aa)
  
  aw<- ww[okset]
  return(aw)
}

#Load data
raw_counts <- read.csv("data/raw_counts.csv")

#Rename variables to fit the model parameters
#time <- raw_counts$age
div <- raw_counts$raw_species
proxy <- raw_counts$formations

#Plot proxy against diversity
#plot(proxy, div)

#Corrleation test
#cor.test(proxy, div, method = "spearman")

#Run the model code
results <- rockmodel.predictCI(proxy,div)

#Plot sampled and modelled diversity
par(mar=c(5,5,1,1), mfrow=c(1,1))
plot(time,div-results$predicted,type="l",xlim=c(max(time),min(time)),
     xlab="Time (Ma)",ylab="Model Detrended Taxonomic Richness")
polygon(x=c(time,rev(time)),y=c(div-results$predicted,
                                rep(0,length(time))),col="grey",border=0)
points(time,div-results$predicted,type="l")
lines(time,rep(0,length(time)))
lines(time,results$seupperCI-results$predicted,lty=2)
lines(time,results$selowerCI-results$predicted,lty=2)
lines(time,results$sdupperCI-results$predicted,lty=4)
lines(time,results$sdlowerCI-results$predicted,lty=4)
