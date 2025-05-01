#Code from Lloyd (2012) A refined modelling approach to assess the influence of
#sampling on palaeobiodiversity curves: new support for declining Cretaceous
#dinosaur richness, Biology Letters, 8, 123-126.

#Load packages
library(nlme)
library(paleoTS)
library(plotrix)
library(earth)

AICc<-function(model,n)
{
  require(stats)
  require(nlme)
  p<-length(coef(model))
  lk<-AIC(model)-(2*p)
  lk+(2*p*(n/(n-p-1)))
}

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

linear.model<-function(x,y)
{
  modelout1<-nls(y~a*x,
                 start=list(a=(max(y)/max(x))),
                 algorithm="port",lower=list(a=0),
                 control=list(warnOnly=TRUE))
  modelout2<-nls(y~(a*x)+b,
                 start=list(a=(max(y)/max(x)),b=1),
                 algorithm="port",lower=list(a=0,b=0),
                 control=list(warnOnly=TRUE))
  modelout3<-lm(y~x)
  wts<-akaike.wts(c(AICc(modelout1,n=length(x)),
                    AICc(modelout2,n=length(x)),
                    AICc(modelout3,n=length(x))))
  best<-grep(TRUE,max(wts) == wts)
  if(best == 1) model<-modelout1
  if(best == 2) model<-modelout2
  if(best == 3) model<-modelout3
  sefit.model<-std.error(y-predict(model))*1.96
  sdfit.model<-sd(y-predict(model))*1.96
  result<-list(model,sefit.model,sdfit.model)
  names(result)<-c("model","sefit.model","sdfit.model")
  return(result)
}

hyperbolic.model<-function(x,y)
{
  modelout1<-NA; modelout2<-NA
  try(modelout1<-nls(y~(a*x)/(b+x),
                     start=list(a=max(y),b=1),
                     algorithm="port",
                     control=list(warnOnly=TRUE)),silent=TRUE)
  try(modelout2<-nls(y~a+((c*x)/(b+x)),
                     start=list(a=1,b=1,c=max(y)),
                     algorithm="port",lower=list(a=0),
                     control=list(warnOnly=TRUE)),silent=TRUE)
  if (is.na(modelout1[1]) == TRUE) modelout1<-nls(y~(max(y)*x)/(b+x),
                                                  start=list(b=1),
                                                  algorithm="port",
                                                  control=list(warnOnly=TRUE))
  if (is.na(modelout2[1]) == TRUE) modelout2<-nls(y~a+((max(y)*x)/(b+x)),
                                                  start=list(a=1,b=1),
                                                  algorithm="port",
                                                  lower=list(a=0),
                                                  control=list(warnOnly=TRUE))
  wts<-akaike.wts(c(AICc(modelout1,n=length(x)),
                    AICc(modelout2,n=length(x))))
  ifelse(wts[1] >= wts[2],
         model<-modelout1,
         model<-modelout2)
  sefit.model<-std.error(y-predict(model))*1.96
  sdfit.model<-sd(y-predict(model))*1.96
  result<-list(model,sefit.model,sdfit.model)
  names(result)<-c("model","sefit.model","sdfit.model")
  return(result)
}

logarithmic.model<-function(x,y)
{
  model<-NA
  try(model<-nls(y~a+log(b+x),
                 start=list(a=1,b=1),
                 algorithm="port",
                 lower=list(a=0,b=0),
                 control=list(warnOnly=TRUE)),
      silent=TRUE)
  if (is.na(model[1]) == TRUE) model<-nls(y~a+log(1+x),
                                          start=list(a=1),
                                          algorithm="port",
                                          lower=list(a=0),
                                          control=list(warnOnly=TRUE))
  sefit.model<-std.error(y-predict(model))*1.96
  sdfit.model<-sd(y-predict(model))*1.96
  result<-list(model,sefit.model,sdfit.model)
  names(result)<-c("model","sefit.model","sdfit.model")
  return(result)
}

exponential.model<-function(x,y)
{
  modelout1<-NA; modelout2<-NA
  try(modelout1<-nls(y~a*(1-exp(-c*x)),
                     start=list(a=max(y),c=0.1),
                     algorithm="port",
                     lower=list(a=0,c=0),
                     control=list(warnOnly=TRUE)),
      silent=TRUE)
  try(modelout2<-nls(y~a-(b*exp(-c*x)),
                     start=list(a=max(y),
                                b=max(y),c=0),
                                algorithm="port",
                                control=list(warnOnly=TRUE)),
                                silent=TRUE)
  if (is.na(modelout1[1]) == TRUE) modelout1<-nls(y~max(y)*(1-exp(-c*x)),
                                                  start=list(c=0.000001),
                                                  algorithm="port",
                                                  lower=list(c=0),
                                                  control=list(warnOnly=TRUE))
  if (is.na(modelout2[1]) == TRUE) modelout2<-nls(y~max(y)-(max(y)*exp(-c*x)),
                                                  start=list(c=0.),
                                                  algorithm="port",
                                                  control=list(warnOnly=TRUE))
  wts<-akaike.wts(c(AICc(modelout1,n=length(x)),
                    AICc(modelout2,n=length(x))))
  ifelse(wts[1] >= wts[2],
         model<-modelout1,
         model<-modelout2)
  sefit.model<-std.error(y-predict(model))*1.96
  sdfit.model<-sd(y-predict(model))*1.96
  result<-list(model,sefit.model,sdfit.model)
  names(result)<-c("model","sefit.model","sdfit.model")
  return(result)
}

sigmoidal.model<-function(x,y)
{
  modelout1<-NA; modelout2<-NA
  try(modelout1<-nls(y~b/(c+exp(-x)),
                     start=list(b=max(y)/100,c=max(y)/10000),
                     algorithm="port",
                     control=list(warnOnly=TRUE)),
                     silent=TRUE)
  try(modelout2<-nls(y~a+(b/(c+exp(-x))),
                     start=list(a=1,b=0.1,c=0.001),
                     algorithm="port",
                     lower=list(a=0),
                     control=list(warnOnly=TRUE)),
                     silent=TRUE)
  if (is.na(modelout1[1]) == TRUE) modelout1<-nls(y~b/((1/max(y))+exp(-x)),
                                                  start=list(b=1),
                                                  algorithm="port",
                                                  control=list(warnOnly=TRUE))
  if (is.na(modelout2[1]) == TRUE) modelout2<-nls(y~a+(max(y)/100)/((max(y)/100)+exp(-x)),
                                                  start=list(a=1),
                                                  algorithm="port",
                                                  lower=list(a=0),
                                                  control=list(warnOnly=TRUE))
  wts<-akaike.wts(c(AICc(modelout1,n=length(x)),
                    AICc(modelout2,n=length(x))))
  ifelse(wts[1] >= wts[2],
         model<-modelout1,
         model<-modelout2)
  sefit.model<-std.error(y-predict(model))*1.96
  sdfit.model<-sd(y-predict(model))*1.96
  result<-list(model,sefit.model,sdfit.model)
  names(result)<-c("model","sefit.model","sdfit.model")
  return(result)
}

polynomial.model<-function(x,y)
{
  modelout2<-lm(y~x+I(x^2))
  modelout3<-lm(y~x+I(x^2)+I(x^3))
  modelout4<-lm(y~x+I(x^2)+I(x^3)+I(x^4))
  wts<-akaike.wts(c(AICc(modelout2,n=length(x)),
                    AICc(modelout3,n=length(x)),
                    AICc(modelout4,n=length(x))))
  best<-grep(TRUE,max(wts) == wts)
  if(best == 1) model<-modelout2
  if(best == 2) model<-modelout3
  if(best == 3) model<-modelout4
  sefit.model<-std.error(y-predict(model))*1.96
  sdfit.model<-sd(y-predict(model))*1.96
  result<-list(model,sefit.model,sdfit.model)
  names(result)<-c("model","sefit.model","sdfit.model")
  return(result)
}

best.model<-function(x,y)
{
  linmod<-linear.model(x,y)$model # Fit linear model
  hypmod<-hyperbolic.model(x,y)$model # Fit hyperbolic model
  logmod<-logarithmic.model(x,y)$model # Fit logarithmic model
  expmod<-exponential.model(x,y)$model # Fit exponential model
  sigmod<-sigmoidal.model(x,y)$model # Fit sigmoidal model
  polmod<-polynomial.model(x,y)$model # Fit polynomial model
  best<-min(c(AICc(linmod,n=length(x)),
              AICc(hypmod,n=length(x)),
              AICc(logmod,n=length(x)),
              AICc(expmod,n=length(x)),
              AICc(sigmod,n=length(x)),
              AICc(polmod,n=length(x))))
  if (AICc(linmod,n=length(x)) == best) model<-linmod; sefit.model<-linear.model(x,y)$sefit.model; sdfit.model<-linear.model(x,y)$sdfit.model
  if (AICc(hypmod,n=length(x)) == best) model<-hypmod; sefit.model<-hyperbolic.model(x,y)$sefit.model; sdfit.model<-hyperbolic.model(x,y)$sdfit.model
  if (AICc(logmod,n=length(x)) == best) model<-logmod; sefit.model<-logarithmic.model(x,y)$sefit.model; sdfit.model<-logarithmic.model(x,y)$sdfit.model
  if (AICc(expmod,n=length(x)) == best) model<-expmod; sefit.model<-exponential.model(x,y)$sefit.model; sdfit.model<-exponential.model(x,y)$sdfit.model
  if (AICc(sigmod,n=length(x)) == best) model<-sigmod; sefit.model<-sigmoidal.model(x,y)$sefit.model; sdfit.model<-sigmoidal.model(x,y)$sdfit.model
  if (AICc(polmod,n=length(x)) == best) model<-polmod; sefit.model<-polynomial.model(x,y)$sefit.model; sdfit.model<-polynomial.model(x,y)$sdfit.model
  result<-list(model,sefit.model,sdfit.model)
  names(result)<-c("model","sefit.model","sdfit.model")
  return(result)
}

rockmodel.predictCI<-function(rockmeasure,diversitymeasure,CI=0.95)
{
  x<-sort(rockmeasure)
  y<-sort(diversitymeasure)
  model<-best.model(x,y)$model
  sefit.model<-best.model(x,y)$sefit.model
  sdfit.model<-best.model(x,y)$sdfit.model
  predicted<-predict(model,list(x=rockmeasure))
  selowerCI<-predicted-sefit.model
  seupperCI<-predicted+sefit.model
  sdlowerCI<-predicted-sdfit.model
  sdupperCI<-predicted+sdfit.model
  result<-list(predicted,selowerCI,seupperCI,sdlowerCI,sdupperCI,model)
  names(result)<-c("predicted","selowerCI","seupperCI","sdlowerCI","sdupperCI","model")
  return(result)
}
