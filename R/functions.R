# .R - 
# /R/.R

# Copyright 2003-2011 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# shepherdCovB {{{

shepherdCovB<-function () {
    logl <- function(a, b, c, f, rec, ssb, covar) {
       hat<-a * ssb/(1 + (ssb/(b*(1+f*covar[[1]])))^c)
       loglAR1(log(rec), log(hat))}

    initial <- structure(function(rec, ssb) {
        c   <- 1.0
        f   <- 0.001
        x   <- ssb^c
        y   <- ssb/rec
        res <- coefficients(lm(c(y) ~ c(x)))
        a   <- max(1/res[1])
        b   <- max(b = 1/((res[2] * a)^(1/c)))
        return(FLPar(a=a, b=b, c=c, f=f))}, 

    lower = c(  0,   0,  1, -1), 
    upper = c(Inf, Inf, 10,  1))

    model <- rec ~ a * ssb/(1 + (ssb/(b*(1+f*covar[[1]])))^c)

    return(list(logl=logl, model=model, initial=initial))}
# }}}


freq=function(x,y,x.n=11,y.n=x.n){
 
  df=data.frame(x=x,y=y)
  df=data.frame(df,xFac=cut(df$x,seq(min(df$x),max(df$x),length.out=x.n)),
                   yFac=cut(df$y,seq(min(df$y),max(df$y),length.out=y.n)))
  
  c.=ddply(data.frame(df,count=1),.(xFac,yFac), function(x) count(x$count))[,c("xFac","yFac","freq")]
  
  p.=merge(df,c.,by=c("xFac","yFac"))[,c("x","y","freq","xFac","yFac")]
  
  return(p.[order(p.$freq),])}

density=function(x,y,x.n=11,y.n=x.n){
#library(MASS)

    dat=data.frame(x=x,y=y,n=50)
    f1 =with(dat, kde2d(x,y,n=n)) 
    f2 =data.frame(expand.grid(x=f1$x, y=f1$y), z=as.vector(f1$z))
  
  return(f2)}


prob=function(x,y,prob=c(0.6,0.9)){
#library(MASS)
#library(coda)
#library(emdbook)

   tmp=HPDregionplot(mcmc(data.frame(x,y)),prob=prob)


   prb=ldply(tmp, function(dat) data.frame(level=dat$level,x=dat$x, y=dat$y))

   return(prb)}