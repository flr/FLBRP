# spr - «Short one line description»
# spr

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# $Id:  $

# Reference:
# Notes:

# TODO Fri 22 Jan 2010 11:13:59 AM CET IM:


setMethod('spr', signature(object='FLBRP'),
  function(object){
    params(object)<-FLPar(1)
    model(object)<-formula(rec~a)
    
    res <- .Call("spr", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)), dimnames=dimnames(params(object))), 
      PACKAGE = "FLBRP")

    return(res)})

setMethod('spr0', signature(ssb='FLBRP', rec='missing', fbar='missing'),
  function(ssb){
    params(ssb)<-FLPar(1)
    model(ssb)<-formula(rec~a)
    fbar(ssb) <- FLQuant(0, quant=quant(fbar(ssb)))
    
    res <- .Call("spr", ssb, SRNameCode(SRModelName(ssb@model)),
      FLQuant(c(params(ssb)),dimnames=dimnames(params(ssb))),
      PACKAGE = "FLBRP")

    return(res)})

setMethod('spr0', signature(ssb='FLQuant', rec='FLQuant', fbar='FLQuant'),
  function(ssb, rec, fbar) {

    if  (any(dim(ssb)[3:5]>1))
      stop("multiple units, seasons, areas not allowed yet")
    if  (any(dim(rec)[3:5]>1))
      stop("multiple units, seasons, areas not allowed yet")
    if  (any(dim(fbar)[3:5]>1))
      stop("multiple units, seasons, areas not allowed yet")

    # years: corrects length if mismatch
    minyear <- max(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) min(as.numeric(dimnames(x)$year)))))
    maxyear <- min(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) max(as.numeric(dimnames(x)$year)))))

    # ssb & f
    ssb  <- ssb[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    rec  <- rec[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    fbar <- fbar[1, as.character(seq(minyear, maxyear)), drop=TRUE]

    # spr0
    spr0 <- lm(c(ssb/rec)~c(fbar))$coefficients[1]
    names(spr0) <- "spr0"

    return(spr0)})

setMethod('spr0', signature(ssb='FLStock', rec='missing', fbar='missing'),
  function(ssb) {
    sr <-as.FLSR(ssb)
    res<-spr0(ssb=ssb(ssb), rec=rec(sr), fbar=fbar(ssb))

    return(res)})

setMethod('spr0', signature(ssb='FLSR', rec='missing', fbar='FLQuant'),
  function(ssb, fbar){
    res<-spr0(ssb=ssb(ssb), rec=rec(ssb), fbar=fbar)

    return(res)})
