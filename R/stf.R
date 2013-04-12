# stf - prepare an object for a Short Term Forecast
# FLAssess/R/stf.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas & Robert Scott, JRC
# $Id: stf.R 1044 2011-06-20 08:14:01Z imosqueira $

if (!isGeneric("stf"))
    setGeneric("stf", function(object,...)
	    standardGeneric("stf"))

## stf(FLStock) {{{
setMethod('stf', signature(object='FLStock'),
  function(object, nyears=3, wts.nyears=3, fbar.nyears=wts.nyears, f.rescale=FALSE,
    arith.mean=TRUE, na.rm=TRUE, end=dims(object)$maxyear + nyears)
  {
    dims <- dims(object)

    # check nyears and end match
    if(missing(nyears))
      nyears <- as.numeric(end) - dims$maxyear
    else if(dims$maxyear + nyears != end)
      stop("'nyears' and 'end' do not match: ", dims$maxyear + nyears, " vs. ", end)

    # years
    years      <- ac((dims$maxyear+1):end)
    wts.years  <- ac(seq(dims$maxyear-wts.nyears+1, dims$maxyear))
    fbar.years <- ac(seq(dims$maxyear-fbar.nyears+1, dims$maxyear))
    fbar.ages  <- ac(range(object, 'minfbar'):range(object, 'maxfbar'))

    # arith or geometric
    if(arith.mean)
      fmean <- mean
    else  
      fmean <- function(x) exp(mean(log(x)))

    # window object
    res <- window(object, end=end)

    # average slots
    # *.wt, mat, m and *.spwn as average over wts.years
    for (i in c('catch.wt', 'landings.wt', 'discards.wt', 'stock.wt', 'mat', 'm', 'harvest.spwn', 'm.spwn')){
      flq<- apply(slot(res, i)[,wts.years], c(1,3:6),fmean, na.rm=na.rm)
      for (j in years)
         slot(res, i)[,j] <-flq
      }

    # landings.n and discards.n as proportions of wts.years
    for (i in years)
       slot(res, 'discards.n')[,i] <- apply(slot(res, 'discards.n')[, wts.years]/slot(res, 'catch.n')[, wts.years], c(1,3:6), mean)
    slot(res, 'landings.n')[,years] <- 1 - slot(res, 'discards.n')[,years]

    # harvest as mean over fbar.nyears
    f <-apply(slot(res, 'harvest')[,fbar.years], c(1,3:6), fmean, na.rm=na.rm)
    for (i in years)
       slot(res, 'harvest')[,i] <- f

    # f.rescale
    if(f.rescale == TRUE)
    {
      # mean f over fbar ages and years
      fbar <- mean(apply(slot(res, 'harvest')[fbar.ages, fbar.years], c(2:6), mean,
        na.rm=na.rm))
      # fbar for last REAL year
      lastfbar <- apply(slot(res, 'harvest')[fbar.ages, ac(dims$maxyear)], 3:6, mean,
        na.rm=na.rm)

      # divide by fbar and multiply by lastfbar
      slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, fbar, '/')
      slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, lastfbar, '*')
    }
    return(res)
  }
) # }}}

## stf(FLBiol) {{{
setMethod('stf', signature(object='FLBiol'),
  function(object, nyears=3, wts.nyears=3, arith.mean=TRUE, na.rm=TRUE,
    end=dims(object)$maxyear + nyears)
  {
    dims <- dims(object)
    
    # check nyears and end match
    if(missing(nyears))
      nyears <- as.numeric(end) - dims$maxyear
    else if(dims$maxyear + nyears != end)
      stop("'nyears' and 'end' do not match: ", dims$maxyear + nyears, " vs. ", end)

    # years
    years <- ac((dims$maxyear+1):end)
    wts.years <- ac(seq(dims$maxyear-wts.nyears+1, dims$maxyear))

    # arith or geometric
    if(arith.mean)
      fmean <- mean
    else  
      fmean <- function(x) exp(mean(log(x)))

    # window object
    res <- window(object, end=end)

    # average slots
    # *.wt, mat, m and *.spwn as average over wts.years
    for (i in c('wt', 'fec', 'm', 'spwn'))
      slot(res, i)[,years] <- apply(slot(res, i)[,wts.years], c(1,3:6), fmean, na.rm=TRUE)
    
    return(res)
  }
) # }}}
