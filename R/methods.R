# methods.R - DESC
# FLBRP/R/methods.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@seaplusplus.co.uk>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# brp {{{

#' @title Fit an FLBRP object
#' 
#' @description 
#' This method carries out the necessary calculation to obtain the reference
#' points for an object of class `FLBRP`. Results of the calculation are stored
#' in `@refpts` slot of the object, and the object is then returned.
#' 
#' @param object An object of class `FLBRP`
#' 
#' @return An object of class \code{FLBRP} with reference points estimated
#' 
#' @docType methods
#' @rdname brp
#' 
#' @seealso [FLBRP] 
#' 
#' @examples
#' data(ple4brp)
#'
#' ple4brp <- brp(ple4brp)
#' refpts(ple4brp)

setMethod('brp', signature(object='FLBRP'),
  function(object)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% c(seq(1,6), 21))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ") in FLBRP object can not be used by brp. See ?ab"))

    # check needed slots are filled up
    for(i in c('landings.sel', 'discards.sel', 'bycatch.harvest', 'stock.wt',
      'landings.wt','discards.wt', 'bycatch.wt','m','mat','harvest.spwn', 'm.spwn',
      'availability'))
      if(all(is.na(slot(object, i))))
        stop("missing necessary information in slot ", i)

    # check dims in object and params
    iter <- c(dims(object)$iter, length(dimnames(params(object))$iter))
    # if > 1, they should be equal
    if(all(iter > 1))
      if(iter[1] != iter[2])
        stop('iter in FLQuant slots and params do not match, ',
          paste(iter, collapse=' vs. '))

    # check harvest.spwn if m.spwn == 0
    if(any(m.spwn(object) == 0 & harvest.spwn(object) > 0))
      stop("harvest.spwn > 0 while m.spwn = 0, cannot solve as no time for fishing
        to take place. Set a small value for m.spwn, e.g. m.spwn(brp) <- 1e-6.")

    # extend refpts as needed
    iter <- max(iter)
    if(iter > 1 && dims(refpts(object))$iter == 1)
      refpts <- propagate(refpts(object), iter, fill.iter=TRUE)
    else if(iter > 1 && dims(refpts(object))$iter != iter)
      stop("iters in refpts and object slots do not match")
    else
      refpts <- refpts(object)
    
    if ("virgin" %in% dimnames(refpts)$refpt){
      refpts@.Data["virgin",,         ] <- as.numeric(NA)
      refpts@.Data["virgin", "harvest",] <- 0
    }

    # RUN over 250 iter blocks
    bls <- split(seq(iter), ceiling(seq_along(seq(iter)) / 250))

    res <- lapply(bls, function(i) {
      srpars <- iter(params(object), i)
      .Call("brp", iter(object, i), 
        iter(refpts, i), SRNameCode(SRModelName(object@model)),
        FLQuant(c(srpars), dimnames=dimnames(srpars)),
        PACKAGE = "FLBRP")
      }
    )
    
    out <- Reduce(combine, res)

    return(out)

    res <- .Call("brp", object, refpts, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLBRP")

    return(res)
  }) # }}}

# landings.n {{{
setMethod('landings.n', signature(object='FLBRP'),
  function(object) {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% c(seq(1,6), 21))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    res <- .Call('landings_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
    res[res < 0] <- 0

    return(res)
  }
) # }}}

# discards.n {{{
setMethod('discards.n', signature(object='FLBRP'),
  function(object) {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% c(seq(1,6), 21))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    res <- .Call('discards_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
    res[res < 0] <- 0

    return(res)
  }
) # }}}

# stock.n {{{
setMethod('stock.n', signature(object='FLBRP'),
  function(object)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% c(seq(1,6), 21))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    res <- .Call('stock_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))
    res[res < 0] <- 0

    return(res)
  }
) # }}}

# catch.n {{{
setMethod('catch.n', signature(object='FLBRP'),
  function(object) {
    res <- landings.n(object) + discards.n(object)
    if (units(discards.n(object)) == units(landings.n(object)))
		  units(res) <- units(discards.n(object))
    else
      warning("unts of discards.n and landings.n do not match")
      
    return(res)
  }
) # }}}

# catch.wt {{{
setMethod('catch.wt', signature(object='FLBRP'),
  function(object) {

      denom<-landings.sel(object) + discards.sel(object)
      denom[denom==0]<-1
      
      res <- (landings.wt(object) * landings.sel(object) +
              discards.wt(object) * discards.sel(object)) / denom

    test=units(discards.wt(object)) == units(landings.wt(object))
    if (!is.na(test))  
    if (test)
				units(res) <- units(discards.wt(object))

    return(res)
  }
) # }}}

# catch.sel {{{
setMethod('catch.sel', signature(object='FLBRP'),
  function(object) {
    return(landings.sel(object) + discards.sel(object))
  }) # }}}

# catch.obs {{{
setMethod('catch.obs', signature(object='FLBRP'),
  function(object) {
    return(discards.obs(object)+landings.obs(object))
  }) # }}}

# biomass.obs {{{
setMethod('biomass.obs', signature(object='FLBRP'),
  function(object) {
    return(stock.obs(object))
  }) # }}}

# yield.obs {{{
setMethod('yield.obs', signature(object='FLBRP'),
  function(object) {
    return(landings.obs(object))
  }) # }}}

# stock {{{
setMethod('stock', signature(object='FLBRP'),
  function(object) {
		return(quantSums(stock.n(object) %*% stock.wt(object)))
  }) # }}}

# rec {{{
setMethod('rec', signature(object='FLBRP'),
  function(object) {
    return(stock.n(object)[1,])
  }) # }}}

# harvest {{{
setMethod("harvest", signature(object="FLBRP", catch="missing"),
	function(object){
    
    # selectivity
    sel <- expand(landings.sel(object) + discards.sel(object),
      year=dimnames(fbar(object))$year)
    dmns <- dimnames(sel)
    dmns$year <- dimnames(fbar(object))$year
    sel <- FLQuant(sel, dimnames=dmns)
    
    sel[,] <- sel[,1]
    sel <- sel %*% fbar(object)
    
    units(sel) <- "f"
    
    return(sel)
  }) # }}}

# ypr {{{
setMethod('ypr', signature(object='FLBRP'),
  function(object)
  {
    params(object)<-FLPar(1)
    model( object)<-formula(rec~a)
    
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% c(seq(1,6), 21))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    res <- .Call("ypr", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLBRP")
    res[res < 0] <- 0

    return(res)
  }) # }}}

# hcrYield {{{
setMethod('hcrYield', signature(object='FLBRP', fbar='FLQuant'),
  function(object, fbar)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% c(seq(1,6), 21))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    # check input fbar dims
    if(!identical(dim(fbar), dim(fbar(object))))
      stop("input fbar must be the same length as fbar(object)")

    if(dims(object)$iter!=1 && dims(object@params)$iter ==1)
       m(object)<-propagate(m(object),iter=dims(params(object))$iter)
    else if (dims(object)$iter!=1 && dims(object@params)$iter !=1)
       if (dims(object)$iter!= dims(object@params)$iter)
          stop("Iters in params don't match")

    res <- .Call("hcrYield", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      fbar, PACKAGE = "FLBRP")
    res[res < 0] <- 0

    # propagate landings.wt
    if(dims(res)$iter != dims(landings.wt(object))$iter)
      landings.wt(object) <- propagate(landings.wt(object), dims(res)$iter)

    return(quantSums(res %*% landings.wt(object)))
   }
)
setMethod('hcrYield', signature(object='FLBRP', fbar='numeric'),
  function(object, fbar)
    hcrYield(object, FLQuant(fbar)))

# }}}

# ssb {{{
setMethod('ssb', signature(object='FLBRP'),
  function(object) {

    f <- harvest(object) %*% harvest.spwn(object)
    m <- m(object) %*% m.spwn(object)
    
    expZ <- exp(-f %-% m)

    res <- quantSums(stock.n(object) %*% expZ %*% stock.wt(object) %*%
      mat(object))

    return(res)
  }
)# }}}

# vb {{{
setMethod("vb", signature(x="FLBRP"),
  function(x) {

    vb <- quantSums(stock.n(x) %*% stock.wt(x) %*% catch.sel(x))
    units(vb) <- units(stock(x))

    return(vb)
  }
)
# }}}

# landings {{{
setMethod('landings', signature(object='FLBRP'),
  function(object){
    return(quantSums(landings.n(object) %*% landings.wt(object)))
  }
) # }}}

# discards {{{
setMethod('discards', signature(object='FLBRP'),
  function(object) {
    return(quantSums(discards.n(object) %*% discards.wt(object)))
  }
) # }}}

# catch {{{
setMethod('catch', signature(object='FLBRP'),
  function(object) {
    return(landings(object) + discards(object))
  }
) # }}}

# revenue {{{
setMethod('revenue', signature(object='FLBRP'),
  function(object) {
    return(quantSums(landings.n(object) %*% landings.wt(object) %*% price(object)))
  }
) # }}}

# cost {{{
setMethod('cost', signature(object='FLBRP'),
  function(object){
    return(quantSums(FLCore::`%+%`(fbar(object) %*% vcost(object), fcost(object))))
    # return(quantSums((fbar(object) %*% vcost(object)) %+% fcost(object)))
  }
) # }}}

# profit {{{
setMethod('profit', signature(object='FLBRP'),
  function(object) {
    return(revenue(object) - cost(object))
  }
) # }}}

# msyRange {{{

#' @title MSY range
#' 
#' @description 
#' Calculates the values of fishing mortality, biomass, SSB etc. for a range of yields either side of MSY. 
#' 
#' @param object \code{FLBRP}
#' @param range the values of yield around MSY 
#' 
#' @return object of class \code{FLPar} with reference point quantities
#' 
#' @docType methods
#' @rdname msyrange
#' 
#' @seealso \code{\link{refpts}} 
#' 
#' @examples
#' data(ple4brp)
#' 
#' msyRange(ple4brp)

setMethod("msyRange", signature(object="FLBRP"),
  function(object, Ftarget="msy", range=0.10) {

    dmns <- dimnames(refpts(object))

    refpts(object) <- FLPar(NA,
      dimnames=list(refpt=c(Ftarget, "min", "max"),
      quantity=c("harvest", "yield", "rec", "ssb", "biomass", "revenue",
        "cost", "profit"), 
      iter=dmns$iter))

    object <- brp(object)

    refpts(object)["min","yield"] <- refpts(object)[Ftarget,"yield"] * (1-range)
    
    refpts(object)["max","yield"] <- refpts(object)[Ftarget,"yield"] * (1-range)
     
    fn <- function(f, target, obj) {
      refpts(obj) <- FLPar(c(f, rep(NA, 7)),
        dimnames=list(refpt=c("f"),
        quantity=c("harvest", "yield", "rec", "ssb", "biomass", "revenue",
          "cost", "profit"), 
        iter =1))
     
     return((refpts(brp(obj))[,"yield"]- target) ^ 2)
    }
     
    for (i in dmns$iter) {

      iter(refpts(object)["min", "harvest"], i) <- optimise(fn,
        c(0.01, iter(refpts(object)[Ftarget, "harvest"], i)),
        target=iter(refpts(object)["min", "yield"], 1),
        obj=iter(object, i), tol=.Machine$double.eps^0.5)$minimum

      iter(refpts(object)["max", "harvest"], i) <- optimise(fn,
        c(1, 10) * c(iter(refpts(object)[Ftarget, "harvest"], i)),
        target=iter(refpts(object)["max", "yield"], i), obj=iter(object, i),
        tol=.Machine$double.eps^0.5)$minimum
    }
  
    refpts(brp(object))
  }
)
# }}}

# r {{{

#' @title Intrisic rate of increase
#' 
#' @description 
#' Calculates the value of *r*, the intrisic rate of increase. 
#' 
#' @param m An object of class `FLBRP`
#' 
#' @return object of class \code{FLQuant} with *r* estimate
#' 
#' @docType methods
#' @rdname r
#' 
#' @seealso [FLCore::r()] 
#' 
#' @examples
#' data(ple4brp)
#'
#' r(ple4brp)

setMethod("r", signature(m="FLBRP", fec="missing"),
	function(m, by = 'year', method = 'el',...)
    do.call('r', list(m=m(m), fec=mat(m), by=by, method=method)))
# }}}

# sp {{{

#' @title Surplus production
#' 
#' @description 
#' Calculates the surplus production. 
#' 
#' @param m An object of class `FLBRP`
#' 
#' @return object of class \code{FLQuant} with *sp* estimate
#' 
#' @docType methods
#' @rdname sp
#' 
#' @seealso [FLCore::sp()] 
#' 
#' @examples
#' data(ple4brp)
#'
#' sp(ple4brp)

setMethod('sp', signature(stock='FLBRP', catch='missing'),
	function(stock, rel=TRUE)
    return(sp(ssb.obs(stock), catch.obs(stock), rel=rel)))
# }}}

# computeRefpts {{{

#' @rdname brp
#' @description To directly obtain the recalculated `refpts` slot of an `FLBRP` object, the
#' `computeRefpts` method can be used. This is equivalent to *fitting* the object
#' using `brp` and then extracting the `@refpts` slot.
#' @examples
#' m(ple4brp)[1:3,] <- 0.2
#' computeRefpts(ple4brp)

setMethod('computeRefpts', signature(object='FLBRP'), function(object){
	refpts(brp(object))})

# }}}

# production {{{ 

setMethod("production", signature(object="FLBRP"),
  function(object, what="ssb", ...) {

    miny <- dims(object)$minyear
    maxy <- dims(object)$maxyear

    switch(tolower(substr(what, 1, 1)),
      # ssb
      s = catch.obs(object) + window(ssb.obs(object), start=miny+1, end=maxy+1) -
        ssb.obs(object),
      # biomass
      b = catch.obs(object) + window(stock.obs(object), start=miny+1, end=maxy+1) -
        stock.obs(object),
    )
  }
)
# }}}

# FLBRP + FLPar {{{

#' Add external reference points to an FLBRP object
#'
#' Reference points computed outside of `FLBRP` can be added to the *@refpts*
#' slot of an `FLBRP` object. A subsequent call to `brp()` will compute all
#' other quantities related to this reference point.
#'
#' The reference points to be added are passed as an `FLPar` object. Names of
#' these reference points need to follow this convention:
#' - For SSB reference points, name should strat with "B", e.g. "Blim".
#' - For F reference points, first letter must be "F", e.g. "Fmsy".
#' - Yield reference points should start with "C", e.g. "Cpa".
#' - For recruitment reference points, use "R" as starting letter, e.g. "R0".
#'
#' @param e1 A **FLBRP** object
#' @param e2 A **FLPar** containing a set of reference points.
#'
#' @return A **FLBRP** object with the added and calculated reference points.
#'
#' @rdname addFLBRP
#'
#' @author The FLR Team
#' @seealso [FLBRP::brp]
#' @keywords methods
#' @md
#' @examples
#' data(ple4brp)
#' refs <- FLPar(Fmsy=0.21, Blim=207288, Bpa=290203)
#' plot(ple4brp + refs)

setMethod("+", signature(e1="FLBRP", e2="FLPar"),
  function(e1, e2) {

    # CHECK & PLACE e2 names
    id1 <- c(F="harvest", C="yield", R="rec", B="ssb", T="biomass")
    id2 <- substr(dimnames(e2)$params, 1, 1)
    idc <- match(id2, names(id1))

    if(any(is.na(idc)))
      stop(cat("Name of new refpt (", dimnames(e2)[[1]][is.na(idc)],
        ") cannot be assigned to refpts columns\n"))

    # EXPAND refpts
    rps <- FLPar(NA,
      dimnames=list(refpt=c(dimnames(refpts(e1))$refpt, dimnames(e2)$params),
      quant=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),
      iter=seq(dims(e1)$iter)))

    idr <- match(dimnames(e2)[[1]], dimnames(rps)$refpt)
    
    # ASSIGN e2 TODO MAKE iter-proof
    rps@.Data[idr + (idc - 1L) * dim(rps)[1]] <- c(e2)

    # RECALCULATE brp
    refpts(e1) <- rps
    e1 <- brp(e1)

    return(e1)
  }
)

# }}}

# sp(FLStock, FLBRP) {{{

#' @examples
#' sp(ple4, ple4brp)
#' sp(ple4, ple4brp, metric="stock")
#' sp(ple4, ple4brp, metric="stock", fbar=FLQuant(seq(0, 1.2, length=201)))

setMethod('sp', signature(stock='FLStock', catch='FLBRP'),
	function(stock, catch, metric="ssb",
    fbar=seq(0, c(fcrash(catch)), length=201)) {
    
    # SET fbar
    fbar(catch) <- FLQuant(fbar)

    # EVAL metric on FLStock
    xout <- do.call(metric, list(stock))
    # CALL approx with metric + catch on FLBRP
    dat <- with(model.frame(FLQuants(catch,
      "stock"=function(x) do.call(metric, list(x)),
      "catch"=function(x) catch(x)),
      drop=TRUE), approx(stock, catch, xout=c(xout), ties="mean"))

    return(FLQuant(dat$y, dimnames=dimnames(xout)))
  }
)
# }}}

# procerr {{{

#' @examples
#' procerr(ple4, ple4brp)
#' procerr(ple4, ple4brp, metric="stock")
#' procerr(ple4, ple4brp, metric=vb)

procerr <- function(stock, brp, metric="ssb") {

  # EVAL metric
  met <- do.call(metric, list(stock))
  
  #
  res <- log(window(met - catch(stock) + (1 / dim(stock)[4] * 
    sp(stock, brp, metric=metric)), end=dims(stock)$maxyear - 1) %/% met[, -1])
  
  units(res) <- ""

  return(res)

}
# }}}

# ageopt {{{

#' @examples
#' ageopt(ple4brp)

setMethod("ageopt", signature(object="FLBRP"),
  function(object) {
  
  fbar(object) <- FLQuant(0)
  
  res <- stock.wt(object)[, 1] * stock.n(object)[, 1]
  
  if (is.na(range(object)["plusgroup"])) {
    return(FLPar(aopt=apply(res, c(3,6), function(x)
      as.numeric(dimnames(x)[[1]][x==max(x)]))))
  } else {
    return(FLPar(aopt=apply(res[-dim(res)[1]], c(3, 6), function(x)
      as.numeric(dimnames(x)[[1]][x==max(x)]))))
  }
})

# }}}

# z {{{
setMethod("z", "FLBRP", function(object, ...) {

   f <- harvest(object)

   # CHECK harvest is in 'f'
   if(units(f) != 'f')
     stop("Your exploitation rate is not defined as F, cannot be added 
to M")
   else
     return(m(object) %+% f)
   }
) # }}}

# leslie {{{
setMethod("leslie", signature(object="FLBRP",  fec="missing"),  
  function(object, fbar=refpts(object)["crash", "harvest"]) {

    # need to coerce to FLQuant and keep iters      
    fbar(object) <- as.FLQuant(fbar[drop=T])
    
    survivors <- exp(-m(object) - harvest(object))

    fec <- stock.n(object) %*% exp(-(harvest(object) %*% 
      (harvest.spwn(object)) %+% m(object) %*% (m.spwn(object)))) %*%
      stock.wt(object) %*% mat(object)
    
    L <- array(0, c(dim(fec)[1], dim(fec)[1], dim(fec)[6]))

    for (i in seq(dims(object)$iter))
      L[,, i] <- .leslie(iter(survivors, i)[drop=TRUE], iter(fec, i)[drop=TRUE])
    
    return(L)
  }
)
# }}}

# combine {{{
setMethod('combine', signature(x='FLBRP', y='FLBRP'),
  function(x, y, ..., check=FALSE) {

    res <- callNextMethod()

    params(res) <- Reduce(combine, lapply(c(list(x,y), list(...)), params))
    
    refpts(res) <- Reduce(combine, lapply(c(list(x,y), list(...)), refpts))

    return(res)
 }
)
# }}}

# sr {{{

setMethod("sr", "FLBRP",
  function(object, model=NULL) {

    res <- as(object, 'FLSR')
    
    # RESET model
    if(!is.null(model))
      model(res) <- model

    return(res)
  }
)

setReplaceMethod("sr", signature("FLBRP", "FLSR"),
  function(object, value){

    # FIT if needed
    if(all(is.na(params(value))))
      value <- fmle(value)

    # ASSIGN model and params
    model(object) <- model(value)
    params(object) <- params(value)
	
  	return(object)
})
