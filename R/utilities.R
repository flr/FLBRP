# utilities.R - DESC
# FLBRP/R/utilities.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@seaplusplus.co.uk>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# window    {{{
setMethod("window", signature(x="FLBRP"),
	function(x, start=dims(x@ssb.obs)$minyear, end=dims(x@ssb.obs)$maxyear,
    extend=TRUE, frequency=1) {
    # To window: *.obs
    obs <- c("fbar.obs", "landings.obs", "discards.obs", "rec.obs", "ssb.obs",
      "stock.obs", "profit.obs")

    for(s in obs) {
      slot(x, s) <- window(slot(x, s), start=start, end=end, extend=extend,
        frequency=frequency)
    }
  	
		return(x)
	}
)	# }}}

# propagate {{{
setMethod('propagate', signature(object='FLBRP'),
  function(object, iter, fill.iter=TRUE, obs=TRUE, params=TRUE) {

    # obs FLQuants
    if(obs) {
      obs <- c('fbar.obs', 'landings.obs', 'discards.obs', 'rec.obs',
      'profit.obs', 'ssb.obs', 'stock.obs')

      for(i in obs)
        slot(object, i) <- propagate(slot(object, i), iter=iter,
          fill.iter=fill.iter)
    }

    # other FLQuants
    quants <- c("fbar", "landings.sel", "discards.sel", "bycatch.harvest",
      "stock.wt", "landings.wt", "discards.wt", "bycatch.wt", "m", "mat",
      "harvest.spwn", "m.spwn", "availability", "price", "vcost", "fcost")

    for(i in quants)
      slot(object, i) <- propagate(slot(object, i), iter=iter,
        fill.iter=fill.iter)

    # refpts
    refpts(object) <- propagate(refpts(object), iter=iter,
      fill.iter=fill.iter)

    # params
    if(params)
      params(object) <- propagate(params(object), iter=iter,
        fill.iter=fill.iter)
  
    return(object)
  }
) # }}}

# summary {{{
setMethod("summary", signature("FLBRP"),
  function(object) {
  
    callNextMethod()
    cat("\n")

    # model
    cat("Model: \t")
    print(model(object), showEnv=FALSE)
    
    # params
    print(params(object), reduced=TRUE)

    # refpts flag
    cat("\n")
    flag <- !all(is.na(refpts(object)))
    cat("refpts: ", ifelse(flag, "calculated", "NA"), "\n")
  }
) # }}}

# remap {{{

#' A function to reshape the refpts slot of an FLBRP object.
#'
#' The FLPar containing all estimated reference points in the 'refpts' slot
#' of an object of class 'FLBRP' can be converted into a simpler format with
#' a selection of relevant points. This makes it easier to use the object for
#' plotting and computation. Individual reference points, for example, can be
#' accessed using the '$' operator. The examples below show the difference
#' between both formats.
#'
#' A series of basic reference poiunts are extracted by default with the following names:
#' - FMSY: fishing mortality at MSY, c("msy", "harvest").
#' - SBMSY: spawning biomass at MSY, c("msy", "ssb").
#' - BMSY: total biomass at MSY, c("msy", "biomass").
#' - B0: virgin biomass, c("virgin", "biomass").
#' - SB0: virgin spawning biomass, c("virgin", "ssb").
#'
#' @param object An FLPar with dimensions 'refpt', 'quant' and 'iter'.
#' @param map A names list of dimnames to extract each reference point from.
#' @param ... Extra reference points to be extracted.
#'
#' @return An FLPar object with dimensions 'params' and 'iter'.
#'
#' @name remap
#' @rdname remap
#'
#' @author Iago Mosqueira (WMR)
#' @seealso \link{FLBRP} \link{refpts}
#' @keywords classes
#' @examples
#' data(ple4brp)
#' # Input is refpts slot in FLBRP
#' refpts(ple4brp)
#' # Call with default map
#' remap(refpts(ple4brp))
#' # Additional refpts can be extracted
#' remap(refpts(ple4brp), FMEY=c("mey", "harvest"))

remap <- function(object, map=list(FMSY=c("msy", "harvest"),
  SBMSY=c("msy", "ssb"), BMSY=c("msy", "biomass"),
  B0=c("virgin", "biomass"), SB0=c("virgin", "ssb")), ...) {

  # ADD extra
  map <- c(map, list(...))

  if(!all(unique(unlist(map)) %in%
    unlist(dimnames(object)[c("refpt", "quant")])))
    stop("Some 'refpt' or 'quant' cannot be found in object")

  Reduce(rbind, Map(function(x, y) FLPar(c(object[x[1], x[2]]),
    dimnames=list(params=y, iter=seq(dim(object)[3]))),
  x=map, y=names(map)))
}
# }}}

# segRef {{{

#' Reference points associated with inflection point of a segmented regression
#'
#' Segmented regression (a.k.a. hockey-stick) stock-recruitment relationships 
#' are often used in short-term forecasts and when other SRRs can not be 
#' applied. This function assigns the 'b' parameter of the relationship as an 
#' SSB-related reference point and then calculates all other related ones at e
#' equilibrium. The functions returns an FLBRP object with the new refpts, 
#' called 'segreg'.
#' The object must be set with a fitted 'segreg' stock-recruit relationship.
#'
#' @param x An object of class FLBRP.
#' @param ssb The SSB value of the inflection point, defaults to the 'b' parameter of the 'params' slot.
#'
#' @return An object of class 'FLBRP' with a new 'segreg' row in 'refpts'.
#'
#' @author The FLR Team
#' @seealso [FLBRP-class] [FLCore::segreg()]
#' @keywords classes
#' @examples
#' data(ple4brp)
#' sr(ple4brp) <- sr(ple4brp, model='segreg')
#' refpts(segRef(ple4brp))

segRef<-function(x, ssb=params(x)["b"]) {

  # CHECK model
  if(SRModelName(model(x)) != "segreg")
    stop("FLBRP object must have model 'segreg'")

  # ADD extra row
  dmns <- dimnames(refpts(x))
  dmns$refpt <- c(dmns$refpt, "segreg")
  refpts(x) <- FLPar(NA, dimnames=dmns)
  refpts(x)["segreg", "ssb"] <- ssb

  # COMPUte refpts
  refpts(x)=computeRefpts(x)
  
  return(x)
}

# }}}
