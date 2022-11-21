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
  function(object, iter, fill.iter=TRUE, obs=FALSE, params=FALSE){
    # obs FLQuants
    if(obs)
    {
      obs <- c('fbar.obs', 'landings.obs', 'discards.obs', 'rec.obs', 'profit.obs')
      for(i in obs)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)
    }

    # other FLQuants
    quants <- c("fbar", "landings.sel", "discards.sel", "bycatch.harvest", "stock.wt",
      "landings.wt", "discards.wt", "bycatch.wt", "m", "mat", "harvest.spwn", "m.spwn", 
      "availability", "price", "vcost", "fcost")
    for(i in quants)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)

    # refpts
    refpts(object) <- propagate(refpts(object), iter=iter, fill.iter=fill.iter)

    # params
    if(params)
      params(object) <- propagate(params(object), iter=iter, fill.iter=fill.iter)
  
    return(object)}) # }}}

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
#' Inout is refpts slot in FLBRP
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

  Reduce(rbind, 
  Map(function(x, y) FLPar(c(object[x[1], x[2]]),
    dimnames=list(param=y, iter=seq(dim(object)[3]))),
  x=map, y=names(map)))
}
# }}}
