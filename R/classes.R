# classes.R - FLBRP and FLBRPs classes
# FLBRP/R/classes.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@seaplusplus.co.uk>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# FLBRP {{{
validFLBRP <- function(object){

  # (1) FLQuant objects must share dimnames[1:5] as follows
  #  fbar.obs with landings.obs, discards.obs, ssb.obs, profit.obs
  for (i in c("landings.obs", "discards.obs", "ssb.obs", "stock.obs", "profit.obs"))
    if(!all.equal(dimnames(object@fbar.obs), dimnames(slot(object, i))))
      return(paste("dimnames mismatch:", i))
  # with rec.obs, except dimnames[1]
  if(!all.equal(dimnames(object@fbar.obs)[-1], dimnames(object@rec.obs)[-1]))

  # and slots with quant > 1
  for(i in c("discards.sel", "stock.wt", "landings.wt", "discards.wt", "bycatch.wt",
    "bycatch.harvest", "m", "mat", "harvest.spwn", "m.spwn", "price", "availability"))
    if(!all.equal(dimnames(object@landings.sel), dimnames(slot(object, i))))
      return(paste("dimnames mismatch:", i))

  # range
  dims <- dims(object)
  range <- as.list(object@range)
  if(range$min < dims$min | range$max > dims$max | range$plusgroup > dims$max |   
    range$minfbar < dims$min | range$maxfbar > dims$max)
    return("mismatch between range and object dimensions")

  return(TRUE)
}

#' The FLR class for biological and economic reference points
#'
#' Estimation of equilibirum reference points on the results obtained from fitting
#' a population model can be carried out using the \code{FLBRP} class. Objects
#' of this classs contain the information necessary for the calculation of
#' population and fisheries quantities (abundance, catches, ...) under a range
#' of levels of fishing mortality ('F'). From these, the values of those
#' quantities that maximize or minimize the values of different indicators can
#' be calculated.
#'
#' The most common input for the calculation of reference points is the result
#' of an stock assessment model fit, usually provided as two objects of class
#' \code{\link{FLStock}} and \code{\link{FLSR}}.
#' 
#' @name FLBRP
#' @rdname FLBRP
#' @docType class
#' @aliases FLBRP-class
#'
#' @slot model The formula for the stock-recruitment model, `formula,`.
#' @slot params Parameters of the stock-recruitment relationship, `FLPar,`.
#' @slot refpts The estimated reference points, `FLPar,`.
#' @slot fbar A vector of mean fishing mortality values for calculating the reference points, `FLQuant,`.
#' @slot fbar.obs The time series of observed mean fishing mortality, `FLQuant,`.
#' @slot landings.obs The time series of observed total landings, `FLQuant,`.
#' @slot discards.obs The time series of observed total discards, `FLQuant,`.
#' @slot rec.obs The time series of observed recruitment, `FLQuant,`.
#' @slot ssb.obs The time series of observed spawning stock biomasses, `FLQuant,`.
#' @slot stock.obs The time series of observed total stock abundance, `FLQuant,`.
#' @slot profit.obs The time series of profits, `FLQuant,`.
#' @slot landings.sel Selectivity at age for the landings, `FLQuant,`.
#' @slot discards.sel Selectivity at age for the discards, `FLQuant,`.
#' @slot bycatch.harvest X, Fishing mortality at age of bycatch species, `FLQuant,`.
#' @slot stock.wt Mean weight-at-age in the stock to use in calculations, `FLQuant,`.
#' @slot landings.wt Mean weight-at-age in the landings to use in calculations, `FLQuant,`.
#' @slot discards.wt Mean weight-at-age in the discards to use in calculations, `FLQuant,`.
#' @slot bycatch.wt Mean weight-at-age in the bycatch to use in calculations, `FLQuant,`.
#' @slot m Natural mortality at age, `FLQuant,`.
#' @slot mat Maturity at age, `FLQuant,`.
#' @slot harvest.spwn Proportion of harvest/fishing mortality before spawning, `FLQuant,`.
#' @slot m.spwn Proportion of natural mortality before spawning, `FLQuant,`.
#' @slot availability , `FLQuant,`.
#' @slot price Price at age, `FLQuant,`.
#' @slot vcost X, `FLQuant,`.
#' @slot fcost X, `FLQuant,`.
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{\code{.obs} slots dims 1:5}{Slots containing observations, named
#'       \code{*.obs}, must share dimensions 1 to 5}
#'     \item{by-age slots dims 1:5}{Slots containing vectors of values at age,
#'        must share dimensions 1 to 5}
#' }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLBRP'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity. If an unnamed \code{FLQuant} object is provided, this is used
#' for sizing but not stored in any slot.
#' 
#' A standard method also exists for object of class \code{FLStock}, with or
#' without additional information on the stock-recruits relationship. This can
#' be provided as an object of class \code{FLSR}, one of class \code{predictModel}
#' or as a list wqith elements named 'params', of class \code{FLPar}, and 'model',
#' of class \code{formula}.
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' \describe{
#'     \item{METHOD}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' @author The FLR Team
#' @seealso \link{FLComp} \link{FLStock} \link{FLSR} \link{brp}
#' @keywords classes
#' @examples
#'
#' data(ple4)
#'
#' # Create the FLSR and FLBRP objects
#' psr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
#' prp <- brp(FLBRP(ple4, sr=psr))
#'
#' summary(prp)
#' refpts(prp)

setClass("FLBRP",
  contains="FLComp",
  representation(
    model           = "formula",
    params          = "FLPar",
    refpts          = "FLPar",
    fbar            = "FLQuant",
    fbar.obs        = "FLQuant",
    landings.obs    = "FLQuant",
    discards.obs    = "FLQuant",
    rec.obs         = "FLQuant",
    ssb.obs         = "FLQuant",
    stock.obs       = "FLQuant",
    profit.obs      = "FLQuant",
    landings.sel    = "FLQuant",
    discards.sel    = "FLQuant",
    bycatch.harvest = "FLQuant",
    stock.wt        = "FLQuant",
    landings.wt     = "FLQuant",
    discards.wt     = "FLQuant",
    bycatch.wt      = "FLQuant",
    m               = "FLQuant",
    mat             = "FLQuant",
    harvest.spwn    = "FLQuant",
    m.spwn          = "FLQuant",
    availability    = "FLQuant",
    price           = "FLQuant",
    vcost           = "FLQuant",
    fcost           = "FLQuant"),
   prototype=prototype(
      name            =character(1),
      desc            =character(1),
      range           =unlist(list(min=as.numeric(NA), max=as.numeric(NA),
        plusgroup=as.numeric(NA), minfbar=as.numeric(NA), maxfbar=as.numeric(NA))),
      model        =formula(rec~a),
      params       =FLPar(1),
      refpts          =  FLPar(array(as.numeric(NA), dim=c(7,8,1), dimnames=list(
          refpt=c('virgin', 'msy', 'crash','f0.1', 'fmax', 'spr.30', 'mey'),
          quant=c('harvest', 'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost', 'profit'),
          iter=1))),
      fbar            =new("FLQuant"),
      landings.sel    =new("FLQuant"),
      fbar.obs        =new("FLQuant"),
      landings.obs    =new("FLQuant"),
      discards.obs    =new("FLQuant"),
      rec.obs         =new("FLQuant"),
      ssb.obs         =new("FLQuant"),
      stock.obs       =new("FLQuant"),
      profit.obs      =new("FLQuant"),
      revnue.obs      =new("FLQuant"),
      landings.sel    =new("FLQuant"),
      discards.sel    =new("FLQuant"),
      bycatch.harvest =new("FLQuant"),
      stock.wt        =new("FLQuant"),
      landings.wt     =new("FLQuant"),
      discards.wt     =new("FLQuant"),
      bycatch.wt      =new("FLQuant"),
      m               =new("FLQuant"),
      mat             =new("FLQuant"),
      harvest.spwn    =new("FLQuant"),
      m.spwn          =new("FLQuant"),
      availability    =new("FLQuant"),
      price           =new("FLQuant"),
      vcost           =new("FLQuant"),
      fcost           =new("FLQuant"),
      validity        =validFLBRP
      ))  # }}}

# FLBRPs {{{
FLBRPs <- setClass("FLBRPs", contains="FLComps",
	validity=function(object) {
    # All items are FLBRP
    if(!all(unlist(lapply(object, is, 'FLBRP'))))
      return("Components must be FLBRP")	
	
	  return(TRUE)})

setGeneric('FLBRPs', function(object, ...)
    standardGeneric('FLBRPs'))

# constructor
setMethod("FLBRPs", signature(object="FLBRP"), function(object, ...) {
    lst <- c(object, list(...))
    FLBRPs(lst)
})

setMethod("FLBRPs", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLBRPs")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLBRPs',  c(list(object=object), args))
	  }
  }
)

setMethod("FLBRPs", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLBRPs", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}
