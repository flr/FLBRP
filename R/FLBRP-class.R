library# class - ?Short one line description?
# flbrp/R/class.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, ICCAT & Santiago Cervi?o, IEO
# $Id: class.R 945 2011-05-10 13:22:49Z lauriekell $


# # refpts class {{{
# validrefpts <- function(object)
# {
#   # array must have 3 dims
#   if(length(dim(object)) != 3 )
#     return('object array must have 3 dimensions')
# 
#   # names of dimnames must be refpt, quantity and iter
#   if(!all.equal(names(dimnames(object)), c('refpt', 'quantity', 'iter')))
#     return('dimnames must be refpt, quantity and iter')
# 
#   return(TRUE)
# }
# 
# setClass('refpts', representation('FLPar'),
#   prototype=prototype(new('FLPar', array(as.numeric(NA), dim=c(5,8,1),
#   dimnames=list(refpt=c('f0.1', 'fmax', 'spr.30', 'msy', 'mey'), quantity=c('harvest', 
#   'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost', 'profit'), iter=1)))),
#   validity=validrefpts)
# 
# # }}}

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

   
setClass("FLBRP",
   representation(
      "FLComp",
      model          ="formula",
      params         ="FLPar",
      refpts         ="FLPar",
      fbar           ="FLQuant",
      fbar.obs       ="FLQuant",
      landings.obs   ="FLQuant",
      discards.obs   ="FLQuant",
      rec.obs        ="FLQuant",
      ssb.obs        ="FLQuant",
      stock.obs      ="FLQuant",
      profit.obs     ="FLQuant",
 #     revenue.obs    ="FLQuant",
      landings.sel   ="FLQuant",
      discards.sel   ="FLQuant",
      bycatch.harvest="FLQuant",
      stock.wt       ="FLQuant",
      landings.wt    ="FLQuant",
      discards.wt    ="FLQuant",
      bycatch.wt     ="FLQuant",
      m              ="FLQuant",
      mat            ="FLQuant",
      harvest.spwn   ="FLQuant",
      m.spwn         ="FLQuant",
      availability   ="FLQuant",
      price          ="FLQuant",
      vcost          ="FLQuant",
      fcost          ="FLQuant"),
   prototype=prototype(
      name            =character(0),
      desc            =character(0),
      range           =unlist(list(min=as.numeric(NA), max=as.numeric(NA),
                                   plusgroup=as.numeric(NA), minfbar=as.numeric(NA), maxfbar=as.numeric(NA))),
      model        =formula(rec~a),
      params       =FLPar(1),
      refpts          =  FLPar(array(as.numeric(NA), 
                                         dim     =c(7,8,1),
                                         dimnames=list(refpt   =c('virgin', 'msy', 'crash','f0.1', 'fmax', 'spr.30', 'mey'),
                                                       quantity=c('harvest', 'yield', 'rec', 'ssb', 'biomass', 'revenue', 'cost', 'profit'),
                                                       iter    =1))),
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
