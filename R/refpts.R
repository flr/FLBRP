# refpts.R - DESC
# /refpts.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# refpts(object, FMSY=c('msy', 'harvest'), BMSY=c('msy', 'biomass'))

# refpts(object, FMSY=c('msy', 'harvest'), BMSY=c('msy', 'biomass'), MSY=msy)

# refpts {{{
setMethod("refpts", signature(object="FLBRP"),
  function(object, refpt=seq(dim(object@refpts)[1]),
    quant=seq(dim(object@refpts)[2]), ...) {
    
    args <- list(...)

    # NO extra arguments
    if(length(args) == 0) {
      return(slot(object, "refpts")[refpt, quant,])
    }

    #
    res <- lapply(args, function(x) {
      if(is.character(x))
        c(do.call("[", list(x=object@refpts, i=x[1], j=x[2])))
      else if(is.function(x))
        c(do.call(x, list(object))) 
    })

    return(do.call("FLPar", res))

  }) # }}}

# refpts<- {{{
setReplaceMethod("refpts", signature(object="FLBRP", value="FLPar"),
  function(object, ..., value) {

  slot(object, "refpts") <- value

  return(object)

  }
) # }}}

# msy et al {{{
setMethod("msy", signature(x="FLBRP"),
  function(x) {
    return(refpts(x, "msy", "yield"))
  })

setMethod("fmsy", signature(x="FLBRP"),
  function(x) {
    return(refpts(x, "msy", "harvest"))
  })

setMethod("bmsy", signature(x="FLBRP"),
  function(x) {
    return(refpts(x, "msy", "biomass"))
  })

setMethod("sbmsy", signature(x="FLBRP"),
  function(x) {
    return(refpts(x, "msy", "ssb"))
  }) # }}}
