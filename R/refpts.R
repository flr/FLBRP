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

# properties {{{
setMethod("properties", signature(object="FLBRP"),

  function(object, ...) {
  
    # GET MSY refpts
    msy <- computeRefpts(object)["msy"]
  
    # CREATE new FLPar
    dmns <- dimnames(msy)
    dmns$refpt <- c(dmns$refpt,"0.5MSY","lower pgy","upper pgy","2*prod","virgin","crash")
    refpts(object) <- FLPar(NA,dimnames=dmns)

    # ASSIGN FMSY and SBMSY values

    refpts(object)["0.5MSY", c("harvest","yield")] <- 
      msy[,c("harvest","yield")]*c(1.2,0.5)
    refpts(object)["lower pgy",c("harvest","yield")] <- 
      msy[,c("harvest","yield")]*c(1.2,0.8)
    refpts(object)["upper pgy",c("harvest","yield")] <- 
      msy[,c("harvest","yield")] * 0.8
    refpts(object)["2*prod", c("yield", "ssb")] <-
      msy[,c("yield", "ssb")] * c(2, 1)
        
    # RECALCULATE to complete rows
    computeRefpts(object)
  }
) # }}}
