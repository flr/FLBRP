# refpts.R - DESC
# /refpts.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0


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

    # TODO: HANDLE position args and treat as numeric, check dims

    slot(object, "refpts") <- value

    return(object)
  }
)

setReplaceMethod("refpts", signature(object="FLBRP", value="numeric"),
  function(object, ..., value) {

  dms <- dimnames(refpts(object))

  args <- list(...)

  # ACCEPT only two position arguments
  if(length(args) > 2)
    stop("Values can only be assigned on rows and columns.")

  i <- args[[1]]
  j <- args[[2]]
  
  if(!j %in% dms[[2]])
    stop(paste0("column '", j, "' does not exist"))

  if(!i %in% dms[[1]]) {
    refpts(object) <- expand(refpts(object), refpt=c(dms[[1]], i))
  }

  # EMPTY row so values are recomputed
  refpts(object)[i,] <- as.numeric(NA)

  # ASSIGN given value
  refpts(object)[i, j] <- value

  # RECOMPUTE and return
  return(brp(object))
  }
)

# }}}

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

# fcrash {{{
setMethod("fcrash", signature(x="FLBRP"),
  function(x) {
    return(FLPar(Fcrash=c(refpts(x, "crash", "harvest")), units="f"))
  })
# }}}

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
