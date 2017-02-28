
# -- QUICK TEST
# library(FLBRP)
# data(ple4)
# srp4 <- fmle(as.FLSR(ple4, model="bevholt"))
# brp4 <- brp(FLBRP(ple4, sr=srp4))
# eqp4 <- fwd(brp4)


setMethod("fwd", signature(object="FLBRP", fishery="missing", control="missing"),
  function(object, sr=object, fbar="missing", ...) {
      
    if(!missing(fbar))
      fbar(object) <- fbar  
      
    control <- fbar(object)[,-1]  
    
    stk <- as(object,"FLStock") 
    
    res <- fwd(object=stk, f=control, sr=object, ... )

    return(res)
  }
)
