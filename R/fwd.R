
setMethod("fwd", signature(biols="FLBRP", control="missing"),
  function(biols, sr=biols, sr.residuals=FLQuant(1,dimnames=dimnames(rec(biols))),
    sr.residuals.mult=TRUE, availability=NULL,maxF=2.0,fbar="missing") {
      if (!missing(fbar))
        fbar(biols)=fbar  
    
      control=fbar(biols)[,-1]  
    
      stk =as(biols,"FLStock") 
    
      res =fwd(stk,f=control,sr=sr,sr.residuals=sr.residuals,
        sr.residuals.mult=sr.residuals.mult,availability=availability,maxF=maxF)

      return(res)
  })
