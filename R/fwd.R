
setMethod("fwd", signature(object="FLBRP", ctrl="missing"),
    function(object,
               sr=object, sr.residuals=FLQuant(1,dimnames=dimnames(rec(object))), sr.residuals.mult=TRUE,
               availability=NULL,maxF=2.0,fbar="missing")
    {                          

    if (!missing(fbar)) fbar(object)=fbar  
    ctrl=fbar(object)[,-1]  
    stk =as(object,"FLStock") 
    res =fwd(stk,f=ctrl,sr=sr,sr.residuals=sr.residuals,sr.residuals.mult=sr.residuals.mult,availability=availability,maxF=maxF)  
                      
    return(res)})
