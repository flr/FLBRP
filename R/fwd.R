#' fwd
#'
#' Coerces an \code{FLBRP}, i.e. equilibrium, object into an \code{FLStock}, i.e. dynamic 
#' object. Projects using fbar and harvest slots, for assumed paramters.
#' 
#' @param   \code{object}, an object of class \code{FLBRP}
#'
#' @export
#' @docType methods
#' @rdname fwd
#'
#' @examples
#' /dontrun{
#' stk=fwd(br)
#' plot(stk)}
setMethod("fwd", signature(object="FLBRP", ctrl="missing"),
    function(object,
               sr=object, sr.residuals=FLQuant(1,dimnames=dimnames(rec(object))), sr.residuals.mult=TRUE,
               availability=NULL,maxF=2.0,f=fbar(object)[,-1],run=T)
    {                          
    
    stk =as(object,"FLStock") 
    
    if (!run) return(stk)
    
    res =fwd(stk,f=f,sr=sr,
                        sr.residuals=sr.residuals,
                        sr.residuals.mult=sr.residuals.mult,
                        availability=availability,
                        maxF=maxF)  
                      
    return(res)})
