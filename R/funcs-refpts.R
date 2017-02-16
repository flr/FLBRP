# refpts - methods for the refpts class
# FLBRP/R/refpts.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# $Id: refpts.R 888 2011-01-17 00:56:11Z lauriekell $


# msy {{{
setMethod("msy", signature(object="FLBRP",params="missing"),
  function(object) {
    refpts(object) <- refpts(as.numeric(NA), refpt='msy',
      iter=as.numeric(dimnames(object@refpts)$iter))
    computeRefpts(object)
  }
) # }}}

# msyRange {{{

#' @title MSY range
#' 
#' @description 
#' Calculates the values of fishing mortality, biomass, SSB etc. for a range of yields either side of MSY. 
#' 
#' @param object \code{FLBRP}
#' @param range the values of yield around MSY 
#' 
#' @return object of class \code{FLPar} with reference point quantities
#' 
#' @export
#' @docType methods
#' @rdname msyrange
#' 
#' @seealso \code{\link{refpts}} 
#' 
#' @examples
#' \dontrun{
#' library(FLBRP)
#' 
#' data(ple4)
#' ple4.sr=fmle(as.FLSR(ple4,model="bevholt"),control=list(silent=TRUE))
#' 
#' ple4.eql=FLBRP(ple4,sr=ple4.sr)
#' 
#' msyRange(ple4.eql)
#' }
setMethod("msyRange", signature(object="FLBRP"),
  function(object,range=0.10){
     refpts(object)=
         FLPar(NA,dimnames=list(refpt   =c("msy","min","max"),
                                quantity=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),
                                iter    =1))
     refpts(object)=computeRefpts(object)
     
     refpts(object)["min","yield"]=refpts(object)["msy","yield"]*(1-range)
     refpts(object)["max","yield"]=refpts(object)["msy","yield"]*(1-range)
     
     fn=function(f,target,obj){
       
       refpts(obj)=
         FLPar(c(f,rep(NA,7)),
                  dimnames=list(refpt   =c("f"),
                                quantity=c("harvest","yield","rec","ssb","biomass","revenue","cost","profit"),
                                iter    =1))
       (computeRefpts(obj)[,"yield"]-target)^2}
     
     for (i in dimnames(refpts(object))$iter){
       refpts(object)["min","harvest",i]=optimise(fn,c(0.01,refpts(object)["msy","harvest",i]),target=refpts(object)["min","yield"],obj=object,tol=.Machine$double.eps^0.5)$minimum
       refpts(object)["max","harvest",i]=optimise(fn,c(1,10)*refpts(object)["msy","harvest",i],target=refpts(object)["max","yield"],obj=object,tol=.Machine$double.eps^0.5)$minimum}
  
     computeRefpts(object)})
# }}}
