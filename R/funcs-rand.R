if (!isGeneric("randFrq"))    setGeneric('randFrq',    function(n, object, ...)    standardGeneric('randFrq'))
if (!isGeneric("randLfd"))    setGeneric('randLfd',    function(n, object, ...)    standardGeneric('randLfd'))
if (!isGeneric("alk"))        setGeneric('alk',        function(n, object, ...)    standardGeneric('alk'))
if (!isGeneric("randAlk"))    setGeneric('randAlk',    function(n, object, ...)    standardGeneric('randAlk'))
if (!isGeneric("sampleAlk"))  setGeneric('sampleAlk',  function(n, object, ...)    standardGeneric('sampleAlk'))
if (!isGeneric("sampleAlk2")) setGeneric('sampleAlk2', function(n, object, ...)    standardGeneric('sampleAlk2'))

#' randFrq
#' Returns a vector with a distribution sampled at random with replacement 
#' from an existing frequency distribution 
#' 
#' @param n A numeric with number of observations to return. If length(n) > 1, 
#' the length is taken to be the number required.
#' @param object A vector, \code{FLQuant} or \code{FLPar} with relative frequency of a measurements
#' 
#' @return A numeric, \code{FLQuant} or \code{FLPar} with a simulated frequency distribution
#' 
#' @docType methods
#' @rdname rand
#' 
#' @examples
#' data(hom)
#' lfd=randFrq(n,hom$F1992)
#' 
#' @export
setMethod('randFrq', signature(n="numeric",object='FLQuant'),
          function(n,object){
            
            if (length(n)>1) n=length(n)
            
            res=FLQuant(apply(object,2:6,function(x,n) randFrq(n,x),n=n))
            
            dimnames(res)=dimnames(object)
            
            res})

setMethod('randFrq', signature(n="numeric",object='FLPar'),
          function(n,object){
                
            if (length(n)>1) n=length(n)
              
            res =FLPar(apply(object,2:length(dim(res)),function(x,n) randFrq(n,x),n=n))
              
            dimnames(res)=dimnames(object)
            
            res})


#' randLfd
#' 
#' Creates a length age distribution based on a mixed distribution
#' 
#' @param n A numeric with number of observations to return. If length(n) > 1, 
#' the length is taken to be the number required.
#' @param object with relative frequency of measurements
#' @param mean with relative frequency of measurements
#' @param sd with relative frequency of measurements
#' @param dist density function
#' @param bin length bin
#' 
#' @return simulated length frequency distribution
#'  
#' @docType methods
#' @rdname rand
#' 
#' @examples
#' randLfd(500,exp(seq(1,.1,-.1)),1:10,.3)
#' 
#' @export
setMethod('randLfd', signature(n="numeric",object="FLQuant"),
          function(n,object,mean=0,sd=1,dist=rnorm,bin=1){
            
            res=alply(object,2:6, function(x,n,mean,sd,dist,bin) c(randLfd(n,c(x),mean,sd,dist=dist,bin)),n=n,mean=mean,sd=sd,dist=dist,bin=bin)
            res=transform(ldply(res, function(x) cbind("data"=x,len=names(x))),
                          data =as.numeric(as.character(data)))
            
            res=as.FLQuant(res)
            
            res[(is.na(res))]=0
            
            return(res)})

#' randAlk
#' 
#' Creates an Age Length Key based on a mixed distribution
#' 
#' @param n A numeric with number of observations to return. If length(n) > 1, 
#' the length is taken to be the number required.
#' @param object with relative frequency of measurements
#' @param mean with relative frequency of measurements
#' @param sd with relative frequency of measurements
#' @param dist density function
#' @param bin length bin
#' 
#' @return simulated Age length Key
#'  
#' @docType methods
#' @rdname rand
#' 
#' @examples
#' randLfd(500,exp(seq(1,.1,-.1)),1:10,.3)
#' 
#' @export
setMethod('randAlk', signature(n="numeric",object="FLQuant"),
          function(n,object,mean=0,sd=1,dist=rnorm,bin=1){
            
            res=alply(object,2:6, function(x,n,mean,sd,dist,bin) c(randAlk(n,c(x),mean,sd,dist=dist,bin)),n=n,mean=mean,sd=sd,dist=dist,bin=bin)
            res=transform(ldply(res, function(x) cbind("data"=x,len=names(x))),
                          data =as.numeric(as.character(data)))
            
            res=as.FLQuant(res)
            
            res[(is.na(res))]=0
            
            return(res)})


#' Returns an Age Length Key (ALK) sampled equal numbers per length class
#'  at random with replacement from an existing ALK 
#' 
#' @param n A numeric with number of observations to return. If length(n) > 1, 
#' the length is taken to be the number required.
#' @param object A matrix with frequency of observations
#' 
#' @return A matrix with a simulated Age Length Key
#'  
#' @docType methods
#' @rdname rand
#' 
#' @examples
#' data(hom)
#' alk=sampleAlk(10,hom$otolith[[1]])
#' 
#' @export
setMethod('sampleAlk', signature(n="numeric",object='numeric'),
          function(n,object){
            
            if (length(n)>1) n=length(n)
            
            fn=function(object){
              rnd   =t(apply(object,1,function(x) cumsum(x)/sum(x)))
              x     =runif(dim(object)[1])
              rnd   =ifelse(rnd<=x,0,1)
              rnd   =t(apply(rnd,1,function(x) cumsum(x)))
              rnd   =ifelse(rnd!=1,0,1)
              
              rnd}  
            
            res=object*0
            for (i in seq(n)) res=res+fn(object)
            
            return(res)})

#' Returns an Age Length Key (ALK) sampled at random in proportion to abundance in the 
#' population with replacement from an existing ALK 
#' 
#' @param n A numeric with number of observations to return. If length(n) > 1, 
#' the length is taken to be the number required.
#' @param object A matrix with frequency of observations
#' 
#' @return A matrix with a simulated Age Length Key
#' 
#' @docType methods
#' @rdname rand
#'   
#' @examples
#' data(hom)
#' alk=sampleAlk(10,hom$otolith[[1]])
#' 
#' @export
#' 
setMethod('sampleAlk2', signature(n="numeric",object='numeric'),
          function(n,object){
            if (is.null((names(n))))
              names(n)=seq(length(n))
            
            res=transform(subset(a2d(object),value>0),p=cumsum(value)/sum(value))
            
            x=runif(n)
            
            res=cbind(mdply(x, function(x,res) res[(res$p>=x),][1,1:2],res=res)[,-1],freq=1)
            
            res=cast(res,len~age,value="freq",sum)
            
            dmns=dimnames(res[,-1])
            #names(dmns)=c("len","age")
            dmns[[1]]=res[,1]
            
            res=as.matrix(res[,-1])
            dimnames(res)=dmns
            
            return(res)})
