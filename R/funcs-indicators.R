#### Indicators ################################################################
setGeneric("mnSwt", function(object,...)
   standardGeneric("mnSwt"))
setGeneric("mnCwt", function(object,...)
   standardGeneric("mnCwt"))
setGeneric("mnLwt", function(object,...)
   standardGeneric("mnLwt"))
setGeneric("mnLen", function(object,...)
   standardGeneric("mnLen"))
setGeneric("wt2z", function(object,...)
   standardGeneric("wt2z"))
setGeneric("ln2z", function(object,Linf,...)
    standardGeneric("ln2z"))

setMethod('mnSwt', signature(object='FLStock'), function(object) apply(stock.wt(   object)*stock.n(   object),2:6,sum)/apply(stock.n(   object),2:6,sum)) 
setMethod('mnCwt', signature(object='FLStock'), function(object) apply(catch.wt(   object)*catch.n(   object),2:6,sum)/apply(catch.n(   object),2:6,sum)) 
setMethod('mnLwt', signature(object='FLStock'), function(object) apply(landings.wt(object)*landings.n(object),2:6,sum)/apply(landings.n(object),2:6,sum)) 
  
setMethod('mnLen', signature(object='FLStock'), 
    function(object,a=0.001,b=3,wt="stock.wt") 
    mnLenFunc(object,a,b,wt))
setMethod('wt2z', signature(object='FLStock'), 
    function(object,a=0.001,b=3,wt="stock.wt") 
          wt2zFunc(object,a,b,wt))
setMethod('ln2z', signature(object='numeric',Linf='numeric'),
    function(object,Linf,Lc,k) 
	  ln2zFunc(object,Linf,Lc,k))
setMethod('ln2z', signature(object='numeric',Linf="FLPar"),
    function(object,Linf) 
	  ln2zFunc(object,Linf["Linf"],Linf["Lc"],Linf["k"]))

mnLenFunc<-function(object,a=0.001,b=3,wt="stock.wt"){
    wt.=slot(object,wt)
    n. =slot(object,gsub(".wt",".n",wt))

    apply((wt./a)^(1/b)*n.,c(2,6),sum)/apply(n.,c(2,6),sum)}

wt2zFunc<-function(object,Linf,Lc,k,a=0.001,b=3,wt="stock.wt"){
    mnSz<-mnSzStock(object,a,b,wt); 
    k*(Linf-mnSz)/(mnSz-Lc)}

ln2zFunc<-function(object,Linf,Lc,k){
    k*(Linf-object)/(object-Lc)}
################################################################################

