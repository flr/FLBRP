# methods - methods for FLBRP
# FLBRP/R/methods.Rd
# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cerviño, IEO
# $Id: methods.R 950 2011-05-11 11:39:47Z imosqueira $

chkIter=function(object){
   if (dims(fbar(object))$iter ==1 & dims(object)$iter>1)
      return(propagate(fbar(object),dims(object)$iter))
   
   if (dims(fbar(object))$iter != dims(object)$iter)
      stop("Iters in fbar mismatch")
   
   return(fbar(object))}

setMethod('landings.n', signature(object='FLBRP'),
  function(object){
    
    fbar(object)=chkIter(object)      
    
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    .Call('landings_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))})

setMethod('discards.n', signature(object='FLBRP'),
  function(object)
  {
    fbar(object)=chkIter(object)      
 
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

   .Call('discards_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))})

setMethod('stock.n', signature(object='FLBRP'),
  function(object)
  {
  fbar(object)=chkIter(object)      
   
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    .Call('stock_n', object, SRNameCode(SRModelName(object@model)),
              FLQuant(c(params(object)),dimnames=dimnames(params(object))))})

setMethod('catch.n', signature(object='FLBRP'),
  function(object) {
    fbar(object)=chkIter(object)      
 
    res <- landings.n(object) + discards.n(object)
    if (units(discards.n(object)) == units(landings.n(object)))
		  units(res) <- units(discards.n(object))
    else
      warning("unts of discards.n and landings.n do not match")
      
    return(res)})

setMethod('catch.wt', signature(object='FLBRP'),
  function(object) {
#      idx1 <- landings.sel(object) == 0
#      idx2 <- discards.sel(object) == 0

#      if(dim(idx1)[6] > dim(idx2)[6]) {
#        idx <- array(idx2, dim=dim(idx1)) == TRUE & idx1 == TRUE
#        landings.sel(object)[idx] <- 1
#        discards.sel(object)[apply(idx, 1:5, function(x) as.logical(sum(x)))] <- 1
#      }

#      if(dim(idx2)[6] > dim(idx1)[6]) {
#        idx <- array(idx1, dim=dim(idx2)) == TRUE & idx2 == TRUE
#        discards.sel(object)[idx] <- 1
#        landings.sel(object)[apply(idx, 1:5, function(x) as.logical(sum(x)))] <- 1
#      }

      denom<-landings.sel(object) + discards.sel(object)
      denom[denom==0]<-1
      
      res <- (landings.wt(object) * landings.sel(object) +
              discards.wt(object) * discards.sel(object)) / denom

    if(!is.na(units(discards.wt(object))) &
       !is.na(units(discards.wt(object))))
 
    if (units(discards.wt(object)) == units(landings.wt(object)))
				units(res) <- units(discards.wt(object))

    return(res)})

setGeneric('catch.obs', function(object, ...)
  	standardGeneric('catch.obs'))
setMethod('catch.obs', signature(object='FLBRP'),
  function(object)
    return(discards.obs(object)+landings.obs(object)))

setGeneric('biomass.obs', function(object, ...)
		standardGeneric('biomass.obs'))
setMethod('biomass.obs', signature(object='FLBRP'),
  function(object)
    return(stock.obs(object)))

setGeneric('yield.obs', function(object, ...)
  	standardGeneric('yield.obs'))
setMethod('yield.obs', signature(object='FLBRP'),
  function(object)
    return(landings.obs(object)))
                               

setGeneric('computeFbar', function(object, ...)
  	standardGeneric('computeFbar'))
setMethod('computeFbar', signature(object='FLBRP'),
  function(object)
    return(apply(harvest(object)[ac(object@range["minfbar"]:object@range["maxfbar"])],c(2:6),mean)))

setMethod('rec', signature(object='FLBRP'),
  function(object)
    return(stock.n(object)[1,]))

setGeneric('rec.hat', function(object, ...)
    standardGeneric('rec.hat'))
setMethod('rec.hat', signature(object='FLBRP'),
   function(object)
    return(stock.n(object)[1,]))

setMethod("harvest", signature(object="FLBRP", catch="missing"),
function(object){
    # selectivity
    #sel<-expand(landings.sel(object) + discards.sel(object),year=dims(discards.sel(object))$minyear+(1:dim(fbar(object))[2])-1)
    # create correct size object
    sel=landings.sel(object) + discards.sel(object)
    yrs=dims(sel)$minyear+(0:(dim(fbar(object))[2]-1))
    sel=window(sel, end=max(yrs))
    sel[]=0
 
    # set dimnames                                 
    dimnames(sel)$year=dimnames(fbar(object))$year
                                   
    # calc values                               
    sel       = sweep(sel, c(1,3:6), landings.sel(object) + discards.sel(object), '+')
    sel       = sweep(sel, 2:6, fbar(object), '*')
    units(sel)= 'f'
    
    return(sel)})

setMethod('ypr', signature(object='FLBRP'),
  function(object)
  {
    params(object)<-FLPar(1)
    model( object)<-formula(rec~a)
    
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    res<-.Call("ypr", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLBRP")

    return(res)})

setMethod('computeRefpts', signature(object='FLBRP'), function(object){
	refpts(brp(object))})


setMethod('brp', signature(object='FLBRP'),
function(object)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ") in FLBRP object can not be used by brp. See ?ab"))

    # check needed slots are filled up
    for(i in c('landings.sel', 'discards.sel', 'bycatch.harvest', 'stock.wt',
      'landings.wt','discards.wt', 'bycatch.wt','m','mat','harvest.spwn', 'm.spwn',
      'availability'))
      if(all(is.na(slot(object, i))))
        stop("missing necessary information in slot ", i)

    # check dims in object and params
    iter <- c(dims(object)$iter, length(dimnames(params(object))$iter))
    # if > 1, they should be equal
    if(all(iter > 1))
      if(iter[1] != iter[2])
        stop('iter in FLQuant slots and params do not match, ',
          paste(iter, collapse=' vs. '))

    # extend refpts as needed
    iter <- max(iter)
    if(iter > 1 && dims(refpts(object))$iter == 1){
      #refpts <- propagate(refpts(object), iter, fill.iter=TRUE)
     dmns=dimnames(refpts(object))
     dmns$iter=1:iter
     refpts=FLPar(array(c(refpts(object)),dim=unlist(lapply(dmns, length)),dimnames=dmns)) 
    }else if(iter > 1 && dims(refpts(object))$iter != iter)
      stop("iters in refpts and object slots do not match")
    else
      refpts <- refpts(object)

    if ("virgin" %in% dimnames(refpts)$refpt){
      refpts@.Data["virgin",,         ] <- as.numeric(NA)
      refpts@.Data["virgin","harvest",] <- 0}

    res <- .Call("brp", object, refpts, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      PACKAGE = "FLBRP")

    return(res)})

setMethod('hcrYield', signature(object='FLBRP', fbar='FLQuant'),
  function(object, fbar)
  {
    # check model is supported by brp
    if(!SRNameCode(SRModelName(model(object))) %in% seq(1,6))
      stop(paste("FLSR model (", SRNameCode(SRModelName(model(object))),
        ")in FLBRP object can not be used by brp. See ?ab"))

    # check input fbar dims
    if(!identical(dim(fbar), dim(fbar(object))))
      stop("input fbar must be the same length as fbar(object)")

    if(dims(object)$iter!=1 && dims(object@params)$iter ==1)
       m(object)<-propagate(m(object),iter=dims(params(object))$iter)
    else if (dims(object)$iter!=1 && dims(object@params)$iter !=1)
       if (dims(object)$iter!= dims(object@params)$iter)
          stop("Iters in params don't match")

    res <- .Call("hcrYield", object, SRNameCode(SRModelName(object@model)),
      FLQuant(c(params(object)),dimnames=dimnames(params(object))),
      fbar, PACKAGE = "FLBRP")
    
    # propagate landings.wt
    if(dims(res)$iter != dims(landings.wt(object))$iter)
      landings.wt(object) <- propagate(landings.wt(object), dims(res)$iter)

    return(apply(res %*% landings.wt(object), 2, sum))
   }
)
setMethod('hcrYield', signature(object='FLBRP', fbar='numeric'),
  function(object, fbar)
    hcrYield(object, FLQuant(fbar)))

setMethod('propagate', signature(object='FLBRP'),
  function(object, iter, fill.iter=TRUE, obs=FALSE, params=FALSE){
    # obs FLQuants
    if(obs)
    {
      obs <- c('fbar.obs', 'landings.obs', 'discards.obs', 'rec.obs', 'profit.obs')
      for(i in obs)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)
    }

    # other FLQuants
    quants <- c("fbar", "landings.sel", "discards.sel", "bycatch.harvest", "stock.wt",
      "landings.wt", "discards.wt", "bycatch.wt", "m", "mat", "harvest.spwn", "m.spwn", 
      "availability", "price", "vcost", "fcost")
    for(i in quants)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)

    # refpts
    refpts(object) <- propagate(refpts(object), iter=iter, fill.iter=fill.iter)

    # params
    if(params)
      params(object) <- propagate(params(object), iter=iter, fill.iter=fill.iter)
  
    return(object)})

setMethod('iter', signature(obj='FLBRP'),
  function(obj, iter, ...){
    object <- callNextMethod(obj, iter, ...)
    params(object) <- iter(params(obj), iter)
    if(dim(refpts(obj))[3] > 1)
      refpts(object) <- refpts(obj)[,,iter]
    return(object)})

setMethod('catch', signature(object='FLBRP'),
  function(object) {
    res <- landings(object) + discards(object)
    if (units(discards(object)) == units(landings(object)))
		  units(res) <- units(discards(object))
    else
      warning("units of discards and landings do not match")

    return(res)})

setMethod('catch.sel', signature(object='FLBRP'),
  function(object) return(landings.sel(object)+discards.sel(object)))

setMethod('catch.hat', signature(object='FLBRP'),
  function(object) return(catch(object)))

setMethod('yield', signature(object='FLBRP'),
  function(object) return(landings(object)))

setMethod('yield.hat', signature(object='FLBRP'),
  function(object) return(landings(object)))

setMethod('discards', signature(object='FLBRP'),
  function(object)
    return(apply(discards.n(object) %*% discards.wt(object),2,sum)))

setMethod('discards.hat', signature(object='FLBRP'),
  function(object) return(discards(object)))

setMethod('landings', signature(object='FLBRP'),
  function(object){
    return(apply(landings.n(object)%*%landings.wt(object),c(2:6),sum))})
           
    #return(apply(sweep(landings.n(object),c(1,3:6),landings.wt(object),"*"),2,sum))})

setMethod('landings.hat', signature(object='FLBRP'),
  function(object) return(landings(object))
)

setMethod('stock', signature(object='FLBRP'),
  function(object)
    return(apply(stock.n(object)%*%stock.wt(object),2:6,sum)))
    #return(apply(sweep(stock.n(object),c(1,3:6),stock.wt(object),"*"),2,sum)))

setMethod('stock.hat', signature(object='FLBRP'),
  function(object) return(stock(object)))

setMethod('ssb', signature(object='FLBRP'),
  function(object){
     f    =harvest(object) %*% harvest.spwn(object)
     M    =      m(object) %*%       m.spwn(object)
     
     t1=stock.n(object) %*% exp(-f) %*% exp(-M)
     t2=stock.wt(object)*mat(object)
     
     return(apply(t1 %*% t2,c(2,6),sum))})

setMethod('ssb.hat', signature(object='FLBRP'),
  function(object) return(ssb(object)))

setGeneric('revenue.hat', function(object, ...)
    standardGeneric('revenue.hat'))
setMethod('revenue.hat', signature(object='FLBRP'),
  function(object)
    return(revenue(object)))

setMethod('revenue', signature(object='FLBRP'),
  function(object)
    apply(landings.n(object) %*% landings.wt(object) %*% price(object),2:6,sum))

setMethod('cost', signature(object='FLBRP'),
  function(object){
    res<-apply(sweep(sweep(fbar(object),3:6,vcost(object),"*"),3:6,fcost(object),"+"),2,sum)
    return(res)})

setMethod('profit', signature(object='FLBRP'),
  function(object)
    return(revenue(object)-cost(object)))

setMethod('profit.hat', signature(object='FLBRP'),
  function(object) return(profit(object)))

# setMethod('refpts', signature(object='FLBRP'),
#   function(object) return(object@refpts))
# 
# setMethod('refpts<-', signature(object='FLBRP',y='FLPar'),
#   function(object,y) {
#      object@refpts=y
#      return(object)})

setMethod("r", signature(m="FLBRP", fec="missing"),
	function(m, by = 'year', method = 'el',...)
    do.call('r', list(m=m(m), fec=mat(m), by=by, method=method)))

setMethod('sp', signature(stock='FLBRP', catch='missing'),
	function(stock, rel=TRUE)
    return(sp(ssb.obs(stock), catch.obs(stock), rel=rel)))


setMethod('myers', signature(object='FLBRP'),
function(object){
  
  slopeAt0=switch(SRModelName(model(object)),
                  "bevholt"  =params(object)["a"]/params(object)["b"],
                  "ricker"   =params(object)["a"],
                  "segreg"   =params(object)["a"],
                  "shepherd" =params(object)["a"])
 
  
  aHat  =slopeAt0*spr0(object)
  surv  =exp(-sum(mat(object)*m(object))/sum(mat(object)))
  aTilde=aHat*(1-surv)
  steep =aTilde/(4+aTilde)
 
  age=as.numeric(dimnames(mat(object))$age[mat(object)==1][1])

  f <- function (r,age,aTilde,ps)   (exp(r)^age-ps*exp(r)^(age-1)-aTilde)^2
  
  r <- optimize(f, c(0.01, 2), tol = 0.0001, age=age,aTilde=aTilde,ps=surv)$minimum

  #r    =1/slopeAt0*log(aTilde)
  
  FLPar(c(r=r,s=steep,a=aTilde,hat=aHat))}) 

