# constructors - constructor methods for FLBRP
# FLBRP/R/constructors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, ICCAT & Santiago Cerviño, IEO
# $Id: constructors.R 926 2011-04-26 14:23:22Z lauriekell $

# FLBRP

# FLBRP(object='missing', sr='missing') {{{
setMethod('FLBRP', signature(object='missing', sr='missing'),
  function(..., model=formula(rec~a), params=FLPar(1, params='a'),
    fbar=FLQuant(seq(0, 4, 0.04), quant='age'))
  {

    args <- list(...)
    
    # quant
    if(length(args) > 1)
      qname <- quant(args[[1]])
    else
      qname <- 'age'
    
    args <- c(args, list(model=model, params=params, fbar=fbar))
    
    # resize: years
    slots <- c('fbar.obs', 'landings.obs', 'discards.obs', 'rec.obs', 'ssb.obs',
      'stock.obs','profit.obs')

    # find slots not provided as argument
    empty <- !slots %in% names(args)
  
    # if any of them given, use for sizing
    if(any(!empty))
      dnames <- dimnames(args[[slots[!empty][1]]])
    else
      dnames <- dimnames(FLQuant(quant=qname))

    for(i in slots[empty])
      args[[i]] <- FLQuant(dimnames=dnames)
    
    # resize: ages
    slots <- c('landings.sel', 'discards.sel', 'bycatch.harvest', 'stock.wt',
      'landings.wt', 'discards.wt', 'bycatch.wt', 'm', 'mat', 'harvest.spwn', 'm.spwn',
      'availability', 'price') #, 'revenue.obs')
    
    # find slots not provided as argument
    empty <- !slots %in% names(args)
  
    # if any of them given, use for sizing
    if(any(!empty))
      dnames <- dimnames(args[[slots[!empty][1]]])
    else
      dnames <- dimnames(FLQuant(quant=qname))

    for(i in slots[empty])
      args[[i]] <- FLQuant(dimnames=dnames)

    # range
    if(!'range' %in% names(args))
    {
      dimsl <- dims(args[['landings.obs']])
      dimsw <- dims(args[['mat']])
      args[['range']] <- c(min=dimsw$min, max=dimsw$max, minfbar=dimsw$min,
        maxfbar=dimsw$max, plusgroup=dimsw$max)
    }
  
    # resize: cost
    slots <- c('vcost', 'fcost')

    # find slots not provided as argument
    empty <- !slots %in% names(args)
  
    # if any of them given, use for sizing
    if(any(!empty))
      dnames <- dimnames(args[[slots[!empty][1]]])
    else
      dnames <- dimnames(FLQuant(quant=qname))

    for(i in slots[empty])
      args[[i]] <- FLQuant(dimnames=dnames)

    res <- do.call(new, c(list('FLBRP'), args))
    return(res)
  }
) # }}}

# FLBRP(object='missing', sr='FLSR')  {{{
setMethod('FLBRP', signature(object='missing', sr='FLSR'),
  function(sr, ...)
  {
    args <- list(...)

    do.call('FLBRP', c(list(model=model(sr), params=params(sr)), args))
  }
) # }}}

# FLBRP(object='FLStock', sr='FLSR')  {{{
setMethod('FLBRP', signature(object='FLStock', sr='FLSR'),
  function(object, sr, ...){
    FLBRP(object=object, model=sr@model, params=sr@params, ...)}) # }}}

# FLBRP(object=FLStock, sr=missing) {{{
setMethod('FLBRP', signature(object='FLStock', sr='missing'),
  function(object, model=formula(rec~a), params=FLPar(1, params='a'),
    fbar=seq(0, 4, 0.04), nyears=3, biol.nyears=nyears, fbar.nyears=nyears,
    sel.nyears=fbar.nyears, na.rm=TRUE, mean='arithmetic', ...)
    {
    #warning("Currently sr params set to 1, i.e. per recruit")

    # dims & dimnames
    dims <- dims(object)
    if (!all(c("minfbar","maxfbar") %in% names(range(object))))
       stop("'minfbar' and 'maxfbar' missing from range")
    
    maxyear <- dims$maxyear
    byears <- ac(seq(maxyear-biol.nyears+1, maxyear))
    fyears <- ac(seq(maxyear-fbar.nyears+1, maxyear))
    syears <- ac(seq(maxyear-sel.nyears+1, maxyear))
    fages <- ac(seq(object@range['minfbar'], object@range['maxfbar']))
    snames <- dimnames(object@catch)
    dnames <- dimnames(object@catch.n)
    dnames[['year']] <- '1'
    cnames <- snames
    cnames[['year']] <- '1'

    # mean
    if(mean == 'arithmetic')
      foo <- function(x) ifelse(all(is.na(x)), 0, mean(x, na.rm=na.rm))
    else if (mean =='geometric')
      foo <- function(x) ifelse(all(is.na(x)), 0, exp(mean(log(x), na.rm=na.rm)))

    # scaling
    # 1. harvest values are divided for that year fbar (mean harvest for fages)
    scaling  <- sweep(object@harvest[,fyears], 2:6, apply(object@harvest[fages,fyears] ,
      2:6, 'mean', na.rm=na.rm), "/")
    # 2. mean across fyears. All years are thus given equal weight
    scaling <- apply(scaling, c(1,3:6), foo)
    
    # NEW FLBRP
    res <- new('FLBRP',
      # range
      range=object@range[c('min', 'max', 'plusgroup', 'minfbar', 'maxfbar')],

      # fbar
      fbar=FLQuant(fbar, units=units(object@harvest), quant=dims$quant),

      # slots to be mean of byears
      # (m, mat, harvest,spwn, m.spwn, discards.wt, landings.wt)
      m = apply(object@m[,byears], c(1,3:6), foo),
      mat = apply(object@mat[,byears], c(1,3:6), foo),
      stock.wt = apply(object@stock.wt[,byears], c(1,3:6), foo),
      harvest.spwn = apply(object@harvest.spwn[,byears], c(1,3:6), foo),
      m.spwn = apply(object@m.spwn[,byears], c(1, 3:6), foo),
      discards.wt = apply(object@discards.wt[,byears], c(1,3:6), foo),
      landings.wt = apply(object@landings.wt[,byears], c(1,3:6), foo),

      # rec.obs
      rec.obs = object@stock.n[ac(dims$min)],

      # ssb.obs
      ssb.obs= ssb(object),

      stock.obs= computeStock(object),

      # landings & discards
      landings.obs = object@landings,
      discards.obs = object@discards,

      # fbar.obs
      fbar.obs = fbar(object),

      # profit.obs
      profit.obs = FLQuant(dimnames=snames),
      
      # revenue.obs
      # revenue.obs = FLQuant(dimnames=snames),

      # vcost & fcost
      vcost=FLQuant(dimnames=cnames),
      fcost=FLQuant(dimnames=cnames),
      
      # discards.sel & landings.sel
      discards.sel = scaling * apply(object@discards.n[,syears]/
        (object@discards.n[,syears] + object@landings.n[,syears]), c(1,3:6), foo),
      landings.sel = scaling * apply(object@landings.n[,syears]/
        (object@discards.n[,syears] + object@landings.n[,syears]), c(1,3:6), foo),

      # bycatch.wt & bycatch.harvest
      bycatch.wt = FLQuant(0, dimnames=dnames),
      bycatch.harvest = FLQuant(0, dimnames=dnames, units=units(object@harvest)),

      # availability
      availability = FLQuant(1, dimnames=dnames),

      # price
      price = FLQuant(as.numeric(NA), dimnames=dnames),

      # model & params
      model = model,
      params = params
    )

    # extra args
    args <- list(...)
    for (i in names(args))
      slot(res, i) <- args[[i]]

    return(res)
  }
) # }}}

# FLBRP(object=data.frame, sr=missing)  {{{
setMethod('FLBRP', signature(object='data.frame', sr='missing'),
  function(object, quant="age", ...){
    
      flqs=names(getSlots("FLBRP")[getSlots("FLBRP")=="FLQuant"])

      object=object[,names(object) %in% c(quant,flqs,"catch.sel","catch.wt")]
      
      object$landings.sel=object$catch.sel-object$discards.sel
      object$landings.wt =(object$catch.sel*object$catch.wt-object$discards.sel*object$discards.wt)/(object$catch.sel+object$discards.sel)
 
      slots=names(object)[names(object) %in% flqs]
      res <- vector("list", length(slots))
      names(res) <- slots
      
      for(i in slots){
        data <- object[,c(quant, i)]
        
        names(data)[2] <- 'data'
        res[[i]] <- as.FLQuant(data)}

      res <- do.call('FLBRP', c(res)) #, list(...)))

     # set some defaults
    defaults <- c(discards.sel=0, bycatch.harvest=0, discards.wt=0, bycatch.wt=1,availability=1)
    for (i in names(defaults))
      slot(res, i)[] <- defaults[i]

    return(res)
  }
) # }}}

# FLBRP(object=data.frame, sr=missing)  {{{
setMethod('FLBRP', signature(object='data.frame', sr='FLSR'),
  function(object, sr=sr, quant="age", ...){

    res        =FLBRP(object)
    model(res) =model(sr)
    params(res)=params(sr)
 
  return(brp(res))}
) # }}}


# FLBRP(object="FLStock", sr="list") {{{
setMethod('FLBRP', signature(object='FLStock', sr='list'),
  function(object, sr, ...){

  if (!(all(c("model","params") %in% names(sr)))) stop("model and params not in sr list")
  if (is(sr[["model"]],"character"))
    sr[["model"]]<-do.call("bevholt", list())$model

    FLBRP(object=object, model=sr[["model"]], params=sr[["params"]], ...)})
# }}}

#brps=FLBRPs(dlply(dbICES$ypr, .(wg,stock), FLBRP))

# FLBRP(object="FLBRP", sr="mssing") {{{
setMethod('FLBRP', signature(object='FLBRP', sr='missing'),
  function(object, sr, ...){

     newObj=FLBRP()
     
     slts=names(getSlots("FLBRP"))
     slts=slts[slts != "refpts"]
     for (i in slts) slot(newObj,i)=slot(object,i)
     refpts(newObj)=FLPar(array(c(object@refpts), dim=dim(object@refpts), dimnames=dimnames(object@refpts)))
    
     args <- list(...)
     for (slt in names(args))
       slot(newObj, slt)<-args[[slt]]

   return(newObj)})
# }}}

setMethod('FLBRP', signature(object='FLBRP', sr='list'),
  function(object, sr, ...){
     
     if ("model"  %in% names(sr)) model(object) =do.call(sr[["model"]], list())$model
     if ("params" %in% names(sr)) params(object)=sr[["params"]]
     
     newObj=FLBRP()
     
     slts=names(getSlots("FLBRP"))
     slts=slts[slts != "refpts"]
     for (i in slts) slot(newObj,i)=slot(object,i)
     refpts(newObj)=FLPar(array(c(object@refpts), dim=dim(object@refpts), dimnames=dimnames(object@refpts)))
 
          
     args <- list(...)
     if ("fbar" %in% names(args))
       args[["fbar"]]=FLQuant(args[["fbar"]])
     for (slt in names(args))
       slot(newObj, slt)<-args[[slt]]

   return(newObj)})

setMethod('FLBRP', signature(object='character', sr='missing'),
          function(object, sr, ...) pro2boxFLBRP(object, ...))

setPA=function(x,df) {
  
  dmns=dimnames(refpts(x))
  
  dmns$refpt=c(dmns$refpt,"blim","bpa","flim","fpa")
  
  refpts(x)=FLPar(NA,dimnames=dmns)
  
  names(df)=tolower(names(df))
  refpts(x)["fpa", "harvest"]=df$fpa
  refpts(x)["fpa", "harvest"]=df$flim
  refpts(x)["bpa", "ssb"]    =df$bpa
  refpts(x)["blim","ssb"]    =df$blim
 
  x}

setGeneric('pa<-', function(object,value)
  standardGeneric('pa<-'))
 
setGeneric('obs<-', function(object,value)
  standardGeneric('obs<-'))

setGeneric('stock.obs<-', function(object,value)
  standardGeneric('stock.obs<-'))
 
setMethod("stock.obs<-", signature(object="FLBRP", value="FLQuant"),
  function(object, value) object@stock.obs=value)
  
setObs=function(x,df) {
  
  flq              =FLQuant(NA,dimnames=list(year=df$year))
  
  x@fbar.obs      =FLQuant(df$fbar.obs,     dimnames=dimnames(flq))
  x@landings.obs  =FLQuant(df$landings.obs, dimnames=dimnames(flq)) 
  x@discards.obs  =flq 
  x@rec.obs       =FLQuant(df$rec.obs,      dimnames=dimnames(flq)) 
  x@ssb.obs       =FLQuant(df$ssb.obs,      dimnames=dimnames(flq)) 
  x@stock.obs     =FLQuant(df$biomass.obs,  dimnames=dimnames(flq)) 
  x@profit.obs    =flq 
#  x@revenue.obs   =flq 
  
  x}

setMethod("pa<-", signature(object="FLBRP", value="data.frame"),
  function(object, value) setPA(object,value))
 
setMethod("obs<-", signature(object="FLBRP", value="data.frame"),
  function(object, value) setObs(object,value))
 
#setRPs(brps[[1]],dbICES$pa[1,])

