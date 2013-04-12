# coerce - cpoercion methods for FLBRP
# FLBRP/R/coerce.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainers: Laurence Kell, Cefas & Santiago Cervi?o, IEO
# Last Change: Wed Sep 14, 2011 at 11:21 AM +0200
# $Id: coerce.R 995 2011-06-03 15:29:02Z lauriekell $

# as(FLSR) {{{
setAs('FLBRP', 'FLSR',
  function(from) {

	  rec.age  <-range(from, "min")
	  recYrCls <-as.numeric(dimnames(rec.obs(from))$year)-rec.age
    ssbYrCls <-as.numeric(dimnames(ssb.obs(from))$year)

    ssbYrCls<-ssbYrCls[ssbYrCls %in% recYrCls]
    recYrCls<-ssbYrCls+rec.age

    # calculate ssb and create FLSR from incorprating rec.age
    rec <- rec.obs(from)[,ac(recYrCls)]
    ssb <- ssb.obs(from)[,ac(ssbYrCls)]

   # create the FLSR from
   return(FLSR(name=from@name,
	  rec     =rec,
    ssb     =ssb,
    desc    = "'rec' and 'ssb' slots obtained from a 'FLBRP' from"))

  }
) # }}}

# as.data.frame {{{
setMethod("as.data.frame", 
signature(x="FLBRP", row.names="ANY", optional="character"),
  function(x, row.names=NULL, optional)
  {
    if(any(c('model', 'params', 'refpts', 'name', 'desc', 'range') %in% optional))
      stop("Only 'FLQuant' slots can be converted")
    df <- function(x, slots, names=slots)
    {
      res <-FLQuants(mlply(slots, function(x,fl)
        do.call(x,list(fl)), fl=x))

      names(res)<-slots
      return(
          as.data.frame(res, row.names=row.names)
          )
    }

    return(df(x,optional))
  }
) # }}}

# as(FLStock) {{{
setAs('FLBRP', 'FLStock',
  function(from){
 
    years <- dimnames(fbar(from))$year
    flq<-landings.n(from)
    flq[]<-NA
    res <- FLStock(flq,
      # TODO extend slots for years: check all slots present
      name=name(from))
      #, desc=paste("Created by coercion from 'FLBRP'", desc(from)))

    # range
    nmsF=names(range(res))
    nmsT=names(range(from))
    nms =nmsT[nmsT %in% nmsF]
    range(res)[nms]<-range(from)[nms]
    range(res, c('minyear', 'maxyear')) <- unlist(dims(fbar(from))[c('minyear','maxyear')])

    for (i in c("stock.n","catch.n","landings.n","discards.n","harvest"))
        res[[i]]<-from[[i]]
    
    years<-dimnames(slot(res,"m"))$year
    for (i in c("stock.wt","m","mat","harvest.spwn","m.spwn")){
        dimnames(slot(from,i))$year<-dimnames(fbar(from))$year[1]
        slot(res,i)                <- FLCore::expand(slot(from,i), year=years)
        recycle6d(slot(res,i))     <- slot(from,i)}
    recycle6d(   catch.wt(res))<-catch.wt(from)
    recycle6d(discards.wt(res))<-discards.wt(from)
    recycle6d(landings.wt(res))<-landings.wt(from)
    catch(res)                 <-computeCatch(res,"all")
    
    units(harvest(res))=units(harvest(from))
    if(validObject(res))
      return(res)
    else
     stop("invalid object created. Please check input object")})
# }}}
