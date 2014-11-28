setGeneric('kobe',     function(object,method,...)    standardGeneric('kobe'))

kobeFn=function(object,what=c("sims","trks","pts","smry","wrms")[1],
                prob=c(0.75,0.5,.25),year=NULL,nwrms=10){         
  
  trks. =NULL
  pts.  =NULL
  smry. =NULL
  wrms. =NULL
  sims. =NULL
  
  ## trks
  if ("trks" %in% what){
    
    trks.=rbind(ddply(object,.(year), function(x) data.frame(quantity="stock",  pctl=prob,value=quantile(x$stock,    prob, na.rm=TRUE))),
                ddply(object,.(year), function(x) data.frame(quantity="harvest",pctl=prob,value=quantile(x$harvest,  prob, na.rm=TRUE))))
    
    trks.=transform(trks.,pctl=paste(substr(ac(signif(pctl,2)),3,nchar(ac(signif(pctl,2)))),ifelse(nchar(ac(trks.$pctl))==3,"0",""),"%",sep=""))
    trks.=cast(trks.,year+pctl~quantity,value="value") 
  }
  
  if ("pts" %in% what & !is.null(year))
    pts. =object[object$year==year,]
  
  
  if ("smry" %in% what)
    smry. =ddply(kobeP(sims), .(year), function(x) data.frame(stock      =median(stock(object),       na.rm=TRUE),
                                                              harvest    =median(harvest(object),     na.rm=TRUE),
                                                              red        =mean(  x$red,         na.rm=TRUE),
                                                              yellow     =mean(  x$yellow,      na.rm=TRUE),
                                                              green      =mean(  x$green,       na.rm=TRUE),
                                                              overFished =mean(  x$overFished,  na.rm=TRUE),
                                                              overFishing=mean(  x$overFishing, na.rm=TRUE)))
  if ("wrms" %in% what){          
    wrms =sample(unique(res$iter),nwrms)
    wrms.=sims[sims$iter %in% wrms,]
  }
  
  if ("sims" %in% what)     
    sims. =object
  
  res=list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.)
  
  if (length(what)==1) res[[what]] else res[what]}

setMethod('kobe',  signature(object="FLBRP",method="missing"),  
          function(object,proxy="msy",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
            if (is.null(year)) year=range(object)["maxyear"]
print(1)            
            dat=model.frame(mcf(FLQuants(stock  =ssb.obs( object)%/%refpts(object)[proxy,"ssb"],
                                         harvest=fbar.obs(object)%/%refpts(object)[proxy,"harvest"])),drop=T)
            
print(head(dat)) 
          res=kobeFn(dat,what=what,prob=prob,year=year,nwrms=nwrms)
print(head(res))
# 
#           if (length(what)==1)
#               return(res[[what]])
#             else
#               return(res[what])
res
})

setMethod('kobe',  signature(object="FLBRPs",method="missing"),  
          function(object,proxy="msy",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
            
            res=llply(object,function(x,proxy,what,prob,year,nwrms)
              kobe(x,proxy=proxy,what=what,prob=prob,year=year,nwrms=nwrms)
              ,proxy,what=what,prob=prob,year=year,nwrms=nwrms)
            
            res=list(trks=ldply(res, function(x) x$trks),
                     pts =ldply(res, function(x) x$pts),
                     smry=ldply(res, function(x) x$smry),
                     wrms=ldply(res, function(x) x$wrms),
                     sims=ldply(res, function(x) x$sims))
            
            if (length(what)==1)
              return(res[[what]])
            else
              return(res[what])})

setMethod('kobe',  signature(object="FLBRP",method="FLStock"),  
          function(object,method,proxy="msy",
                   what=c("sims","trks","pts","smry","wrms")[1],
                   prob=c(0.75,0.5,.25),
                   year=NULL,
                   nwrms=10){
            
            if (is.null(year)) year=range(method)["maxyear"]
            
            dat=model.frame(mcf(FLQuants(stock  =ssb( method)%/%refpts(object)[proxy,"ssb"],
                                         harvest=fbar(method)%/%refpts(object)[proxy,"harvest"])),drop=T)
            
            res=kobeFn(dat,what=what,prob=prob,year=year,nwrms=nwrms)
            
            if (length(what)==1)
              return(res)
            else
              return(res[what])})

setMethod('kobe',  signature(object="FLBRP",method="FLStocks"),  
          function(object,method,proxy="msy",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
            
            res=mlply(method,function(x,object,proxy,what,prob,year,nwrms)
              kobe(object,x,proxy=proxy,what=what,prob=prob,year=year,nwrms=nwrms)
              ,proxy,what=what,prob=prob,year=year,nwrms=nwrms)
            
            res=list(trks=ldply(res, function(x) x$trks),
                     pts =ldply(res, function(x) x$pts),
                     smry=ldply(res, function(x) x$smry),
                     wrms=ldply(res, function(x) x$wrms),
                     sims=ldply(res, function(x) x$sims))
            
            if (length(what)==1)
              return(res[[what]])
            else
              return(res[what])})
