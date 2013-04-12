#######################################################################################
### SS3 stuff for Kobe ################################################################
#######################################################################################
          
#TACs=seq(0,1600,200)
#object=paste("/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/SS3/mcmc_",TACs,"/derived_posteriors.sso",sep="")


setGeneric('kobeSS3', function(object, ...) standardGeneric('kobeSS3'))
setGeneric('k2smSS3', function(object, ...) standardGeneric('k2smSS3'))
setGeneric('readSS3', function(object, ...) standardGeneric('readSS3'))

setMethod('kobeSS3', signature(object='character'),
  function(object,xlim=c(0,2),ylim=xlim,refyear=NULL,nrows=-1,plot=FALSE){
  
    if (any(grep("derived_posteriors.sso",object)<1)) stop("file not found")
    
    if (length(object)==1){  
        res=kb1(object,refyear,nrows=nrows)
        
        if (!plot) return(res)
          
        res=kobe(res$ts,xlim=xlim,ylim=ylim) + geom_path(aes(ssb,harvest))  +
                                               geom_point(aes(ssb,harvest),data=res$pts)
        
        return(res)}
    if (length(object) >1){  
        
        res=mlply(object, function(x, refyear,nrows)
                                 kb1(x,refyear,nrows), refyear=refyear, nrows=nrows)
        
        ts =ldply(res, function(x) x$ts)
        pts=ldply(res, function(x) x$pts)
        res=list(ts=ts,pts=pts)
        if (!plot) return(res)
          
        res=kobe(res$ts,xlim=xlim,ylim=ylim) + geom_path( aes(ssb,harvest,group=X1,colour=X1))  +
                                               geom_point(aes(ssb,harvest,group=X1,colour=X1),data=res$pts)
        
        return(res)}
        
    })
          
setMethod('k2smSS3', signature(object='character'),
  function(object,minyear=NULL,raw=FALSE,nrows=-1){
    if (any(grep("derived_posteriors.sso",object)<1)) stop("file not found")

    if (length(object)==1)
       res=kb2(object,minyear=minyear,raw=raw,nrows=nrows)
    
    if (length(object) >1)
       res=mdply(object, function(x,minyear=minyear,raw=raw,nrows=nrows)
                            res=kb2(x,minyear=minyear,raw=raw,nrows=nrows),minyear=minyear,raw=raw,nrows=nrows)
             
    return(res)})

setMethod('readSS3', signature(object='character'),
  function(object,nrows=-1,prob=c(0.75,0.5,.025),nworm=10){
  
    if (any(grep("derived_posteriors.sso",object)<1)) stop("file not found")
    
    if (length(object)==1){  
        res=kb3(object,nrows=nrows,prob=prob,nworm=nworm)
        
        return(res)}
    if (length(object) >1){  
        
        res=mlply(object, function(x,nrows,prob,nworm)
                                 kb3(x,nrows,prob,nworm),nrows=nrows,prob=prob,nworm=nworm)
        
        ts  =ldply(res, function(x) x$ts)
        wrms=ldply(res, function(x) x$wrms)
        res =list(ts=ts,wrms=wrms)
        
        return(res)}
    })
 

## Historic time series medians and marginals in last year
kb1=function(x,refyear=NULL,nrows=-1){
    nms=names(read.csv(x,sep=" ",nrows=nrows))
    yrs=nms[substr(nms,1,3)=="Bra"]
    yrs=as.numeric(substr(yrs,8,nchar(yrs)))
    
    if (is.null(refyear)) refyear=max(yrs)
    
    Fs =paste("F",     yrs,sep="_")
    Bs =paste("Bratio",yrs,sep="_")
     
    res=apply(read.csv(x,sep=" ",nrows=nrows)[,c(Bs,Fs)],2, function(x) as.numeric(ac(x)))
    res=melt(res,id.vars="Iter")[,-1]
    
    res$year=as.numeric(gsub("Bratio_","",ac(res[,"X2"])))
    res$year[is.na(res$year)]=as.numeric(gsub("F_","",ac(res[is.na(res$year),"X2"])))
    res$var=substr(res$X2,1,1)
    
    ts =ddply(res,.(var,year),function(x) median(x$value))
    ts =data.frame(year=ts[ts$var=="B","year"],ssb=ts[ts$var=="B","V1"],harvest=ts[ts$var=="F","V1"])
    
    sct=subset(res,year %in% refyear)[,-1]
    sct=data.frame(ssb=sct[sct$var=="B","value"],harvest=sct[sct$var=="F","value"])
    
    return(list(ts=ts,pts=sct))}

kb2=function(x,minyear=NULL,raw=FALSE,nrows=-1){
    nms=names(read.csv(x,sep=" ",nrows=1))
    yrs=nms[substr(nms,1,3)=="For"]
    yrs=as.numeric(substr(yrs,11,nchar(yrs)))
    
    if (!is.null(minyear)) yrs=minyear:max(yrs)
 
    Fs =paste("F",     yrs,sep="_")
    Bs =paste("Bratio",yrs,sep="_")
     
    res=apply(read.csv(x,sep=" ",nrows=nrows)[,c(Bs,Fs)],2, function(x) as.numeric(ac(x)))
    res=melt(res,id.vars="Iter")[,-1]
    
    res$year=as.numeric(gsub("Bratio_","",ac(res[,"X2"])))
    res$year[is.na(res$year)]=as.numeric(gsub("F_","",ac(res[is.na(res$year),"X2"])))
    res$var=substr(res$X2,1,1)
    
    ts =data.frame(year=res[res$var=="B","year"],ssb=res[res$var=="B","value"],harvest=res[res$var=="F","value"])
        
    smry=data.frame(ts, kobeP(ts$ssb,ts$harvest))
  
    if (raw) return(smry)
    
    smry$p[is.na(smry$p)]=0
    smry$p[is.na(smry$f)]=0
    smry$p[is.na(smry$b)]=0
    smry     =ddply(smry,.(year), function(x) data.frame(f=mean(x$f,na.rm=T),
                                                         b=mean(x$b,na.rm=T),
                                                         p=mean(x$p,na.rm=T)))
    
    return(smry)}

## Historic time series medians percentiles and single iters
kb3=function(x,nrows=-1,prob=c(0.75,0.5,.025),nworm=10){
    nms=names(read.csv(x,sep=" ",nrows=nrows))
    yrs=nms[substr(nms,1,3)=="Bra"]
    yrs=as.numeric(substr(yrs,8,nchar(yrs)))
    
    Fs =paste("F",     yrs,sep="_")
    Bs =paste("Bratio",yrs,sep="_")
     
    res=apply(read.csv(x,sep=" ",nrows=nrows)[,c("Iter",Bs,Fs)],2, function(x) as.numeric(ac(x)))
    res=melt(data.frame(res),id.vars="Iter")
    
    res$year=as.numeric(gsub("Bratio_","",ac(res[,"variable"])))
    res$year[is.na(res$year)]=as.numeric(gsub("F_","",ac(res[is.na(res$year),"variable"])))
    res$var=substr(res$variable,1,1)
    
    ts =ddply(res,.(var,year),function(x,prob) quantile(x$value,prob),prob=prob)
    ts =melt(ts,id.vars=c("var","year"))
    ts =data.frame(ts[ts$var=="B",c("year","variable")],ssb=ts[ts$var=="B",-(1:3)],harvest=ts[ts$var=="F",-(1:3)])
    names(ts)[2]="Percentile"
    
    wrms=subset(res,Iter %in% sample(unique(res$Iter),nworm))[,c("Iter","value","year","var")]
    wrms=data.frame(wrms[wrms$var=="B",c("Iter","year")],ssb=wrms[wrms$var=="B",2],harvest=wrms[wrms$var=="F",2])
 
    return(list(ts=ts,wrms=wrms))}
