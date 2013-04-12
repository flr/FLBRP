################################################################################
# MSE functions                                                                #
################################################################################

#### Observation Error Models ##################################################
# These generate psuedo data from the OM for use by the MP                     #
################################################################################

#### OEM for the stock
OEMStock<-function(OM,start=range(OM,"minyear"),end=start,...){

    ## Sampling years
    yrs=ac(start:end)
    stk=trim(OM, year=as.numeric(yrs))
    
    ## replace any slots so desired
    args <- list(...)
    if (length(args)>0) stk[[names(args)]]=args

    return(stk)}

#### OEM for unbiased index
OEMSurvey<-function(OM,start=range(OM,"minyear"),end=start,startf=0,endf=0.01,deviates=NULL,...){

    ## Sampling years
    yrs=ac(start:end)
    res=trim(OM, year=as.numeric(yrs))

    idx                          =as(res,"FLIndex")
    range(idx,c("startf","endf"))=c(startf,endf)

    if (!is.null(deviates))
      idx@index<-idx@index*deviates[dimnames(idx@index)$age,ac(yrs)]

    ## replace any slots so desired
    args <- list(...)
    if (length(args)>0) idx[[names(args)]]=args

    return(idx)}
    
#### OEM for Fishery CPUE Index
OEMCpue<-function(OM,start=range(OM,"minyear"),end=start,startf=0,endf=0.01,deviates=NULL){
     OM         =window(OM,start=start,end=end)
     idx        =OEMSurvey(OM,start,end,startf,endf,deviates=NULL)
     index( idx)=sweep(catch.n(OM),2:6,fbar(OM),"/")
     effort(idx)=fbar(OM)
 
     return(idx)}
  
## function to apply a linearly increasing trend to an FLQuant
biasLinear<-function(x,obj){
   if (x>0)
      res  <-1-(sort(cumsum(rep(x, dims(obj)$year)),d=T)-x)
   else
      res  <-sort(1-(sort(cumsum(rep(x, dims(obj)$year)),d=T)-x),d=T)
 
   return(obj*FLQuant(rep(res,each=dims(obj)$age),dimnames=dimnames(obj)))}

misAgeing=function(x,misAge){
   tmp=function(x,misAge) t(t(x[,drop=T])%*%as.matrix(misAge))

   for (i in 1:10)
     x[,,,,,i]=tmp(x[,,,,,i],misAge)

   return(x)}
 
## VPA Based MP
vpaMP<-function(MP,harvest,fratio=1,CV=0.25,bias=NULL){
   harvest(MP)          =harvest
   iYr                  =range(MP,"maxyear")
   harvest(MP)[,ac(iYr)]=apply(harvest(MP)[,ac(range(MP,"maxyear"))]*rlnorm(prod(unlist(dims(stock.n(MP)))[c(1,10)]),0,CV),c(1,6),mean)

   
   if (!is.null(bias))
      harvest(MP)[,ac(iYr)]=sweep(harvest(MP)[,ac(iYr)],2,bias[,ac(iYr)]/bias[,ac(iYr-1)],"/")
   
   MP<-MP+VPA(MP,fratio=fratio)

   return(MP)}

## PA HCR
hcr<-function(SSB,refpt,Ftar=0.8,Btrig=0.75,Fmin=0.025,Blim=0.25){
    if (Blim>=Btrig) stop("Btrig must be greater than Blim")
    Btrig=refpt[1,    "ssb"]*Btrig
    Blim =refpt[1,    "ssb"]*Blim
    Fmin =refpt[1,"harvest"]*Fmin
    Ftar =refpt[1,"harvest"]*Ftar

    a= FLPar((Ftar-Fmin)/(Btrig-Blim))
    b= FLPar(Ftar-a*Btrig)
    
    val =qmax(qmin(sweep(sweep(SSB,6,a,"*"),6,b,"+"),Ftar),Fmin)
#    val2=qmax(qmin((Ftar-(Ftar-Fmin)*(Btrig-SSB)/(Btrig-Blim)),Ftar),Fmin)
  
    return(val)}

runMSE<-function(OM,start,srPar,srRsdl=srRsdl,plusgroup=range(OM,"plusgroup"),fmult=0.6,test=FALSE,fratio=1.0,CV=0.3,
                 Ftar=0.75,Btrig=0.50,Fmin=Ftar*0.1,Blim=Btrig*0.0,biasM=FLQuant(1,dimnames=dimnames(m(OM)[1])),biasU=NULL,
                 misAge=diag(1,dims(OM)$age,dims(OM)$age)){

  ## Get number of iterations in OM
  nits  =dims(OM)$iter

  #### Observation Error (OEM) setup #######################
  ## Random variation for Catch & CPUE, CV=0.25%
  ctcDev=FLQuant(rlnorm(prod(dim(catch.n(OM))),0,CV),dimnames=dimnames(catch.n(OM)))
  OM.   =transform(OM, catch.n=catch.n(OM)*ctcDev)
  OM.   =setPlusGroup(OM.,plusgroup)
  MPstk =OEMStock(OM., start=range(OM,"minyear"),end=range(OM,"maxyear"))
  m(MPstk)=sweep(m(OM.),2,biasM,"*")
  catch.n(MPstk)=misAgeing(catch.n(MPstk),misAge)
  
  MPidx =OEMCpue( OM., start=range(OM,"minyear"),end=start)
  ## bug
  MPidx@index=sweep(MPidx@index,2,biasU[,ac(range(OM,"minyear"):start)],"*")
  
  ## Loop round years
  for (iYr in start:(range(OM,"maxyear")-2)){
  #iYr = (start:(range(OM,"maxyear")-2))[1]
     cat("===================", iYr, "===================\n")
     #### OEM, i.e. sample from OM with error
     OM.  =setPlusGroup(OM,plusgroup)
     MPstk=window(MPstk,end=iYr)

     MPidx=window(MPidx,end=iYr)
     MPstk[,ac(iYr)]=OEMStock(OM.,start=iYr)
     m(MPstk)[,ac(iYr)]=sweep(m(OM.),2,biasM,"*")[,ac(iYr)]
     catch.n(MPstk)[,ac(iYr)]=misAgeing(catch.n(MPstk)[,ac(iYr)],misAge)
 
     MPidx[,ac(iYr)]=OEMCpue(transform(OM.[,ac(iYr)],catch.n=(catch.n(OM.)[,ac(iYr)]*ctcDev[,ac(iYr)])),start=iYr)
     MPidx@index[,ac(iYr)]=sweep(MPidx@index[,ac(iYr)],2,biasU[,ac(iYr)],"*")
 
     #### Stock Assessment
     MPstk=vpaMP(window(MPstk,end=iYr),harvest(OM.)[,ac(range(MPstk,"minyear"):iYr)],fratio=fratio,CV=CV,bias=biasU)
     #MPstk=MPstk+FLXSA(MPstk,FLIndices(MPidx),FLXSA.control(),diag.flag=FALSE)

     #### In 1st year calculate reference points
     #if (iYr==start)
       {
        MPbrp        =FLBRP(MPstk)
        params(MPbrp)=FLPar(a=c(apply(rec(MPstk), 6, function(x) exp(mean(log(x),na.rm=TRUE)))))
        MPbrp        =brp(MPbrp)
        }
   
     if (test)
        return(list(OM=OM,MPstk=MPstk,MPidx=MPidx,MPBrp=MPbrp))
     
     #### Calculate TAC using fwd ##############################################
     MP  <-stf(MPstk,nyears=2)

     #### Project to TAC year
     ctrl<-fwdControl(data.frame(year=1:2+iYr,quantity=c("f","f")))

     #### calc TAC
     dms     <-dimnames(ctrl@trgtArray)
     dms$iter<-1:nits
     ctrl@trgtArray<-array(NA,lapply(dms,length),dms)
     ctrl@trgtArray[1,"val", ]<-mean(fbar(MP)[,ac(iYr-(1:3)),drop=T])
     #ctrl@trgtArray[2,"val", ]<-MPbrp@refpts["msy","harvest",drop=T]*fmult

     #print(refpts(MPbrp)@.Data["f0.1",,])
     ctrl@trgtArray[2,"val", ]<-c(hcr(apply(ssb(MP[,ac(iYr)]),6,mean),MPbrp@refpts["f0.1",,],Ftar,Btrig,Fmin,Blim))
     
     SRrs<-FLQuant(c(apply(rec(MP)[,ac(range(MP,"minyear"):(iYr-1))],6,function(x) exp(mean(log(x))))),dimnames=list(year=0:2+iYr,iter=1:nits))

     MP  <-fwd(MP,ctrl=ctrl,sr=list(model="mean",params=FLPar(1)),sr.residuals=SRrs)

     TAC<-catch(MP)[,ac(iYr+2),drop=T]
     ###########################################################################
     
     #### Now you have TAC take it from OM
     ctrl    <-fwdControl(data.frame(year=iYr+2,max=c(NA,2),quantity=c("catch","f")))
     dms     <-dimnames(ctrl@trgtArray)
     dms$iter<-1:nits
     ctrl@trgtArray<-array(NA,lapply(dms,length),dms)
     ctrl@trgtArray[1,"val", ]<-TAC
     ctrl@trgtArray[2,"max", ]<-2.0

     OM <-fwd(OM,ctrl=ctrl,sr=list(model="bevholt",params=srPar),sr.residuals=srRsdl)

     #print(plot(FLStocks(lapply(FLStocks(OM=OM,MP=MP),window,end=iYr))))
     }

   invisible(list(OM=OM,MP=MP,BRP=MPbrp))}
