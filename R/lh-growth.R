################################################################################
# 2) FLQuant/FLCohort to ages ##################################################
# creates FLQuant with ages in cell
setGeneric('ages', function(data, ...)
   standardGeneric('ages'))
setMethod("ages", signature(data="FLQuant"),
   function(data,timing=NULL){
      res<-FLQuant(dimnames(data)$age,dimnames=dimnames(data))

      if (is.null(timing))
         res<-sweep(res,4,(1:dim(res)[4]-1)/dim(res)[4],"+") else
         res<-sweep(res,4,timing,"+")

      return(res)})
setMethod("ages", signature(data="FLCohort"),
   function(data,timing=NULL){
      res<-FLCohort(dimnames(data)$age,dimnames=dimnames(data))

      if (is.null(timing))
         res<-sweep(res,4,(1:dim(res)[4]-1)/dim(res)[4],"+") else
         res<-sweep(res,4,timing,"+")

      return(res)})
################################################################################

# 3) length to weight ##########################################################
## converts wt to len using condition factor
setGeneric('len2wt', function(params,data,...)
  standardGeneric('len2wt'))
  
setMethod("len2wt", signature(params="FLPar", data="FLQuant"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(params="FLPar", data="FLCohort"),
   function(params,data) params["a"]*data^params["b"])
setMethod("len2wt", signature(params="FLPar", data="numeric"),
   function(params,data) params["a"]*data^params["b"])
################################################################################

# 4) Weight to length ##########################################################
## converts len to wr using condition factor
setGeneric('wt2len', function(params,data, ...)
  standardGeneric('wt2len'))

setMethod("wt2len", signature(params="FLPar", data="FLQuant"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(params="FLPar", data="FLCohort"),
   function(params,data) (data/params["a"])^(1/params["b"]))
setMethod("wt2len", signature(params="FLPar", data="numeric"),
   function(params,data) (data/params["a"])^(1/params["b"]))
################################################################################

vonB=function(params,data){
  res=params["linf"]%*%(1.0-exp(-params["k"]%*%(data-params["t0"])))
  dimnames(res)=dimnames(data)
  res}

invVonB=function(params,data)
    -log(1.0-(data/params["linf"]))/params["k"]+params["t0"]

gompertz=function(params, data) 
   params["linf"]*exp(-params["a"]*params["b"]^data)

dnormal <- function(params,data){
   pow <-function(a,b) a^b
   func<- function(data,a1,sl,sr){
      if (data < a1)
        return(pow(2.0,-((data-a1)/sl*(data-a1)/sl)))
      else
	      return(pow(2.0,-((data-a1)/sr*(data-a1)/sr)))}

    sapply(data,func,params["a1"],params["sl"],params["sr"])}


dnormal <- function(params,data){
    
    a1=FLQuant(1,dimnames=dimnames(data))%*%params["a1"]
    s =FLQuant(1,dimnames=dimnames(data))%*%params["sl"]
    sr=FLQuant(1,dimnames=dimnames(data))%*%params["sr"]
    
    if (dims(data)$iter==1 &  dims(a1)$iter>1)
      data=propagate(data,dims(a1)$iter)
    
    s[data>=a1]=sr[data>=a1]
    
    res=pow(2.0,-((data-a1)/s*(data-a1)/s))
    }

dnormalCapped <- function(params,data){ #x,a,sL,sR,amax=1.0) {
  func<-function(x,a,sL,sR,amax) {
    if (x < a)
       return(amax*pow(2.0,-((x-a)/sL*(x-a)/sL)))
    else
       return(amax*pow(2.0,-((x-a)/sR*(x-a)/sR)))
  }
  
  sapply(data,func,params["a"],params["sl"],params["sr"],params["amax"])}

dnormalPlateau <- function(x, a1, a2, amax, sL, sR) {
  if (x<=a1) 
     return(amax*2^-((x-a1)/sL)^2)
  else if (a1<x & x<=(a1+a2))
     return(amax*2^-((x-a1)/sL)^2)
  else
     return(amax*2^-((x-(a1+a2))/sR)^2)}

dnormalColeraine <- function(x, a, b, c) {
  if (x<a)
      return(exp(-(x-a)^2/b^2))
  else
      return((-(x-a)^2/c^2))}

logistic <- function(params,data) { #x, a50, ato95){
  func <- function(x,a50,ato95,asym){
    if ((a50-x)/ato95 > 5)
      return(0)
    if ((a50-x)/ato95 < -5)
      return(asym)
    return(asym/(1.0+pow(19.0,(a50-x)/ato95)))}
  
  sapply(data,func,params["a50"],params["ato95"],params["asym"])} 
    
pow<-function(a,b) a^b
logisticFn<-function(params,data) { #x,a50,ato95,asym=1.0){  
 
  res<-params["asym"]%/%(1.0+pow(19.0,(params["a50"]%-%data)%/%params["ato95"]))
  asym=FLQuant(1,dimnames=dimnames(data))%*%params["asym"]
  res[(params["a50"]%-%data)%/%params["ato95"] >  5]<-0
  res[(params["a50"]%-%data)%/%params["ato95"] < -5]<-asym[(params["a50"]%-%data)%/%params["ato95"] < -5]
  
  dmns=dimnames(res)
  names(dmns)[1]="age"
  dimnames(res)=dmns
  
  return(res)}

logisticDouble <- function(params,data) { #x, a50, ato95, b50, bto95, amax=1.0){
  
  func <- function(x,a50,ato95,b50,bto95,amax){
    if (ato95 < 0.02 && bto95 < 0.02)
    {
      if (a50 <= x && x <= (a50+b50)) return(amax) else return(0)
    } else if (ato95 < 0.02) {
      p = a50
      funcMax = 1+pow(19.0,(p-(a50+b50))/bto95)
      return(amax * funcMax * (1+pow(19.0,(x-(a50+b50))/bto95)))
    } else if (bto95 < 0.02) {
      p = a50+b50
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * (1+pow(19.0,(a50-x)/ato95)))
    } else {
      p = (a50 * bto95 + ato95 * (a50 + b50)) / (ato95 + bto95)
      funcMax = 1+pow(19.0,(a50-p)/ato95)
      return(amax * funcMax * min(1/(1+pow(19.0,(a50-x)/ato95)),
        1/(1+pow(19.0,(x-(a50+b50))/bto95))))
    }}

  sapply(x,func,a50,ato95,b50,bto95,amax)} 

dnormalFn<-function(params,data) { #x,a50,ato95,asym=1.0){  
  data=propagate(data, dims(params)$iter)
 
  right=qmin(qmax(data-params["a1"],0),1)
  rFlag=right> 0
  lFlag=right<=0
 
  res<-data*0
 
  if (length(res[rFlag])>0){
    a1=-(data-params["a1"])
    a2=1/params["sr"]
    res[rFlag]<-pow(2,-pow(a1%*%a2,2))[rFlag]
    }
  
  if (length(res[lFlag])>0){
    a1=-(data-params["a1"])
    a2=1/params["sl"]
    res[lFlag]<-pow(2,-pow(a1%*%a2,2))[lFlag]
    }
  
  return(FLQuant(res,dimnames=dimnames(data)))}

logisticProduct <- function(params,data) { #x,a50,ato95,b50,bto95,amax=1.0){
  func <- function(x,a50,ato95,b50,bto95,amax){
    if (ato95 < 0.02 && bto95 < 0.02)
    {
      if (a50 <= x && x <= (a50+b50))
        return(amax)
      else
        return(0)
    } else if (ato95 < 0.02) {
        funcMax = 1+pow(19.0,(-b50)/bto95)
        return(amax * funcMax * (1/(1+pow(19.0,(x-(a50+b50))/bto95))))
    } else if (bto95 < 0.02) {
        funcMax = 1+pow(19.0,(-b50)/ato95)
        return(amax * funcMax * (1/(1+pow(19.0,(a50-x)/ato95))))
    } else {
        funcMax = 0
        for (i in 0:100) {
          tempvar = a50 - ato95 + i * (b50 + bto95 + ato95) / 100
          funcMax = max(funcMax, (1+pow(19.0,(a50-tempvar)/ato95))*
            (1+pow(19.0,(tempvar-(a50+b50))/bto95)))
        }
        return(amax * funcMax * (1/((1+pow(19.0,(a50-x)/ato95))
          * (1+pow(19.0,(x-(a50+b50))/bto95)))))
     }
  }    
  sapply(x,func,a50,ato95,b50,bto95,amax)}

richards <- function(params,data) { #x, a50, ato95, sigma) {
  beta <- ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha <- a50+beta*log(2^sigma-1)/log(19)

  return((1/(1+19^(alpha-x)/beta))^1/sigma)} 

richardsCapped <- function(params,data) { #x, a50, ato95, sigma, amax) {
  beta <- ato95*log(19)/(log(2^sigma-1)-log((20/19)^sigma-1))
  alpha <- a50+beta*log(2^sigma-1)/log(19)

  return((amax/(1+19^(alpha-x)/beta))^1/sigma)}

schnute<-function(params,data){
  fn1<-function(params,data) (params["y1"]^params["b"]+(params["y2"]^params["b"]-params["y1"]^params["b"])*(1.0-exp(-params["a"]*(data-params["t1"])))/(1.0-exp(-params["a"]*(params["t2"]-params["t1"]))))^(-1/params["b"])
  fn2<-function(params,data)  params["y1"]*exp(log(params["y2"]/params["y1"])*(1.0-exp(-params["a"]*(data-params["t1"])))/(1.0-exp(-params["a"]*(params["t2"]-params["t1"]))))
  fn3<-function(params,data) (params["y1"]^params["b"]+(params["y2"]^params["b"]-params["y1"]^params["b"])*(data-params["t1"])/(params["t2"]-params["t1"]))^(-1/params["b"])
  fn4<-function(params,data)  params["y1"]*exp(log(params["y2"]/params["y1"])*(data-params["t1"])/(params["t2"]-params["t1"]))

  if (params["a"]!=0 & params["b"]!=0) return(fn1(params,data))
  if (params["a"]!=0 & params["b"]==0) return(fn2(params,data))
  if (params["a"]==0 & params["b"]!=0) return(fn3(params,data))
  if (params["a"]==0 & params["b"]==0) return(fn4(params,data))}

seldnc <- function(params,data){ #x,a,b,c) {
  d <- rep(b,length(x))
  d[x>a] <- c
  return(exp(-(x-a)^2/d^2))}


##########################################################################################################
##########################################################################################################
glst=list("dnormal"           =dnormal,
          "dnormalCapped"     =dnormalCapped,
          "dnormalPlateau"    =dnormalPlateau,
          "dnormalColeraine"  =dnormalColeraine,
          "vonB"              =vonB,
          "logistic"          =logisticFn,
          "logisticProduct"   =logisticProduct,
          "logisticDouble"    =logisticDouble,
          "gompertz"          =gompertz,
          "richards"          =richards,
          "richardsCapped"    =richardsCapped,
          "schnute"           =schnute,
          "seldnc"            =seldnc,
          "invVonB"           =invVonB)

#rm(list=names(glst))

setGeneric('gFn', function(model,params,data, ...)
   standardGeneric('gFn'))
setMethod("gFn", signature(model="character",params="FLPar",data="ANY"),
   function(model,params,data="missing",...) {
     if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
          data=ages(data)
         
      mlst[[model]](params,data,...)})

setGeneric('vonB', function(params,data, ...)
   standardGeneric('vonB'))
setMethod("vonB", signature(params="FLPar",data="ANY"),
   function(params,data="missing",...) {
     if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
          data=ages(data)
         
      glst[["vonB"]](params,data,...)})

setGeneric('invVonB', function(params,data, ...)
   standardGeneric('invVonB'))
setMethod("invVonB", signature(params="FLPar",data="ANY"),
   function(params,data="missing",...) {
     if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
          data=ages(data)
         
      glst[["invVonB"]](params,data,...)})

setGeneric('logistic', function(params,data, ...)
   standardGeneric('logistic'))
setMethod("logistic", signature(params="FLPar",data="ANY"),
   function(params,data="missing",...) {
     if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
          data=ages(data)
         
      glst[["logistic"]](params,data,...)})

setGeneric('dnormal', function(params,data, ...)
   standardGeneric('dnormal'))
setMethod("dnormal", signature(params="FLPar",data="ANY"),
   function(params,data="missing",...) {
     if (!missing(data) & "FLQuant" %in% is(data) ||  "FLCohort" %in% is(data))
          data=ages(data)
         
      glst[["dnormal"]](params,data,...)})

## 14) Density dependence ######################################################
# Where a parameter is a function of a covariate                               # 
################################################################################
dd<-function(par,dd,covar,ff="linear",multiplicative=TRUE){
    logistic<-function(x,min,max){
	y=1/(1+exp(-x))
        return((max-min)*y+min)}


    delta<-switch(ff,
          linear   =dd["a"]+dd["b"]*covariate,
          loglinear=exp(dd["a"]+dd["b"]*log(covariate)),
          logistic =logistic(covariate,dd["a"]+dd["b"]),
          )

    if (multiplicative) par=par*(1+delta)
    else                par=par+delta

    return(delta)}


#x<-seq(-10,10,length.out=100)
#plot(bnd(x,10,50)~x,type="l")

SS3SelParam<-function(param){
    #p1 – PEAK: ascending inflection size (in cm)
    #p2 – TOP: width of plateau, as logistic between PEAK and MAXLEN
    #p3 – ASC-WIDTH: parameter value is ln(width)
    #p4 – DESC-WIDTH: parameter value is ln(width)
    #p5 – INIT: selectivity at first bin, as logistic between 0 and 1.
    #P6 – FINAL: selectivity at last bin, as logistic between 0 and 1.


    #Lmin is the midpoint of the smallest length bin,
    #Lmax is the midpoint of the largest length bin, and
    beta   <-numeric(5)

    #β1 is the size at which selectivity=1.0 begins,
    beta[1]<-p1

    #β2 is the size at which selectivity=1.0 ends,
    beta[2]<-p2-p1

    #β3 determines the slope of the ascending section,
    beta[3]<-p3

    #β4 determines the slope of the descending section,
    beta[4]<-p4

    #β5 is the selectivity at Lmin,
    beta[5]<-p5

    #β6 is the selectivity at Lmin,
    beta[6]<-p6

    return(beta)}

knife=function(param, data){
   res=data
   
   res[data> c(param["break"])][]=param["M1"]
   res[data<=c(param["break"])][]=param["M2"]

   return(res)}




