setMethod('sv', signature(x='FLPar', model='character'),
  function(x, model, spr0=NA){
 
   a=x["a"]
   b=x["b"]
   s=FLPar(a=1,dimnames=dimnames(a))  
   v=FLPar(b=1,dimnames=dimnames(a))  
   spr0=FLPar(spr0,dimnames=dimnames(a))  

   if ("spr0" %in% dimnames(x)$params)
     spr0=x["spr0"] 

   c=FLPar(c=1,dimnames=dimnames(a))  
   d=FLPar(d=1,dimnames=dimnames(a))  
   if (("c" %in% dimnames(x)$params))  c=x["c"]
   if (("d" %in% dimnames(x)$params))  d=x["d"]

   v <- v*spr2v(model, spr0, a, b, c, d)
   s <- s*srr2s(model, ssb=v*.2, a=a, b=b, c=c, d=d) / srr2s(model, ssb=v, a=a, b=b, c=c, d=d)
  
   res=rbind(s, v, spr0)
 
   if ("c" %in% dimnames(x)$params)
     res=rbind(res, c)
 
   if ("d" %in% dimnames(x)$params)
     res=rbind(res, d)
 
   res=rbind(res, spr0)
 
   return(res)})

abPars. <- function(x,spr0=NA,model){
  s=x["s"]
  v=x["v"]
  if ("c" %in% names(x))
     c=x["c"]
  if ("d" %in% names(x))
     d=x["d"]
  if ("spr0" %in% names(x))
     spr0=x["spr0"]
  # converts a & b parameterisation into steepness & virgin biomass (s & v)
  switch(model,
    "bevholt"   ={a=(v%+%(v%-%s%*%v)%/%(5%*%s%-%1))%/%spr0; b=(v%-%s%*%v)%/%(5%*%s%-%1)},
    "bevholtSV" ={a=(v+(v-s*v)/(5*s-1))/spr0; b=(v-s*v)/(5*s-1)},
    "ricker"    ={b=log(5*s)/(v*0.8); a=exp(v*b)/spr0},
    "rickerSV"  ={b=log(5*s)/(v*0.8); a=exp(v*b)/spr0},
    "cushing"   ={b=log(s)/log(0.2); a=(v^(1-b))/(spr0)},
    "cushingSV" ={b=log(s)/log(0.2); a=(v^(1-b))/(spr0)},
    "shepherd"  ={b=v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c)); a=((v/b)^c+1)/spr0},
    "shepherdSV"={b=v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c)); a=((v/b)^c+1)/spr0},
    "mean"      ={a=v/spr0;b=NULL},
    "meanSV"    ={a=v/spr0;b=NULL},
    "segreg"    ={a=5*s/spr0; b=v/(a*spr0)},
    "segregSV"  ={a=5*s/spr0; b=v/(a*spr0)},
    {stop("model name not recognized")})

  res <- c(a=a, b=b)
  return(res[!is.null(res)])} 


# setMethod('ab', signature(x='FLPar', model='character'),
#   function(x, model, spr0=NA){
#  
#    s=x["a"]
#    v=x["b"]
#    a=FLPar(a=1,dimnames=dimnames(s))  
#    b=FLPar(b=1,dimnames=dimnames(v)) 
#    
#    if ("spr0" %in% dimnames(x)$params)
#       spr0=x["spr0"]  else 
#       spr0=FLPar(spr0,dimnames=dimnames(a)) 
# 
#    c=FLPar(c=1,dimnames=dimnames(a))  
#    d=FLPar(d=1,dimnames=dimnames(a))  
#    if (("c" %in% dimnames(x)$params))  c=x["c"]
#    if (("d" %in% dimnames(x)$params))  d=x["d"]
# 
#    v <- v*spr2v(model, spr0, a, b, c, d)
#    s <- s*srr2s(model, ssb=v*.2, a=a, b=b, c=c, d=d) / srr2s(model, ssb=v, a=a, b=b, c=c, d=d)
#   
#    res=rbind(s, v, spr0)
#  
#    if ("c" %in% dimnames(x)$params)
#      res=rbind(res, c)
#  
#    if ("d" %in% dimnames(x)$params)
#      res=rbind(res, d)
#  
#    res=rbind(res, spr0)
#  
#    return(res)})

setGeneric('rbind.', function(...)
  standardGeneric('rbind.'))

# rbind {{{
setMethod('rbind.', signature('FLPar'),
          function(..., deparse.level=1) {
            
            args <- list(...)
            
            # dims
            dimar <- lapply(args, function(x) dim(x))
            iterar <- lapply(dimar, function(x) x[length(x)])
            
            
            # idx <- unlist(lapply(args, is, 'FLPar'))
            # if(!all(idx))
            #   stop("input objects must all be of class 'FLPar'")
            
            # extend iters
            
            res <- args[[1]]@.Data
            if(length(args) > 1)
              for (i in seq(length(args))[-1])
                res <- rbind(res, args[[i]]@.Data)
            
            # dimnames
            names(dimnames(res)) <- names(dimnames(args[[1]]))
            if(any(unlist(lapply(dimnames(res), function(x) any((x==x[1])[-1])))))
              warning("Repeated dimnames in output FLPar")
            
            return(FLPar(res, units=units(args[[1]])))
          }
) # }}}

gislasim=function(par,t0=-0.1,a=0.00001,b=3,ato95=1,sl=2,sr=5000,s=0.9,v=1000){
  
  names(dimnames(par)) <- tolower(names(dimnames(par)))
  
  if (!("t0"    %in% dimnames(par)$params)) par=rbind.(par,FLPar("t0"    =t0, iter=dims(par)$iter))
  if (!("a"     %in% dimnames(par)$params)) par=rbind.(par,FLPar("a"     =a,  iter=dims(par)$iter))
  if (!("b"     %in% dimnames(par)$params)) par=rbind.(par,FLPar("b"     =b,  iter=dims(par)$iter))
  if (!("asym"  %in% dimnames(par)$params)) par=rbind.(par,FLPar("asym"  =1,  iter=dims(par)$iter))
  if (!("bg"    %in% dimnames(par)$params)) par=rbind.(par,FLPar("bg"    =b,  iter=dims(par)$iter))
  if (!("sl"    %in% dimnames(par)$params)) par=rbind.(par,FLPar("sl"    =sl, iter=dims(par)$iter))
  if (!("sr"    %in% dimnames(par)$params)) par=rbind.(par,FLPar("sr"    =sr, iter=dims(par)$iter))
  if (!("s"     %in% dimnames(par)$params)) par=rbind.(par,FLPar("s"     =s,  iter=dims(par)$iter))
  if (!("v"     %in% dimnames(par)$params)) par=rbind.(par,FLPar("v"     =v,  iter=dims(par)$iter))

  ## growth parameters
  if (!("k"     %in% dimnames(par)$params)) par=rbind.(par,FLPar("k"=3.15*par["linf"]^(-0.64), iter=dims(par)$iter)) # From Gislason et al 2008, all species combined
  
  # Natural mortality parameters from Model 2, Table 1 Gislason 2010
  par=rbind.(par,FLPar(M1=0.55+1.44*log(par["linf"])+log(par["k"]), iter=dims(par)$iter),
                FLPar(M2=-1.61                                   , iter=dims(par)$iter))

  if (!("ato95" %in% dimnames(par)$params)) par=rbind.(par,FLPar("ato95" =ato95, iter=dims(par)$iter))
  if (!("sl"    %in% dimnames(par)$params)) par=rbind.(par,FLPar("sl"    =sl,    iter=dims(par)$iter))
  if (!("sr"    %in% dimnames(par)$params)) par=rbind.(par,FLPar("sr"    =sr,    iter=dims(par)$iter))
 
  ## maturity parameters from http://www.fishbase.org/manual/FishbaseThe_MATURITY_Table.htm
  if (!("asym"    %in% dimnames(par)$params)) par=rbind.(par,FLPar("asym"    =asym, iter=dims(par)$iter))

  if (!("a50" %in% dimnames(par)$params)){
    par=rbind.(par,FLPar(a50=0.72*par["linf"]^0.93, iter=dims(par)$iter))
    par["a50"]=invVonB(par,c(par["a50"]))
    }

  ## selectivity guestimate
  a1=par["a50"]
 
  dimnames(a1)$params="a1"
 
  par=rbind.(par,a1)
  
  attributes(par)$units=c("cm","kg","1000s")
  
  return(par)}

setUnits=function(res, par){

    units=attributes(par)$units
    #browser()
    allUnits=list("params"=      "",          
               "refpts"=         "",            
               "fbar"=           "",        
               "fbar.obs"=       "",    
               "landings.obs"=   paste(units[2],units[3]),
               "discards.obs"=   paste(units[2],units[3]),
               "rec.obs"=        units[3],         
               "ssb.obs"=        paste(units[2],units[3]),
               "stock.obs"=      paste(units[2],units[3]),
               "profit.obs"=     "",     
 #              "revenue.obs"=    "",    
               "landings.sel"=   "",    
               "discards.sel"=   "", 
               "bycatch.harvest"="",        
               "stock.wt"=       units[2],     
               "landings.wt"=    units[2],     
               "discards.wt"=    units[2],      
               "bycatch.wt"=     units[2],               
               "m"=              "",             
               "mat"=            "proportion", 
               "harvest.spwn"=   "proportion",          
               "m.spwn"=         "proportion",    
               "availability"=   "proportion",           
               "price"=          "",           
               "vcost"=          "",           
               "fcost"=          "")            

    for (i in names(allUnits))
      units(slot(res,i))=allUnits[i]
    
    return(res)}


#### Life History Generator ####################################################
lh=function(par,
            growth       =vonB,
            fnM          =function(par,len) exp(par["M1"]+par["M2"]*log(len)),
#            fnM          =function(par,len,T=290,a=FLPar(c(a=-2.1104327,b=-1).7023068,c=1.5067827,d=0.9664798,e=763.5074169),iter=dims(par)$iter))
#                                    exp(a[1]+a[2]*log(len) + a[3]*log(par["linf"]) + a[4]*log(par["k"]) + a[5]/T),
            fnMat        =logistic,
            fnSel        =dnormal,
            sr           ="bevholt",
            range        =c(min=1,max=40,minfbar=1,maxfbar=40,plusgroup=40),
            spwn         = 0,
            fish = 0.5, # proportion of year when fishing happens
            units=if("units" %in% names(attributes(par))) attributes(par)$units else NULL,
            ...){

  if (!("min" %in% names(range)))
    range=c(range,min=1)
  if (!("minfbar" %in% names(range))){
    val=c(range["min"])
    names(val)="minfbar"
    range=c(range,val)}
  if (!("maxfbar" %in% names(range))){
    val=c(range["max"])
    names(val)="maxfbar"
    range=c(range,val)}
  if (!("plusgroup" %in% names(range))){
    val=c(range["plusgroup"])
    names(val)="plusgroup"
    range=c(range,val)}
  
  range=range[c("min","max","minfbar","maxfbar","plusgroup")]
  
  # Check that m.spwn and harvest.spwn are 0 - 1
  if (spwn > 1 | spwn < 0 | fish > 1 | fish < 0)
    stop("spwn and fish must be in the range 0 to 1\n")
  
  if (("m.spwn" %in% names(args)))
     m.spwn =args[["m.spwn"]]
  else
    m.spwn=FLQuant(spwn, dimnames=list(age=range["min"]:range["max"]))

  if (("harvest.spwn" %in% names(args)))
    harvest.spwn =args[["harvest.spwn"]]
  else
    harvest.spwn=FLQuant(spwn, dimnames=list(age=range["min"]:range["max"]))

  age=propagate(FLQuant(range["min"]:range["max"],dimnames=list(age=range["min"]:range["max"])),length(dimnames(par)$iter))

   # Get the lengths through different times of the year
   stocklen   <- growth(par[c("linf","t0","k")],age+m.spwn)    # stocklen is length at spawning time
   catchlen   <- growth(par[c("linf","t0","k")],age+fish) # catchlen is length when fishing happens
   midyearlen <- growth(par[c("linf","t0","k")],age+0.5) # midyear length used for natural mortality

   # Corresponding weights
   swt=par["a"]*stocklen^par["b"]
   cwt=par["a"]*catchlen^par["b"]
   if ("bg" %in% dimnames(par)$param)  
      swt=par["a"]*stocklen^par["bg"]
  
   args<-list(...)

   m.   =fnM(  par=par,len=midyearlen) # natural mortality is always based on mid year length
   mat. =fnMat(par,age + m.spwn) # maturity is biological therefore + m.spwn
   sel. =fnSel(par,age + fish) # selectivty is fishery  based therefore + fish

   ## create a FLBRP object to   calculate expected equilibrium values and ref pts
   dms=dimnames(m.)
   res=FLBRP(stock.wt       =swt,
             landings.wt    =cwt,
             discards.wt    =cwt,
             bycatch.wt     =cwt,
             m              =m.,
             mat            =FLQuant(mat., dimnames=dimnames(m.)),
             landings.sel   =FLQuant(sel., dimnames=dimnames(m.)),
             discards.sel   =FLQuant(0,    dimnames=dimnames(m.)),
             bycatch.harvest=FLQuant(0,    dimnames=dimnames(m.)),
             harvest.spwn   =FLQuant(harvest.spwn,    dimnames=dimnames(m.)),
             m.spwn         =FLQuant(m.spwn,    dimnames=dimnames(m.)),
             availability   =FLQuant(1,    dimnames=dimnames(m.)),
             range          =range
             )

   ## FApex
   #if (!("range" %in% names(args))) range(res,c("minfbar","maxfbar"))[]<-as.numeric(dimnames(landings.sel(res)[landings.sel(res)==max(landings.sel(res))][1])$age)

   ## replace any slot passed in as an arg
   for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))[names(getSlots("FLBRP"))!="fbar"]])
     slot(res, slt)<-args[[slt]]
   params(res)=propagate(params(res),dims(res)$iter)
   ## Stock recruitment relationship
   model(res) =do.call(sr,list())$model

  if (dims(par)$iter>1) {
     warning("Scarab, iters dont work for SRR:sv/ab etc")
  
     params(res)=FLPar(c(a=NA,b=NA),iter=dims(par)$iter)
       
     for (i in seq(dims(par)$iter))
       params(res)[,i][]=unlist(c(ab(par[c("s","v"),i],sr,spr0=iter(spr0(res),i))[c("a","b")]))

      warning("iter(params(res),i)=ab(par[c(s,v),i],sr,spr0=iter(spr0(res),i))[c(a,b)] assignment doesnt work")
      warning("iter(FLBRP,i) doesn't work")
  }else
    params(res)=ab(par[c("s","v")],sr,spr0=spr0(res))[c("a","b")]
 
   refpts(res)=refpts(res)[c("virgin","msy","crash","f0.1","fmax")]

   res=brp(res)
  
   if ("fbar" %in% names(args)) 
      fbar(res)<-args[["fbar"]] else 
   if (any((!is.nan(refpts(res)["crash","harvest"])))) 
      fbar(res)<-FLQuant(seq(0,1,length.out=101),quant="age")*refpts(res)["crash","harvest"]

   res=brp(res)
  
  if (!("units" %in% names(attributes(par))))  return(res)
  
  if (all(is.na(attributes(par)$units)))  return(res)
 
  #res <- setUnits(res, par)
  
  return(res)}
