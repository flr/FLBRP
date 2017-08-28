
## expands & fills
setGeneric("recycle6d<-", function(object,value){
  standardGeneric("recycle6d<-")})
setMethod("recycle6d<-", signature(object="FLQuant", value="FLQuant"),
	function(object, value) {
   if (any(dim(value)[-6]>dim(object)[-6]))
      stop("dims in 2nd arg can't be greater than those in 1st")
   if (dims(value)$iter>1 & dims(object)$iter==1)
      object=propagate(object,dims(value)$iter)
    
   ## dims to expand in value
   nDim<-(1:6)[dim(value)!=pmax(dim(object),dim(value))]

   if (!all(dim(value)[nDim]==1 | dim(value)[nDim]==dim(object)[nDim]))
      stop("dims in 2nd arg can't be greater than 1 and != those in arg 1")
       
   return(sweep(FLQuant(0,dimnames=dimnames(object)), (1:6)[!(1:6 %in% nDim)], value, "+"))})     

setGeneric("fwdWindow", function(x,y,...){
  standardGeneric("fwdWindow")})

setMethod('fwdWindow', signature(x='FLStock',y="FLBRP"),
  function(x,y,start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1,...){
      object =qapply(x, FLCore::window, start=start, end=end, extend=extend, frequency=frequency)

        object@range["minyear"] <- start
     	object@range["maxyear"] <- end
      
      yr1 =dimnames(m(object))$year
      yr2 =dimnames(m(x))$year
      yrs =yr1[!(yr1 %in% yr2)]
 
      object=CheckNor1(object)
    
      stock.n(object)[,yrs]=NA  
      catch.n(object)[,yrs]=NA  
      
      args<-data.frame(e1=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.n",  "discards.n",   "m","mat","harvest"  ,"harvest.spwn","m.spwn"),    
                       e2=c("stock.wt","landings.wt","discards.wt","catch.wt","landings.sel","discards.sel", "m","mat","catch.sel","harvest.spwn","m.spwn"))
                       
       t. <-FLQuants(mlply(args,function(e1,e2,stk,flb) {#cat(ac(e1),ac(e2),"\n"); 
                                                         recycle6d(stk[[ac(e1)]][[1]])<-flb[[ac(e2)]][[1]]; return(stk[[ac(e1)]][[1]])},stk=object[,yrs],flb=y))
       names(t.)<-args[,1]

       args=cbind(args[,1:2],ldply(ac(args[,1]),function(x,a,b)  data.frame("O"=dims(a[[x]][[1]])$iter,"I"=dims(b[[x]])$iter),a=object,b=t.))
      
       t..=FLQuants(mlply(args, function(e1,e2,O,I,x) if (I>O) propagate(x[[ac(e1)]][[1]],I) else x[[ac(e1)]][[1]],x=object))
       names(t..)<-args[,1]
       object[[ac(args[,1])]]=t..

       for (i in names(t.))
          slot(object,i)[,yrs]=t.[[i]]

     ## replace any slot passed in as an arg option
     args<-list(...)
     for (slt in names(args)[names(args) %in% names(getSlots(class(object)))]) 
        slot(object, slt)[,ac(years)]<-fn(args[[slt]],slot(object, slt)[,ac(years)])
 
     return(object)})

setMethod('fwdWindow', signature(x='FLStock',y="missing"),
  function(x,y,start=dims(x)$minyear, end=dims(x)$maxyear,...){
      stfCtrl=list(nyears=3, wts.nyears=3, fbar.nyears=NA,f.rescale=FALSE, arith.mean=TRUE, na.rm=TRUE)
      args=list(...)
      if (!("stf" %in% names(args))) stop("Only stf for now")
      
      object=CheckNor1(x)
      
      stfCtrl[names(args[["stf"]])[names(args[["stf"]]) %in% names(stfCtrl)]]=args[["stf"]][names(args[["stf"]]) %in% names(stfCtrl)]     
      if (is.na(stfCtrl$fbar.nyears)) 
         stfCtrl$fbar.nyears<-stfCtrl$wts.nyears

      nyears=stfCtrl$nyears     
      stfCtrl$nyears=max(nyears,c(end-x@range["maxyear"]))
      
      if (nyears!=stfCtrl$nyears) warning("end and stf$nyears mismatch, max used")
      
      object@range["minyear"] <- start
      object@range["maxyear"] <- end
      
 
      object<-do.call("stf",c(list(object=object),stfCtrl))

      ## replace any slot passed in as an arg option
      args<-list(...)
      for (slt in names(args)[names(args) %in% names(getSlots(class(object)))]) 
        slot(object, slt)[,ac(years)]<-fn(args[[slt]],slot(object, slt)[,ac(years)])
  
    return(object)})

setMethod('window', signature(x='FLStock'),
    function(x,start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1,...){
  
      args=list(...)
      if ("FLBRP" %in% names(args)) {
        return(fwdWindow(x,args[["FLBRP"]],  start=start,end=end,extend=extend,frequency=frequency)) 
      }else if ("stf"   %in% names(args)) {
        return(fwdWindow(x,start=start,end=end,extend=extend,frequency=frequency,stf=args[["stf"]]))  
        }
       
      x =qapply(x, FLCore::window, start,end,extend,frequency)
      x@range["minyear"] = start
	  	x@range["maxyear"] = end
      
      return(x)})

if (FALSE){
  library(FLBRP)
  
  tmp=FLStock(m=FLQuant(0.1,dimnames=list(age=1:5,year=2001:2020)))
  units(harvest(tmp))="f"
  tmp2=fwdWindow(tmp,FLBRP(tmp),end=2030)
  tmp3=fwdWindow(tmp,stf=list(nyears=3),end=2020)
  tmp2=window(tmp,FLBRP=FLBRP(tmp),end=2030)
  tmp3=window(tmp,stf=list(nyears=3),end=2030)
 }




    
    
