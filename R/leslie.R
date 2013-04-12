setGeneric('leslie', function(object, ...)
  	standardGeneric('leslie'))

setMethod("leslie", signature(object="FLBRP"),
  function(object,fbar=FLQuant(0),numbers=TRUE,...){

  args=list(...)  
  for (slt in names(args)[names(args) %in% names(getSlots("FLBRP"))])
     slot(object, slt)=args[[slt]]
  fbar(object)=fbar  
 
  ages=dims(object)$min:dims(object)$max
  mx  =matrix(0,nrow=length(ages),ncol=length(ages),dimnames=list(age=ages,age=ages))
  
  #survivorship
  diag(mx[-1,-length(ages)])   =exp(-m(object)[-length(ages)])
  if (range(object)["plusgroup"]==range(object)["max"])
    mx[length(ages),length(ages)]=exp(-m(object)[length(ages)])
  
  #recruitment
  tmp   = mat(object)*stock.wt(object)*stock.n(object)[,1]
  tmp2  = apply(tmp,2:6,sum)
  mx[1,]= rec(object)[1]%*%tmp%/%tmp2%/%stock.n(object)[,1]
  
  if (!numbers){
       diag(mx[-1,-length(ages)])=c(stock.wt(object)[-1,1])/c(stock.wt(object)[-length(ages),1])
     mx[1,]=c(stock.wt(object)[,1])*mx[1,]
     }
  
  return(mx)})


