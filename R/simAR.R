setGeneric('simAR', function(object, ...)
  standardGeneric('simAR'))

setMethod('simAR', signature(object='FLSR'),
  function(object,nits,start=dims(object)$minyear,end=dims(object)$maxyear,n=10){
  simAR(residuals(object),nits,start,end,n)})

setMethod('simAR', signature(object='FLQuant'),
  function(object,nits,start=dims(object)$minyear,end=dims(object)$maxyear,n=10){
  arobject  =acf(c(object),lag.max=n)
  plot(arobject)
  
  cvobject  =sd(object)
  objectRsdl=window(object,end=end)
  objectRsdl[]=0
  objectRsdl=log(rlnorm(nits,objectRsdl,cvobject))
  
  for( i in end:(dims(objectRsdl)$minyear+n))
    objectRsdl[,ac(i)]=apply(sweep(objectRsdl[,rev(ac(i-1:n))],2,arobject$acf[n:1],"*"),6,sum)
 
  return(objectRsdl)})
  
#simAR(nao.flq[,-(1:2),,1])
  