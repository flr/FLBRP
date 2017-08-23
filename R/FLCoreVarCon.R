# List of vectors of slots to sort out what slots can vary and which are constant.
# This is for checking / forcing some slots to have 1 or multiple iterations
# Only the Var slots can have multiple iterations

flqVar <- function(){
    res <- list()
    res[["FLStock"]] <- c("stock", "stock.n", "catch", "catch.n", "discards", "landings", "discards.n", "landings.n", "harvest")
    res[["FLIndex"]] <- c("index", "catch.n")
    res[["FLBiol"]]  <-c("n")
    res[["FLFleet"]] <-c("effort")
    res[["FLMetier"]]<- c("effshare", "vcost")
    res[["FLCatch"]] <- c("landings", "landings.n", "discards", "discards.n")
    
    return(res)}

flqCon <- function(){
  
    res<-list()
    res[["FLStock"]] <-c("catch.wt", "discards.wt", "landings.wt", "stock.wt", "m", "mat", "harvest.spwn", "m.spwn")
    res[["FLIndex"]] <-c("index.var", "catch.wt", "effort", "sel.pattern", "index.q")
    res[["FLBiol"]]  <-c("m", "wt", "fec", "spwn")
    res[["FLFleet"]] <-c("fcost", "capacity", "crewshare")
    res[["FLMetier"]]<-NULL
    res[["FLCatch"]] <-c("discards.wt", "landings.wt", "landings.sel", "discards.sel", "catch.q", "price")
    
    return(res)}

CheckNor1<-function(x,Var=flqVar(),Con=flqCon(),nDim="missing"){
   if (!validObject(x)) stop("Object not valid")

   cls  <-class(x)
   nmFlq<-names(Var)
   
   if (!(cls %in% nmFlq)) return(x)
   
   if (is(x,"FLlst"))
      for (i in 1:length(x))
         x[[i]]<-CheckNor1(x[[i]])
   else { 
      if (missing(nDim))
         nDim=dims(x)$iter
      for (i in Var[[class(x)]])
         if (dims(slot(x,i))$iter==1) slot(x,i)<-propagate(slot(x,i),iter=nDim)}
   
   return(x)}

# Used for burrowing into a Fleet list and checking the dims
chkFLlst<-function(x){
  snms    <-names(getSlots(class(x)))
  anyFLlst<-unlist(lapply(snms, function(arg) is(slot(x,arg),"FLlst")))
  if (any(anyFLlst))
    chkFLlst(slot(x,snms[anyFLlst]))
  }
