elasticity=function(par,sel,fn){

   elasFn=function(x,dmns,what,sel,fn) {

    pr       =exp(x)
    names(pr)=dmns
    br       =fn(FLPar(pr),sel)
      
    br       =brp(br)
    rp       =br@refpts
 
    smy=c(c(rp[what,"ssb"]),
          c(rp[what,"biomass"]),
          c(rp[what,"harvest"]),
          c(rp[what,"yield"]),
          c(rp[what,"rec"]),
          c(ssb(    br)),
          c(stock(  br)),
          c(fbar(   br)),
          c(catch(  br)),
          c(rec(    br)),
          c(ssb(    br)/rp[what,"ssb"]),
          c(stock(  br)/rp[what,"biomass"]),
          c(fbar(   br)/rp[what,"harvest"]),
          c(catch(  br)/rp[what,"yield"]),
          c(rec(    br)/rp[what,"rec"]),
          maply(c(fbar(br)), function(x,br) log(lambda(leslie(br,x))),br=br))

    return(log(smy))}

   jbn=jacobian(elasFn,log(c(par)),dmns=dimnames(par)$params,what="msy",sel=sel,fn=fn)
   
   lbl=data.frame(Year    =c(rep(1,5),rep(seq(dims(fbar(br))$year),11)),
                  Quantity=c(c("SSB","Biomass","Harvest","Yield","Recruits"),rep(rep(c("SSB","Biomass","Harvest","Yield","Recruits"),each=dims(fbar(br))$year),2),rep("r",each=dims(fbar(br))$year)),
                  Type    =c(rep("Reference Point",5),rep(c("Absolute","Relative"),each=dims(fbar(br))$year*5),rep("Pop",dims(fbar(br))$year)))
  
   res=cbind(lbl,jbn)
   res=melt(res,id.var=c("Year","Quantity","Type"))
   res$Parameter      =factor(dimnames(par)$params[res$variable])
            
   return(res[,c("Year","Quantity","Type","Parameter","value")])}

elasticity2=function(par,sel,fn){

   elasFn=function(x,dmns,sel,fn) {

    pr       =exp(x)
    names(pr)=dmns
    br       =fn(FLPar(pr),sel)
      
    br       =brp(br)
    
    smry=function(what,br){
          
        rp =br@refpts
 
        res=c(c(rp[what,"ssb"]),
              c(rp[what,"biomass"]),
              c(rp[what,"harvest"]),
              c(rp[what,"yield"]),
              c(rp[what,"rec"]),
              c(ssb(    br)),
              c(stock(  br)),
              c(fbar(   br)),
              c(catch(  br)),
              c(rec(    br)),
              c(ssb(    br)/rp[what,"ssb"]),
              c(stock(  br)/rp[what,"biomass"]),
              c(fbar(   br)/rp[what,"harvest"]),
              c(catch(  br)/rp[what,"yield"]),
              c(rec(    br)/rp[what,"rec"]),
              maply(c(fbar(br)), function(x,br) log(lambda(leslie(br,x))),br=br))
        
        log(res)}
        
      smy=mdply(dimnames(refpts(br))$refpt, smry, br=br)
          
      return(unlist(c(smy)))}

   #tmp=elasFn(log(c(par)),dmns=dimnames(par)$params,sel=sel,fn=fn)
 
   jbn=jacobian(elasFn,log(c(par)),dmns=dimnames(par)$params,sel=sel,fn=fn)
   
   lbl=data.frame(Year    =c(rep(1,5),rep(seq(dims(fbar(br))$year),11)),
                  Quantity=c(c("SSB","Biomass","Harvest","Yield","Recruits"),rep(rep(c("SSB","Biomass","Harvest","Yield","Recruits"),each=dims(fbar(br))$year),2),rep("r",each=dims(fbar(br))$year)),
                  Type    =c(rep("Reference Point",5),rep(c("Absolute","Relative"),each=dims(fbar(br))$year*5),rep("Pop",dims(fbar(br))$year)))
  
   res=cbind(lbl,jbn)
   res=melt(res,id.var=c("Year","Quantity","Type"))
   res$Parameter      =factor(dimnames(par)$params[res$variable])
            
   return(res[,c("Year","Quantity","Type","Parameter","value")])}
