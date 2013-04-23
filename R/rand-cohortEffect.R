
## cohort effects
cEff=function(stk,sigma,rho=0){
  
  cv=sigma #getS(sigma,rho)
  dmns       =dimnames(par)
  dmns$cohort=dimnames(FLCohort(m(stk)))$cohort
  parC       =FLPar(rep(c(par),length(dmns$cohort)),dimnames=dmns[c(1,3,2)])
  units(parC)=""  
  
  dev        =log(rlnorm(length(dmns$cohort),0,cv))   
  for(i in 2:(length(dev)))
       dev[i]=dev[i]+dev[i-1]*rho
  parC[var]=parC[var]*exp(dev)
  
  tmp=len2wt(parC,vonB(parC[c("linf","t0","k")],ages(FLCohort(m(stk)))))
  res=window(as(tmp,"FLQuant"),start=1,end=dims(m(stk))$year)
  
  res}
