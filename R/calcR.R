calcR=function(object){
    ## calculates slope at origin of Yield/Stock curve
    gradYS=function(x,rp) {
    
              dimnames(refpts(rp))$refpt[5]="fcrash"
              refpts(rp)@.Data[5,,1]       =c(NA,NA,NA,NA,x,NA,NA,NA)
              grad                         =c(computeRefpts(rp)@.Data["fcrash","yield",1])
              
              return(grad)}
     
    res=grad(gradYS, 0, rp=object)
    
    return(res)}