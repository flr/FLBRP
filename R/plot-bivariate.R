## Bivariate CI
biOrder<-function(x,y){
    dt      = cbind(x,y)
    dt[,1]  =dt[,1]-mean(dt[,1],na.rm=TRUE)
    dt[,2]  =dt[,2]-mean(dt[,2],na.rm=TRUE)
 
    t.      <-var(dt,na.rm=TRUE)
    cholesky<-chol(t.)
    dt      <-as.matrix(dt)%*%ginv(cholesky)
    dist    <-dt[,1]^2+dt[,2]^2

    return(rank(dist))}


# covar=array(c(1,0.5,0.25,1),c(2,2))
# # calculate the lower trinagular decompostion
# cholesky<-t(chol(covar))
# 
# cholesky
# 
# # generate a pair of random variates
# t.=as.array(cholesky%*%matrix(rnorm(200),c(100)))
# 
# #check that these come from the original distribution
# var(t.)
# herCovar
# 
# #plot
# plot(   params(nsher)[c("s","v"),])
# plot(mc.params[,"a"]~mc.params[,"b"])
# ################################################################################

