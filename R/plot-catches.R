
#### Function to plot time series of catches ###################################
plotCatches <- function(df,smooth=FALSE) {
  stack.df <- df[,-1]
  stack.df <- stack.df[,sort(colnames(stack.df))]
  stack.df <- apply(stack.df, 1, cumsum)
  stack.df <- apply(stack.df, 1, function(x) sapply(x, cumsum))
  stack.df <- t(apply(stack.df, 1, function(x) x - mean(x)))

 if(!smooth){
    #usethisforactualdata
    coords.df<-data.frame(x =rep(c(df[,1],rev(df[,1])),times=dim(stack.df)[2]),
                          y =c(apply(stack.df,1,min),as.numeric(apply(stack.df,2,
                                      function(x) c(rev(x),x)))[1:(length(df[,1])*length(colnames(stack.df))*2-length(df[,1]))]),
                          id=rep(colnames(stack.df),each=2*length(df[,1])))
    qplot(x=x,y=y,data=coords.df,geom="polygon",fill=id) #,color=I("white")
 }else{
    #usethisforsmootheddata
    density.df<-apply(stack.df,2,function(x)spline(x=df[,1],y=x))
    id.df<-sort(rep(colnames(stack.df),each=as.numeric(lapply(density.df, function(x)length(x$x)))))
    density.df<-do.call("rbind",lapply(density.df,as.data.frame))
    density.df<-data.frame(density.df,id=id.df)
    smooth.df<-data.frame(x =unlist(tapply(density.df$x,density.df$id,function(x)c(x,rev(x)))),
                          y =c(apply(unstack(density.df[,2:3]),1,min),unlist(tapply(density.df$y,density.df$id,
                                       function(x) c(rev(x),x)))[1:(table(density.df$id)[1]+2*max(cumsum(table(density.df$id))[-dim(stack.df)[2]]))]),
                          id=rep(names(table(density.df$id)),each=2*table(density.df$id)))

    qplot(x=x,y=y,data=smooth.df,geom="polygon",color=I("white"),fill=id)}}
