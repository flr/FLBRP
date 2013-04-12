panelBrp<-function()  c("SSB v. F",     "Recruitment v. SSB", "Yield v. F", 
                        "Yield v. SSB", "Profit v. F",        "Profit v. SSB")

varBrp<-function() data.frame(x=rep(c("fbar","ssb"),3),y=c("ssb","rec","yield","yield","profit","profit"),pnl=panelBrp(),stringsAsFactors=FALSE)

getBrp<-function(x,y,pnl,obj) {
  
    if (x %in% names(getSlots("FLBRP"))) if (dims(obj[[x]][[1]])$iter==1 & dims(obj)$iter>1)
      obj[[x]][[1]]=propagate(obj[[x]][[1]],dims(obj)$iter)
    if (y %in% names(getSlots("FLBRP"))) if (dims(obj[[y]][[1]])$iter==1 & dims(obj)$iter>1)
      obj[[y]][[1]]=propagate(obj[[y]][[1]],dims(obj)$iter)
 
    dim(obj[[x]])
    res<-cbind(model.frame(obj[[c(x,y)]]))[,-1]
    names(res)[6:7]<-c("x","y")
    res}

getBrpHat<-function(x,y,pnl,obj) getBrp(x,paste(y,"hat",sep="."),                       paste("Equilibrium",pnl),obj)
getBrpObs<-function(x,y,pnl,obj) getBrp(  paste(x,"obs",sep="."),paste(y,"obs",sep="."),paste("Equilibrium",pnl),obj)

getBrpRefpts<-function(x,y,pnl,obj) {
    x[x=="fbar"]<-"harvest"
    res<-cbind(  as.data.frame(refpts(obj)[,x,]),
	       y=as.data.frame(refpts(obj)[,y,])[,4])
    names(res)[4:5]<-c("x","y")
    res}

setMethod("plot", signature(x="FLBRP", y="missing"),
               function(x,y,obs=FALSE,refpts=TRUE,
                        panel=c("SSB v. F",     "Recruitment v. SSB", "Yield v. F", 
                                "Yield v. SSB", "Profit v. F",        "Profit v. SSB"),...){
  hat.    =mdply(varBrp(),getBrpHat,     obj=x)
  hat.$pnl=factor(paste("Equilibrium",hat.$pnl),levels=paste("Equilibrium",panel))
  hat.=subset(hat., pnl %in% paste("Equilibrium",panel))
 
  obs.=mdply(varBrp(),getBrpObs,     obj=x)
  obs.$pnl=factor(paste("Equilibrium",obs.$pnl),levels=paste("Equilibrium",panel))
  obs.=subset(obs., pnl %in% paste("Equilibrium",panel))
 
  ref.=mdply(varBrp(),getBrpRefpts,  obj=x)

  ref.$pnl=factor(paste("Equilibrium",ref.$pnl),levels=paste("Equilibrium",panel))
  ref.=subset(ref., pnl %in% paste("Equilibrium",panel))

  chk=table(subset(ref., !is.na(y))$pnl)
  ref.=subset(ref., pnl %in% names(chk[chk>0]))
    
  p<-ggplot(subset(hat.,x>=0 & y>=0 & !is.na(x) & !is.na(y))) + 
     geom_line(aes(x,y,group=iter)) +
     facet_wrap(~pnl,ncol=min(length(unique(ref.$pnl)),2),scale="free")
  if (obs)
    p<-p+geom_point(aes(x,y,group=iter),data=subset(obs.,x>=0 & y>=0 & !is.na(x) & !is.na(y))) 

  if (refpts & !all(is.na(refpts(x))))
    p<-p+geom_point(aes(x,y,colour=refpt),data=subset(ref.,x>=0 & y>=0 & !is.na(x) & !is.na(y)))

  print(p)

  invisible(p)})

#x<-brp(FLBRP(ple4,params=FLPar(exp(mean(log(rec(ple4)))))))

#ggplot(lhBrp[[c("m","mat","stock.wt","catch.sel")]]) + 
#      geom_line(aes(age,data))+facet_wrap(~qname,scale="free_y")

setMethod("plot", signature(x="FLBRPs", y="missing"),
               function(x,y,obs=FALSE,refpts=TRUE,
                        panel=c("SSB v. F",     "Recruitment v. SSB", "Yield v. F", 
                                "Yield v. SSB", "Profit v. F",        "Profit v. SSB"),...){

     if ("split_labels" %in% names(attributes(x)))
       attributes(x)[["split_labels"]]<-NULL

     hat.=ldply(x, function(x) mdply(varBrp(),getBrpHat,     obj=x))
     hat.=transform(hat.,pnl=factor(paste("Equilibrium",pnl),levels=paste("Equilibrium",panelBrp())),.id=factor(.id))
     p   =ggplot(subset(hat.,x>=0 & y>=0 & !is.na(x) & !is.na(y)))+geom_line(aes(x,y,group=iter:.id,col=.id)) +facet_wrap(~pnl,ncol=2,scale="free")


     if (refpts){
      ref.=ldply(x, function(x) mdply(varBrp(),getBrpRefpts,   obj=x))
      ref.=transform(ref.,pnl=factor(paste("Equilibrium",pnl),levels=paste("Equilibrium",pnl)),.id=factor(.id))
      #ref.=subset(ref., pnl %in% paste("Equilibrium",panel))
      if (!all(is.na(ref.$x) & is.na(ref.$y)))
        p=p+geom_point(aes(x,y,group=iter:.id,colour=.id,shape=refpt),data=subset(ref.,(x>=0 & y>=0) & !(is.na(x) | is.na(y))))
      }

     if (obs){
      obs.=ldply(x, function(x) mdply(varBrp(),getBrpObs,     obj=x))
      obs.=transform(ref.,pnl=factor(paste("Equilibrium",panel),levels=paste("Equilibrium",panelBrp())),.id=factor(.id))
      obs.=subset(obs., pnl %in% paste("Equilibrium",panel))
      p<-p+geom_point(aes(x,y,group=iter:.id,col=.id),data=subset(obs.,x>=0 & y>=0 & !is.na(x) & !is.na(y)))} 

     print(p)

     invisible(p)})

## Bivariate CI
bvOrder<-function(dt)
    {
    mn      <-apply(dt,2,mean)
    dt[,1]  <-dt[,1]-mn[1]
    dt[,2]  <-dt[,2]-mn[2]
    t.      <-var(dt)
    cholesky<-chol(t.)
    dt      <-as.matrix(dt)%*%ginv(cholesky)
    dist    <-dt[,1]^2+dt[,2]^2

    return(order(dist))
    }

plotBeer=function(v){
  	usr=par("usr")
  	ymin=usr[3]
  	ymax=usr[4]
  	yrng=ymax-ymin
  	ymin1=usr[3]+.1*yrng
  	ymax1=usr[4]-.2*yrng

  	yrng1=ymax1-ymin1
  	ymax2=ymin1+abs(v)*yrng1
  	xmid=(usr[2]+usr[1])/2
  	ymid=(ymax1+ymin1)/2
  	xrng=(usr[2]-usr[1])
  	xpoly=c(xmid-.15*xrng,xmid-(.15+abs(v)*.1)*xrng,xmid+(.15+abs(v)*.1)*xrng,xmid+.15*xrng,xmid-.15*xrng)
  	ypoly=c(ymin1,ymax2,ymax2,ymin1,ymin1)
  	polygon(xpoly,ypoly,col="gold",border="burlywood")
  	bubblex=runif(round(500*abs(v),0),xmid-(.15+abs(v*.95)*.1)*xrng,xmid+(.15+abs(v*.95)*.1)*xrng)
  	bubbley=runif(round(500*abs(v),0),ymax2-.02*yrng1,ymax2+.02*yrng1)
  	points(bubblex,bubbley,pch=21,col = "gold", bg = "white",cex=seq(0.1,1,length=10))
  	points(c(xmid-.15*xrng,xmid+.15*xrng),c(ymin1,ymin1),type="l",lwd=4)
  	points(c(xmid-.15*xrng,xmid-.25*xrng),c(ymin1,ymax1),type="l",lwd=4)
  	points(c(xmid+.15*xrng,xmid+.25*xrng),c(ymin1,ymax1),type="l",lwd=4)
  	if(v<0){
  		text(xmid,ymid,labels=c(paste("-",abs(v))),cex=.5+abs(v)*2)
  	}else{
  		text(xmid,ymid,labels=c(paste("+",abs(v))),cex=.5+abs(v)*2)
      }
    }

    
# plot(FLStock) {{{
setMethod("plot", signature(x="FLStock", y="missing"),
  function(x, probs=c(0.75,0.50,0.25), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free"),
    fn=list("SSB"=ssb, "Recruits" = rec, "Yield"=catch, F=fbar),...) {
    
    plotComp(x,fn,probs,size,lty,facet)
  }
) # }}}


# plot(FLStocks) {{{
setMethod("plot", signature(x="FLStocks", y="missing"),
  function(x, probs=c(0.75,0.50,0.25), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free"),
    fn=list("SSB"=ssb, "Recruits"=rec, "Yield"=catch, F=fbar),...)
    plotComps(x,fn,probs,size,lty,facet)
) # }}}
