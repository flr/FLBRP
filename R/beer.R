plotBeer=function(v,text=FALSE){
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

    if (text)
    if(v<0)
  		text(xmid,ymid,labels=c(paste("-",abs(v))),cex=.5+abs(v)*2)
  	else
  		text(xmid,ymid,labels=c(paste("+",abs(v))),cex=.5+abs(v)*2)

    }

beer <- function(v, col.glass=c("#C6D5D8","#6D939E"),text=FALSE) {
		usr=par("usr")
		ymin=usr[3]; ymax=usr[4]; yrng=ymax-ymin
		ymin1=usr[3]+.1*yrng
		ymax1=usr[4]-.2*yrng
		yrng1=ymax1-ymin1
		ymax2=ymin1+abs(v)*yrng1
		xmid=(usr[2]+usr[1])/2
		ymid=(ymax1+ymin1)/2
		xrng=(usr[2]-usr[1])
		Lbot=xmid-.15*xrng; Rbot=xmid+.15*xrng; Ltop=xmid-.25*xrng; Rtop=xmid+.25*xrng             # sides of glass
		Lbeer=xmid-(.15+abs(v)*.1)*xrng;  Rbeer=xmid+(.15+abs(v)*.1)*xrng                          # top of beer

		curved=function(x,yval,scale=0.2) {
			xmin=min(x,na.rm=TRUE); xmax=max(x,na.rm=TRUE)
			s=(2/pi)*asin(sqrt((x-xmin)/(xmax-xmin))) # scale between 0 and 1
			smid=mean(s,na.rm=TRUE)
			ycur=scale/cos(s-smid); ycur=yval+ycur-max(ycur,na.rm=TRUE); return(ycur) }

		xbot=seq(Lbot,Rbot,len=100); ybot=curved(xbot,ymin1) # bottom of glass
		xtop=seq(Ltop,Rtop,len=100); ytop=curved(xtop,ymax1) # top of glass
		xglass=c(xbot,rev(xtop)); yglass=c(ybot,rev(ytop))   # curved glass poly

		#xbeer=c(Lbot,Lbeer,Rbeer,Rbot,Lbot); ybeer=c(ymin1,ymax2,ymax2,ymin1,ymin1) # rectangular beer poly
		xsip=seq(Lbeer,Rbeer,len=100); 
		if (v==0) ysip=ybot else ysip=curved(xsip,ymax2)  # top of beer
		xbeer=c(xbot,rev(xsip)); ybeer=c(ybot,rev(ysip))        # curved beer poly
		bubblex=runif(round(500*abs(v),0),xmid-(.15+abs(v*.95)*.1)*xrng,xmid+(.15+abs(v*.95)*.1)*xrng)
		if (v==0) yfroth=ybot else yfroth=curved(bubblex,ymax2)
		#bubbley=runif(round(500*abs(v),0),ymax2-.02*yrng1,ymax2+.02*yrng1)
		bubbley=runif(round(500*abs(v),0),yfroth-.02*yrng1,yfroth+.02*yrng1)

		polygon(xglass,yglass,,col="aliceblue",border=FALSE)              # glass surface
		lines(xtop,ymax1+abs(ytop-ymax1),lwd=2,col=col.glass[1])          # top back rim
		if (v!=0) {
			polygon(xbeer,ybeer,col=ifelse(v<0,"orange","gold"),border="burlywood")                # beer
			points(bubblex,bubbley,pch=21,col=ifelse(v<0,"orange","gold"),bg="white",cex=seq(0.1,1,length=10)) }
		lines(xbot,ybot,lwd=3,col=col.glass[1])                           # bottom edge
		lines(xbot,ybot-.005,lwd=1,col=col.glass[2])                      # bottom edge shadow
		lines(c(Lbot,Ltop),c(ymin1,ymax1),lwd=4,col=col.glass[1])         # left edge
		lines(c(Lbot-.01,Ltop-.01),c(ymin1,ymax1),lwd=1,col=col.glass[2]) # left edge shadow
		lines(c(Rbot,Rtop),c(ymin1,ymax1),lwd=4,col=col.glass[1])         # right edge
		lines(c(Rbot+.01,Rtop+.01),c(ymin1,ymax1),lwd=1,col=col.glass[2]) # right edge shadow
		lines(xtop,ytop,lwd=3,col=col.glass[1])                           # top front rim
    if (text)
 		text(xmid,ymid,labels=c(paste(ifelse(v<0,"-","+"),abs(v))),cex=abs(v*100)^.1)
	}