setGeneric('kobe',       function(file,method,...)    standardGeneric('kobe'))

setMethod('kobe', signature(file="FLBRPs",method="missing"),  
          function(file,proxy="msy",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),ptYrs=NULL,nwrms=10){
  if (is.null(ptYrs)) ptYrs=range(file[[1]])["maxyear"]
  
  res=llply(file, function(x,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms)
    kobe(model.frame(mcf(FLQuants(stock  =ssb.obs( x)%/%x@refpts[proxy,"ssb"],
                                  harvest=fbar.obs(x)%/%x@refpts[proxy,"harvest"])),drop=T),
            what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms),
            what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms)
  
  res=list(trks=ldply(res, function(x) x$trks),
           pts =ldply(res, function(x) x$pts),
           smry=ldply(res, function(x) x$smry),
           wrms=ldply(res, function(x) x$wrms),
           sims=ldply(res, function(x) x$sims))
  
  if (length(what)==1)
    return(res[[what]])
  else
    return(res[what]) })

setMethod('kobe',  signature(file="FLBRP",method="missing"),  
          function(file,proxy="msy",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),ptYrs=NULL,nwrms=10){
            if (is.null(ptYrs)) ptYrs=range(file)["maxyear"]
            
            dat=model.frame(mcf(FLQuants(stock  =ssb.obs( file)%/%refpts(file)[proxy,"ssb"],
                                         harvest=fbar.obs(file)%/%refpts(file)[proxy,"harvest"])),drop=T)
            
            res=kobeFn(dat,what=what,prob=prob,ptYrs=ptYrs,nwrms=nwrms)
            if (length(what)==1)
              return(res[[what]])
            else
              return(res[what])})

# 
# #### Quadrants
# ##### Kobe I ############################################################################################3
# ### provides the back drop on which to overlay data
# kobeFn=function(object,xlim,ylim){    
#     quads<- rbind(data.frame(x=c(-Inf,-Inf,Inf,Inf), y=c(-Inf,Inf,Inf,-Inf), fill=as.factor("yellow")),
#                      data.frame(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf), fill=as.factor("green")),
#                      data.frame(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1), fill=as.factor("red")))
# 
#        p=ggplot(object)+geom_polygon(data=quads,aes(x,y,fill=fill)) +
#                         scale_fill_manual(values = c("yellow","green","red"), legend=FALSE) +
#                         ylab(expression(F/F[MSY]))        +
#                         xlab(expression(SSB/B[MSY]))      +
#                         scale_y_continuous(limits=ylim)   +
#                         scale_x_continuous(limits=xlim)
#     
#       invisible(p)}
#     
 setGeneric('kobe', function(object, ...)
     standardGeneric('kobe'))
# setMethod('kobe', signature(object='missing'),
#   function(object,xlim=c(0,2),ylim=xlim){
#     
#       quads<- rbind(data.frame(x=c(-Inf,-Inf,Inf,Inf), y=c(-Inf,Inf,Inf,-Inf), fill=as.factor("yellow")),
#                      data.frame(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf), fill=as.factor("green")),
#                      data.frame(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1), fill=as.factor("red")))
# 
#        p=object+geom_polygon(data=quads,aes(x,y,fill=fill)) 
#                          scale_fill_manual(values = c("yellow","green","red"), legend=FALSE) +
#                          ylab(expression(F/F[MSY]))        +
#                          xlab(expression(SSB/B[MSY]))      +
#                          scale_y_continuous(limits=ylim)   +
#                          scale_x_continuous(limits=xlim)
#      
#       invisible(p)})
#  
# setGeneric('kobe', function(object, ...)
#     standardGeneric('kobe'))
# setMethod('kobe', signature(object='missing'),
#   function(object,xlim=c(0,2),ylim=xlim){
#     
#        invisible(kobeFn(NULL,xlim,ylim))})
# 
# setMethod('kobe', signature(object='data.frame'),
#   function(object,xlim=c(0,2),ylim=xlim){
#     
#        invisible(kobeFn(object,xlim,ylim))})
# 
# setMethod('kobe', signature(object='FLBRP'),
#   function(object,xlim=c(0,2),ylim=xlim){
#     
#        object=model.frame(FLQuants(ssb    =sweep( ssb.obs(object),6,refpts(object)["msy","ssb"],    "/"),
#                                    harvest=sweep(fbar.obs(object),6,refpts(object)["msy","harvest"],"/")))
# 
#        invisible(kobeFn(object,xlim,ylim))})
# 
# setMethod('kobe', signature(object='FLlst'),
#   function(object,xlim=c(0,2),ylim=xlim){
#        
#        ldply(object, function(object)
#                               model.frame(FLQuants(ssb    =sweep( ssb.obs(object),6,refpts(object)["msy","ssb"],    "/"),
#                                                    harvest=sweep(fbar.obs(object),6,refpts(object)["msy","harvest"],"/"))))
# 
#        invisible(kobeFn(object,xlim,ylim))})
# 
# setGeneric('kobeP', function(x,y,...)
#     standardGeneric('kobeP'))
# 
# setMethod('kobeP', signature(x="FLQuant",y="FLQuant"),
#  function(x,y) {
#             
#             b =  pmax(pmin(as.integer(biomass),1),0)
#             f =1-pmax(pmin(as.integer(harvest),1),0)
#             p =f*b
# 
#             data.frame(f=f,b=b,p=p,collapsed=(1-b)*(1-f))})
# 
# setMethod('kobeP', signature(x="FLStock",y="FLBRP"),
#  function(x,y,rp="msy"){
#    res=model.frame(SSB    =ssb(    x),refpts(y)[rp,"ssb"],
#                    harvest=harvest(x),refpts(y)[rp,"harvest"],drop=TRUE)
#    
#    res=cbind(res,kobeP(res[,"SSB"],res[,"harvest"]))
#    
#    return(res)})
# 
# 
# ### Kobe Matrix ##################################################################################################
# #x=matrix(runif(20)*100,c(4,5))
# setGeneric('kobeShade', function(object,...) standardGeneric('kobeShade'))
# setMethod('kobeShade', signature(object='numeric'),
#           function(object,breaks=c(-0.1,50,60,70,80,90,100),
#                      shades=c("\\{","\\grey50{","\\gey60{","\\grey70{","\\grey80{","\\grey90{"),
#                      percent=100,...){
#     
#   #Kobe II strategy matrices to be prepared by the SCRS should highlight in a similar format as
#   #shown in Annex Table 2 a progression of probabilities over 50 % and in the range of 50-59 %, 60-
#   #69 %, 70-79 %, 80-89 % and ≥ 90 %.
#   
#   object=object*100          
#   res=cut(object,breaks)
#   gry=data.frame(level=attributes(unique(res))$levels,shades)
#   res=merge(data.frame(object=as.integer(object),level=res),gry,all.x=TRUE)
#   
#   res=with(res,paste(shades,object,"}",sep=""))
#   
#   return(res)})
# 
# setMethod('kobeShade', signature(object='data.frame'),
#           function(object,breaks =c(-0.1,50,60,70,80,90,100),
#                      shades =c("\\{","\\grey50{","\\gey60{","\\grey70{","\\grey80{","\\grey90{"),
#                      percent=100,...){
# 
#      apply(object,2,kobeShade,breaks=breaks,shades=shades,percent=percent)})
# setMethod('kobeShade', signature(object='matrix'),
#           function(object,breaks =c(-0.1,50,60,70,80,90,100),
#                      shades =c("\\{","\\grey50{","\\gey60{","\\grey70{","\\grey80{","\\grey90{"),
#                      percent=100,...){
# 
#      apply(object,2,kobeShade,breaks=breaks,shades=shades,percent=percent)})
#           
# # k2sm<-function(x, image  =list(levels=seq(0.0,1.0,0.05),
# #                                 col    =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8))),
# #                    contour=list(levels=c(.6,.7,1.0,.9),
# #                                 col   =c("black")),
# #                    nIterp=501,xlab="Year",ylab="TAC"){
# # 
# #             x=subset(x, !is.na(x[,1]) & !is.na(x[,2]) & !is.na(x[,3]))
# # 
# #             ##### plot Kobe matrix
# #             t.<-akima::interp(x[,1],x[,2],x[,3],
# #                        xo=seq(min(x[,1]),   max(x[,1]), length=nIterp),
# #                        yo=seq(min(x[,2]),   max(x[,2]), length=nIterp),
# #                        duplicate="mean")
# # 
# #             if (!is.null(image)){      
# # 	      ## Check ##################################################
# #               if (!(length(image$levels)-1 == length(image$col))) stop("image options differ")
# #               image(t., breaks=image$levels,col=image$col,
# # 			            xlab  =ifelse(is.null(xlab),names(x)[1],xlab),
# # 			            ylab  =ifelse(is.null(ylab),names(x)[2],ylab))}
# # 	
# #             contour(t.,levels=contour$levels,
# #                        col   =contour$col,lwd=2,
# #                        method="edge",
# #                        labcex=2,
# #                        add   =!is.null(image$col),
# #   		            xlab  =ifelse(is.null(xlab),names(x)[1],xlab),
# # 			            ylab  =ifelse(is.null(ylab),names(x)[2],ylab))
# #        
# #             grid()
# #        
# #             invisible(t(tapply(x[,3],x[,c(1,2)],mean)))}
# 
# ##### plot Kobe lines
# kobeL <- function(x,image=list(levels=seq(0.0,1.0,0.05),
#                             col   =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8))),
#                offSet  =1.5,
#                cex.lgnd=0.75){
#     iTAC =unique(x[,2])
#     nTAC =length(iTAC)
# 
#     x[,1]<-as.numeric(as.character(x[,1]))
#     cols =colorRampPalette(c("#2C2C2C" ,"#EEEEEE"))(nTAC)
# 
#     plot(x[x[,2]==iTAC[1],1],x[x[,2]==iTAC[1],3],
#        type="n", ylab="Probability",xlab="Year",
#        ylim=c(0,1), axes=F, #xlim=c(min(x[,1])-max(as.integer(diff(range(x[,1]))*0.1),offSet),max(x[,1])))
#                             xlim=c(min(x[,1]),max(x[,1])++max(as.integer(diff(range(x[,1]))*0.1),offSet)))
#      
#     axis(side=1, at=seq(min(x[,1]),max(x[,1]),2))
#     axis(side=2 ) ; box()
#     if (!is.null(image)){      
#       xRng<-rep(range(x[,1]),each=2)
#       for (i in 1:length(image$col))
# 	polygon(xRng,c(image$levels[i],image$levels[i+1],image$levels[i+1],image$levels[i]),col=image$col[i],border=NA)}
# 
#     icol=0
#     for( i in unique(x[,2])) {
#        icol=icol+1
#        lines(x[x[,2]==i,1], x[x[,2]==i,3],lwd=2,col=cols[icol],lty=1)}
# 
#     grid()
#     legend("topright", as.character(iTAC), cex=cex.lgnd, lty=rep(1,nTAC), lwd=rep(2,16)    ,
#     col= cols , bty="n")
#     
#     invisible(tapply(x[,3],x[,2:1],mean))}
# 
# kobeS<-function(x,cex=1.2,image  =list(levels=seq(0.0,1.0,0.05),
#                                        col   =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8))),
#                           contour=list(levels=c(.6,.7,1.0,.9),
#                                        col   =c("black"))){
#     ops<-par(mfrow=c(2,2), mex=.5,mai=c( 0.5, 0.75 ,0.5, 0.1),cex=par()$cex)
# 
#          kobeM(x[,c(1:2,4)],image=image,contour=contour);mtext(expression(plain(P) (SSB>=SSB[MSY])),                       line=0.5, cex=cex)
#          kobeM(x[,c(1:2,5)],image=image,contour=contour);mtext(expression(plain(P) (F<=F[MSY])),                           line=0.5, cex=cex)
#     res<-kobeM(x[,c(1:2,3)],image=image,contour=contour);mtext(expression(plain(P) (F<=F[MSY]) %*% plain(P)(SSB>=B[MSY])), line=0.5, cex=cex)
#          kobeL(x[,c(1:2,3)],image=image);mtext(expression(plain(P) (F<=F[MSY]) %*% plain(P)(SSB>=SSB[MSY])),               line=0.5, cex=cex, side=3)
#     
#     #kobeM(xCont[,c("Year","P","TAC")]);mtext(expression(plain(P) (F<=F[MSY]) %*% plain(P)(SSB>=B[MSY])), line=.5, cex=.8)
#     
#     par(mfrow=ops$mfrow,mex=ops$mex,mai=ops$mai,cex=ops$cex)
# 
#     invisible(res)}
