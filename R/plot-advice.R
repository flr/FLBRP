# advicePlots.R - 
# FLadvice/R/advicePlots.R
# Advice plots Brydes Stylie

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# TODO
#   1) add biomass.obs & revenue.obs
#   2) sr<- for passing sr(brp) to other objects 

# plot(FLBRP) {{{
setMethod("plot", signature(x="FLBRP", y="missing"),
 function(x, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
   facet=facet_wrap(~qname,scale="free"), scale="msy",
   fn=list("SSB"=function(x) ssb.obs(  x)/refpts(x)[scale,"ssb"],
				"yield"  =yield.obs(x)/refpts(x)[scale,  "yield"],
				"rec"    =rec.obs(  x)/refpts(x)[scale,    "rec"],
				"harvest"=fbar.obs( x)/refpts(x)[scale,"harvest"]),...) {

      plotComp(x, fn, probs, size, lty, facet)
 }
) # }}}

# [plpot(FLBRP, FLStocks) {{{
setMethod("plot", signature(x="FLStocks", y="missing"),
  function(x, probs=c(0.95,0.50,0.05), size=c(0.5,1.0,0.5), lty=c(2,1,2),
    facet=facet_wrap(~qname,scale="free"),
    fn=list("SSB"=function(x) ssb.obs(X)/refpts(x)[scale,"ssb"],
      "yield"=yield.obs(x)/refpts(x)[scale, "yield"],
			"rec"=rec.obs(x)/refpts(x)[scale, "rec"],
			"harvest"=fbar.obs( x)/refpts(x)[scale,"harvest"]),...) {

    plotComps(x,fn,probs,size,lty,facet)
  }
) # }}}
