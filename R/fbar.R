# fbar.R - 
# FLBRP/R/fbar.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# fbar {{{
setMethod("fbar<-", signature(object="FLBRP", value="numeric"),
	function(object, value) {

		object@fbar <- FLQuant(value, quant="age")

		return(object)
  }
)

setMethod("fbar<-", signature(object="FLBRP", value="FLQuant"),
	function(object, value) {

		object@fbar<-value

		return(object)
  }
) # }}}
