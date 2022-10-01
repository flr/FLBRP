# generics.R - New generic methods
# FLBRP/R/generics.R

# constructors {{{

#" @rdname FLBRP
#" @aliases FLBRP FLBRP-methods
setGeneric("FLBRP", function(object, sr, ...)
		standardGeneric("FLBRP"))
# }}}

# accessors {{{

setGeneric("availability", function(object, ...)
		standardGeneric("availability"))
setGeneric("availability<-", function(object, ..., value)
		standardGeneric("availability<-"))

setGeneric("bycatch.harvest", function(object, ...)
		standardGeneric("bycatch.harvest"))
setGeneric("bycatch.harvest<-", function(object, ..., value)
		standardGeneric("bycatch.harvest<-"))

setGeneric("bycatch.wt", function(object, ...)
		standardGeneric("bycatch.wt"))
setGeneric("bycatch.wt<-", function(object, ..., value)
		standardGeneric("bycatch.wt<-"))

setGeneric("bycatch.wt", function(object, ...)
		standardGeneric("bycatch.wt"))
setGeneric("bycatch.wt<-", function(object, ..., value)
		standardGeneric("bycatch.wt<-"))

setGeneric("discards.obs", function(object, ...)
    standardGeneric("discards.obs"))
setGeneric("discards.obs<-", function(object, ..., value)
    standardGeneric("discards.obs<-"))

setGeneric("fbar<-", function(object, ..., value)
		standardGeneric("fbar<-"))

setGeneric("fbar.obs", function(object, ...)
		standardGeneric("fbar.obs"))
setGeneric("fbar.obs<-", function(object, ..., value)
		standardGeneric("fbar.obs<-"))

setGeneric("landings.obs", function(object, ...)
		standardGeneric("landings.obs"))
setGeneric("landings.obs<-", function(object, ..., value)
		standardGeneric("landings.obs<-"))

setGeneric("profit.obs", function(object, ...)
		standardGeneric("profit.obs"))
setGeneric("profit.obs<-", function(object, ..., value)
		standardGeneric("profit.obs<-"))

setGeneric("rec.obs<-", function(object, ..., value)
		standardGeneric("rec.obs<-"))

setGeneric("ssb.obs", function(object, ...)
		standardGeneric("ssb.obs"))
setGeneric("ssb.obs<-", function(object, ..., value)
		standardGeneric("ssb.obs<-"))

setGeneric("stock.obs", function(object, ...)
		standardGeneric("stock.obs"))
setGeneric("stock.obs<-", function(object, ..., value)
		standardGeneric("stock.obs<-"))
# }}}

# methods {{{

setGeneric("biomass.obs", function(object, ...)
		standardGeneric("biomass.obs"))

setGeneric("brp", function(object, ...)
		standardGeneric("brp"))

setGeneric("catch.obs", function(object, ...)
		standardGeneric("catch.obs"))

setGeneric("computeRefpts", function(object, ...)
		standardGeneric("computeRefpts"))

setGeneric("hcrYield", function(object, fbar, ...)
		standardGeneric("hcrYield"))

setGeneric("msyRange", function(object, ...)
		standardGeneric("msyRange"))

setGeneric("spr", function(object, ...)
		standardGeneric("spr"))

setGeneric("yield.obs", function(object, ...)
		standardGeneric("yield.obs"))

setGeneric("ypr", function(object, ...)
		standardGeneric("ypr"))
# }}}

# refpts {{{
setGeneric("fcrash", function(x, ...)
		standardGeneric("fcrash"))
# }}}
