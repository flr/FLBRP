# utilities.R - DESC
# /utilities.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@seaplusplus.co.uk>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# window    {{{
setMethod("window", signature(x="FLBRP"),
	function(x, start=dims(x@ssb.obs)$minyear, end=dims(x@ssb.obs)$maxyear,
    extend=TRUE, frequency=1) {
    # To window: *.obs
    obs <- c("fbar.obs", "landings.obs", "discards.obs", "rec.obs", "ssb.obs",
      "stock.obs", "profit.obs")

    for(s in obs) {
      slot(x, s) <- window(slot(x, s), start=start, end=end, extend=extend,
        frequency=frequency)
    }
  	
		return(x)
	}
)	# }}}

# propagate {{{
setMethod('propagate', signature(object='FLBRP'),
  function(object, iter, fill.iter=TRUE, obs=FALSE, params=FALSE){
    # obs FLQuants
    if(obs)
    {
      obs <- c('fbar.obs', 'landings.obs', 'discards.obs', 'rec.obs', 'profit.obs')
      for(i in obs)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)
    }

    # other FLQuants
    quants <- c("fbar", "landings.sel", "discards.sel", "bycatch.harvest", "stock.wt",
      "landings.wt", "discards.wt", "bycatch.wt", "m", "mat", "harvest.spwn", "m.spwn", 
      "availability", "price", "vcost", "fcost")
    for(i in quants)
        slot(object, i) <- propagate(slot(object, i), iter=iter, fill.iter=fill.iter)

    # refpts
    refpts(object) <- propagate(refpts(object), iter=iter, fill.iter=fill.iter)

    # params
    if(params)
      params(object) <- propagate(params(object), iter=iter, fill.iter=fill.iter)
  
    return(object)}) # }}}

# iter {{{
setMethod('iter', signature(obj='FLBRP'),
  function(obj, iter, ...){
    obj <- callNextMethod(obj, iter, ...)
    params(obj) <- iter(params(obj), iter)
    if(dim(refpts(obj))[3] > 1)
      refpts(obj) <- iter(refpts(obj), iter)

    return(obj)}) # }}}

# summary {{{
setMethod("summary", signature("FLBRP"),
  function(object) {
  
    callNextMethod()
    cat("\n")

    # model
    cat("Model: \t")
    print(model(object), showEnv=FALSE)
    
    # params
    print(params(object), reduced=TRUE)

    # refpts flag
    cat("\n")
    flag <- !all(is.na(refpts(object)))
    cat("refpts: ", ifelse(flag, "calculated", "NA"), "\n")
  }
) # }}}
