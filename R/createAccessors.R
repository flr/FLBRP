# createAccessors - «Short one line description»
# FLBRP/R/createAccessors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: Fri Jul 01, 2011 at 02:38 PM +0200
# $Id: createAccessors.R 888 2011-01-17 00:56:11Z lauriekell $

# createFLAccessors {{{
createFLAccesors <- function(class, exclude=character(1), include=missing) {
  
  object <- class

  if(!missing(include))
  	slots <- getSlots(class)[include]
  else
  	slots <- getSlots(class)[!names(getSlots(class))%in%exclude]

	defined <- list()

	for (x in names(slots)) {
		# check method is defined already and signatures match
		eval(
		substitute(if(isGeneric(x) && names(formals(x)) != "object") {warning(paste("Accesor
			method for", x, "conflicts with a differently defined generic. Type", x,
			"for more information")); break}, list(x=x))
			)
		# create new generic and accesor method
		eval(
		substitute(if(!isGeneric(x)) setGeneric(x, function(object, ...) standardGeneric(x)),
		list(x=x))
		)
		eval(
		substitute(setMethod(x, signature(y), function(object) return(slot(object, x))),
      list(x=x, y=class))
		)
		# create replacement method
		xr <- paste(x, "<-", sep="")
		eval(
		substitute(if(!isGeneric(x)) setGeneric(x,
			function(object, ..., value) standardGeneric(x)), list(x=xr))
		)
		eval(
		substitute(setMethod(x, signature(object=y, value=v), function(object, value)
			{slot(object, s) <- value; if(validObject(object)) object else stop("")}),
      list(x=xr, y=class, s=x, v=unname(slots[x])))
		)
    if(any(unname(slots[x]) %in% c('FLArray', 'FLQuant', 'FLCohort', 'refpts', 'FLPar')))
    eval(
		substitute(setMethod(x, signature(object=y, value="numeric"), function(object, value)
			{slot(object, s)[] <- value; object}), list(x=xr, y=object, s=x))
		)
		defined[[x]] <- c(x, xr, paste('alias{',x,',', class,'-method}', sep=''),
			paste('\alias{',xr,',', class,',',unname(slots[x]), '-method}', sep=''),
			paste('\alias{',x,'-methods}', sep=''),
			paste('\alias{"',xr, '"-methods}', sep='')
		)
	}
	return(defined)} # }}}

invisible(createFLAccesors("FLBRP", exclude=c("range","name","desc")))
