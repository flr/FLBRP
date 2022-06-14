# accessors.R - FLBRP class accessors
# FLBRP/R/accessors.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

# createFLAccesors    {{{

#' Create accesor methods for a given class
#' 
#' This function creates a complete set of standard S4 class accessors and
#' replacers. Not intended for direct use.
#'
#' @param class name of the class
#' @param exclude Slot names to exclude
#' @param include Slot names to include
#' @author The FLR Team
#' @keywords methods
#'
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
    substitute(if(!is.null(getGeneric(x)) && names(formals(x))[[1]] != "object") {
      warning(paste("Accesor method for", x, "conflicts with a differently defined 
      generic. Type", x, "for more information")); break}, list(x=x))
    )
    # accessor
    eval(
    substitute(setMethod(x, signature(object=y),
      function(object) return(slot(object, x))),
      list(x=x, y=class))
    )
    # replacer
    eval(
    substitute(setReplaceMethod(x, signature(object=y, value=v),
      function(object, value)
      {slot(object, s) <- value; if(validObject(object)) object else
        stop("Object not valid")}),
      list(x=x, y=class, s=x, v=unname(slots[x])))
    )
    if(any(unname(slots[x]) %in% c('FLArray', 'FLQuant', 'FLCohort')))
    eval(
    substitute(setReplaceMethod(x, signature(object=y, value="numeric"),
      function(object, value)
      {slot(object, s)[] <- value; object}), list(x=x, y=object, s=x))
    )
    xr <- paste(x, "<-", sep="")
    defined[[x]] <- c(x, xr, paste('alias{',x,',', class,'-method}', sep=''),
      paste('\alias{',xr,',', class,',',unname(slots[x]), '-method}', sep=''),
      paste('\alias{',x,'-methods}', sep=''),
      paste('\alias{"',xr, '"-methods}', sep='')
    )
  }
  return(defined)
}  # }}}

invisible(createFLAccesors("FLBRP", exclude=c("range", "name", "desc", "refpts")))
