# setplusgroup - «Short one line description»
# FLBRP/R/setplusgroup.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 22 Apr 2010 11:29
# $Id: setplusgroup.R 670 2010-04-22 09:29:54Z imosqueira $

# setPlusGroup  {{{
setMethod('setPlusGroup', signature(x='FLBRP'),
  function(x, plusgroup)
    {
      # check
      if(!plusgroup > dims(x)$max)
        stop("plusgroup in FLBRP can only be used to extend the object: plusgroup < max")
      
      # ages
      oldage <- dims(x)$max
      ages   <- seq(dims(x)$min, plusgroup)
      newages<- seq(dims(x)$max, plusgroup)

      slts<-c("landings.sel", "discards.sel",
        "bycatch.harvest", "stock.wt",
        "landings.wt", "discards.wt",
        "bycatch.wt", "m", "mat",
        "harvest.spwn", "m.spwn",
        "availability", "price")
            
      for (i in slts) {
        slot(x, i)           <-expand(slot(x, i), age=ages)
        slot(x,i)[ac(newages),] <-slot(x,i)[ac(oldage),]
       }

    range(x, 'max') <- plusgroup
    range(x, 'plusgroup') <- plusgroup

    return(x)
  }
) # }}}
