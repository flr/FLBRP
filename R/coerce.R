# coerce.R - DESC
# /coerce.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# To FLSR {{{
setAs('FLBRP', 'FLSR',
  function(from) {

	  rec.age  <- dims(from)$min
	  recYrCls <-as.numeric(dimnames(rec.obs(from))$year)-rec.age
    ssbYrCls <-as.numeric(dimnames(ssb.obs(from))$year)

    ssbYrCls<-ssbYrCls[ssbYrCls %in% recYrCls]
    recYrCls<-ssbYrCls+rec.age

    # calculate ssb and create FLSR from rec.age
    rec <- rec.obs(from)[,ac(recYrCls)]
    ssb <- ssb.obs(from)[,ac(ssbYrCls)]

   # create the FLSR from
   res=FLSR(name=from@name,
            desc=from@desc,
	          rec     =rec,
            ssb     =ssb,
            params  =params(from),
            model   =model(from))
    
    res@params=params(from)

    residuals(res)=log(rec(res)/predict(res))
    units(residuals(res))="NA"
    
    return(res)
  }
) # }}}

# To predictModel {{{
setAs('FLBRP', 'predictModel',
  function(from) {
    return(predictModel(model=model(from), params=params(from)))
  }
) # }}}

# To FLStock {{{
setAs('FLBRP', 'FLStock',
  function(from){
    
    # EXTRACT fbar-sized FLQuants
    full <- metrics(from, list(landings.n=landings.n, discards.n=discards.n,
      catch.n=catch.n, stock.n=stock.n, harvest=harvest))

    # RESIZE short FLQuants
    shor <- lapply(metrics(from, list(m=m, mat=mat, landings.wt=landings.wt,
      discards.wt=discards.wt, catch.wt=catch.wt, stock.wt=stock.wt,
      m.spwn=m.spwn, harvest.spwn=harvest.spwn)), expand,
        year=dimnames(fbar(from))$year)

    # BUG COPY values: should be done by expand?
    shor <- lapply(shor, function(x) {
      x[,-1] <- x[,1]
      return(x)
    })

    # CREATE FLStock
    res <- do.call("FLStock", c(full, shor, list(name=name(from), desc=desc(from))))

    # COMPUTE totals
    landings(res) <- computeLandings(res)
    discards(res) <- computeDiscards(res)
    catch(res) <- computeCatch(res)

    # CORRECT range fbar
    range(res, c("minfbar", "maxfbar")) <- range(from, c("minfbar", "maxfbar"))
 
    return(res)
  }
) # }}}

# To FLBRP {{{

# FLBiol

# }}}
