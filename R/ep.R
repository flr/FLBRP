setGeneric('ep', function(object, ...) standardGeneric('ep'))
setMethod("ep", signature(object="FLStock"),
  function(object, ...) {

  if(units(harvest(object)) == 'f'){
		res <- (stock.n(object) * exp(-harvest(object) * harvest.spwn(object) -
      m(object) * m.spwn(object)) * stock.wt(object) * mat(object))
		
		return(res)
	} else if(units(harvest(object)) == 'hr')
  {
		res <- (stock.n(object) * (1 - harvest(object) * harvest.spwn(object) *
      exp(-m(object) * m.spwn(object)) * harvest.spwn(object) * mat(object) * stock.wt(object)))
		
		return(res)
  } else
		stop("Correct units (f or hr) not specified in the harvest slot")})


setMethod("ep", signature(object="FLBRP"),
  function(object, ...) {

  if(units(harvest(object)) == 'f'){
  	res <- (stock.n(object) * exp(-(harvest(object) %*% harvest.spwn(object)) %-% (m(object) * m.spwn(object) * stock.wt(object) * mat(object))))
		
		return(res)
	} else if(units(harvest(object)) == 'hr')
  {
		res <- (stock.n(object) * (1 - harvest(object) * harvest.spwn(object) *
      exp(-m(object) * m.spwn(object)) * harvest.spwn(object) * mat(object) * stock.wt(object)))
		
		return(res)
  } else
		stop("Correct units (f or hr) not specified in the harvest slot")})
