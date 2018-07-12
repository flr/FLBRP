# fwdWindow.R - DESC
# FLBRP/R/fwdWindow.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# fwdwindow (FLStock, FLBRP) {{{

#' @title fwdWindow
#'
#' @description Extends an object representing a fish population for projecting into the
#' future using the assumed equilibirum values used in the calculation of
#' reference points 
#'
#' @param x The population object, for example of class \code{\link{FLStock}}
#' @param y The reference points object, of class \code{\link{FLBRP}}
#' @param end Final year of the extended object, always interpreted as a *character*
#' @return An object of the same class a *x*, extended to *year=end*
#' @details
#'     slts <- c("stock.wt", "landings.wt", "discards.wt", "catch.wt",
#'        "m", "mat", "harvest.spwn", "m.spwn")
#' @rdname fwdWindow
#' @docType methods
#' @md
#' @examples
#' data(ple4)
#'
#' # Create the FLSR and FLBRP objects
#' psr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
#' prp <- brp(FLBRP(ple4, sr=psr))
#'
#' res <- fwdWindow(ple4, prp, end=2014)

setMethod("fwdWindow", signature(x="FLStock", y="FLBRP"),
  function(x, y, end=dims(x)$maxyear) {

    # EXTEND x with window
    res <- window(x, end=end, extend=TRUE, frequency=1)

    # NEW window years
    wyrs <- seq(dim(m(x))[2] + 1, dim(m(res))[2])
      
    # CHECKS
    its <- unlist(lapply(qapply(x, dim), '[', 6))

    # COMPLETE *.wt, landings.n, discards.n, mat, m, harvest, *.spwn
    slts <- c("stock.wt", "landings.wt", "discards.wt", "catch.wt",
       "m", "mat", "harvest.spwn", "m.spwn")

    for(s in slts) {
      slot(res, s)[,wyrs] <- iter(do.call(s, list(y)), seq(its[s]))
    }

    # COMPLETE landings.n, discards.n, harvest, any year > 1 will do
    landings.n(res)[,wyrs] <- iter(landings.n(y)[,3], seq(its["landings.n"]))
    discards.n(res)[,wyrs] <- iter(discards.n(y)[,3], seq(its["discards.n"]))
    harvest(res)[,wyrs] <- iter(harvest(y)[,3], seq(its["discards.n"]))
    
    return(res)
  }
) # }}}

# TODO fwdwindow (FLBiol, FLBRP) {{{

# }}}
