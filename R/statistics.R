# statistics.R - DESC
# /home/mosqu003/FLR/pkgs/mine/FLBRP/R/statistics.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# abiF {{{

#' abiF
#' Computes ABI for target F, e.g. ABImsy (Griffith et al. 2023)
#'
#' @param stock object of class FLStock 
#' @param target target F at equilibrium, e.g. Fmsy
#' @param threshold quantile ageref treshold, default 0.9
#' @param brp An optional input FLBRP object
#' @return FLQuant 
#' @export
#' @examples
#' data(ple4)
#' abimsy <- abiF(ple4, target=0.22, threshold=0.9)
#' plot(abimsy) + ylim(0, 2) +
#'  geom_hline(yintercept = 1) + ylab(expression(ABI[MSY]))

abiF <- function(stock, target=0.2, threshold=0.9, brp=NULL) {

  # COMPUTE FLBRP
  if(is.null(brp)) {
    equ <- FLBRP(stock)
  } else if(is(brp, 'FLBRP')) {
    equ <- brp
    if(missing(target)) {
      target <- fmsy(brp)
    }
  } else {
    stop("brp must be of class 'FLBRP'")
  }

  # SET single F target
  fbar(equ) <- FLQuant(target, dim=c(rep(1,5), dim(stock)[6]))

  # COMPUTE for F target
  equ <- brp(equ)

  # CONVERT to FLStock
  eqs <- as(equ, "FLStock")

  # REMOVE first age
  na <- stock.n(eqs)[-1, ]

  ages <- seq(dims(na)$min, dims(na)$max)

  cums <- apply(na, 2:6, cumsum)

  nthresh <- quantSums(na * threshold)

  maxage <- dims(stock)$max
 
  aref <- pmin(c(apply((nthresh %-% cums) ^ 2, 2:6,
    function(x) which(x == min(x)))),
    range(equ)["plusgroup"] - 1)

  # PROPORTION
  rp <- unlist(Map(function(x,y) sum(x[ac(seq(y + 1, max(ages)))]) / sum(x),
    x=split(na), y=aref))
  
  # CALCULATE
  res <- Reduce(combine, Map(function(x, y, z)
    quantSums(x[ac(seq(y + 1, maxage)), ]) / quantSums(x[-1, ]) / z,
    x=split(stock.n(stock)), y=aref, z=rp))

  return(res)
}

# }}}
