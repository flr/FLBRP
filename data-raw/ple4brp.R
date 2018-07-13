# ple4brp.R - DESC
# /ple4brp.R

# Copyright FLR Team, 2017
# Authors: Laurie Kell <laurie@seaplusplus.co.uk>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#          Finlay Scott (EC JRC) <finlay.scott@ec.europa.eu>
#
# Distributed under the terms of the GNU Public License v 3.0

library(FLBRP)

data(ple4)

# RESCALE SR

ple4sr <- fmle(transform(as.FLSR(ple4, model=ricker), ssb=ssb/100, rec=rec/100))
params(ple4sr)['b',] <- params(ple4sr)['b',] / 100
ple4sr <- transform(ple4sr, ssb=ssb*100, rec=rec*100)

# NO RESCALING
# ple4sr <- fmle(as.FLSR(ple4, model="bevholt"))

ple4brp <- FLBRP::FLBRP(ple4, sr=ple4sr, fbar=FLQuant(seq(0, 3, length=101)))

ple4brp <- FLBRP::brp(ple4brp)

# ECONOMIC data

price(ple4brp) <- c(rep(1.15,3),rep(1.3,2),rep(1.55,5)) * 1000
units(price(ple4brp)) <- "euro / t"

vcost(ple4brp) <- 30000 * 1000
units(vcost(ple4brp)) <- "euro / effort"

fcost(ple4brp) <- 35000 * 1000
units(fcost(ple4brp)) <- "euro / effort"

ple4brp <- brp(ple4brp)

save(ple4brp, file="../data/ple4brp.RData", compress="xz")
