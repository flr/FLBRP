# brp.R - DESC
# tests/brp.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}

data(ple4)

a<-brp(FLBRP(ple4))
b<-brp(FLBRP(ple4))

all.equal(a, b)
