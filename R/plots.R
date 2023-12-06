# plots.R - DESC
# FLBRP/R/plots.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# plot(FLBRP) {{{

#' @title plot method for FLBRP class
#' 
#' @description
#' The plot method for the `FLBRP` class will show the relationship between
#' equilibrium SSB, Yield and Profit against levels of fishing mortality and
#' that of recruitment, Yield and profit against SSB.
#' 
#' @param x An object of class `FLBRP`
#' @param refpts Reference points to include, defaults to all in standard `refpts` slot bu `virgin`. A vector of class `character`
#' @param obs Should observations be plotted? Defaults to `FALSE`.
#' @return An object of class [ggplot2][gg].
#' @docType methods
#' @rdname brp
#' @seealso [FLBRP]
#' @examples
#' data(ple4brp)
#' plot(ple4brp)
#' # ADD observations
#' plot(ple4brp, obs=TRUE)
#' # SELECT which refpts to plot
#' plot(ple4brp, refpts=c("msy", "crash", "f0.1"))
#' plot(ple4brp, refpts=c("msy", "crash", "f0.1"), colours=c("green", "red", "blue"))
#' plot(ple4brp, refpts=c("msy", "crash", "f0.1"),
#'   colours=c("green", "red", "blue"), shapes=c(21, 22, 23))
#' # method will only plot existing referenced points
#' refpts(ple4brp) <- refpts(ple4brp)[c("msy", "fmax", "spr.30"),]
#' plot(ple4brp)
#' # Select which panels to plot
#' plot(ple4brp, panels=1:4)
#' # and in which format
#' plot(ple4brp, panels=1:3, ncol=1)

setMethod("plot", signature("FLBRP", "missing"),
  function(x, refpts=dimnames(x@refpts)$refpt, obs=FALSE, labels=TRUE,
    shapes="missing", colours="missing", panels=NULL, ncol=2, ...) {

    # EXTRACT metrics
    df <- model.frame(metrics(x,
      list(ssb=ssb, harvest=fbar, rec=rec, yield=landings, profit=profit)),
      drop=FALSE)
    
    # refpts
    drps <- dimnames(refpts(x))$refpt
    rps <- refpts(x)[drps %in% refpts,]

    # estimated?
    rpf <- !all(is.na(rps))

    # SUBSET df IF rpf
    if(rpf && "crash" %in% dimnames(rps)$refpt && !is.na(c(rps['crash', 'harvest'])))
      df <- df[df$harvest <= c(rps['crash', 'harvest']),]

    # NO economics
    plots <- list(
      P1=c(x="harvest", y="ssb", panel="Equilibrium SSB v. F", pos=1),
      P2=c(x="ssb", y="rec", panel="Equilibrium Recruitment v. SSB", pos=2),
      P3=c(x="harvest", y="yield", panel="Equilibrium Yield v. F", pos=3),
      P4=c(x="ssb", y="yield", panel="Equilibrium Yield v. SSB", pos=4))

    # WITH economics
    if(!all(is.na(rps[, 'profit']))) {
      plots <- c(plots, list(
        P5=c(x="harvest", y="profit", panel="Equilibrium Profit v. F", pos=5),
        P6=c(x="ssb", y="profit", panel="Equilibrium Profit v. SSB", pos=6)))
    } else {
      dms <- dimnames(rps)
      rps <- rps[!dms$refpt %in% "mey",
        !dms$quant %in% c("revenue", "cost", "profit")]
    }
  
    # SUBSET panels if not NULL
    if(!is.null(panels))
      plots <- plots[panels]

    # APPLY over plots to extract x, y and panel for each element
    dat <- lapply(plots, function(p) {
      data.frame(x=df[,p['x']], y=df[,p['y']], iter=df[,'iter'],
        panel=p['panel'], pos=p['pos'], row.names=NULL)
    })

    # RBIND into single df
    dat <- do.call(rbind, c(dat, list(make.row.names = FALSE)))

    # Limit y to 0 in panels 1:4
    dat[dat$pos %in% 1:4 & dat$y<0, "y"] <- 0

    # CREATE facet labels vector
    facl <- setNames(unique(dat$panel), nm=unique(dat$pos))

    # PLOT
    p <- ggplot(dat, aes_(x=~x, y=~y, group=~iter)) + geom_line() +
      facet_wrap(~pos, scales="free", ncol=ncol, labeller=labeller(pos=facl)) +
      xlab("") + ylab("") +
      scale_x_continuous(labels=human_numbers, limits=c(0, NA))

    # PLOT refpts
    if(rpf) {
      rpdat <- lapply(plots, function(p) {
        # CBIND x, + refpt, iter ...
        cbind(as(rps[, p['x']], 'data.frame')[, -2],
        # ... y, panel
        y=c(rps[,p['y']]), pos=unname(p['pos']))
      })
      rpdat <- do.call(rbind, c(rpdat, list(make.row.names = FALSE)))
      
      # CALCULATE ymin per panel
      rpdat$ymin <- ave(rpdat$y, rpdat$pos, FUN=function(x) pmin(min(x), 0))

      # SET shapes and colors
      if(missing(shapes))
        shapes <- rep(c(21, 24, 22, 23), each=3)
      if(missing(colours))
        colours <- c(c("white", "#009e73", "#000000"), rep(c("#e69f00",
          "#56b4e9", "#f0e442", "#0072b2", "#d55e00", "#cc79a7"), 4))
      
      # ADD rps points
      p <- p + geom_point(data=rpdat, size=2.5,
        aes_(x=~data, y=~y, group=~refpt, fill=~refpt, shape=~refpt)) +
        scale_shape_manual(values=shapes) +
        scale_fill_manual(values=colours) 

      # ADD refpts labels and text
      if(length(refpts) > 0 & is.character(refpts)){
          
        rpdat <- rpdat[rpdat$refpt %in% refpts,]
      
        # CALCULATE limits of lines
        rpdat$yend <- rpdat$y * 0.99
        rpdat$ymax <- ave(rpdat$y, rpdat$pos, FUN=max)
        rpdat$ystart <- rpdat$ymin + (rpdat$ymax * 0.05)

        rpdat$ymin <- 0
        rpdat$ystart <- 0
        
        # LABEL
        if(labels) {
          p <- p + geom_text(data=rpdat,
            aes_(x=~data, y=~ymin, label=~refpt),
            angle = 90, size=3, vjust="left") +
            # LINES
            geom_segment(data=rpdat,
            aes_(x=~data, y=~ystart, xend=~data, yend=~yend), colour="grey")
        }
      }
    }

    # PLOT observations
    if(obs) {
      
      dfo <- model.frame(metrics(x,
        list(ssb=ssb.obs, harvest=fbar.obs, rec=rec.obs, yield=landings.obs,
        profit=profit.obs)), drop=FALSE)
    
      # APPLY over plots to extract x, y and panel for each element
      dato <- lapply(plots, function(p)
        data.frame(x=dfo[,p['x']], y=dfo[,p['y']], iter=dfo[,'iter'],
        pos=p['pos'], row.names=NULL))

      # REMOVE if NA
      idx <- unlist(lapply(dato, function(x) all(is.na(x$y))))
      
      dato <- do.call(rbind, c(dato[!idx], list(make.row.names = FALSE)))
      
      p <- p + geom_point(data=dato)
    }

    return(p)
  }
) # }}}

# TODO plot(FLBRPs)

# plot(FLStock, FLBRP) {{{

#' @examples
#' data(ple4)
#' data(ple4brp)
#' plot(ple4, ple4brp) +
#'   geom_hline(yintercept=1, linetype=2)

setMethod("plot", signature(x="FLStock", y="FLBRP"),
  function(x, y, metrics=list(`SSB/SSB[MSY]`=function(x, y) ssb(x) / sbmsy(y),
    `F/F[MSY]`=function(x, y) fbar(x) / fmsy(y),
    `C/MSY`=function(x, y) catch(x) / msy(y))) {

    # EXTRACT metrics
    fqs <- FLQuants(lapply(metrics, function(m) m(x, y)))

    # PLOT
    plot(fqs) + ylim(c(0, NA)) + facet_grid(qname~., labeller=label_parsed)
  }
)
# }}}

# plot(FLStocks, FLBRP) {{{

#' @examples
#' data(ple4)
#' data(ple4brp)
#' stks <- FLStocks(A=ple4, B=qapply(ple4, `*`, 0.87))
#' plot(stks, ple4brp) +
#'   geom_hline(yintercept=1, linetype=2)

setMethod("plot", signature(x="FLStocks", y="FLBRP"),
  function(x, y, ...) {

    # CREATE FLBRPs
    y <- FLBRPs(lapply(setNames(nm=names(x)), function(i) y))

    # PLOT
    plot(x, y, ...)
  }
)
# }}}

# plot(FLStocks, FLBRPs) {{{

#' @examples
#' data(ple4)
#' data(ple4brp)
#' stks <- FLStocks(A=ple4, B=qapply(ple4, `*`, 0.87))
#' brps <- FLBRPs(A=ple4brp, B=ple4brp)
#' plot(stks, brps) +
#'   geom_hline(yintercept=1, linetype=2)

setMethod("plot", signature(x="FLStocks", y="FLBRPs"),
  function(x, y, metrics=list(`SSB/SSB[MSY]`=function(x, y) ssb(x) / sbmsy(y),
    `F/F[MSY]`=function(x, y) fbar(x) / fmsy(y),
    `C/MSY`=function(x, y) catch(x) / msy(y))) {

    # EXTRACT metrics
    fqs <- FLQuants(lapply(metrics, function(m)
      dbind(FLQuants(Map(function(x, y) m(x, y), x=x, y=y)), dim=3)))

    # PLOT
    plot(fqs) + ylim(c(0, NA)) + facet_grid(qname~., labeller=label_parsed)
  }
)
# }}}
