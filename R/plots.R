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
#' # method will only plot existing referenced points
#' refpts(ple4brp) <- refpts(ple4brp)[c("msy", "fmax", "spr.30"),]
#' plot(ple4brp)

setMethod("plot", signature("FLBRP", "missing"),
  function(x, refpts=c("msy", "mey", "f0.1", "spr.30", "fmax","crash"),
    obs=FALSE, ...) {

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
    if(rpf && "crash" %in% dimnames(rps)$refpt)
      df <- df[df$harvest <= c(rps['crash', 'harvest']),]

    # NO economics
    panels <- list(
      P1=c(x="harvest", y="ssb", panel="Equilibrium SSB v. F"),
      P2=c(x="ssb", y="rec", panel="Equilibrium Recruitment v. SSB"),
      P3=c(x="harvest", y="yield", panel="Equilibrium Yield v. F"),
      P4=c(x="ssb", y="yield", panel="Equilibrium Yield v. SSB"))

    # WITH economics
    if(!all(is.na(rps[, 'profit']))) {
      panels <- c(panels, list(
        P5=c(x="harvest", y="profit", panel="Equilibrium Profit v. F"),
        P6=c(x="ssb", y="profit", panel="Equilibrium Profit v. SSB")))
    } else {
      dms <- dimnames(rps)
      rps <- rps[!dms$refpt %in% "mey",
        !dms$quant %in% c("revenue", "cost", "profit")]
    }
                             
    # APPLY over panels to extract x, y and panel for each element
    dat <- lapply(panels, function(p) {
      data.frame(x=df[,p['x']], y=df[,p['y']], iter=df[,'iter'],
        panel=p['panel'], row.names=NULL)
    })

    # RBIND into single df
    dat <- do.call(rbind, c(dat, list(make.row.names = FALSE)))

    # PLOT
    p <- ggplot(dat, aes_(x=~x, y=~y, group=~iter)) + geom_line() +
      facet_wrap(~panel, scales="free", ncol=2) +
      xlab("") + ylab("") + 
      scale_x_continuous(labels=human_numbers, limits=c(0,NA))

    # PLOT refpts
    if(rpf) {
      rpdat <- lapply(panels, function(p) {
        # CBIND x, + refpt, iter ...
        cbind(as(rps[, p['x']], 'data.frame')[, -2],
        # ... y, panel
        y=c(rps[,p['y']]), panel=unname(p['panel']))
      })
      rpdat <- do.call(rbind, c(rpdat, list(make.row.names = FALSE)))
      
      # CALCULATE ymin per panel
      rpdat$ymin <- ave(rpdat$y, rpdat$panel, FUN=function(x) pmin(min(x), 0))
      
      # ADD rps points
      p <- p + geom_point(data=rpdat, size=2.5,
        aes_(x=~data, y=~y, group=~refpt, fill=~refpt, shape=~refpt)) +
        scale_shape_manual(values=c(21, 21, 21, 24, 24, 24, 21)) +
        scale_fill_manual(values=c("white", "#4dac26", "black", "#f1b6da",
          "#f7f7f7", "#b8e186", "#d01c8b")) 

      # ADD refpts labels and text
      if(length(refpts) > 0 & is.character(refpts)){
          
        rpdat <- rpdat[rpdat$refpt %in% refpts,]
      
        # CALCULATE limits of lines
        rpdat$yend <- rpdat$y * 0.95
        rpdat$ymax <- ave(rpdat$y, rpdat$panel, FUN=max)
        rpdat$ystart <- rpdat$ymin + (rpdat$ymax * 0.05)
        
        # LABEL
        p <- p + geom_text(data=rpdat,
          aes_(x=~data, y=~ymin, label=~refpt), angle = 90, size=3, vjust="left") +
          # LINES
          geom_segment(data=rpdat, aes_(x=~data, y=~ystart, xend=~data, yend=~yend),
          colour="grey")
      }
    }

    # PLOT observations
    if(obs) {

      dfo <- model.frame(metrics(x,
        list(ssb=ssb.obs, harvest=fbar.obs, rec=rec.obs, yield=landings.obs)),
        drop=FALSE)
    
      # APPLY over panels to extract x, y and panel for each element
      dato <- lapply(panels[1:4], function(p)
        data.frame(x=dfo[,p['x']], y=dfo[,p['y']], iter=dfo[,'iter'],
        panel=p['panel'], row.names=NULL))
      
      dato <- do.call(rbind, c(dato, list(make.row.names = FALSE)))

      p <- p + geom_point(data=dato)
    }

    return(p)
  }
) # }}}

# TODO plot(FLBRPs)
