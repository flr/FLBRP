#### Diagnostics for a glm
plotDiag<-function(x){
   vplayout <-function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

   smry<-data.frame(resStd    =rstandard(x),
                    res       =x$residuals,
                    hatLn     =x$linear.predictors,
                    hat       =x$fitted.values,
                    y         =x$y)

  grid.newpage()
  pushViewport(viewport(layout=grid.layout(2,2)))

  rsdl<-qqnorm(rstandard(x),plot.it=FALSE)
  rsdl<-data.frame(x=rsdl$x,y=rsdl$y)

  p<-ggplot(rsdl) + geom_point(aes(x,y),size=0.5)   +
                    opts(title = "Normal Q-Q Plot") + scale_x_continuous(name="Theoretical Quantiles") +
                                                      scale_y_continuous(name="Sample Quantiles")  +
                    geom_abline(intercept=0, slope=1)
  print(p, vp=vplayout(1,1))

  p<-ggplot(smry) +
                geom_point(aes(hat,resStd),size=0.5) + stat_smooth(aes(hat,resStd),method="gam") +
                opts(title="Error Distributions")    + scale_x_continuous(name="Predicted") +
                                                       scale_y_continuous(name="Standardised Residuals")
  print(p, vp=vplayout(1,2))

  p<-ggplot(smry) +
                geom_point(aes(hatLn,res), size=0.5) + stat_smooth(aes(hatLn,res),method="gam") +
                opts(title="Assumed Variance") + scale_x_continuous(name="Predicted on Link") +
                                                 scale_y_continuous(name="Absolute Residuals")
  print(p, vp=vplayout(2,1))

  p<-ggplot(smry) +
                geom_point(aes(hatLn,y), size=0.5) + stat_smooth(aes(hatLn,y),method="gam") +
                opts(title="Link Function") + scale_x_continuous(name="Predicted on Link") +
                                              scale_y_continuous(name="Observed")
  print(p, vp=vplayout(2,2))}
################################################################################


