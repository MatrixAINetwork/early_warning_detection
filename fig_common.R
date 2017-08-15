plot_statistic_indicators <- function(timeseries,
                                    indicators,
                                    main='original time series',
                                    xlab='time',
                                    ylab='measured value'){
  
  win.graph(13,6)
  layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))
  par(mar = c(3, 4, 3, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(timeseries,type='b',main=main,xlab=xlab,ylab=ylab)
  
  # variance
  plot(indicators$time,indicators$variance,main='Variance',type='b',xlab='',ylab='')
  var_kendall = MannKendall(na.omit(indicators$variance))
  mtext(sprintf("kendall = %.2f",var_kendall$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1.5)
  
  # autocorrelation
  plot(indicators$time,indicators$autocorrelation,main='Autocorrelation(1)',type='b',xlab='',ylab='')
  auto_kendall = MannKendall(na.omit(indicators$autocorrelation))
  mtext(sprintf("kendall = %.2f",auto_kendall$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1.5)
  
  # skewness
  sk_kendall = MannKendall(na.omit(indicators$skewness))
  plot(indicators$time,scale(indicators$skewness),main='Skewness ',type='b',xlab='',ylab='')
  mtext(sprintf("kendall = %.2f",sk_kendall$tau[1]),side =3,line = -5, adj = 0.1,cex = 1.5)
  
}

