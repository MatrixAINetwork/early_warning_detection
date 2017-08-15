igbt_detrend_plot <- function(originalSeries,
                              detrendedSeries,
                              indicators){
  
  win.graph(800,400)
  layout(matrix(c(1,1,1,2,2,2,3,4,5), 3, 3, byrow = TRUE))
  par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  
  plot(originalSeries,type='b',main='The Collector Current Signal in IGBT Accelerated Aging Test [NASA PCoE Datasets]',
       xlab='Time',ylab='Current(A)')
  
  plot(detrendedSeries ,type='b',main='detrended time series',xlab='time',ylab='Amps')
  
  # variance
  plot(indicators$time,indicators$variance,main='Variance',type='b',xlab='',ylab='')
  var_kendall = MannKendall(na.omit(indicators$variance))
  mtext(sprintf("kendall = %g",var_kendall$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1)
  
  # autocorrelation
  plot(indicators$time,indicators$autocorrelation,main='Autocorrelation(1)',type='b',xlab='',ylab='')
  auto_kendall = MannKendall(na.omit(indicators$autocorrelation))
  mtext(sprintf("kendall = %.2f",auto_kendall$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1)
  
  # skewness
  plot(indicators$time,scale(indicators$skewness),main='Skewness ',type='b',xlab='',ylab='')
  auto_sk = MannKendall(na.omit(indicators$skewness))
  mtext(sprintf("kendall = %.2f",auto_sk$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1)
}

igbt_detrend_plot_split <- function(originalSeries,
                                    detrendedSeries,
                                    indicators,
                                    orig_ylab){
  
  win.graph(10,3)
  par(mar = c(3, 4, 2, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(originalSeries,type='b',xlab='Time',ylab=orig_ylab)

  
  win.graph(10,3)
  par(mar = c(3, 2, 1, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(detrendedSeries ,type='b',xlab='',ylab='')
  #plot(detrendedSeries ,type='h',xlab='',ylab='',axes=FALSE)
  
  
  win.graph(10,3)
  # par(mfrow=c(1,3),cex.axis = 2, cex.lab = 2,cex.main=2,
  #     tcl = 0.5)
  par(mfrow=c(1,3),mar = c(3, 2, 3, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  # variance
  plot(indicators$time,indicators$variance,main='Variance',type='b',xlab='',ylab='')
  
  var_kendall = MannKendall(na.omit(indicators$variance))
  mtext(sprintf("kendall = %.2f",var_kendall$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1.5)
  
  # autocorrelation
  plot(indicators$time,indicators$autocorrelation,main='Autocorrelation(1)',type='b',xlab='',ylab='')
  
  auto_kendall = MannKendall(na.omit(indicators$autocorrelation))
  mtext(sprintf("kendall = %.2f",auto_kendall$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1.5)
  
  # skewness
  plot(indicators$time,scale(indicators$skewness),main='Skewness ',type='b',xlab='',ylab='')
  auto_sk = MannKendall(na.omit(indicators$skewness))
  mtext(sprintf("kendall = %.2f",auto_sk$tau[1]),side = 3,line = -5, adj = 0.1,cex = 1.5)

}


