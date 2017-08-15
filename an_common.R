

#' plot the different statistic indicators of rolling window analysis for series
#' 
sliding_window <- function(series,
                           win.rate=0.5,
                           detrending = c('no','gaussian','linear','quadratic','first-diff'),
                           band.rate = 0.1){
  
  detrending = match.arg(detrending)
  df.indicators = lead.indicators(series,
                                  win.rate = win.rate,
                                  detrending = detrending, 
                                  band.rate = band.rate)
  
  dev.new()
  par(mar = (c(2, 2, 0, 1) + 0), oma = c(7, 2, 3, 1), mfrow = c(4, 2))
  
  plot(series,xlab='',ylab='',type='b')
  
  xlim = c(1,df.indicators$timeindex[nrow(df.indicators)])
  names = colnames(df.indicators)
  for(i in 2:ncol(df.indicators)){
    indic = df.indicators[,i]
    ken = MannKendall(na.omit(indic))
    plot(df.indicators$timeindex,indic,
         xlab='',ylab='',type='l',las = 1,xlim=xlim)
    legend("left",paste("Kendall tau=",round(ken$tau[1],digits = 3)),bty = 'n')
    legend("topleft",sprintf("%s",names[i]),bty = 'n')
  }
  mtext("Generic Early-Warnings", side = 3, line = 0.2, outer = TRUE)  #outer=TRUE print on the outer margin
  
}

#' Dive 'series' into two part and plot density for this two part
#' at the same figure for comparing. The diving point is specified by 'critical'
gaussian_density <- function(series,critical){
  win.graph()
  former_density = density(series[1:critical],kernel = "gaussian")
  latter_density = density(series[(critical+1):length(series)],kernel = "gaussian")
  color = c("blue","red")
  legend_text = c(sprintf("index from 1 to %g (critical=%g)",
                          index(series)[critical],critical),
                  sprintf("index from %g to the end",
                          index(series)[critical+1]))
  plot( former_density, 
        xlim = c(min(series)-sd(series),max(series)+sd(series)), 
        ylim = c(0,max(max(former_density$y),max(latter_density$y))))
  #lines(latter_density)
  polygon(former_density, col=color[1], border=color[1])
  polygon(latter_density, col=color[2], border=color[2])
  legend("topleft", "(x,y)",legend_text,lty=c(1,1), lwd=c(2.5,2.5), col=color)
}

#' get all statistic indicator -- old
get_indicators <- function(time,series,windowsize = NULL){
  variance = window_var(series,windowsize = windowsize)
  var_kendall = 0
  if(sum(is.na(variance)) == 0){
    var_kendall = MannKendall(variance)
  }
  autocorrelation = window_autocorr(series,windowsize = windowsize)
  auto_kendall = 0
  if( sum( is.na(autocorrelation) ) == 0 ){
    auto_kendall = MannKendall(autocorrelation)
  }
  skewness = window_skew(series,windowsize = windowsize)
  skew_kendall = 0
  if( sum( is.na( skewness ) ) == 0 ){
    skew_kendall = MannKendall(skewness)
  }
  means = window_mean(series,windowsize = windowsize)
  mean_kendall = 0
  if( sum( is.na( means ) ) == 0 ){
    mean_kendall = MannKendall(means)
   
  }
  covariation = window_cv(series,windowsize = windowsize)
  kurtosis = window_kurtosi(series,windowsize = windowsize)
  
  return(data.frame(time,variance,autocorrelation,skewness,means,covariation,kurtosis))
}

# calculate leading indicator: 
lead.indicators <- function(x,
                           indicators = c('var','ar1','acf1','skew','cv','kurtosis','mean'),
                           win.rate = 0.5,
                           detrending = c('no','gaussian','linear','quadratic','first-diff'),
                           band.rate = 0.1
                           )
{
  # detrend
  detrending = match.arg(detrending)
  xsm = x
  if( detrending != 'no'){
    xsm = detrend(x,method = detrending,band.rate = band.rate)
  }
  
  win.size = round( win.rate * length(xsm) )
  df.indicators = data.frame(time=win.size:length(xsm))
  for( i in 1:length(indicators)){
    name.indicator = indicators[i]
    value.indicator = numeric()
    if( name.indicator == 'var'){
      variance = sapply(win.size:length(xsm), function(k){
        var(xsm[(k-win.size+1):k],na.rm = TRUE)
      })
      df.indicators = cbind(df.indicators,variance) 
      print("finish to calculate the variance")
    }else if( name.indicator == 'ar1' ){
      ar1 = sapply(win.size:length(xsm), function(k){
        ret = ar.ols(xsm[(k-win.size+1):k],demean = TRUE,order.max = 1,aic = FALSE,intercept = FALSE)
        ret$ar[1]
      })
      df.indicators = cbind(df.indicators,ar1)
      print("finish to calculate the ar1")
    }else if( name.indicator == 'acf1'){
      acf1 = sapply(win.size:length(xsm), function(k){
        ACF = acf(xsm[(k-win.size+1):k],type='correlation',plot=FALSE,lag.max = 1)
        ACF$acf[2]
      })
      df.indicators = cbind(df.indicators,acf1)
      print("finish to calculate the acf1")
    }else if( name.indicator== 'skew' ){
      sk = sapply(win.size:length(xsm), function(k){
        sk = moments::skewness(xsm[(k-win.size+1):k],na.rm = TRUE)
      })
      df.indicators = cbind(df.indicators,sk)
      print("finish to calculate the skew")
    }else if( name.indicator == 'cv'){
      cv = sapply(win.size:length(xsm), function(k){
        var(xsm[(k-win.size+1):k])/mean(xsm[(k-win.size+1):k])
      })
      df.indicators = cbind(df.indicators,cv)
      print("finish to calculate the cv")
    }else if( name.indicator == 'kurtosis'){
      kur = sapply(win.size:length(xsm), function(k){
        kurtosis(xsm[(k-win.size+1):k])
      })
      df.indicators = cbind(df.indicators,kur)
      print("finisht to calculate the kur")
    }else if( name.indicator == 'mean'){
      mean = sapply(win.size:length(xsm), function(k){
        mean(xsm[(k-win.size+1):k])
      })
      df.indicators = cbind(df.indicators,mean)
      print("finish to calculate the mean")
    }
  }
  colnames(df.indicators) = append('timeindex',indicators) 
  return(df.indicators)
  
}

# calculate leading indicator: 
lead.indicators.parallel <- function(x,
                            indicators = c('var','ar1','acf1','skew','cv','kurtosis','mean'),
                            win.rate = 0.5,
                            detrending = c('no','gaussian','linear','quadratic','first-diff'),
                            band.rate = 0.1
)
{
  no_cores = detectCores() - 2
  cl = makeCluster(no_cores)
  # detrend
  detrending = match.arg(detrending)
  xsm = x
  if( detrending != 'no'){
    xsm = detrend(x,method = detrending,band.rate = band.rate)
  }
  
  win.size = round( win.rate * length(xsm) )
  df.indicators = data.frame(time=win.size:length(xsm))
  for( i in 1:length(indicators)){
    name.indicator = indicators[i]
    value.indicator = numeric()
    if( name.indicator == 'var'){
      variance = parSapply(cl,win.size:length(xsm), function(k){
        var(xsm[(k-win.size+1):k],na.rm = TRUE)
      })
      df.indicators = cbind(df.indicators,variance) 
    }else if( name.indicator == 'ar1' ){
      ar1 = parSapply(cl,win.size:length(xsm), function(k){
        ret = ar.ols(xsm[(k-win.size+1):k],demean = TRUE,order.max = 1,aic = FALSE,intercept = FALSE)
        ret$ar[1]
      })
      df.indicators = cbind(df.indicators,ar1)
    }else if( name.indicator == 'acf1'){
      acf1 = parSapply(cl,win.size:length(xsm), function(k){
        ACF = acf(xsm[(k-win.size+1):k],type='correlation',plot=FALSE,lag.max = 1)
        ACF$acf[2]
      })
      df.indicators = cbind(df.indicators,acf1)
    }else if( name.indicator== 'skew' ){
      sk = parSapply(win.size:length(xsm), function(k){
        sk = skew(xsm[(k-win.size+1):k],na.rm = TRUE)
      })
      df.indicators = cbind(df.indicators,sk) 
    }else if( name.indicator == 'cv'){
      cv = parSapply(cl,win.size:length(xsm), function(k){
        var(xsm[(k-win.size+1):k])/mean(xsm[(k-win.size+1):k])
      })
      df.indicators = cbind(df.indicators,cv)
    }else if( name.indicator == 'kurtosis'){
      kur = parSapply(win.size:length(xsm), function(k){
        kurtosis(xsm[(k-win.size+1):k])
      })
      df.indicators = cbind(df.indicators,kur)
    }else if( name.indicator == 'mean'){
      mean = parSapply(cl,win.size:length(xsm), function(k){
        mean(xsm[(k-win.size+1):k])
      })
      df.indicators = cbind(df.indicators,mean)
    }
  }
  stopCluster(cl)
  colnames(df.indicators) = append('timeindex',indicators) 
  return(df.indicators)
  
}


#' return a series, which is the result of smoothing original series 'x'
detrend <- function(x,
                    method = c('gaussian','linear','quadratic','first-diff'),
                    band.rate = 0.1){
  method <- match.arg(method)
  timeindex = index(x)
  y = x
  if(method == 'gaussian'){
    bw =round(length(x)*band.rate)
    ksmY = ksmooth(timeindex,x,kernel = "normal", bandwidth =bw, range.x = range(timeindex),x.points = timeindex)
    y = y - ksmY$y
  }else if(method == 'linear'){
    y = lm(x ~ c(1:length(x)))$residuals
  }else if(method == 'quadratic'){
    y = lm( x ~ c(1:length(x)) + I(c(1:length(x))^2))$residuals
  }else if(method == 'first-diff'){
    y = diff(x)
  }
  
  return(y)
}

#' variogram can be used to determine the stability of time series x
variogram <- function(x,lag.max = 20)
{
  acf.result = acf(x,lag.max = lag.max)
  acf.value = acf.result$acf[2:length(acf.result$acf)]
  
  variogram.value = (1-acf.value)/(1-acf.value[1])
  
  return(variogram.value)
}

####### Location test
location_test_plot <- function(series,num2mu=round(length(series)/3)){
  mu = mean(series[1:num2mu])
  re.t = c()
  re.wilcox = c()
  re.index = c()
  i = num2mu
  while(i<length(series)){
    re.index = append(re.index,i)
    t = t.test(series[1:i],mu=mu)
    wilcox = wilcox.test(series[1:i],mu=mu)
    re.t = append(re.t,t$p.value)
    re.wilcox = append(re.wilcox,wilcox$p.value)
    i = i+10
  }
  dev.new()
  par(mar = c(3, 4, 3, 1),cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  color = c('blue','red')
  plot(re.index,re.t,col=color[1],type='b',lwd=3,xlab='index',ylab='p-value',ylim=c(0,1))
  lines(re.index,re.wilcox,col=color[2],type='b',lwd=3)
  legend_text = c(sprintf("student t-test"),
                  sprintf("wilcox test"))
  legend("topright","(x,y)",legend_text, lty = c(1,1), lwd = c(2.5,2.5), col = color,bty='n')
}

t_test_plot <- function(series,num2mu=round(length(series)/3)){
  mu = mean(series[1:num2mu])
  re.t = c()
  re.index = c()
  i = num2mu
  while(i<length(series)){
    re.index = append(re.index,i)
    t = t.test(series[1:i],mu=mu)
    re.t = append(re.t,t$p.value)
    i = i+10
  }
  win.graph(8,8)
  par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(re.index,re.t,type='b',lwd=3,xlab='index',ylab='p-value',ylim=c(0,1))
  print(re.t)
  
}

wilcox_test_plot <- function(series,num2mu=round(length(series)/3)){
  mu = mean(series[1:num2mu])
  re.wilcox = c()
  re.index = c()
  i = num2mu
  while(i<length(series)){
    re.index = append(re.index,i)
    wilcox = wilcox.test(series[1:i],mu=mu)
    re.wilcox = append(re.wilcox,wilcox$p.value)
    i = i+10
  }
  win.graph(8,8)
  par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(re.index,re.wilcox,type='b',lwd=3,xlab='index',ylab='p-value',ylim=c(0,1))
  
}


