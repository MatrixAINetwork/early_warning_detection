




######  Fit Arima model
# data [1:170] are used as the learning data set
# base on the model we want to predict the points at [171:192]
turbofan_arima_forecast <- function(series)
{
  h = 22
  level = 95
  len = length(series)
  arima.model = arima(series[1:(len-h)])
  predic.result = forecast.Arima(arima.model,h=h,level = level)
  win.graph(18,6)
  par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(series,type='b',xlab='Time',ylab='Measured Value',lwd=2)
  points(c((len-h+1):len),predic.result$mean,col='red',lwd=2)
  lines(c((len-h+1):len),predic.result$lower,col='black',lwd=2)
  lines(c((len-h+1):len),predic.result$upper,col='black',lwd=2)
  segments((len-h+1),predic.result$lower[1],(len-h+1),predic.result$upper[1],col='black',lwd=2)
  segments(len,predic.result$lower[h],len,predic.result$upper[h],col='black',lwd=2)
}



# load data
#turbofan = read.csv("data/final_dataset/TurbofanEngine_trainfd001_uni1.csv",header = TRUE)


####################
# fit arima model for variables: V19, V16, V17, V18, V25, V26
 # turbofan_arima_forecast(turbofan$v19)
