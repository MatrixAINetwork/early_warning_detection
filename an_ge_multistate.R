# ge_move = ge_move_m5[ge_move_m5$BP>=350,] # just single point less than 350
# bp_series_move_10 = ge_move$BP[ge_move$Speed>10]

## BP
# series = bp_series_move_10
# section1 = series[1:10000]
# section2 = series[13500:15000]
# section3 = series[19000:length(series)]

## BC
# ge_move_10 = ge_move[ge_move$Speed>10,]
# series = ge_move_10$BC
# section1 = series[1:8000]
# section2 = series[8001:16000]
# section3 = series[16001:length(series)]

##### density plot

# kernel = 'gaussian'
# density_sec1 = density(section1,kernel = kernel)
# density_sec2 = density(section2,kernel = kernel)
# density_sec3 = density(section3,kernel = kernel)
# 
# color = c('black','blue','red')
# legend_text = c(sprintf("index from 1 to 8000"),
#                 sprintf("index from 8001 to 16000"),
#                 sprintf("index from 16001 to %g",length(series)))
# win.graph(7,7)
# par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# plot(density_sec1,
#      xlim = c( min(series)-sd(series), max(series)+sd(series)),
#      ylim = c(0, max(max(density_sec1$y),max(density_sec2$y),max(density_sec3$y))),
#      main='',
#      col = color[1],
#      lwd = 3,
#      xlab=''
#      )
# lines(density_sec2,col=color[2],lwd=3)
# lines(density_sec3,col=color[3],lwd=3)
# legend("topleft","(x,y)",legend_text, lty = c(1,1), lwd = c(2.5,2.5), col = color,bty='n',cex=2)

###### arima fitted

# tail_len = length(section3)
# train_series = series[1:(length(series)-tail_len)]
# arima.model = auto.arima(train_series)
# print(summary(arima.model))
# predic.result = forecast.Arima(arima.model,h=tail_len,level = 95)
# 
# dev.new()
# par(mar = c(3, 4, 3, 1),cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# plot(series,xlab='Time',ylab='Measured Value',ylim=c(-50,max(series)))
# points(c((length(train_series)+1):length(series)),predic.result$mean,col='red',lwd=4)
# points( c((length(train_series)+1):length(series)), predic.result$lower, col='blue', lwd=4)
# points(c((length(train_series)+1):length(series)), predic.result$upper, col='blue', lwd=4)