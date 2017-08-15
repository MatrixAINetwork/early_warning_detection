igbt = read.csv('data/final_dataset/igbt_resample_mean_10.csv',header = TRUE) # resample by step 10
igbt_old = read.table("data/final_dataset/IIGBTAgingData.txt")

igbt_COLLECTOR_CURRENT = igbt_old[290000:nrow(igbt_old),2]
igbt_COLLECTOR_VOLTAGE = igbt_old[290000:nrow(igbt_old),3]
igbt_GATE_CURRENT = igbt_old[290000:nrow(igbt_old),4]
igbt_GATE_VOLTAGE = igbt_old[290000:nrow(igbt_old),5]
igbt_HEAT_SINK_TEMP = igbt_old[290000:nrow(igbt_old),6]
igbt_PACKAGE_TEMP = igbt_old[290000:nrow(igbt_old),7]


igbt_density_plot <- function(series){
  section1 = series[1:5000]
  section2 = series[5001:10000]
  section3 = series[10001:length(series)]
  
  ## density plot
  
  density_sec1 = density(section1)
  density_sec2 = density(section2)
  density_sec3 = density(section3)
  
  color = c('black','blue','red')
  legend_text = c(sprintf("index from 1 to 5000"),
                  sprintf("index from 5001 to 10000"),
                  sprintf("index from 10001 to %g",length(series)))
  
  dev.new()
  par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(density_sec1,
       xlim = c( min(series)-sd(series),max(series)+sd(series) ),
       ylim = c( 0, max(max(density_sec1$y),max(density_sec2$y),max(density_sec3$y))),
       xlab='',
       main='',
       col=color[1],
       lwd = 3)
  lines(density_sec2,col=color[2],lwd = 3)
  lines(density_sec3,col=color[3],lwd = 3)
  legend('topleft',"(x,y)",legend_text, lty = c(1,1), lwd = c(2.5,2.5), col = color,bty='n',cex=1.5)
  
}

igbt_density_plot_period <- function(period_list){
  series = concate_list(period_list)
  tail_len = 6
  unit_size = round((length(period_list)-tail_len)/2)
  section1 = concate_list(period_list[1:unit_size])
  section2 = concate_list(period_list[(unit_size+1):(length(period_list)-tail_len)])
  section3 = concate_list(period_list[(length(period_list)-tail_len+1):length(period_list)])
  
  ## density plot
  
  density_sec1 = density(section1)
  density_sec2 = density(section2)
  density_sec3 = density(section3)
  
  color = c('black','blue','red')
  legend_text = c(sprintf("index from 1 to %g",length(section1)),
                  sprintf("index from %g to %g",(length(section1)+1),(length(section1)+length(section2))),
                  sprintf("index from %g to %g",(length(section1)+length(section2)+1),length(series)))
  
  dev.new()
  par(mar = c(3, 4, 3, 1),cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(density_sec1,
       xlim = c( min(series)-sd(series),max(series)+sd(series) ),
       ylim = c( 0, max(max(density_sec1$y),max(density_sec2$y),max(density_sec3$y))),
       xlab='',
       main='',
       col=color[1],
       lwd = 3)
  lines(density_sec2,col=color[2],lwd = 3)
  lines(density_sec3,col=color[3],lwd = 3)
  legend('topleft',"(x,y)",legend_text, lty = c(1,1), lwd = c(2.5,2.5), col = color,bty='n',cex=2)
  
}

igbt_loc.test_period <- function(period_list){
  series = concate_list(period_list)
  tail_len = 6
  unit_size = round((length(period_list)-tail_len)/2)
  section1 = concate_list(period_list[1:unit_size])
  section2 = concate_list(period_list[(unit_size+1):(length(period_list)-tail_len)])
  section3 = concate_list(period_list[(length(period_list)-tail_len+1):length(period_list)])
  
  
  print("------------t.test--------------")
  print("section1 vs section2")
  print(t.test(section1,section2))
  print("section1 vs section3")
  print(t.test(section1,section3))
  print("section2 vs section3")
  print(t.test(section2,section3))
  
  print("------------wilcox.test---------")
  print("section1 vs section2")
  print(wilcox.test(section1,section2))
  print("section1 vs section3")
  print(wilcox.test(section1,section3))
  print("section2 vs section3")
  print(wilcox.test(section2,section3))
  
}

igbt_loc.test_section <- function(period_list){
  series = concate_list(period_list)
  section1 = series[1:5000]
  section2 = series[5001:10000]
  section3 = series[10001:length(series)]
  
  
  print("------------t.test--------------")
  print("section1 vs section2")
  print(t.test(section1,section2))
  print("section1 vs section3")
  print(t.test(section1,section3))
  print("section2 vs section3")
  print(t.test(section2,section3))
  
  print("------------wilcox.test---------")
  print("section1 vs section2")
  print(wilcox.test(section1,section2))
  print("section1 vs section3")
  print(wilcox.test(section1,section3))
  print("section2 vs section3")
  print(wilcox.test(section2,section3))
  
}
  

# ########## density plot by sections
# igbt_density_plot(igbt_COLLECTOR_CURRENT)
# igbt_density_plot(igbt_COLLECTOR_VOLTAGE)
# igbt_density_plot(igbt_GATE_CURRENT)
# igbt_density_plot(igbt_GATE_VOLTAGE)
# igbt_density_plot(igbt_HEAT_SINK_TEMP)
# igbt_density_plot(igbt_PACKAGE_TEMP)

###### density plot by periods


# ############### split by period
# series = igbt_GATE_CURRENT
# series_seg = split_by_cycle_2mode(series)
# series_seg_filter = eliminate_noise(series_seg)
# series_filter = concate_list(series_seg_filter)
# igbt_density_plot_period(series_seg_filter)
# #igbt_loc.test_period(series_seg_filter)

############ arima
# series = igbt_PACKAGE_TEMP[1:8000]
# tail_len = 2000
# series = igbt_PACKAGE_TEMP
# section1 = series[1:5000]
# section2 = series[5001:10000]
# section3 = series[10001:length(series)]
# tail_len = length(section3)
# train_series = series[1:(length(series)-tail_len)]
# arima.model = auto.arima(train_series)
# print(summary(arima.model))
# predic.result = forecast.Arima(arima.model,h=tail_len,level = 95)
# 
# win.graph(13,7)
# par(mar = c(3, 4, 3, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# plot(series,xlab='Time',ylab='Measured Value',ylim=c(250,max(series)))
# points(c((length(train_series)+1):length(series)),predic.result$mean,col='red',lwd=4)
# points( c((length(train_series)+1):length(series)), predic.result$lower, col='blue', lwd=4 )
# points(c((length(train_series)+1):length(series)), predic.result$upper, col='blue', lwd=4)




############ arima with flunctuation
 series = igbt_PACKAGE_TEMP
# tail_len = 2000
#series = timeseries
tail_len = 2000
train_series = series[1:(length(series)-tail_len)]
arima.model = auto.arima(train_series)
print(summary(arima.model))
predic.result = forecast.Arima(arima.model,h=tail_len,level = 95)

win.graph(30,7)
par(mar = c(3, 4, 3, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
plot(series,xlab='Time',ylab='Measured Value',ylim=c(min(series),max(series)),type='b')
points(c((length(train_series)+1):length(series)),predic.result$mean,col='red',lwd=3)
points( c((length(train_series)+1):length(series)), predic.result$lower, col='blue', lwd=3 )
points(c((length(train_series)+1):length(series)), predic.result$upper, col='blue', lwd=3)
