## plot for 5 month ge data

ge_plot_indicator <- function(timeseries,indicators){
  ## plot
  #dev.new()
  win.graph(800,500)
  layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))
  par(mar = c(3, 5, 3, 1),cex.axis = 2, cex.lab = 2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
      tcl = 0.5)
  plot(timeseries,type='b',main='original time series',xlab='time',ylab='BP(KPa)')
 # mtext('hahaha test', side = 3, line = -1, adj = 0.1, cex = 0.6,col = "grey40")
  
  plot(indicators$time,indicators$variance,main='rolling variance',type='b',xlab='',ylab='')
  
  plot(indicators$time,indicators$autocorrelation,main='rolling autocorrelation lag-1',type='b',xlab='',ylab='')
  
  plot(indicators$time,scale(indicators$skewness),main='rolling skewness ',type='b',xlab='',ylab='')
  
}

## adjust the 
# bp_series_move_0 = ge_data_m5_sample$BP[ge_data_m5_sample$Speed>0]
# bp_series_move_0 = bp_series_move_0[bp_series_move_0>350]
# bp_series_move_10 = ge_data_m5_sample$BP[ge_data_m5_sample$Speed>10]
# bp_series_move_20 = ge_data_m5_sample$BP[ge_data_m5_sample$Speed>20]

# ge_move = ge_move_m5[ge_move_m5$BP>=350,] # just single point less than 350
# bp_series_move_0 = ge_move$BP # just single point less than 350
# bp_series_move_10 = ge_move$BP[ge_move$Speed>10]
# bp_series_move_20 = ge_move$BP[ge_move$Speed>20]
# 
# 
# windowsize = 10000
# indicators_10 = get_indicators(index(bp_series_move_10)[windowsize:length(bp_series_move_10)],
#                                bp_series_move_10,windowsize = windowsize)
# plot_statistic_indicators(bp_series_move_10,
#                           indicators_10,
#                           main = 'Pressure Signal in a Locomotive Brake Pipe Before It Fails',
#                           xlab = 'Time',
#                           ylab = 'Pressure(KPa)')

### analysis other variable for supplement
#ge_move_10 = ge_move_m5[ge_move_m5$Speed>10,]
ge_move_10 = ge_move[ge_move$Speed>10,]
time_series = ge_move_10$BC
indicators = get_indicators(index(time_series)[windowsize:length(time_series)],
                            time_series,
                            windowsize = windowsize)
plot_statistic_indicators(time_series,
                          indicators,
                          main = 'Pressure Signal in a Locomotive Brake Cylinder Before It Fails',
                          xlab = 'Time',
                          ylab = 'Pressure(KPa)')

# time_series = ge_move_10$ER
# indicators = get_indicators(index(time_series)[windowsize:length(time_series)],
#                             time_series,
#                             windowsize = windowsize)
# plot_statistic_indicators(time_series,
#                           indicators,
#                           main = 'Pressure Signal in a Locomotive Eq Res Pressure Before It Fails',
#                           xlab = 'Time',
#                           ylab = 'Pressure(KPa)')



############################### the other test for choosing the speed key point ##################
# indicators_20 = get_indicators(index(bp_series_move_20)[windowsize:length(bp_series_move_20)],
#                                bp_series_move_20,windowsize = windowsize)
# ge_plot_indicator(bp_series_move_20,indicators_20)


############# ajust the largest BP value to the same level (comprimise)

# bp_series_move_0_aline = bp_series_move_0
# bp_series_move_0_aline[bp_series_move_0_aline==503] = 496
# bp_series_move_0_aline[bp_series_move_0_aline==489] = 496
# 
# bp_series_move_10_aline = bp_series_move_10
# bp_series_move_10_aline[bp_series_move_10_aline==503] = 496
# bp_series_move_10_aline[bp_series_move_10_aline==489] = 496
# 
# bp_series_move_20_aline = bp_series_move_20
# bp_series_move_20_aline[bp_series_move_20_aline==503] = 496
# bp_series_move_20_aline[bp_series_move_20_aline==489] = 496
# 
# 
# 
# indicators_0_aline = get_indicators(index(bp_series_move_0_aline)[windowsize:length(bp_series_move_0_aline)],
#                                     bp_series_move_0_aline,windowsize = windowsize)
# ge_plot_indicator(bp_series_move_0_aline,indicators_0_aline)
# 
# indicators_10_aline = get_indicators(index(bp_series_move_10_aline)[windowsize:length(bp_series_move_10_aline)],
#                                       bp_series_move_10_aline,windowsize = windowsize)
# ge_plot_indicator(bp_series_move_10_aline,indicators_10_aline)
# 
# indicators_20_aline = get_indicators(index(bp_series_move_20_aline)[windowsize:length(bp_series_move_20_aline)],
#                                      bp_series_move_20_aline,windowsize = windowsize)
# ge_plot_indicator(bp_series_move_20_aline,indicators_20_aline)



