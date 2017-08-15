
igbt = read.csv('data/final_dataset/igbt_resample_mean_10.csv',header = TRUE) # resample by step 10
igbt_old = read.table("data/final_dataset/IIGBTAgingData.txt")

igbt_COLLECTOR_CURRENT = igbt_old[290000:nrow(igbt_old),2]
igbt_COLLECTOR_VOLTAGE = igbt_old[290000:nrow(igbt_old),3]
igbt_GATE_CURRENT = igbt_old[290000:nrow(igbt_old),4]
igbt_GATE_VOLTAGE = igbt_old[290000:nrow(igbt_old),5]
igbt_HEAT_SINK_TEMP = igbt_old[290000:nrow(igbt_old),6]
igbt_PACKAGE_TEMP = igbt_old[290000:nrow(igbt_old),7]

# # fig 1
# time_series = igbt_COLLECTOR_CURRENT
# windowsize = 0.5*length(time_series)
# igbt_indicators = get_indicators(index(time_series)[windowsize:length(time_series)],
#                                        time_series,
#                                        windowsize = windowsize)
# plot_statistic_indicators(time_series,
#                           igbt_indicators,
#                           main = 'The Collector Current Signal in IGBT Accelerated Aging Test [NASA PCoE Datasets]',
#                           xlab = 'Time',
#                           ylab = 'Current(A)')


############################## Supplement ###################################

########################## COLLECTOR_VOLTAGE
time_series = igbt_COLLECTOR_VOLTAGE
windowsize = 0.5*length(time_series)
igbt_indicators = get_indicators(index(time_series)[windowsize:length(time_series)],
                                 time_series,
                                 windowsize = windowsize)
plot_statistic_indicators(time_series,
                          igbt_indicators,
                          main = 'The Collector Voltage Signal in IGBT Accelerated Aging Test [NASA PCoE Datasets]',
                          xlab = 'Time',
                          ylab = 'Voltage(V)')

# ######################### GATE_CURRENT
# time_series = igbt_GATE_CURRENT
# windowsize = 0.5*length(time_series)
# igbt_indicators = get_indicators(index(time_series)[windowsize:length(time_series)],
#                                  time_series,
#                                  windowsize = windowsize)
# plot_statistic_indicators(time_series,
#                           igbt_indicators,
#                           main = 'The Gate Current Signal in IGBT Accelerated Aging Test [NASA PCoE Datasets]',
#                           xlab = 'Time',
#                           ylab = 'Current(A)')
# 
# ######################### GATE_CURRENT
# time_series = igbt_GATE_VOLTAGE
# windowsize = 0.5*length(time_series)
# igbt_indicators = get_indicators(index(time_series)[windowsize:length(time_series)],
#                                  time_series,
#                                  windowsize = windowsize)
# plot_statistic_indicators(time_series,
#                           igbt_indicators,
#                           main = 'The Gate Voltage Signal in IGBT Accelerated Aging Test [NASA PCoE Datasets]',
#                           xlab = 'Time',
#                           ylab = 'Voltage(V)')
# 
# ######################### PACKAGE_TEMP
# 
# time_series = igbt_PACKAGE_TEMP
# windowsize = 0.5*length(time_series)
# igbt_indicators = get_indicators(index(time_series)[windowsize:length(time_series)],
#                                  time_series,
#                                  windowsize = windowsize)
# plot_statistic_indicators(time_series,
#                           igbt_indicators,
#                           main = 'The Package Temperature Signal in IGBT Accelerated Aging Test [NASA PCoE Datasets]',
#                           xlab = 'Time',
#                           ylab = 'Temperature(Celsius)')


# ######### density plot
# time_series = igbt_PACKAGE_TEMP
# out_dir = "figure/supplement/igbt_density"
# png_path = sprintf("%s/%s.png",out_dir,'Package_Temp')
# png(filename = png_path,width = 2048,height = 768)
# par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# 
# key = 9500
# kernel = 'gaussian'
# seg_1 = time_series[1:key]
# seg_2 = time_series[(key+1):length(time_series)]
# density_1 = density(seg_1,kernel = kernel)
# density_2 = density(seg_2,kernel = kernel)
# 
# color = c('blue','red')
# legend_text = c(sprintf("index from 1 to %g",key),
#                 sprintf("index from %g to the %g",(key+1),length(time_series)))
# plot( density_1,
#       xlim = c( (min(time_series)-var(time_series)), (max(time_series)+var(time_series)) ),
#       ylim = c( 0, max(max(density_1$y),max(density_2$y)) ),
#       main='The Package Temperature Signal in IGBT Accelerated Aging Test [NASA PCoE Datasets]')
# polygon(density_1,col = color[1], border = color[1])
# polygon(density_2, col = color[2], border = color[2])
# legend("topleft","(x,y)",legend_text, lty = c(1,1), lwd = c(2.5,2.5), col = color)
# dev.off()