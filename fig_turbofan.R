
# # plot the fig1 for Unit1-variable 19
#Train_FD001_Uni1_V19
# turbofan = read.csv("data/final_dataset/TurbofanEngine_trainfd001_uni1.csv",header = TRUE)
# turbofan_v19 = turbofan$V19
# windowsize = 0.5*length(turbofan_v19)
# turbofan_v19_indicators = get_indicators(index(turbofan_v19)[windowsize:length(turbofan_v19)],
#                                          turbofan_v19,windowsize = windowsize)
# plot_statistic_indicators(turbofan_v19,
#                         turbofan_v19_indicators,
#                         main = 'A Measured Signal in the Turbofan Engine Degradation Simulation [NASA PCoE Datasets]',
#                         xlab='Time',
#                         ylab='Measured Value')



# # plot for the supplement,which are the other variable plot
# 
turbofan = read.csv("data/final_dataset/TurbofanEngine_trainfd001_uni1.csv",header = TRUE)
turbofan_v = turbofan$V26
windowsize = 0.5*length(turbofan_v)
turbofan_v_indicators = get_indicators(index(turbofan_v)[windowsize:length(turbofan_v)],
                                         turbofan_v,windowsize = windowsize)
plot_statistic_indicators(turbofan_v,
                          turbofan_v_indicators,
                          main = 'A Measured Signal in the Turbofan Engine Degradation Simulation [NASA PCoE Datasets]',
                          xlab='Time',
                          ylab='Measured Value')


# plot for the supplement, the leading indicators with the detrending value
# timeseries = turbofan$V14
# dev.new()
# layout(matrix(c(1,1,1,2,2,2,3,4,5), 3, 3, byrow = TRUE))
# par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# 
# plot(timeseries,type='b')
# band.rate = 0.4
# method = 'gaussian'
# smy = detrend(timeseries,method = method,band.rate = band.rate)
# plot(smy,type='h')
# 
# df.indicators = lead.indicators(timeseries,indicators = c("var","acf1","skew"),
#                                 win.rate = 0.5,detrending = 'gaussian',band.rate = band.rate)
# plot(df.indicators$timeindex,df.indicators$var,type='b',xlab='time',ylab='variance')
# ken_var = MannKendall(df.indicators$var)$tau[1]
# legend('topleft',sprintf("Kendall tau = %.2f",ken_var),bty = 'n')
# plot(df.indicators$timeindex,df.indicators$acf1,type='b',xlab='time',ylab='acf1')
# ken_acf1 = MannKendall(df.indicators$acf1)$tau[1]
# legend("topleft",sprintf("Kendall tau = %.2f",ken_acf1),bty = 'n')
# plot(df.indicators$timeindex,df.indicators$skew,type='b',xlab='time',ylab='skew')



