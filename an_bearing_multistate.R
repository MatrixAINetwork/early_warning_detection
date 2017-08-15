
bearing = read.csv("data/final_dataset/IMSBearingNo2_mean_1024.csv",header = TRUE)

# v1 failure
#bearing_v1 = bearing$V1 # v1 failure
bearing_v1 = bearing$V1[1000:nrow(bearing)] # v1 failure without the noise data in the beginning


###### density fitted
section1 = bearing_v1[1:7000]
section2 = bearing_v1[7001:18000]
section3 = bearing_v1[18001:length(bearing_v1)]

##### density plot

kernel = 'gaussian'
density_sec1 = density(section1,kernel = kernel)
density_sec2 = density(section2,kernel = kernel)
density_sec3 = density(section3,kernel = kernel)

color = c('black','blue','red')
legend_text = c(sprintf("index from 1 to 7000"),
                sprintf("index from 7001 to 18000"),
                sprintf("index from 18001 to %g",length(bearing_v1)))
win.graph(8,7)
par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
plot(density_sec1,
     xlim = c( min(bearing_v1)-sd(bearing_v1), max(bearing_v1)+sd(bearing_v1)),
     ylim = c(0, max(max(density_sec1$y),max(density_sec2$y),max(density_sec3$y))),
     main='',
     col = color[1],
     lwd = 4,
     xlab=''
     )
lines(density_sec2,col=color[2],lwd=4)
lines(density_sec3,col=color[3],lwd=4)
legend("topleft","(x,y)",legend_text, lty = c(1,1), lwd = c(4,4), col = color,bty='n',cex=1.8)

##### location test

# print("------------t.test-------------------")
# print("section1 vs section2")
# print(t.test(section1,section2))
# print("section1 vs section3")
# print(t.test(section1,section3))
# print("section2 vs section3")
# print(t.test(section2,section3))
# 
# print("------------wilcox.test--------------")
# print("section1 vs section2")
# print(wilcox.test(section1,section2))
# print("section1 vs section3")
# print(wilcox.test(section1,section3))
# print("section2 vs section3")
# print(wilcox.test(section2,section3))


###### arima fitted

# tail_len = length(section3)
# tail_len = length(section3)
# train_series = bearing_v1[1:(length(bearing_v1)-tail_len)]
# arima.model = auto.arima(train_series)
# print(summary(arima.model))
# predic.result = forecast.Arima(arima.model,h=tail_len,level = 95)
# 
# win.graph(8,9)
# par(mar = c(3, 4, 3, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# #plot(bearing_v1,xlab='Time',ylab='Measured Value',type='b')
# plot(c(15000:length(bearing_v1)),bearing_v1[15000:length(bearing_v1)],xlab='Time',ylab='Measured Value',
#      type='b',xlim=c(15000,(length(bearing_v1)+10)))
# points(c((length(train_series)+1):length(bearing_v1)),predic.result$mean,col='red',lwd=4)
# points( c((length(train_series)+1):length(bearing_v1)), predic.result$lower, col='red', lwd=4)
# points(c((length(train_series)+1):length(bearing_v1)), predic.result$upper, col='red', lwd=4)


