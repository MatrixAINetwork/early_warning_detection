


## bearing
bearing = read.csv("data/final_dataset/IMSBearingNo2_mean_1024.csv",header = TRUE)
bearing_v1 = bearing$V1[1000:nrow(bearing)] # v1 failure without the noise data in the beginning

win.graph(800,210)
par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
plot(bearing_v1,type='b',
     xlim=c(1,(length(bearing_v1)+round(length(bearing_v1)/3))),
     xlab='',ylab='')

## igbt collector_current detrended
win.graph(800,210)
par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
plot(smoothed_series,type='b',
     xlim=c(1,(length(smoothed_series)+round(length(smoothed_series)/3))),
     xlab='',ylab='')



