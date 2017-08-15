bearing = read.csv("data/final_dataset/IMSBearingNo2_mean_1024.csv",header = TRUE)

# v1 failure
bearing_v1 = bearing$V1[1000:nrow(bearing)] # v1 failure

xt1 = bearing_v1[1:(length(bearing_v1)-1)]
xt2 = bearing_v1[2:length(bearing_v1)]

# original
win.graph(13,4)
par(mar = c(3, 3, 0.5, 0.5),cex = 1,cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 0, 0, 0),mgp = c(1.5, 0.5, 0),tcl = 0.5)
plot(bearing_v1,type='b',xlab='',ylab='')
abline(v=18000,col='grey60',lty=2,lwd=7)
abline(v=7000,col='grey60',lty=2,lwd=7)

# # divide into stable and unstable part
# stable_part = bearing_v1[1:1500]
# unstable_part = bearing_v1[15001:length(bearing_v1)]
# xlim = c(min(unstable_part),max(unstable_part))
# 
# win.graph(8,8)
# par(mar = c(3, 4, 0.5, 0.5),oma = c(0, 0, 0, 0),mgp = c(2, 0.4, 0),tcl = 0.5,cex.axis = 2, cex.lab = 2,cex.main=2)
# stable_xt1 = stable_part[1:(length(stable_part)-1)]
# stable_xt2 = stable_part[2:length(stable_part)]
# plot(stable_xt1,stable_xt2,pch=16,xlim = xlim,ylim = xlim,xlab = 'x(t)',ylab = 'x(t+1)',cex=1.5)
# 
# win.graph(8,8)
# par(mar = c(3, 4, 0.5, 0.5),oma = c(0, 0, 0, 0),mgp = c(2, 0.4, 0),tcl = 0.5,cex.axis = 2, cex.lab = 2,cex.main=2)
# unstable_xt1 = unstable_part[1:(length(unstable_part)-1)]
# unstable_xt2 = unstable_part[2:(length(unstable_part))]
# plot(unstable_xt1,unstable_xt2,pch=16,xlim = xlim,ylim = xlim,xlab = 'x(t)',ylab = 'x(t+1)',cex=1.5)


### divide into 2 part, same picture but different colour
# win.graph(8,8)
# names = c('Stable Motion Phase','Approaching Failure Motion Phase')
# cols = c('blue','red')
# par(mar = c(3, 4, 0.5, 0.5),oma = c(0, 0, 0, 0),mgp = c(2, 0.4, 0),tcl = 0.5,cex.axis = 2, cex.lab = 2,cex.main=2)
# plot(xt1[(length(xt1)-200):(length(xt1)-40)],xt2[(length(xt2)-200):(length(xt2)-40)],pch=1,col=cols[1],lwd=2,cex=2,
#      xlab='x(t)',ylab='x(t+1)')
# points(xt1[1:200],xt2[1:200],type='p',col=cols[2],pch=2,lwd=2,cex=2)
# legend('topleft',names,lty=1,lwd=2,col = cols)

# # plot 3d
# xt1_3d = bearing_v1[1:(length(bearing_v1)-2)]
# xt2_3d = bearing_v1[2:(length(bearing_v1)-1)]
# xt3_3d = bearing_v1[3:length(bearing_v1)]
#
# win.graph(800,800)
# par(mar = c(5, 5, 5, 5),cex = 1,cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 0, 0, 0),mgp = c(1, 1.8, 0),tcl = 0.5)
# scatter3D(xt1, xt2, xt3,phi = 0,ticktype="detailed",bty = "g",xlab='x(t)',ylab='x(t+1)',zlab='x(t+2)',cex=2,lwd=2)

