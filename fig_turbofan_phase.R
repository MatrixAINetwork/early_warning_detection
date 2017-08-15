
turbofan = read.csv("data/final_dataset/TurbofanEngine_trainfd001_uni1.csv",header = TRUE)
turbofan_v19 = turbofan$V19
turbofan_v19 = scale(turbofan_v19)
win.graph(11,7)
par(mar = c(3, 3, 1, 1),cex.axis = 2, cex.lab = 2,cex.main=2,
    oma = c(0, 0, 0, 0),mgp = c(1.5, 0.5, 0),tcl = 0.5)
plot(turbofan_v19,type='b',xlab='',ylab='',lwd=2)
abline(v=100,lty=2,col='grey60',lwd=7)

subset1 = turbofan_v19[1:100]
subset2 = turbofan_v19[101:length(turbofan_v19)]

x1 = seq(min(subset1),max(subset1),0.05)
x2 = seq(min(subset2),max(subset2),0.05)

dx1 = 0.0092-0.0451*x1
dx2 = 0.0131-0.0261*x2-0.0424*x2*x2

win.graph(8,8)
par(mar = c(3, 3, 1, 1),cex.axis = 2, cex.lab = 2,cex.main=2,
    oma = c(0, 0, 0, 0),mgp = c(1.5, 0.5, 0),tcl = 0.5)
cols = c('blue','red')
names = c('0.0092-0.0451*x','0.0131-0.0261*x-0.0424*x^2')
plot(x1,dx1,xlim=c(-3.5,2.5),ylim = c(-0.4,0.1),col=cols[1],type='l',xlab='x',ylab='dx/dt',lwd=4)
lines(x2,dx2,col=cols[2],lwd=4,type='l')
points(0.2039911308203991130820399113082,0,col='blue', pch = 16,cex=1.5,lwd=2)
points( -0.9431511411831667354914164365958,0,col='red',pch = 16, cex=1.5,lwd=2)
points(0.32758510344731767888764285169014,0,col='red',pch=16,cex=1.5,lwd=2)
abline(h=0,col='grey60',lty=2,lwd=2)
abline(v=0,col='grey60',lty=2,lwd=2)
legend("bottomright",names,pch = 1,lty=1,lwd=2,cex=2,col = cols)

arrows(x1[40],dx1[40],x1[39],dx1[39],col=cols[1],lwd=2,length = 0.2,code = 2)
arrows(x1[3],dx1[3],x1[4],dx1[4],col=cols[1],lwd=2,length = 0.2,code = 2)

arrows(x2[40],dx2[40],x2[39],dx2[39],col=cols[2],lwd=2,length = 0.2,code = 2)
arrows(x2[52],dx2[52],x2[53],dx2[53],col=cols[2],lwd=2,length = 0.2,code = 2)
arrows(x2[83],dx2[83],x2[82],dx2[82],col=cols[2],lwd=2,length = 0.2,code = 2)



