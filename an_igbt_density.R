series = timeseries
section1 = series[1:2000]
section2 = series[2001:4000]
section3 = series[4001:length(series)]

## density plot

density_sec1 = density(section1)
density_sec2 = density(section2)
density_sec3 = density(section3)

color = c('black','blue','red')
legend_text = c(sprintf("index from 1 to %g",length(section1)),
                sprintf("index from %g to %g",(length(section1)+1),(length(section1)+length(section2))),
                sprintf("index from %g to %g",(length(section1)+length(section2)+1),length(series)))

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
legend('topright',"(x,y)",legend_text, lty = c(1,1), lwd = c(2.5,2.5), col = color,bty='n',cex=1.5)