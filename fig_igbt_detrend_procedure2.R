


# the first two interval
win.graph(24,6)
par(mar = c(3, 4, 0.5, 0.5),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
#plot(collecter_current[16:360],type='b')
plot(collecter_current[76:450],type='b',xlab='Time',ylab='Current(A)')

# corresponding series
win.graph(24,6)
par(mar = c(2.2, 2.2, 0.5, 0.5),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
y1 = current_split_list[[2]]
y2 = current_split_list[[3]]
plot(append(y1,y2),xlab='',ylab='')

t1 = index(y1)
t2 = index(y2)

coef_intercep =model_coef[1]
coef_x = model_coef[2]

y1_fitted = exp(coef_x*t1 + coef_intercep)
y2_fitted = exp(coef_x*t2 + coef_intercep)

 lines(t1,y1_fitted,col='red',lwd=2)
 lines(t2+length(y1),y2_fitted,col='blue',lwd=2)

y1_residuals = y1 - y1_fitted
y2_residuals = y2 - y2_fitted


win.graph(24,6)
par(mar = c(2.2, 2.2, 2, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
plot(append(y1_residuals,y2_residuals),type='b',xlab='',ylab='')
abline(h=0,lty=2,lwd=2,col = "grey60")

win.graph(24,6)
par(mar = c(2.2, 2.2, 2, 2),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
len_noise = 5
plot(append(y1_residuals[len_noise:length(y1_residuals)],y2_residuals[len_noise:length(y2_residuals)]),
     type='b',xlab='',ylab='')
abline(h=0,lty=2,lwd=2,col = "grey60")





