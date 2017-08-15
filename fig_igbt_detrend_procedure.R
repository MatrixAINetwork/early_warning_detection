


# the first two interval
win.graph(80,20)
par(mar = c(3, 4, 0.5, 0.5),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
#plot(collecter_current[16:360],type='b')
plot(collecter_current[76:450],type='b',xlab='Time',ylab='Current(A)')

# corresponding series
win.graph(80,20)
par(mar = c(2.2, 2.2, 0.5, 0.5),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
y1 = current_split_list[[2]]
y2 = current_split_list[[3]]
plot(append(y1,y2),xlab='',ylab='')

t1 = index(y1)
t2 = index(y2)
size = 55
start_i = 1
end_i = start_i + size - 1
y1_sub = y1[c(start_i:end_i)]
y2_sub = y2[c(start_i:end_i)]
t = c(1:size)

coef_intercep =model_coef[1]
coef_x = model_coef[2]

y_fitted = exp(coef_x*t + coef_intercep)

lines(t,y_fitted,col='red',lwd=2)
lines(t+length(y1),y_fitted,col='blue',lwd=2)

y1_residuals = y1_sub - y_fitted
y2_residuals = y2_sub - y_fitted


win.graph(80,20)
# par(mar = c(2.2, 2.2, 0.5, 0.5),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# plot(t1,y1_residuals,type='l',xlab='',ylab='',lwd=2,col='red',xlim=c(0,length(t)))
# lines(t2,y2_residuals,type='l',xlab='',ylab='',lwd=2,col='blue')
plot(append(y1_residuals,y2_residuals),type='b',xlab='',ylab='')
abline(h=0,lty=2,lwd=2,col = "grey60")


 


