# sect_1 = bearing_1_abs[1:19500]
# sect_2 = bearing_1[19201:length(bearing_1)]
# 
# win.graph()
# plot(sect_1[1:(length(sect_1)-1)],sect_1[2:length(sect_1)])
# win.graph()
# plot(sect_1[1:(length(sect_1)-1)],sect_1[2:length(sect_1)],type='b')

## linear smooth
y = bearing_1_abs
x = c(1:length(y))
lm.model = lm( y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
win.graph()
plot(lm.model$fitted.values)

## gaussian smooth
bearing_gaus = smth.gaussian(bearing_1_abs,window = 5000)
