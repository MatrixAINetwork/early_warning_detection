
igbt_old = read.table("data/final_dataset/IIGBTAgingData.txt")


igbt_COLLECTOR_CURRENT = igbt_old[,2]
igbt_COLLECTOR_VOLTAGE = igbt_old[,3]
igbt_GATE_CURRENT = igbt_old[,4]


colnames = c('COLLECTOR_CURRENT','COLLECTOR_VOLTAGE','GATE_CURRENT','GATE_VOLTAGE','HEAT_SINK_TEMP','PACKAGE_TEMP')


## plot stable state 1
win.graph(10,8)
par(mfrow=c(3,1),mar = c(3, 3.5, 1, 2),cex.axis = 1.8, cex.lab = 1.8,cex.main=1.8,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
    tcl = 0.5)
rows = c(10000:15400)
# plot(rows,igbt_COLLECTOR_CURRENT[rows],type='b',axes = FALSE,ylab='Collector Current',xlab='')
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
# 
# plot(rows,igbt_COLLECTOR_VOLTAGE[rows],type='b',axes = FALSE,ylab='Collector voltage',xlab='')
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
# 
# plot(rows,igbt_GATE_CURRENT[rows],type='b',axes = FALSE,ylab='Gate Current',xlab='Time')
# axis(1, col = "black", col.axis = "black")
# axis(2, col = "black", col.axis = "black")
# box(col = "black")


# time = rows
# collecter_current = igbt_COLLECTOR_CURRENT[rows]
# collecter_voltage = igbt_COLLECTOR_VOLTAGE[rows]
# gate_current = igbt_GATE_CURRENT[rows]
# dt = data.frame(time,collecter_current,collecter_voltage,gate_current)
# library(reshape2)
# mm <- melt(subset(dt, select=c(time,collecter_current,collecter_voltage,gate_current)),id.var="time")
# library(lattice)
# xyplot(value~time|variable,data=mm,type="l",
#        scales=list(y=list(relation="free")),
#        layout=c(1,3))

