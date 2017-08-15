igbt_old = read.table("data/final_dataset/IIGBTAgingData.txt")


igbt_COLLECTOR_CURRENT = igbt_old[,2]
igbt_COLLECTOR_VOLTAGE = igbt_old[,3]
igbt_GATE_CURRENT = igbt_old[,4]


colnames = c('COLLECTOR_CURRENT','COLLECTOR_VOLTAGE','GATE_CURRENT','GATE_VOLTAGE','HEAT_SINK_TEMP','PACKAGE_TEMP')


## plot stable state 1
win.graph(7,7)
par(mfrow=c(3,1),mar = c(4, 5, 2.2, 0),cex.axis = 2.5, cex.lab = 2.5,cex.main=2.5,oma = c(0, 0, 0, 0),mgp = c(3, 1, 0),
    tcl = 0.5)
# par(mfrow = c(3, 1))
# #par(cex = 0.6)
# par(mar = c(0, 0, 0, 0), oma = c(4, 10, 3, 0.5))
# par(tcl = -0.25)
# par(mgp = c(2, 0.6, 0))
# par(cex.axis = 2.5, cex.lab = 2.5,cex.main=2.5)

rows = c(10000:15400)
plot(rows,igbt_COLLECTOR_CURRENT[rows],type='b',axes = FALSE,ylab='Collector Current',xlab='')
axis(2, col = "black", col.axis = "black")
box(col = "black")

plot(rows,igbt_COLLECTOR_VOLTAGE[rows],type='b',axes = FALSE,ylab='Collector voltage',xlab='')
axis(2, col = "black", col.axis = "black")
box(col = "black")

plot(rows,igbt_GATE_CURRENT[rows],type='b',axes = FALSE,ylab='Gate Current',xlab='Time')
axis(1, col = "black", col.axis = "black")
axis(2, col = "black", col.axis = "black")
box(col = "black")


# ## plot stable state 2
# win.graph(10,8)
# par(mfrow=c(3,1),mar = c(3, 3.5, 1, 2),cex.axis = 1.8, cex.lab = 1.8,cex.main=1.8,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# rows = c(200000:205400)
# plot(rows,igbt_COLLECTOR_CURRENT[rows],type='b',axes = FALSE,ylab='Collector Current',xlab='')
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
# 
# 
# plot(rows,igbt_COLLECTOR_VOLTAGE[rows],type='b',axes = FALSE,ylab='Collector Voltage',xlab='')
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
# 
# plot(rows,igbt_GATE_CURRENT[rows],type='b',axes = FALSE,ylab='Gate Current',xlab='Time')
# axis(1, col = "black", col.axis = "black")
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
# 
# 
# ## plot the last state
# win.graph(10,8)
# par(mfrow=c(3,1),mar = c(3, 3.5, 1, 2),cex.axis = 1.8, cex.lab = 1.8,cex.main=1.8,oma = c(0, 0, 0, 0),mgp = c(2, 0.6, 0),
#     tcl = 0.5)
# rows = c(296280:301680)
# plot(rows,igbt_COLLECTOR_CURRENT[rows],type='b',axes = FALSE,ylab='Collector Current',xlab='')
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
# 
# 
# plot(rows,igbt_COLLECTOR_VOLTAGE[rows],type='b',axes = FALSE,ylab='Collector Voltage',xlab='')
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
# 
# plot(rows,igbt_GATE_CURRENT[rows],type='b',axes = FALSE,ylab='Gate Current',xlab='Time')
# axis(1, col = "black", col.axis = "black")
# axis(2, col = "black", col.axis = "black")
# box(col = "black")
