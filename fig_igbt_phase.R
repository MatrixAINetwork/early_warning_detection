# igbt = read.csv('final_dataset/igbt_resample_mean_10.csv',header = TRUE) # resample by step 10
# igbt_old = read.table("final_dataset/IIGBTAgingData.txt")
# 
# igbt_COLLECTOR_CURRENT = igbt_old[,2]
# igbt_COLLECTOR_VOLTAGE = igbt_old[,3]
# igbt_GATE_CURRENT = igbt_old[,4]
# igbt_GATE_VOLTAGE = igbt_old[,5]
# igbt_HEAT_SINK_TEMP = igbt_old[,6]  # bad, exclude
# igbt_PACKAGE_TEMP = igbt_old[,7]

colnames = c('COLLECTOR_CURRENT','COLLECTOR_VOLTAGE','GATE_CURRENT','GATE_VOLTAGE','HEAT_SINK_TEMP','PACKAGE_TEMP')

x = igbt_GATE_CURRENT[10000:100000]
y = igbt_COLLECTOR_CURRENT[10000:100000]
z = igbt_COLLECTOR_VOLTAGE[10000:100000]

interval_size = 600
total_len = length(x)
split_num = as.integer(total_len/interval_size)
coltype = c()
for( i in c(1:split_num)){
  sub_col = rep(i,interval_size)
  coltype = append(coltype,sub_col)
}
if(split_num*interval_size < total_len){
  sub_col = rep(split_num+1,(total_len - (split_num*interval_size)))
  coltype = append(coltype,sub_col)
}

data = data.frame(x,y,z,coltype)
# 
# win.graph(800,800)
# par(mar = c(5, 5, 5, 5),cex = 1,cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 0, 0, 0),
#     mgp = c(1, 1.8, 0),tcl = 0.5)
# 
# lines3D(data$x,data$y,data$z,phi = 0,ticktype="detailed",
#         xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
#         col=data$coltype)

win.graph(800,800)
par(mar = c(5, 5, 5, 5),cex = 1,cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 0, 0, 0),
    mgp = c(1, 1.8, 0),tcl = 0.5)
split_list = seq(1,total_len,interval_size)
cols = c('black','blue','red','brown')
col_i = 2
sub_ = data[c(split_list[1]:(split_list[2]-1)),]
lines3D(sub_$x,sub_$y,sub_$z,phi = 0,ticktype="detailed",
       xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
       col=cols[2], colvar=NULL,lwd=1.5)
for( i in c(3:length(split_list)) ){
  start_i = split_list[i-1]
  end_i = split_list[i] - 1
  sub_ = data[c(start_i:end_i),]

  lines3D(sub_$x,sub_$y,sub_$z,phi = 0,ticktype="detailed",
         xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
         col=cols[2], colvar=NULL,lwd=1.5,add=TRUE)


  col_i = (col_i+1)%%length(cols)+1

}


# x = data$x[1:300]
# y = data$y[1:300]
# z = data$z[1:300]
# win.graph(800,800)
# par(mar = c(5, 5, 5, 5),cex = 1,cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 0, 0, 0),
#     mgp = c(1, 1.8, 0),tcl = 0.5)
# 
# lines3D(x,y,z,phi = 0,ticktype="detailed",
#         xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
#         col=NULL, colvar=NULL)
# 
# x = data$x[301:600]
# y = data$y[301:600]
# z = data$z[301:600]
# lines3D(x,y,z,phi = 0,ticktype="detailed",
#         xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
#         col='red', colvar=NULL,add=TRUE)


#arrows3D(x[150],y[150],z[150],x[152],y[152],z[152],lwd=2,length = 0.2,code = 2)
#arrows2D(x[150],y[150],x[151],y[151])

