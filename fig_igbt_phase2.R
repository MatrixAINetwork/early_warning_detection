
igbt_old = read.table("final_dataset/IIGBTAgingData.txt")

igbt_COLLECTOR_CURRENT = igbt_old[,2]
igbt_COLLECTOR_VOLTAGE = igbt_old[,3]
igbt_GATE_CURRENT = igbt_old[,4]


colnames = c('COLLECTOR_CURRENT','COLLECTOR_VOLTAGE','GATE_CURRENT','GATE_VOLTAGE','HEAT_SINK_TEMP','PACKAGE_TEMP')

x = igbt_COLLECTOR_CURRENT[10000:14800]
y = igbt_GATE_CURRENT[10000:14800]
z = igbt_COLLECTOR_VOLTAGE[10000:14800]

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

win.graph(800,800)
lines3D(data$x,data$y,data$z,phi = 0,ticktype="detailed",
        xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
        col='black', colvar=NULL,lwd=1.5,theta=340)

x2 = igbt_COLLECTOR_CURRENT[300000:301680]
y2 = igbt_GATE_CURRENT[300000:301680]
z2 = igbt_COLLECTOR_VOLTAGE[300000:301680]

win.graph(800,800)
lines3D(x2,y2,z2,phi = 0,ticktype="detailed",
        xlab='COLLECTOR_CURRENT',ylab='GATE_CURRENT',zlab='COLLECTOR_VOLTAGE',
        col='black', colvar=NULL,lwd=1.5,theta=340)


# win.graph(800,800)
# par(mar = c(5, 5, 5, 5),cex = 1,cex.axis = 1.5, cex.lab = 1.5,cex.main=1.5,oma = c(0, 0, 0, 0),
#     mgp = c(1, 1.8, 0),tcl = 0.5)
# split_list = seq(1,total_len,interval_size)
# cols = c('black','blue','red','brown')
# col_i = 2
# sub_ = data[c(split_list[1]:(split_list[2]-1)),]
# lines3D(sub_$x,sub_$y,sub_$z,phi = 0,ticktype="detailed",
#         xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
#         col=cols[2], colvar=NULL,lwd=1.5)
# for( i in c(3:length(split_list)) ){
#   start_i = split_list[i-1]
#   end_i = split_list[i] - 1
#   sub_ = data[c(start_i:end_i),]
# 
#   lines3D(sub_$x,sub_$y,sub_$z,phi = 0,ticktype="detailed",
#           xlab='GATE_CURRENT',ylab='COLLECTOR_CURRENT',zlab='COLLECTOR_VOLTAGE',
#           col=cols[2], colvar=NULL,lwd=1.5,add=TRUE)
# 
# 
#   col_i = (col_i+1)%%length(cols)+1
# 
# }


