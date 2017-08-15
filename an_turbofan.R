
work_dir = 'data/CMAPSSData'

## load data
data = read.table(sprintf("%s/train_FD001.txt",work_dir))
unit = nasa[nasa$V1==1,]

# constant_variables = c(5,6,10,11,15,21,23,24)
# unit = unit[,-constant_variables]
# var.name = colnames(unit)

variables_leave = c(19,16,17,18,25,26)
unit = unit[,variables_leave]
var.name = colnames(unit)

########## plot gaussian density

# for(i in c(1:length(unit))){
#   out_dir = 'density_unit1'
#   win.graph(9,7)
#  # png_path = sprintf("%s/%s/%s.png",work_dir,out_dir,var.name[i])
# #  png(filename = png_path,width = 2048,height = 768)
#   par(mar = c(3, 4, 3, 1),cex.axis = 2, cex.lab = 2,cex.main=2,oma = c(0, 1, 0, 0),mgp = c(2, 0.6, 0),
#       tcl = 0.5)
#   
#   key = 110
#   kernel = 'gaussian'
#   first.density = density(unit[1:key,i],kernel = kernel)
#   second.density = density(unit[(key+1):nrow(unit),i],kernel = kernel)
#   
#   color = c('blue','red')
#   legend_text = c(sprintf("index from 1 to %g",key),
#                   sprintf("index from %g to the %g",(key+1),nrow(unit)))
#   plot( first.density,
#         xlim = c( (min(unit[,i])-sd(unit[,i])), (max(unit[,i])+sd(unit[,i])) ),
#         ylim = c( 0, max(max(first.density$y),max(second.density$y)) ),
#         main='',
#         col=color[1],
#         lwd=3,
#         xlab='')
#   lines(second.density,col=color[2],lwd=3)
#   # polygon(first.density,col = color[1], border = color[1])
#   # polygon(second.density, col = color[2], border = color[2])
#   legend("topleft","(x,y)",legend_text, lty = c(1,1), lwd = c(2.5,2.5), col = color,bty='n',cex=1.5)
#   #dev.off()
#   
# }


### location test
for(i in c(1:length(unit))){
  series = unit[,i]
  section1 = series[1:30]
  section2 = series[31:80]
  section3 = series[81:length(series)]
  
  # section1 = series[1:50]
  # section2 = series[51:100]
  # section3 = series[101:length(series)]
    
  re1 = t.test(section1,section2)
  re2 = t.test(section1,section3)
  re3 = t.test(section2,section3)
    
  print(paste(re1$p.value,"  ",re2$p.value,"  ",re3$p.value))
}






