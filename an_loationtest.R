## bearing
# series = bearing_v1
# section3 = bearing_v1[18001:length(bearing_v1)]
# 
# len = length(section3)
# section1 = bearing_v1[1:len]
# section2 = bearing_v1[7001:(7001+len-1)]
# 
# re1 = wilcox.test(section1,section2,paired = TRUE)
# re2 = wilcox.test(section1,section3,paired = TRUE)
# re3 = wilcox.test(section2,section3,paired = TRUE)
# 
# print(paste(re1$p.value,"  ",re2$p.value,"  ",re3$p.value))


## igbt
series = igbt_COLLECTOR_CURRENT
section3 = series[10001:length(series)]

len = length(section3)
section1 = series[1:len]
section2 = series[5001:(5001+len-1)]

re1 = wilcox.test(section1,section2,paired = TRUE)
re2 = wilcox.test(section1,section3,paired = TRUE)
re3 = wilcox.test(section2,section3,paired = TRUE)

print(paste(re1$p.value,"  ",re2$p.value,"  ",re3$p.value))
