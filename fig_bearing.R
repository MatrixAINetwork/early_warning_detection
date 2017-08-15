bearing = read.csv("data/final_dataset/IMSBearingNo2_mean_1024.csv",header = TRUE)

# v1 failure
#bearing_v1 = bearing$V1 # v1 failure
bearing_v1 = bearing$V1[1000:nrow(bearing)] # v1 failure
#bearing_v1 = bearing$V1[1000:(nrow(bearing)-150)] # v1 failure
#bearing_v1 = bearing$V1[18000:(nrow(bearing)-150)] # v1 failure
windowsize = 0.5*length(bearing_v1)
bearing_v1_indicators = get_indicators(index(bearing_v1)[windowsize:length(bearing_v1)],
                                       bearing_v1,
                                       windowsize = windowsize)
plot_statistic_indicators(bearing_v1,
                          bearing_v1_indicators,
                          main = 'The Vibration Signal in the Bearing Failure Test [NASA PCoE Datasets]',
                          xlab='Time',
                          ylab='Measured Value')





