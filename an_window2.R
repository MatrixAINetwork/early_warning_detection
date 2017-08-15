
#' resample data

# ge_data_05_resample_10s = concate_data (root_dir ="GeDataNew",
#                                         pattern ="290-05",
#                                         resample = TRUE,
#                                         resample_size = 10)

#' plot segment
# segment_plot_series(ge_data_02_resample_10s$BPRate,
#                     ge_data_02_resample_10s$Date_Time,
#                     segsize=1000,
#                     out_dir="GeDataNew/M02_BPRate_segment_plot_10s",
#                     add_gaussian_line = FALSE)


#' rolling analysis


# ge_rolling_analysis(ge_data_05_resample_10s$BP,
#                     ge_data_05_resample_10s$Date_Time,
#                     windowsize = 25000,
#                     methods=c('var','cv','mean','skew','kurtosi','autocorr'),
#                     #methods=c('var','mean','skew','kurtosi','autocorr'),
#                     output_dir = "GeDataNew/StatisticIndicators/M05",
#                     segsize = 1000)
# 
# # standardize
# 
# BP = ge_data_03_resample_10s$BP
# #BP = (BP - mean(BP))/sd(BP)
# emd_result = emd(BP)

# for( col in c(1:ncol(emd_result$imf))){
#   data = emd_result$imf[,col]
#   win.graph()
#   plot(data,type='b')
#   abline(h=0,col="red")
#   win.graph()
#   sliding_window(data)
# }

sq_size = 60
seq_mean <- function(series,seq_list){
  ret_series = c()
  for(i in c(2:length(seq_list))){
    section = series[seq_list[i-1]:(seq_list[i]-1)]
    ret_series = append(ret_series,mean(section))
  }
  section = series[seq_list[length(seq_list)]:(length(series))]
  ret_series = append(ret_series,mean(section))
}

# BP_05 = ge_data_05$BP
# BP_05_mean = seq_mean(BP_05,seq(1,length(BP_05),sq_size))
series = ge_m345$BP
series_mean = seq_mean(series,seq(1,length(series),sq_size))



