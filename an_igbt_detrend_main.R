igbt_ = read.table("data/IIGBTAgingData.txt")

##############################
# fluctuation mining
#############################

#################################################################################
##figure for the paper main body —— collector current
# 
# collecter_current = igbt_[282000:nrow(igbt_),2]
# collecter_current_residuals = collecter_current[collecter_current<=2]
# 
# # these two threshold are not different for the current variable
# series = collecter_current_residuals
# threshold = (max(series[1:round(0.5*length(series))]) - mean(series[1:round(0.5*length(series))]))/2
# #threshold = (max(series[1:round(0.5*length(series))]) - mean(series[1:round(0.5*length(series))]))
# 
# current_split_list = split_by_cycle(collecter_current_residuals,threshold)
# model_coef = model_learing(current_split_list,len = as.integer(0.5*length(current_split_list)))
# smoothed_series = smooth_exp(current_split_list,coef_intercep = model_coef[1],coef_x = model_coef[2],
#                              diff_start = 5,diff_end =  0)
# 
# # smooth_size = 55
# # smoothed_series = smooth_debug(current_split_list,size = smooth_size,
# #                                                coef_intercep = model_coef[1],coef_x = model_coef[2])
# 
# timeseries = smoothed_series
# windowsize = 0.5*length(timeseries)
# indicators = get_indicators(index(timeseries)[windowsize:length(timeseries)],
#                                                         timeseries,
#                                                         windowsize = windowsize)
# 
# igbt_detrend_plot_split(collecter_current,
#                         timeseries,
#                         indicators,
#                         'Current(A)')

##################################################################################
### figure for the supplement

#########################################
####  COLLECTOR_VOLTAGE

# signal = igbt_[282000:nrow(igbt_),3]
# signal_residuals = signal[signal>=9.5]
# threshold = (max(signal_residuals[1:round(0.5*length(signal_residuals))]) - mean(signal_residuals[1:round(0.5*length(signal_residuals))]))
# #threshold = 0.1233584 the result of above
# #threshold = 0.1233584
# split_list = split_by_cycle(signal_residuals,threshold)
# 
# model_coef = model_learing(split_list,len = as.integer(0.5*length(split_list)))
# smoothed_series = smooth_exp(split_list,coef_intercep = model_coef[1],coef_x = model_coef[2],
#                              diff_start = 5,diff_end = 0)
# #smoothed_series = smooth_exp_unique(split_list)
# # smooth_size = 55
# # smoothed_series = smooth_debug(split_list,size = smooth_size,
# #                                       coef_intercep = model_coef[1],coef_x = model_coef[2])
# 
# timeseries = smoothed_series
# windowsize = 0.5*length(timeseries)
# indicators = get_indicators(index(timeseries)[windowsize:length(timeseries)],
#                             timeseries,
#                             windowsize = windowsize)
# 
# igbt_detrend_plot_split(signal,
#                         timeseries,
#                         indicators,
#                         'Voltage(V)')


#########################################
####  GATE_CURRENT

signal = igbt_[282000:nrow(igbt_),4]
signal_residuals = signal[signal<=0.002]
#threshold = (max(signal_residuals[1:round(0.5*length(signal_residuals))]) - mean(signal_residuals[1:round(0.5*length(signal_residuals))]))
threshold = 0.00019 # the result above equation
#threshold = 0.00035
split_list = split_by_cycle(signal_residuals,threshold)

new_list = list()
cycle_list = split_list
threshold = 10  # when the number of point in per period less than 10, it is the noise.
k = 1
i = 1
while( i <= length(cycle_list)){
  if(length(cycle_list[[i]])>1){
    new_list[[k]] =  cycle_list[[i]]
    k = k+1
  }else{
    new_list[[k]] = append(cycle_list[[i]],cycle_list[[i+1]])
    i = i+1
  }
  i = i+1
}
split_list = new_list

# split_list = eliminate_noise(split_list)
model_coef = model_learing(split_list,len = as.integer(0.5*length(split_list)))
smoothed_series = smooth_exp(split_list,coef_intercep = model_coef[1],coef_x = model_coef[2],
                             diff_start = 18,diff_end = 7)  ## 18,7

# smooth_size = 57
# smoothed_series = smooth_debug(split_list,size = smooth_size,
#                                       coef_intercep = model_coef[1],coef_x = model_coef[2])
#
timeseries = smoothed_series
windowsize = 0.5*length(timeseries)
indicators = get_indicators(index(timeseries)[windowsize:length(timeseries)],
                            timeseries,
                            windowsize = windowsize)

igbt_detrend_plot_split(signal,
                        timeseries,
                        indicators,
                        'Current(A)')

