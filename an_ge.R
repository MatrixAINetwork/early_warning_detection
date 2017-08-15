#' @description realized the calculation of BPRate based on the input BP
ge_get_BPRate <- function(BP){
  BP = as.numeric(BP)
  BPRate = rep(NA,5)
  for(i in c(6:length(BP))){
    BPRate = append(BPRate,(BP[i-5]-BP[i]))
  }
  return(BPRate)
}

#' @description Plot 'series' by segment and the size of each segment is spercified by the param 'segsize'.
#' Each plot have a small overlap part, which is for better visualization.
segment_plot_series <- function(series,
                                times,
                                segsize=1000,
                                out_dir="BP_BPRate_segment_plot",
                                add_gaussian_line = FALSE,
                                windowsize = 0.5 * segsize){
  dir.create(out_dir)
  seg_list = seq(1,length(series),segsize)
  indexs = c(1:length(series))
  if(add_gaussian_line){
    data_smt_gaussian = smth.gaussian(series,window = windowsize)
  }
  
  # the first segment:
  seg_values = series[seg_list[1]:(seg_list[2]-1)]
  seg_times = times[seg_list[1]:(seg_list[2]-1)]
  seg_index = indexs[seg_list[1]:(seg_list[2]-1)]
  seg_name = sprintf("from_%s_to_%s",seg_times[1],seg_times[length(seg_times)])
  
  png_path = sprintf("%s/f%g_%s.png",out_dir,1,seg_name)
  png(filename = png_path,width = 2048,height = 768)
  plot(seg_index,seg_values,type='b')
  if(add_gaussian_line){
    lines(data_smt_gaussian[seg_list[1]:(seg_list[2]-1)],type='b',col="red")
  }
  dev.off()
  
  for( i in c(2:(length(seg_list)-1)) ){
    seg_start = (seg_list[i]-0.1*segsize)
    seg_end = (seg_list[i+1]-1)
    seg_values = series[seg_start:seg_end]
    seg_index = indexs[seg_start:seg_end]
    seg_times = times[seg_start:seg_end]
    seg_name = sprintf("from_%s_to_%s",seg_times[1],seg_times[length(seg_times)])
    
    png_path = sprintf("%s/f%g_%s.png",out_dir,i,seg_name)
    png(filename = png_path,width = 2048,height = 768)
    plot(seg_index,seg_values,type='b')
    if(add_gaussian_line){
      lines(data_smt_gaussian[seg_start:seg_end],type='b',col="red")
    }
    dev.off()
  }
  
}

#' @description 
#' Compute different statistic indicators over a sliding window, which is spercified by the 
#' param 'methods'. And then plot by segment and size is spercified by the param 'segsize'
#' and then save the plots below the dir 'output_dir'
ge_rolling_analysis <- function(series,
                                times,
                                windowsize = 0.5*length(series),
                                methods=c('var','cv','mean','skew','kurtosi','autocorr'),
                                output_dir = "statistics_indicators",
                                segsize = 1000)
{
  print("------------------------caculate statistics indicator------------")
  output_dir = sprintf("%s_winsize.%g",output_dir,windowsize)
  dir.create(output_dir)
  #windowsize = length(series)*windowsize_rate
  for(method in methods){
    rolling_result = c()
    rolling_result = switch (method,
                             'var' = window_var(series,windowsize = windowsize),
                             'cv' = window_cv(series,windowsize = windowsize),
                             'mean' = window_mean(series,windowsize = windowsize),
                             'skew' = window_skew(series,windowsize = windowsize),
                             'kurtosi' = window_kurtosi(series,windowsize = windowsize),
                             'autocorr' = window_autocorr(series,windowsize = windowsize),
                             'ar.ols' = window_ar.ols(series,windowsize = windowsize)
    )
    times = times[c((length(times)-length(rolling_result)+1):length(times))]
    segment_plot_series(rolling_result,
                        times,
                        segsize = segsize,
                        out_dir=sprintf("%s/%s/",output_dir,method))
    print(sprintf("it is done to calculate the inticator %s",method))
    print(sprintf("the length of series is %g",length(times)))
    print(sprintf("time from %s to %s",times[1],times[length(times)]))
    
  }
  
  print("------------------------------all done-----------------------------")
  
}

#' @description plot original time-series and leading indicator 
ge_analysis_original <- function(file_dir,resample_size = 10){
  dir_split = unlist(strsplit(file_dir,'/'))
  filename = dir_split[length(dir_split)]
  ge_data = read.csv(file_dir,header = TRUE)
  start_time = sprintf("%s , %s ",ge_data$Date[1],ge_data$Time[1])
  end_time = sprintf("%s , %s ",ge_data$Date[length(ge_data$Date)],ge_data$Time[length(ge_data$Time)])
  ge_data = ge_data[,columname]
  ge_data = na.omit(ge_data)
  ge_data = ge_data[ge_data$BP!='---',]
  ge_data_sample = ge_data[seq(1,length(ge_data$BP),resample_size),]
  print("------------------------------------------------------")
  print(sprintf("----------analysis result for %s file --------",filename))
  print(sprintf("original dataset size : %g ",length(ge_data$BP)))
  print(sprintf("resample dataset size : %g ",length(ge_data_sample$BP)))
  print(sprintf("the time interval is :"))
  print(start_time)
  print(end_time)
  print("------------------------------------------------------")
  win.graph(200,80)
  plot(ge_data_sample$BP,type='b',main="original time series plot")
  sliding_window(as.numeric(ge_data_sample$BP))
}

#' @description get ARIMA model and Ljung-Box test result, original data plot, indicators plot
ge_analysis_arima <- function(file_dir,resample_size = 10){
  dir_split = unlist(strsplit(file_dir,'/'))
  filename = dir_split[length(dir_split)]
  ge_data = read.csv(file_dir,header = TRUE)
  start_time = sprintf("%s , %s ",ge_data$Date[1],ge_data$Time[1])
  end_time = sprintf("%s , %s ",ge_data$Date[length(ge_data$Date)],ge_data$Time[length(ge_data$Time)])
  ge_data = ge_data[,columname]
  ge_data = na.omit(ge_data)
  ge_data = ge_data[ge_data$BP!='---',]
  ge_data_sample = ge_data[seq(1,length(ge_data$BP),resample_size),]
  ge_data_arima_model = auto.arima(as.numeric(ge_data_sample$BP))
  ge_data_arima_residuals = ge_data_arima_model$residuals
  test_result = Box.test(ge_data_arima_residuals,lag = 20,type="Ljung-Box")
  print("------------------------------------------------------")
  print(sprintf("----------analysis result for %s file --------",filename))
  print(sprintf("original dataset size : %g ",length(ge_data$BP)))
  print(sprintf("resample dataset size : %g ",length(ge_data_sample$BP)))
  print(sprintf("the time interval is :"))
  print(start_time)
  print(end_time)
  print("arima model:")
  print(ge_data_arima_model)
  print("Ljung-Box Test Result:  ")
  print(test_result)
  print("------------------------------------------------------")
  win.graph(200,80)
  plot(ge_data_arima_residuals,type='b',main="residual plot")
  sliding_window(ge_data_arima_residuals)
}

##################################################################################################
#' @description analysis ge_data by file. Arima model, smooth, Ljung-Box Test, rolling indicators.
ge_analysis_unit <- function(root,
                             filename,
                             length_rate=0.5,
                             smt_method = c('Original','Gaussian'),
                             bandwidth_rate = 0.5
                             ){
  # read data
  ge_data = read.csv(sprintf("%s/%s",root,filename), header = TRUE)
  leave_columname = c('Date','Time','KM','Meters','Speed','Trac.Effort','BP','BC','ER','Fuel','BP.Flow')
  ge_data = ge_data[,leave_columname]
  ge_data = na.omit(ge_data)
  ge_data = ge_data[ge_data$BP!='---',]
  
  if(length(ge_data$BP)<500) ## data set too small
  {
    print(sprintf("data length is %g, too small",length(ge_data$BP)))
    return()
  }
    
  
  # resample, interval is 10s
  #ge_data = ge_data[seq(1,length(ge_data$BP),10),]
  
  # calculate BP Rate
  BP = as.numeric(ge_data$BP)
  BPRate = c()
  for(i in c(6:length(BP))){
    BPRate = append(BPRate,(BP[i-5]-BP[i]))
  }
  
  BP = BP[-c(1:5)] # re-scale BP
  ge_data = ge_data[-c(1:5),] # re-scale
  
  if(smt_method == 'Gaussian'){
    BP = na.omit(smth.gaussian(BP,window = length(BP)*bandwidth_rate))
    BPRate =na.omit( smth.gaussian(BP,window = length(BPRate)*bandwidth_rate))
  }
  
  # plot sliding window analysis result
  png_path = sprintf("%s/BP_%s_%f/%s.png",root,smt_method[1],bandwidth_rate,filename)
  png(filename = png_path,width = 2048,height = 768)
  sliding_window(BP,length(BP)*length_rate)
  dev.off()

  png_path = sprintf("%s/BPRate_%s_%f/%s.png",root,smt_method[1],bandwidth_rate,filename)
  png(filename = png_path,width = 2048,height = 768)
  sliding_window(BPRate,length(BPRate)*length_rate)
  dev.off()

  # print result
  BP_arima_model = auto.arima(BP)
  BPRate_arima_model = auto.arima(BPRate)
  BP_arima_residuals = BP_arima_model$residuals
  BPRate_arima_residuals = BPRate_arima_model$residuals

  BP_test_result = Box.test(BP_arima_residuals,lag = 20,type="Ljung-Box")
  BPRate_test_result = Box.test(BPRate_arima_residuals,lag = 20,type="Ljung-Box")

  start_time = sprintf("%s , %s ",ge_data$Date[1],ge_data$Time[1])
  end_time = sprintf("%s , %s ",ge_data$Date[length(ge_data$Date)],ge_data$Time[length(ge_data$Time)])

  print(sprintf("dataset size : %g ",length(BP)))
  print(sprintf("the time interval is :"))
  print(start_time)
  print(end_time)
  print("---BP series result: ")
  print("arima model:")
  print(BP_arima_model)
  print("Ljung-Box Test:  ")
  print(BP_test_result)
  print("---BPRate series result: ")
  print("arima model:")
  print(BPRate_arima_model)
  print("Ljung-Box Test:  ")
  print(BPRate_test_result)
  
}

# The main function of analysis ge data
ge_analysis <- function(root,
                        length_rate=0.5,
                        smt_method = c('Original','Gaussian'),
                        bandwidth_rate = 0.5){
  file_list = list.files(root,pattern = ".csv")
  dir.create(sprintf("%s/BP_%s_%f",root,smt_method[1],bandwidth_rate))
  dir.create(sprintf("%s/BPRate_%s_%f",root,smt_method[1],bandwidth_rate))
  output_file = sprintf("%s/result_%s_%f.txt",root,smt_method[1],bandwidth_rate)
  sink(output_file)
  for( filename in file_list){
    print(sprintf("------------- file: %s ---------------",filename))
    ge_analysis_unit(root,
                     filename,
                     length_rate,
                     smt_method = smt_method,
                     bandwidth_rate = bandwidth_rate)
    print("----------------------------------------------")
    cat("\n\n")
  }
  unlink(output_file)
  sink()
}
############################################################################################





