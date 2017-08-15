


# read data file
read_data <- function(root_dir = "bearing_IMS/2nd_test"){
  file_list = list.files(root_dir)
  ret_data = c()
  for(filename in file_list){
    print(sprintf("process %s ...................",filename))
    data = read.table(sprintf("%s/%s",root_dir,filename))
    ret_data = rbind(ret_data,data)
    print("...................done..............")
  }
  return(ret_data)
}

#' read data and calculate mean for each file
read_data_mean <- function(root_dir = "bearing_IMS/2nd_test",section_size=1024){
  file_list = list.files(root_dir)
  ret_data = c()
  for(filename in file_list){
    print(sprintf("process %s ...................",filename))
    data = read.table(sprintf("%s/%s",root_dir,filename))
    sections = nrow(data)/section_size
    for (i in c(0:(sections-1))) {
      start_ = i*section_size+1
      end_ = i*section_size+section_size
      sub_data = data[c(start_:end_),]
      means = colMeans(sub_data)
      ret_data = rbind(ret_data,means)
    }
    if( nrow(data)%%section_size !=0 ){
      sub_data = data[sections*section_size+1,nrow(data)]
      means = colMeans(sub_data)
      ret_data = rbind(ret_data,means)
    }
    #data = colMeans(data)
    #ret_data = rbind(ret_data,data)
    print("...................done..............")
  }
  return(ret_data)
}


#' split data into sub_section and calculate mean to represent it. 
split_data <- function(split_seq,series){
  data = c()
  for( i in c(2:(length(split_seq)))){
    section = series[split_seq[i-1]:(split_seq[i]-1)]
    data = append(data,mean(section))
  }
  section = series[(split_seq[length(split_seq)]):length(series)]
  data = append(data,mean(section))
  return(data)
}

 no.2.bearing = read_data_mean(root_dir = "bearing_IMS/2nd_test")
# seq_size = 2048
# series = no.1.bearing$V8
# mean_bear = split_data(seq(1,length(series),seq_size),series)
# indexs = c(1:length(mean_bear))
# win.graph()
# plot(mean_bear,type='b')
# win.graph()
# sliding_window(mean_bear,windowsize = 0.2*length(mean_bear))

#'-------------------------------------------

#' read file and plot sliding window
# out_dir_original = "bearing_IMS/2nd_test_result"
# out_dir_sample = "bearing_IMS/2nd_test_result_resample_10s"
# file_list = list.files(root_dir)
# for(filename in file_list){
#   print(sprintf("process %s ...................",filename))
#   data = read.table(sprintf("%s/%s",root_dir,filename))
#   bear1 = data[,1]
#   bear1_sample = bear1[seq(1,length(bear1),10)]
# 
#   png(filename =sprintf("%s/%s.png",out_dir_original,filename),width = 2048,height = 768)
#   sliding_window(bear1)
#   dev.off()
# 
#   png(filename = sprintf("%s/%s.png",out_dir_sample,filename),width = 2048,height = 768)
#   sliding_window(bear1_sample)
#   dev.off()
#   print("...................done..............")
# 
# }

#' read data
# bear1_conca_ = c()
# bear1_conca_sample_10s = c()
# file_list = list.files(root_dir)
# for(filename in file_list){
#   print(sprintf("process %s ...................",filename))
#   data = read.table(sprintf("%s/%s",root_dir,filename))
#   bear1 = data[,1]
#   bear1_conca_ = append(bear1_conca_,bear1)
#   bear1_conca_sample_10s = append(bear1_conca_sample_10s,bear1[seq(1,length(bear1),10)])
#   
#   print("...................done..............")
#   
# }

#' -----------------------
### no use
cal.indicator <- function(series = bear1_conca_,win_size = 1024){
  out_dir = "bearing_IMS/2nd_test_statistic_indicator"
  methods = c('var','cv','mean','skew','kurtosi','autocorr')
  statistic_indicators = c()
  for( m in methods){
    out_sub_dir = sprintf("%s/%s",out_dir,m)
    dir.create(out_sub_dir)
    indicator = switch (m,
      'var' = window_var(series,windowsize = win_size),
      'cv'  = window_cv(series, windowsize = win_size),
      'mean' = window_mean(series,windowsize = win_size),
      'skew' = window_skew(series,windowsize = win_size),
      'kurtosi' = window_kurtosi(series,windowsize = win_size),
      'autocorr' = window_autocorr(series,windowsize = win_size)
    )
    
    statistic_indicators = rbind(statistic_indicators,indicator)
    print(sprintf("calculate %s done...........",m))
  }
  return(statistic_indicators)
}
# win_size = 1024
# indicator_bear1_concat = cal.indicator(bear1_conca_,win_size = win_size)
# indicator_bear1_concat_sample = cal.indicator(bear1_conca_sample_10s,win_size = win_size)






