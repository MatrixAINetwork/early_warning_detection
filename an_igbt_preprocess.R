
seq_mean <- function(series,seq_list){
  ret_series = c()
  for(i in c(2:length(seq_list))){
    section = series[seq_list[i-1]:(seq_list[i]-1)]
    ret_series = append(ret_series,mean(section))
  }
  section = series[seq_list[length(seq_list)]:(length(series))]
  ret_series = append(ret_series,mean(section))
}

igbt_ = read.table("IIGBTAgingData.txt")
sq_size = 10
seq_list = seq(1,nrow(igbt_),sq_size)
COLLECTOR_CURRENT = seq_mean(igbt_[,2],seq_list = seq_list)
COLLECTOR_VOLTAGE = seq_mean(igbt_[,3],seq_list = seq_list)
GATE_CURRENT      = seq_mean(igbt_[,4],seq_list = seq_list)
GATE_VOLTAGE      = seq_mean(igbt_[,5],seq_list = seq_list)
HEAT_SINK_TEMP    = seq_mean(igbt_[,6],seq_list = seq_list)
PACKAGE_TEMP      = seq_mean(igbt_[,7],seq_list = seq_list)
igbt_resample = data.frame(COLLECTOR_CURRENT,COLLECTOR_VOLTAGE,GATE_CURRENT,
                           GATE_VOLTAGE,HEAT_SINK_TEMP,PACKAGE_TEMP)

#write.csv(igbt_resample,"final_dataset/igbt_resample_mean_10.csv",row.names = FALSE)