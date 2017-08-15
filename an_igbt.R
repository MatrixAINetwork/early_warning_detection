## analysis with the original data set


igbt_ = read.table("IIGBTAgingData.txt")
collecter_current = igbt_[,2]
work_series = collecter_current[collecter_current<=2]
nearest_detrended = work_series[c(118980:length(work_series))] # just keep 30000 records
nearest_original = collecter_current[c((length(collecter_current)-60000+1):length(collecter_current))] # coresponding,60000 records to compare
alind2fitted = nearest_detrended[c(43:126)]

