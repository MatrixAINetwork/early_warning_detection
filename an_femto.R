
### read acc file
root_dir = "data/FEMTOBearingDataSet/Learning_set/Bearing1_1"

list.file.femto <- function (path){
  filelist = list.files(path = path,pattern = ".csv")
  filelist_acc = filelist[grepl("acc_",filelist)]
  filelist_temp = filelist[grepl("temp_",filelist)]
  out = list(acc = filelist_acc,temp = filelist_temp)
  return(out)
}

readAccelByIntervl.femto <- function(path, from, to){
  filelist = list.file.femto(path)
  filelist = filelist$acc
  if( from <=0 | to > length(filelist)){
    print("error:  the index is error")
    return()
  }
  horiz_accel = c()
  vert_accel = c()
  for (i in c(from:to)) {
    fname = filelist[i]
    fpath = sprintf("%s/%s",path,fname)
    data = read.csv(fpath,header = FALSE)
    horiz_accel = append(horiz_accel,data[,5])
    vert_accel = append(vert_accel,data[,6])
  }
  out = data.frame(horiz_accel,vert_accel)
  return(out)
}

resample.femto <- function(series,interval_size=64){
  start = 1
  end = interval_size
  new.series = c()
  while(end<=length(series)){
    new.series = append(new.series,mean(series[start:end]))
    start = start + interval_size
    end = end + interval_size
  }
  return(new.series)
}

denoise.femto <- function(series,winsize = 5000){
  start=1
  end = winsize
  len = length(series)
  newseries = c()
  while(end<=len){
    sub.set = series[start:end]
    sub.sd = var(sub.set)
    sub.mean = mean(sub.set)
    I = sub.set>(sub.mean+sub.sd)
    sub.set[I] = sub.mean + sub.sd
    I = sub.set<(sub.mean-sub.sd)
    sub.set[I] = sub.mean - sub.sd
    newseries = append(newseries,sub.set)
    start = start + winsize
    end = end + winsize
  }
  return(newseries)
}
