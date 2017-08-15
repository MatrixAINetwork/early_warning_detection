#' @description  The GE data marked by '2009' in Date segment are reboot noise, based on 
#' which this function realize to divide original dataset into several subset which exclude 
#' the reboot nose.
#' @param root_dir where save the original data file
#' @param out_dir  
ge_seg_original_file <- function(root_dir = "data/GEData/m1_3/original",
                                 out_dir = "data/GEData/m1_3/parsed"){
 
  dir.create(out_dir)
  file_list = list.files(path = root_dir,pattern = ".csv")
  for(file_name in file_list){
    print("-------------------------------------------------------------------------------------------")
    data = read.csv(sprintf("%s/%s",root_dir,file_name),header = TRUE)
    #columname = c('Date','Time','KM','Meters','Speed','Trac.Effort','BP','BC','ER','Fuel','BP.Flow')
    #data = data[,columname]
    indexs = c(1:nrow(data))
    split_points = indexs[grepl('2009',data$Date)]
    print("split points: ")
    print(split_points)
    
    select_points = c()
    if(length(split_points)!=0){
      select_points = c(1,(split_points[1]-1))
      flag = TRUE
      for( i in c( 2:length(split_points) )){
        if( split_points[i] != (split_points[i-1]+1) ){
          select_points = append(select_points,c((split_points[i-1]+1),(split_points[i]-1)))
        }
      }
      if(split_points[length(split_points)]<nrow(data))
        select_points = append(select_points,c((split_points[length(split_points)]+1), nrow(data)))
    }
    print("select points:  ")
    print(select_points)
    
    ## out_put
    file_first_name = strsplit(file_name,'[.]')[[1]][1]
    for(i in seq(1,length(select_points),2)){
      seg_indexs = c(select_points[i]:select_points[i+1])
      seg_data = data[seg_indexs,]
      seg_out_dir = sprintf("%s/%s_%g.csv",out_dir,file_first_name,i)
      write.csv(seg_data,file = seg_out_dir)
    }
    
    print(sprintf("-----------done to parse file %s-------------",file_name))
  }
}

#' Concatenate different segment data for GE data, which are saved in .csv file, 
#' and their names have the same pattern.
#' @description This function realize to concatenate the files which have the specific pattern. 
#' @param root_dir where save the files be concatenated
#' @param pattern 
#' @param resample whether resample data set, default is TRUE
#' @param resample_size
concate_data <- function(root_dir ="data/GEData/m4_5/parsed",
                         pattern ="290-04",
                         resample = TRUE,
                         resample_size = 10){
  file_list = list.files(sprintf("%s/",root_dir),pattern = ".csv")
  data_concate = c()
  #  Sys.setenv(TZ="CST")
  leave_columname = c('Date','Time','KM','Meters','Speed','Trac.Effort','BP','BC','ER','Fuel','BP.Flow')
  for(file_name in file_list){
    if(grepl(pattern,file_name)){
      print(sprintf("process file: %s ......",file_name))
      
      data = read.csv(sprintf("%s/%s",root_dir,file_name),header = TRUE)
      data = data[data$BP!='---',]
      data = data[!is.na(data$BP),]
      data = data[,leave_columname]
      if(resample){
        data = data[seq(1,nrow(data),10),]
      }
      data_concate = rbind(data_concate,data)
    }
  }
  
  data_concate[['Date_Time']] = format(strptime( paste(data_concate$Date,data_concate$Time,sep="-"),
                                                 format = '%d/%m/%Y-%H:%M:%S'),
                                       format="%d-%m-%Y-%H-%M-%S")
  data_concate = data_concate[!duplicated(data_concate$Date_Time),]
  data_concate = data_concate[,!(names(data_concate) %in% c('Date','Time'))]
  data_concate[['BPRate']] = ge_get_BPRate(data_concate$BP)
  return(data_concate)
}



