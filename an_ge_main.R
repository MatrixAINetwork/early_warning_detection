

# ge_move_1314 = read.csv("GEData_2state/move/290-0513-0514_move.csv",header = TRUE)
# ge_move_1415 = read.csv("GEData_2state/move/290-0514-0515_move.csv",header = TRUE)
# ge_move_1516 = read.csv("GEData_2state/move/290-0515-0516_move.csv",header = TRUE)
# ge_move_1617 = read.csv("GEData_2state/move/290-0516-0517_move.csv",header = TRUE)
# 
# ge_move_BPRate_13_17 = append(ge_move_1314$BPRate,ge_move_1415$BPRate)
# ge_move_BPRate_13_17 = append(ge_move_BPRate_13_17,ge_move_1516$BPRate)
# ge_move_BPRate_13_17 = append(ge_move_BPRate_13_17,ge_move_1617$BPRate)


# ge_static_1314 = read.csv("GEData_2state/static/290-0513-0514_static.csv",header = TRUE)
# ge_static_1415 = read.csv("GEData_2state/static/290-0514-0515_static.csv",header = TRUE)
# ge_static_1516 = read.csv("GEData_2state/static/290-0515-0516_static.csv",header = TRUE)
# ge_static_1617 = read.csv("GEData_2state/static/290-0516-0517_static.csv",header = TRUE)
# 
# ge_static_BPRate_ = append(ge_static_1314$BPRate,ge_static_1415$BPRate)
# ge_static_BPRate_ = append(ge_static_BPRate_,ge_static_1516$BPRate)
# ge_static_BPRate_ = append(ge_static_BPRate_,ge_static_1617$BPRate)

#' read 05 month data
ge_move_m5 = c()
root_dir ="data/GEData/2state_identification/5month/move"
filelist = list.files(path = root_dir,pattern = ".csv")
filelist = filelist[grepl("290-05",filelist)]
for (fname in filelist){
  ge_data = read.csv(sprintf("%s/%s",root_dir,fname),header = TRUE)
  print(sprintf("process %s and data size is %g",fname,nrow(ge_data)))
  ge_move_m5 = rbind(ge_move_m5,ge_data)
}


# ## read original data
# ge_data_con_ = c()
# root_dir ="GeDataNew"
# filelist = list.files(path = root_dir,pattern = ".csv")
# filelist = filelist[grepl("290-05",filelist)]
# for (fname in filelist){
#   ge_data = read.csv(sprintf("%s/%s",root_dir,fname),header = TRUE)
#   print(sprintf("process %s and data size is %g",fname,nrow(ge_data)))
#   ge_data_con_ = rbind(ge_data_con_,ge_data)
# }
#   