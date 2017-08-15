split_by_cycle_2mode <- function(series){
  split_list = list()
  one_cycle = c()
  one_cycle = append(one_cycle,series[1])
  cur_index = 1
  #threshold = max(series[1:round(0.5*length(series))]) - mean(series[1:round(0.5*length(series))])
  threshold = mean(series)/2
  for (i in c(2:length(series))) {
    cur = series[i]
    pre = series[i-1]
    if( abs(cur-pre) <=threshold ){
      one_cycle = append(one_cycle,cur)
    }else{
      split_list[[cur_index]] = one_cycle
      cur_index = cur_index + 1
      one_cycle = c()
      one_cycle = append(one_cycle,cur)
    }
  }
  
  return(split_list)
}

split_by_cycle_continue <- function(series){
  split_list = list()
  one_cycle = c()
  one_cycle = append(one_cycle,series[1])
  cur_index = 1
  flag = 0
  for (i in c(2:length(series))) {
    if(flag == 0 & series[i]>series[i-1] ){
      one_cycle = append(one_cycle,series[i])
    }
    if(flag == 0 & series[i]<=series[i-1]){
      flag = 1
    }
    if(flag == 1 & series[i]<=series[i-1]){
      one_cycle = append(one_cycle,series[i])
    }
    if(flag == 1 & series[i]>series[i-1]){
      split_list[[cur_index]] = one_cycle
      cur_index = cur_index+1
      one_cycle = c()
      one_cycle = append(one_cycle,series[i])
      flag=0
    }
  }
  return(split_list)
}

split_by_cycle <- function(series,threshold){
  split_list = list()
  one_cycle = c()
  one_cycle = append(one_cycle,series[1])
  cur_index = 1
  for (i in c(2:length(series))) {
    cur = series[i]
    pre = series[i-1]
    if( abs(cur-pre) <=threshold ){
      one_cycle = append(one_cycle,cur)
    }else{
      split_list[[cur_index]] = one_cycle
      cur_index = cur_index + 1
      one_cycle = c()
      one_cycle = append(one_cycle,cur)
    }
  }
  
  return(split_list)
}
eliminate_noise <- function(cycle_list)
{
  new_list = list()
  threshold = 10  # when the number of point in per period less than 10, it is the noise.
  k = 1
  for( i in c(1:length(cycle_list))){
    if(length(cycle_list[[i]])>threshold){
      new_list[[k]] =  cycle_list[[i]]
      k = k+1
    }
  }
  return(new_list)
}

eliminate_noise2 <- function(cycle_list)
{
  new_list = list()
  threshold = 10  # when the number of point in per period less than 10, it is the noise.
  k = 1
  for( i in c(1:length(cycle_list))){
    if(length(cycle_list[[i]])>threshold){
      cur_  = cycle_list[[i]]
      cur_min = mean(cur_) - sd(cur_)
      cur_max = mean(cur_) + sd(cur_)
      cur_ = cur_[cur_>=cur_min & cur_<=cur_max]
      if(length(cur_)>threshold){
        new_list[[k]] = cur_
        k = k+1
      }
    }
  }
  return(new_list)
}

concate_list <- function(cycle_list){
  series = c()
  for(i in c(1:length(cycle_list))){
    series = append(series,cycle_list[[i]])
  }
  return(series)
}

smooth_exp <- function(series_list,coef_intercep,coef_x,diff_start,diff_end){
  smoothed_series = c()
  for(i in c(2:(length(series_list)-1))){ # the first and last section no enough a cycle, ignore
    y = series_list[[i]]
    t = index(y)
    ynew = exp(coef_x*t + coef_intercep)
    residual = y - ynew
    smoothed_series = append(smoothed_series,residual[diff_start:(length(residual)-diff_end)])
    #smoothed_series = append(smoothed_series,residual)
  }
  return(smoothed_series)
}

smooth_exp_same_len <- function(series_list,size = 50, coef_intercep,coef_x){
  smoothed_series = c()
  for(i in c(2:(length(series_list)-1))){ # the first and last section no enough a cycle, ignore
    y = series_list[[i]]
    t = index(y)
    start_i = 1
    end_i = start_i + size - 1
    y = y[c(start_i:end_i)]
    t = t[c(start_i:end_i)]
    ynew = exp(coef_x*t+coef_intercep)
    residual = y - ynew
    smoothed_series = append(smoothed_series,residual)
  }
  return(smoothed_series)
}
smooth_debug <- function(series_list,size = 50, coef_intercep,coef_x){
  smoothed_series = c()
  for(i in c(2:(length(series_list)-1))){ # the first and last section no enough a cycle, ignore
    y = series_list[[i]]
    t = index(y)
    start_i = 1
    end_i = start_i + size - 1
    y = y[c(start_i:end_i)]
    t = t[c(start_i:end_i)]
    ynew = exp(coef_x*t+coef_intercep)
    residual = y - ynew
    smoothed_series = append(smoothed_series,residual[3:(length(residual))])
  }
  return(smoothed_series)
}

# bad
smooth_exp_unique <- function(series_list){
  smoothed_series = c()
  for(i in c(1:(length(series_list)))){ # the first and last section no enough a cycle, ignore
    y = series_list[[i]]
    y = y[y>=(mean(y)-sd(y)) & y<=(mean(y)+sd(y))]
    t = index(y)
    exp_model = lm(log(y) ~ 0+t)
    residual = exp_model$residuals
    smoothed_series = append(smoothed_series,residual)
  }
  return(smoothed_series)
}

model_learing <- function(series_list,len=20){
  sum_coef_inter = 0
  sum_x = 0
  for(i in c(1:(1+len-1))){
    y = series_list[[i]]
    x = index(y)
    exp.model = lm(log(y)~ x)
    sum_coef_inter = sum_coef_inter + exp.model$coefficients[1]
    sum_x = sum_x + exp.model$coefficients[2]
  }
  return(c(sum_coef_inter/len, sum_x/len))
}