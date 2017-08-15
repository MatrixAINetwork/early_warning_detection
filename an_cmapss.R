

nasa_plot_original_data <- function(data_path,
                                    save_path,
                                    unit=1,
                                    type=c('b','o'),
                                    add_gaussian_line = TRUE,
                                    windowsize_rate = 0.1){
  nasa = read.table(data_path)
  nasa_unit1 = nasa[nasa$V1==unit,]
  for( col in c(3:length(nasa_unit1))){
    dir.create(save_path)
    png_path = sprintf("%s/v%g.png",save_path,col)
    png(filename = png_path,width = 2048,height = 768)
    
    plot(nasa_unit1[,col],type=type[1])
    if(add_gaussian_line){
      data_smt_gaussian = smth.gaussian(nasa_unit1[,col],window = length(data)*windowsize_rate)
      lines(data_smt_gaussian,type='b',col="red")
    }
      
    dev.off()
  }
}

nasa_analysis <- function(index,time_series){
  
  win.graph(200,80)
  plot(index,time_series,type='b')
  arima_model = auto.arima(time_series)
  
  print("arima result:    ")
  print(arima_model)
  print("Box testing result:  ")
  box_testing = Box.test(arima_model$residuals,lag = 20, type = "Ljung-Box")
  
  print("box test result: ")
  print(box_testing)
  win.graph(200,80)
  plot.forecast(forecast.Arima(arima_model,h=10))
  
  sliding_window(time_series)
  
}


nasa_analysis_plot <- function(indexs,values,number){
  sink("sink-examp.txt")
  print("----------------------------------------------")
  print(sprintf("the analysis for value v%g",number))
  
  win.graph(400, 150)
  plot(indexs,values,type='b',main=sprintf("original vale of v%g",number))
  
  arima_model = auto.arima(values)
  print("arima model fitting result:  ")
  print(arima_model)
  
  win.graph(400, 150)
  plot.forecast(forecast.Arima(arima_model,h=10))
  
  residuls = arima_model$residuals
  win.graph(200, 100)
  acf(residuls,lag.max = 20)
  boxtest_result = Box.test(residuls,lag = 20,type = "Ljung-Box")
  print("Box test with Ljung-Box result:  ")
  print(boxtest_result)
  
  win.graph(400, 150)
  plot(indexs,residuls,type='b',main=sprintf("the residuals vale of v%g",number))
  
  sliding_window(residuls)
  
  gaussian_density(values, length(values)/2)
  
  print("-------------------------------------------------")
  sink()
  
}
nasa_analysis_unit <- function(indexs,values,number){
  sink("sink-examp.txt")
  print("----------------------------------------------")
  print(sprintf("the analysis for value v%g",number))
  
  win.graph(400, 150)
  plot(indexs,values,type='b',main=sprintf("original vale of v%g",number))
  
  arima_model = auto.arima(values)
  print("arima model fitting result:  ")
  print(arima_model)
  
  win.graph(400, 150)
  plot.forecast(forecast.Arima(arima_model,h=10))
  
  residuls = arima_model$residuals
  win.graph(200, 100)
  acf(residuls,lag.max = 20)
  boxtest_result = Box.test(residuls,lag = 20,type = "Ljung-Box")
  print("Box test with Ljung-Box result:  ")
  print(boxtest_result)
  
  win.graph(400, 150)
  plot(indexs,residuls,type='b',main=sprintf("the residuals vale of v%g",number))
  
  sliding_window(residuls)
  
  gaussian_density(values, length(values)/2)
  
  print("-------------------------------------------------")
  sink()
  
}

##
##  analysis a nasa data file 
##
nasa_analysis_arima <- function(file_name,dir_output,length_rate=0.5,constant_cols=NULL)
{
  sink(sprintf("%s_result.txt",file_name))
  dir.create(sprintf("%s/",dir_output))
  nasa = read.table(file_name)
  units = unique(nasa$V1)
  culmulate = rep(0,length(nasa))
  for( u in units){
    u_section = nasa[nasa$V1==u,]
    u_arima_result = c()
    u_boxtest_result = c()
    for( col in c(3:(length(u_section)))){
      series = u_section[,col]
      arima_model = auto.arima(series)
      arima_residuals = arima_model$residuals
      boxtest = Box.test(arima_residuals,lag = 20,type = "Ljung-Box")
      u_arima_result = append(u_arima_result,
                              sprintf("(%g %g %g)",
                                      arima_model$arma[1],
                                      arima_model$arma[length(arima_model$arma)-1],
                                      arima_model$arma[2]))
      #u_boxtest_result = append(u_boxtest_result,sprintf("%f",boxtest$p.value))
      u_boxtest_result = append(u_boxtest_result,boxtest$p.value)
      
    }
    print(sprintf("------the %g unit-------",u))
    print(colnames(u_section)[-c(1:2)])
    print(u_arima_result)
    print(u_boxtest_result)
    print("----------------------------------")
    
    ## plot the smallest 3 picture
    tmp_pvalue = na.omit(u_boxtest_result) ## just omit the NAN value,
    if(!is.null(constant_cols)){
      tmp_pvalue = na.omit(u_boxtest_result[-constant_cols]) ## omit the NAN and constant columns
    }
    min_pvalues = c()
    min_indexs = c()
    for (i in c(1:3)) {
      min_pvalue = min(tmp_pvalue)
      cur_col = match(min_pvalue,u_boxtest_result)
      cur_col_original = cur_col+2; # add shift
      culmulate[cur_col_original] = culmulate[cur_col_original] + 1 
      series = u_section[,cur_col_original]
      
      png_path = sprintf("%s/uinit%g_r%g_v%g.png",dir_output,u,i,(cur_col_original))
      png(filename = png_path,width = 2048,height = 768)
      sliding_window(series,length(series)*length_rate)
      dev.off()
      
      min_pvalues = append(min_pvalues,u_boxtest_result[cur_col])
      min_indexs = append(min_indexs,(cur_col_original))
      tmp_pvalue = tmp_pvalue[-c(which.min(tmp_pvalue))]
    }
    
    print("----------------------------------")
    print("the 3 smallest param:")
    print(min_pvalues)
    print(min_indexs)
    print("----------------------------------")
    cat("\n\n\n")
  
  }
  print("-----------------the times of each parameter become the indicator -----------------------")
  print( names(nasa) )
  print( culmulate )
  print("-----------------------------------------------------------------------------------------")
  unlink(sprintf("%s_result.txt",file_name))
  sink()
  
}