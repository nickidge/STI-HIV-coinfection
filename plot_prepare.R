## create data frames for plotting

prepare_plots <- function(SID_list){
  y <<- SID_list[[1]]
  HIV <<- SID_list[[2]]
  sti <<- SID_list[[3]]
  prev <<- SID_list[[4]]
  first_year <<- as.numeric(head(dimnames(SID_list[[1]])[[1]], n=1))[1]
  # last_year <<- tail(as.numeric(tail(dimnames(SID_list[[1]])[[1]])), 1)
  last_year <<- as.numeric(tail(dimnames(SID_list[[1]])[[1]], n=1))[1]
  
  y_df = as.data.frame.table(SID_list[[1]])
  HIV_df = as.data.frame(SID_list[[2]])
  sti_df = as.data.frame.table(SID_list[[3]])
  prev_df = as.data.frame.table(SID_list[[4]])
  
  #HIV
  
  HIV_df = cbind(rownames(HIV_df), data.frame(HIV_df, row.names=NULL))
  colnames(HIV_df)[1] = "year"
  
  colnames(y_df) = c("year", "HIV_group", "sti_group", "value")
  y_HIV_df = subset(y_df, sti_group == "pop_sti")
  
  
  y_HIV_df$N = factor("model")
  HIV_df$N = factor("model")
  
  y_HIV_df$year = as.numeric(as.character(y_HIV_df$year))
  HIV_df$year = as.numeric(as.character(HIV_df$year))
  
  y_HIV_df <<- y_HIV_df
  HIV_df <<- HIV_df
  
  #sti
  
  colnames(sti_df) = c("year", "HIV_group", "sti_group", "value")
  y_sti_df = subset(y_df, (HIV_group == "pop_HIV" | HIV_group=="HIV_minus" | HIV_group=="HIV_plus"))
  
  y_sti_df$N = factor("model")
  sti_df$N = factor("model")
  
  y_df$year = as.numeric(as.character(y_df$year))
  y_sti_df$year = as.numeric(as.character(y_sti_df$year))
  sti_df$year = as.numeric(as.character(sti_df$year))
  
  y_df <<- y_df
  y_sti_df <<- y_sti_df
  sti_df <<- sti_df
  
  
  #prevalence
  colnames(prev_df) = c("year", "prev_group", "value")
  prev_df$N = factor("model")
  prev_df$year = as.numeric(as.character(prev_df$year))
  prev_df <<- prev_df
  
}

prepare_plots(SID_list)
