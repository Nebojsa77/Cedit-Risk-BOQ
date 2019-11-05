# Function takes a data frame, the target variable (dataframe$column_name) and the training
# percent (0 - 1) and returns two data frames, one containing the training data set and the
# other containing the testing data set based on the parameters provided.

# Requires installing / loading the caret package.

library(caret)

split_data <- function(data_frame, target_var, train_percent){
  # set seed so it's reproducable
  set.seed (100)
  
  # call function from the caret package
  inTrain <- createDataPartition(y = target_var,
                                 p = train_percent,
                                 list = FALSE)
  
  # Create data frames
  training <- data_frame[inTrain,]
  testing <- data_frame[-inTrain,]
  
  # Return list with two data frames
  df_list <- list(training, testing)
  return(df_list)
}

plot_cat_var<-function(dframe,col,scale=25000){
  
  agg<-( do.call(data.frame,aggregate(repay_fail ~ dframe[,col], data = dframe, FUN = function(x) c(mn = mean(x), n = length(x) ) )))
  names(agg)<-c(col,'fail_rate','Distribution')
  
  p<-ggplot(agg,aes_string(x=col))+
    geom_col(aes(y=Distribution), fill = "lightskyblue3")+
    geom_point(aes(y=fail_rate*scale),size=2)+
    scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Repay failure rate"))+
    theme_bw()+
    labs(title = paste(col," vs repay failure rate"))+
    geom_hline(yintercept=mean(dframe$repay_fail)*scale,col='red', alpha = 0.25)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return( p)
}

plot_cont_var<-function(dframe,col,bins,scale=25000){
  df<-dframe
  colmin=0
  colmax=quantile(df[col], 0.99,na.rm = T)
  colmaxmax=max(df[col],na.rm = T)
  
  #quantile breaks
  #brs<-quantile(df[[col]],seq(0,100,round(100/bins))/100,na.rm = T)
  
  #equal breaks
  brs<-append(seq(0,colmax,round(colmax/bins)),colmaxmax)
  brs<-unique(brs)
  #print(brs)
  
  df[col]<-cut(df[[col]], breaks =brs ,dig.lab=10)
  
  agg<-( do.call(data.frame,aggregate(repay_fail ~ df[,col], data = df, FUN = function(x) c(mn = mean(x), n = length(x) ) )))
  names(agg)<-c(col,'fail_rate','Distribution')
  
  p<-ggplot(agg,aes_string(x=col))+
    geom_col(aes(y=Distribution), fill = "lightskyblue3")+
    geom_point(aes(y=fail_rate*scale),size=2)+
    scale_y_continuous(sec.axis = sec_axis(~./scale, name = "Repay failure rate"))+
    theme_bw()+
    labs(title = paste(col," vs repay failure rate"))+
    geom_hline(yintercept=mean(df$repay_fail)*scale,col='red', alpha = 0.25)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return( p)
}

# Function that takes data frame, y-value and x-value and returns a binomial model
bi_model<-function(data, y, x){
  df <- data
  m <- glm(data = df, sprintf("%s ~ %s", y, x), family = binomial("logit"))
  rm(df)
  return(m)
}


# Function that takes a data frame and variable and returns a histogram
plot_hist<-function(data, x){
  df <- data
  
  hist_plot <- ggplot(data = df, (aes_string(x = x)))+
    geom_histogram(fill = "lightskyblue3", colour = "lightskyblue4")
  
  rm(df)
  return(hist_plot)
}
