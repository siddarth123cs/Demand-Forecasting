library(TSstudio)
library(forecast)
library(forecastHybrid)

rm(list = ls())
setwd("E:\\Clients\\Godrej\\Forecasting\\Nigeria")

rolled_up <- read_csv("Outputs\\Modelling\\final_data_4.csv") %>% 
  mutate(Date = as.yearmon(Date))
  
roll_final_1 <- read_csv("Outputs\\Modelling\\file_to_loop_thru_3.csv")

dec1 <- roll_final_1 %>% 
  filter(dec=="1") #, !cov=='NaN') 

mnth_label <- c("August","September","October")
test_mnth <- c(8,9,10)

mnth_link <- data.frame(mnth_label,test_mnth)

ff <- rolled_up %>% 
  # mutate(Date = as.yearmon(Date)) %>%
  rowwise() %>% 
  dplyr::select(Date) %>% 
  distinct(Date) %>% 
  filter(Date > 'Jun 2019', Date < 'Oct 2019') #%>% 

mnth_link$train_max_mnth <- ff$Date
mnth_link$row_count <- row.names(mnth_link)
# model_link$row_count <- row.names(model_link)

data_subset <- dec1 %>% 
  filter(count_months > 6) %>% 
  filter(Region == "East1") %>% 
  mutate(row_count = row_number(cust_sku))#, 
# as.numeric(row_count) < 11)

rm(ff)
required_variables <- read_csv("Data\\variable_selection.csv", col_types = cols())

i <- 1
k <- 1

for(i in 1:nrow(data_subset)) {
  
  cat("iteration-i=",i, "\n")
  if(i == 1 && exists("actual_final")) {
    rm(actuals_preds, actual_final, ads_1, ads_2)
  }
  
  input <- data_subset %>% 
    filter(row_count == i)# %>% 
  # as.data.table()
  
  ads_1 <- rolled_up %>% 
    filter(cust_sku %in% input$cust_sku, Region %in% input$Region) %>%
    filter(!is.na(order_qty)) #%>% 
  
  # Imputing NA's to next value for certain variables
  ads_1 <- as.data.frame(ads_1) %>%
    fill(lag_qty, lag_fr, lag_sku_sal, lag_n_dist, lag_order_price, lag_order_price, lag_discount, .direction = 'up')
  
  # variable selection
  ads_2 <- ads_1 %>% 
    dplyr::select(required_variables$Colnames[required_variables$Required == 1])
  
  ads_2$darling_relaunch_flag <- as.factor(ads_2$darling_relaunch_flag )
  ads_2$jan_feb_fire_flg <- as.factor(ads_2$jan_feb_fire_flg)
  ads_2$Mar_Apr_fire_flg <- as.factor(ads_2$Mar_Apr_fire_flg)
  ads_2$preload_flg <- as.factor(ads_2$preload_flg)
  ads_2$promo_flag <- as.factor(ads_2$promo_flag)
  
  #removing columns with only zeroes
  ads_2<-ads_2 %>% select_if(~n_distinct(.) > 1)
  
  # removing date column from the ads
  ads_2_date <- ads_2$Date
  ads_2$Date <- NULL
  
  # imputing NA values with median
  ads_2 <- do.call(data.frame, lapply(ads_2, function(x) replace(x, is.infinite(x), NA)))
  ads_2 <- na.roughfix(ads_2)
  
  # adding back date column in ads
  ads_2$Date <- ads_2_date
  
  ads_2 <- ads_2 %>% 
    dplyr::select(Date, order_qty, productwise_L3M_IYA, categorywise_L3M_qty, categorywise_trend_L3M_over_L6M, 
                  lag_n_dist, LYSM_Ndist, dist_trend_L3M_L6M, preload_flg, promo_flag, darling_relaunch_flag, 
                  lag_fr, LYSm_FR, FR_trend_L3M_L6M, lag_order_price, lag_discount, jan_feb_fire_flg, Mar_Apr_fire_flg, 
                  lag_sku_sal, LYSm_sku_sal, Sal_trend_L3M_L6M)
  
  
  # mat_ads_2 <- matrix(ads_2_xreg)
  
  for(k in 1:nrow(mnth_link)){
    cat("Month-k=",k, "\n")
    
    input_mnth <- mnth_link %>% 
      filter(row_count == k) %>% 
      mutate(test_monyear = as.yearmon(paste("2019", test_mnth), "%Y %m"))
    
    test <- ads_2 %>% 
      filter(Date >= input_mnth$train_max_mnth, 
             Date <= input_mnth$test_monyear) #%>% 
    # dplyr::select(-Date)
    
    train <- ads_2 %>% 
      filter(Date < input_mnth$train_max_mnth)  #%>%
    # select_if(~n_distinct(.) > 1)
    # dplyr::select(-c("Date")) %>% 
    #removing columns with only one value
    
    trainXreg <- train %>% 
      dplyr::select(-Date, -order_qty)
    
    trainXreg <- matrix(as.numeric(unlist(trainXreg)),nrow=nrow(trainXreg))
    
    testXreg <- test %>% 
      dplyr::select(-Date, -order_qty)
    
    testXreg <- matrix(as.numeric(unlist(testXreg)),nrow=nrow(testXreg))
    
    # convert to ts object
    ts_train <- ts(train$order_qty, start = min(train$Date), end = max(train$Date), frequency = 12)
    
    # ts_test <- ts(ads_2$order_qty, start = as.yearmon("2019-08-01"), end = max(ads_2$Date), frequency = 12)
    # model <- auto.arima(ts_train, trace=TRUE, ic="aicc", approximation = FALSE, stepwise = TRUE)
    
    model <- tryCatch({
      model <- hybridModel(ts_train, 
                           models = "aefnst",
                           weights="equal", 
                           a.args = list(xreg = trainXreg),
                           n.args = list(xreg = trainXreg),
                           s.args = list(xreg = trainXreg, method = "arima"),
                           verbose = FALSE)
    },
    error = function(cond){ # when error for thetam is shown
      model <- hybridModel(ts_train, 
                           models = "aenst",
                           weights="equal", 
                           # a.args = list(xreg = trainXreg),
                           n.args = list(xreg = trainXreg),
                           # s.args = list(xreg = trainXreg, method = "arima"),
                           verbose = FALSE)
      return(model)
    })
    
    # model <- hybridModel(ts_train, 
    #                      models = "aefnst",
    #                      weights="equal", 
    #                      verbose = FALSE)
    # as.data.frame(fitted(model))$x[13:24]
    forecast_data <- as.data.frame(forecast(model, h=2, xreg = testXreg))$`Point Forecast`
    actuals_preds <- data.frame(cbind(actuals = test$order_qty[2], predicteds = forecast_data[2]))
    
    actuals_preds <- actuals_preds %>% 
      mutate(mape = abs(actuals - predicteds)/actuals, 
             iteration = i, 
             cust_sku = input$cust_sku, 
             Region = input$Region, 
             # type = input_mdl$type, 
             category = input$Category, 
             month = input_mnth$mnth_label) # change this part of the code to adjust for month & everything
    
    ifelse(!exists("actual_final"), 
           actual_final <- actuals_preds, 
           actual_final <- rbind(actual_final, actuals_preds))
    
    both <- actual_final  
  }
}

both <- actual_final
actual_final<-actual_final %>% arrange(cust_sku,type,month)

write.csv(actual_final,"Outputs\\Modelling\\Hybrid_ts_xreg_results_Dec1_East1_20191215.csv", row.names = FALSE)
write.csv(data_subset, "Outputs\\Modelling\\iteration_nos_20191215.csv", row.names = FALSE)


