## BSTS Regression
# Initial dataset creation to loop stuff later

type <- c("Neither", "Both", "Only Season", "Only Dyn Reg")
mdl_cnt <- c(1, 2, 3, 4)
mnth_label <- c("August", "September", "October")
test_mnth <- c(8, 9, 10)

mnth_link <- data.frame(mnth_label, test_mnth)
model_link <- data.frame(mdl_cnt, type)

# rolled_up <- lazy_dt(setDT(rolled_up), immutable = FALSE)

ff <- rolled_up %>% 
  rowwise() %>% 
  dplyr::select(Date) %>% 
  distinct(Date) %>% 
  filter(Date > 'Jun 2019', Date < 'Oct 2019') #%>% 

mnth_link$train_max_mnth <- ff$Date
mnth_link$row_count <- row.names(mnth_link)
model_link$row_count <- row.names(model_link)

# dec1$row_count <- rownames(dec1)
data_subset <- dec1 %>% 
  filter(count_months > 6) %>% 
  filter(Region == "East1") %>% 
  mutate(row_count = row_number(cust_sku))#, 
         # as.numeric(row_count) < 11)

rm(ff)
required_variables <- read_csv("Data\\variable_selection.csv", col_types = cols())
# iteration_nos_not_run <- NULL

for(i in 1:nrow(data_subset)){
  
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
    # tidyr::fill(lag_qty, lag_fr, lag_sku_sal, lag_n_dist, lag_order_price, lag_order_price, lag_discount, .direction = 'up') %>% 
    # as.data.table()
  
  # ads_1 <- subset(rolled_up, cust_sku %in% input$cust_sku & Region %in% input$Region)
  # ads_1 <- ads_1 %>% filter(!is.na(order_qty))
  
  # Imputing NA's to next value for certain variables
  ads_1 <- as.data.frame(ads_1) %>%
    fill(lag_qty, lag_fr, lag_sku_sal, lag_n_dist, lag_order_price, lag_order_price, lag_discount, .direction = 'up')
  
  # variable selection
  # ads_1 <- lazy_dt(setDT(ads_1), immutable = FALSE)
  ads_2 <- ads_1 %>% 
    dplyr::select(required_variables$Colnames[required_variables$Required == 1]) %>% 
    dplyr::select(-c("year","month"))
  
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
  
  # bsts testing
  # splitting into test and train
  
  for(k in 1:nrow(mnth_link)){
    cat("Month-k=",k, "\n")
    
    input_mnth <- mnth_link %>% 
      filter(row_count == k) %>% 
      mutate(test_monyear = as.yearmon(paste("2019", test_mnth), "%Y %m"))
    
    test <- ads_2 %>% 
      filter(Date == input_mnth$test_monyear) %>% 
      dplyr::select(-Date)
    
    train <- ads_2 %>% 
      filter(Date < input_mnth$train_max_mnth)  %>%
      dplyr::select(-c("Date")) %>% 
      #removing columns with only one value
      select_if(~n_distinct(.) > 1)
    
    #removing columns with only one value
    # train <- train %>% select_if(~n_distinct(.) > 1)
    # train <- train[-1,]
    
    for(j in 1:nrow(model_link)){
      cat("model type -j=",j, "\n")
      
      input_mdl <- model_link %>% filter(row_count==j)
      
      if(j==1) {
        model_components <- list()
        model_components <- AddLocalLinearTrend(model_components, y = train$order_qty)
        model_components <- AddStudentLocalLinearTrend(model_components, y = train$order_qty)
        #model_components <- AddSeasonal(model_components, y = train$order_qty, nseasons = 12)
        # model_components <- AddAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        # model_components <- AddAutoAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        #model_components <- AddDynamicRegression(model_components, order_qty ~ ., data = train)
      }
      if(j==2){
        model_components <- list()
        model_components <- AddLocalLinearTrend(model_components, y = train$order_qty)
        model_components <- AddStudentLocalLinearTrend(model_components, y = train$order_qty)
        model_components <- AddSeasonal(model_components, y = train$order_qty, nseasons = 12)
        # model_components <- AddAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        # model_components <- AddAutoAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        model_components <- AddDynamicRegression(model_components, order_qty ~ ., data = train)
      }
      
      if(j==3){
        model_components <- list()
        model_components <- AddLocalLinearTrend(model_components, y = train$order_qty)
        model_components <- AddStudentLocalLinearTrend(model_components, y = train$order_qty)
        model_components <- AddSeasonal(model_components, y = train$order_qty, nseasons = 12)
        # model_components <- AddAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        # model_components <- AddAutoAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        # model_components <- AddDynamicRegression(model_components, order_qty ~ ., data = train)
      }
      
      if(j==4){
        model_components <- list()
        model_components <- AddLocalLinearTrend(model_components, y = train$order_qty)
        model_components <- AddStudentLocalLinearTrend(model_components, y = train$order_qty)
        #model_components <- AddSeasonal(model_components, y = train$order_qty, nseasons = 12)
        # model_components <- AddAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        # model_components <- AddAutoAr(model_components, y = train$order_qty, lags = 2) # to specificy the Auto Regressive Lag
        model_components <- AddDynamicRegression(model_components, order_qty ~ ., data = train)
      }
      
      # fit <- 0 
      
      # tryCatch({
      #   val_return <- withTimeout(
      #     {
      #       fit <- bsts(order_qty ~ ., 
      #                  state.specification = model_components, 
      #                  niter = 2000, 
      #                  # family = 'poisson', 
      #                  data = train, 
      #                  ping = 0,
      #                  expected.model.size = 10)
      #     }, 
      #     timeout = 60)}, 
      #   TimeoutException = function(ex) cat("Timed out\n"))
      
      fit <- bsts(order_qty ~ .,
                  state.specification = model_components,
                  niter = 2000,
                  # family = 'poisson',
                  data = train,
                  ping = 0,
                  expected.model.size = 10, 
                  model.options = BstsOptions(timeout.seconds = 60))
      
      # if(is.vector(fit)) {
      #   iteration_nos_not_run <- c(0, iteration_nos_not_run)
      # } else {
        
        pred <- predict(fit, 
                        horizon = 3, 
                        burn = 1000,
                        quantiles = c(.1, .9),
                        newdata = subset(test, select =-c(order_qty)))
        
        # pred$mean
        # pred$median
        #exp(pred$median)
        
        ### Get the number of burn-ins to discard
        #burn <- SuggestBurn(0.1, fit)
        
        actuals_preds <- data.frame(cbind(actuals=test$order_qty,predicteds=pred$median))
        
        # actuals_preds <- lazy_dt(actuals_preds, immutable = FALSE)
        
        actuals_preds <- actuals_preds %>% 
          mutate(mape = abs(actuals - predicteds)/actuals, 
                 iteration = i, 
                 cust_sku = input$cust_sku, 
                 Region = input$Region, 
                 type = input_mdl$type, 
                 category = input$Category, 
                 month = input_mnth$mnth_label) #%>% 
        # as.data.table()
        
        # actuals_preds$mape = abs(actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals
        # actuals_preds$mape_ovrall = sum(abs(actuals_preds$predicteds - actuals_preds$actuals))/sum(actuals_preds$actuals)
        # actuals_preds$iteration<-i
        # actuals_preds$sku<-input$cust_sku
        # actuals_preds$Region<-input$Region
        # actuals_preds$Category<-input$Category
        # actuals_preds$Decilw<-input$dec
        # actuals_preds$month<-input_mnth$mnth_label
        # actuals_preds$type<-input_mdl$type
        
        ifelse(!exists("actual_final"), 
               actual_final <- actuals_preds, 
               actual_final <- rbind(actual_final, actuals_preds))
        
        both <- actual_final
         
      # }
    }
  }
}

both<-actual_final
actual_final<-actual_final %>% arrange(cust_sku,type,month)

write.csv(actual_final,"Outputs\\Modelling\\BSTS_results_Dec1_East1_20191214.csv", row.names = FALSE)
write.csv(data_subset, "Outputs\\Modelling\\iteration_nos_20191214.csv", row.names = FALSE)
