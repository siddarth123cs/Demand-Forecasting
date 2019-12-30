mnth_label <- c("August","September","October")
test_mnth <- c(8,9,10)

mnth_link <- data.frame(mnth_label,test_mnth)

ff <- rolled_up %>% 
  rowwise() %>% 
  dplyr::select(Date) %>% 
  distinct(Date) %>% 
  filter(Date > 'Jun 2019', Date < 'Oct 2019') #%>% 

mnth_link$train_max_mnth <- ff$Date
mnth_link$row_count <- row.names(mnth_link)
model_link$row_count <- row.names(model_link)

dec1_east$row_count <- rownames(dec1_east)
dec1_east_subset <- dec1_east %>% 
  filter(count_months >12)#, 
# as.numeric(row_count) < 11)

rm(ff)
# required_variables <- read_csv("Data\\variable_selection.csv", col_types = cols())

input <- dec1_east_subset %>% 
  filter(row_count == 1)# %>% 
# as.data.table()

ads_1 <- rolled_up %>% 
  filter(cust_sku %in% input$cust_sku, Region %in% input$Region) %>%
  filter(!is.na(order_qty)) #%>% 

# Imputing NA's to next value for certain variables
ads_1 <- as.data.frame(ads_1) %>%
  fill(lag_qty, lag_fr, lag_sku_sal, lag_n_dist, lag_order_price, lag_order_price, lag_discount, .direction = 'up')

# variable selection
# ads_2 <- ads_1 %>% 
  # dplyr::select(required_variables$Colnames[required_variables$Required == 1])

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

# splitting into test and train
test <- ads_2 %>% filter(year %in% c("2019"), 
                         month %in% c(9,8,10)) %>% 
  dplyr::select(-c("year","month","Date"))

train <- ads_2 %>% filter(Date < "Aug 2019")  %>% 
  dplyr::select(-c("year","month","Date"))

# Manual Search

library(GA)

fitness_fn <- function(vars) {
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
  tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_subset))))
  set.seed(1)
  random_ft <- train(order_qty ~ ., 
                     data = train_subset, 
                     method = "rf", 
                     metric = "RMSE", 
                     tuneGrid = tunegrid, 
                     trControl = control, 
                     ntree = 2000, 
                     importance = TRUE)  
  
}

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_subset))))
set.seed(1)
random_ft <- train(order_qty ~ ., 
             data = train_subset, 
             method = "rf", 
             metric = "RMSE", 
             tuneGrid = tunegrid, 
             trControl = control, 
             ntree = 2000, 
             importance = TRUE)

predicted_step_rf <- predict(random_ft, subset(ads_2, select =-c(order_qty)))

actuals_preds_rf <- ads_2 %>% 
  dplyr::select(Date, order_qty) %>% 
  rename(actual = order_qty)

actuals_preds_rf$predicted <- predicted_step_rf
  
actuals_preds_rf <- actuals_preds_rf %>% 
  mutate(period = ifelse(Date < "Aug 2019", "Train", "Test"), 
         actual = as.numeric(actual), 
         predicted = as.numeric(predicted), 
         mape = abs(actual - predicted)/actual)

par(lwd=2, cex=1)
plot(actuals_preds_rf$actual, type = "l", col = "green")
lines(actuals_preds_rf$predicted, type = "l", col = "red")

Imp <- varImp(random_ft, scale = TRUE)
random_ft$finalModel

write.table(actuals_preds_rf,"clipboard-16834",sep="\t",row.names = F)
write.table(Imp$importance,"clipboard-16834",sep="\t",row.names = T)

# assign("last.warning", NULL, envir = baseenv())


# xgboost
# str(ads_2)
