### Created by: Diego Afonso de Castro
### Date: 31/07/2019
### Objective: evaluate relation between daytime image features (mean, median,  
###            max, min, std for each RGB) and water idx using only lights selected.

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------

features_basic = data.table::fread('input/model/google_image_features_basic.csv', 
                                   header = FALSE,
                                   sep = ' ')

lights <- data.table::fread("input/model/download_coordinates.txt",
                            colClasses = c("character", rep("numeric", 9))) %>% 
  select(row_raster, col_raster, city_code)

water_data <- data.table::fread("input/model/water_index.txt", 
                                 colClasses = c(rep("character", 3), 
                                                rep("numeric", 4))) %>% 
  filter(state == "RS") %>% 
  select(city_code_reduced = city_code, water_index)


# Join dataframes ---------------------------------------------------------

images_water <- features_basic %>% 
  inner_join(., lights, by = c("V1" = "row_raster", "V2" = "col_raster")) %>% 
  select(-(1:2)) %>% 
  mutate(city_code_reduced = str_sub(city_code, 1, 6)) %>%
  select(-city_code) %>% 
  group_by(city_code_reduced) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  inner_join(., water_data, by = "city_code_reduced") %>% 
  mutate(water_index_log = log(water_index),
         label_class = ifelse(water_index <= 0.667, # Median of water index
                              "low index", 
                              "high index")) %>% 
  select(-city_code_reduced)

# Clean enviroment
rm(lights, water_data, features_basic)
gc()


# Evaluate Regression -------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$water_index, 
                method = "glmnet", 
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         savePredictions = "final"),
                metric = "Rsquared",
                tuneGrid = expand.grid(alpha = 0,
                                       lambda = 10^seq(3, -2, by = -.1)))


best_lambda <- cv_fit$bestTune[, 2]

metric_best_lambda <- cv_fit$results %>% 
  filter(lambda == best_lambda)

print(paste("R2:", metric_best_lambda$Rsquared))
print(paste("RMSE:", metric_best_lambda$RMSE))
print(paste("MAE:", metric_best_lambda$MAE))

ggplot(cv_fit$pred, aes(x = obs, y = pred)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "Darkblue", fill = "gray")


# Log income

set.seed(123)

cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$water_index_log, 
                method = "glmnet", 
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         savePredictions = "final"),
                metric = "Rsquared",
                tuneGrid = expand.grid(alpha = 0,
                                       lambda = 10^seq(3, -2, by = -.1)))


best_lambda <- cv_fit$bestTune[, 2]

metric_best_lambda <- cv_fit$results %>% 
  filter(lambda == best_lambda)

print(paste("R2:", metric_best_lambda$Rsquared))
print(paste("RMSE:", metric_best_lambda$RMSE))
print(paste("MAE:", metric_best_lambda$MAE))

ggplot(cv_fit$pred, aes(x = obs, y = pred)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "Darkblue", fill = "gray")


# Scaled variables and log income

set.seed(123)

cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$water_index_log, 
                method = "glmnet", 
                preProcess = c("center", "scale"),
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         savePredictions = "final"),
                metric = "Rsquared",
                tuneGrid = expand.grid(alpha = 0,
                                       lambda = 10^seq(3, -2, by = -.1)))


best_lambda <- cv_fit$bestTune[, 2]

metric_best_lambda <- cv_fit$results %>% 
  filter(lambda == best_lambda)

print(paste("R2:", metric_best_lambda$Rsquared))
print(paste("RMSE:", metric_best_lambda$RMSE))
print(paste("MAE:", metric_best_lambda$MAE))

ggplot(cv_fit$pred, aes(x = obs, y = pred)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "Darkblue", fill = "gray")


# Evaluate Classification -------------------------------------------------

# Logistic Regression (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_water$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final")
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         sampling = i)
    
  }
  
  
  cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$label_class, 
                  method = "glmnet", 
                  family="binomial",
                  trControl = ctrl,
                  metric = "Accuracy",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = 10^seq(3, -2, by = -.1)))
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low index")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Logistic Regression (Scaled features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_water$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final")
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         sampling = i)
    
  }
  
  
  cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$label_class, 
                  method = "glmnet", 
                  family="binomial",
                  preProcess = c("center", "scale"),
                  trControl = ctrl,
                  metric = "Accuracy",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = 10^seq(3, -2, by = -.1)))
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low index")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Decision Tree (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_water$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final")
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         sampling = i)
    
  }
  
  
  cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$label_class, 
                  method = "rpart",
                  trControl = ctrl,
                  metric = "Accuracy")
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low index")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Decision Tree (Scaled features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_water$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final")
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         sampling = i)
    
  }
  
  
  cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$label_class, 
                  method = "rpart",
                  preProcess = c("center", "scale"),
                  trControl = ctrl,
                  metric = "Accuracy")
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low index")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Boosted decision tree (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_water$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final")
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         sampling = i)
    
  }
  
  
  cv_fit <- train(as.data.frame(images_water[, 1:15]), images_water$label_class, 
                  method = "xgbTree", 
                  trControl = ctrl,
                  verbose = 0)
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low index")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}