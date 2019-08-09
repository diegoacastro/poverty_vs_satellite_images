### Created by: Diego Afonso de Castro
### Date: 31/07/2019
### Objective: evaluate relation between lights (mean, median, max, min, std) 
###            and water index using only lights selected

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/download_coordinates.txt",
                            colClasses = c("character", rep("numeric", 9)))

water_data <- data.table::fread("input/model/water_index.txt", 
                                colClasses = c(rep("character", 3), 
                                               rep("numeric", 4))) %>% 
  filter(state == "RS") %>% 
  select(city_code_reduced = city_code, water_index)


# Join DFs and get the mean radiance per city -----------------------------
water_lights <- lights %>% 
  mutate(city_code_reduced = str_sub(city_code, 1, 6)) %>% 
  left_join(., water_data, by = "city_code_reduced") %>% 
  filter(!is.na(water_index)) %>% 
  group_by(city_code) %>% 
  summarise(radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance),
            water_index = min(water_index),
            water_index_log = log(water_index)) %>% 
  ungroup() %>% 
  mutate(label_class = ifelse(water_index <= 0.667, # Median of water index
                              "low index", 
                              "high index")) %>% 
  select(-city_code)

# Clean enviroment
rm(lights, water_data)
gc()


#### Obs: 43 cities are in the shapefile but don't have water data. 
####      PINTO BANDEIRA (4314548) has water data but is not in the shapefile


# Plot relation -----------------------------------------------------------

# Level variables
ggplot(water_lights, aes(x = radiance_mean, y = water_index)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(water_lights, aes(x = radiance_median, y = water_index)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Log salary
ggplot(water_lights, aes(x = radiance_mean, y = water_index_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(water_lights, aes(x = radiance_median, y = water_index_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Evaluate Regression ------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$water_index, 
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

cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$water_index_log, 
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

cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$water_index, 
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
  
  stratified_kfold <- createFolds(y = factor(water_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$label_class, 
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
  
  stratified_kfold <- createFolds(y = factor(water_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$label_class, 
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
  
  stratified_kfold <- createFolds(y = factor(water_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$label_class, 
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
  
  stratified_kfold <- createFolds(y = factor(water_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$label_class, 
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
  
  stratified_kfold <- createFolds(y = factor(water_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(water_lights[, 1:5]), water_lights$label_class, 
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


