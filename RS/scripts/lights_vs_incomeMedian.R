### Created by: Diego Afonso de Castro
### Date: 15/07/2019
### Objective: evaluate relation between lights (mean, median, max, min, std) 
###            and income using only lights selected

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/download_coordinates.txt")

income_data <- data.table::fread("input/model/income_median_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code = code, income)


# Join DFs and get the mean radiance per city -----------------------------
income_lights <- lights %>% 
  left_join(., income_data, by = "city_code") %>% 
  filter(!is.na(income)) %>% 
  group_by(city_code) %>% 
  summarise(radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance),
            income = min(income),
            income_log = log(income)) %>% 
  ungroup() %>% 
  mutate(label_class = ifelse(income <= 450, # Median of median income
                              "low income", 
                              "high income")) %>%
  select(-city_code)

income_lights_scaled <- income_lights %>% 
  mutate_each_(list(~scale(., center = TRUE, scale = TRUE) %>% as.vector), 
               vars=c("radiance_mean","radiance_median","radiance_max",
                      "radiance_min","radiance_std"))

# Check cities in the shapefile that dont have gdp data
setdiff(unique(lights$city_code), income_data$city_code)

# Check cities that have gdp data and are not in the shapefile 
setdiff(income_data$city_code, unique(lights$city_code))

# Clean enviroment
rm(lights, income_data)
gc()


# Plot relation -----------------------------------------------------------

# Level variables
ggplot(income_lights, aes(x = radiance_mean, y = income)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(income_lights, aes(x = radiance_median, y = income)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

# Log income
ggplot(income_lights, aes(x = radiance_mean, y = income_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(income_lights, aes(x = radiance_median, y = income_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

# Scaled variables and log income
ggplot(income_lights_scaled, aes(x = radiance_mean, y = income_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(income_lights_scaled, aes(x = radiance_median, y = income_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Evaluate Regression ------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(as.data.frame(income_lights[, 1:5]), income_lights$income, 
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

cv_fit <- train(as.data.frame(income_lights[, 1:5]), income_lights$income_log, 
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

cv_fit <- train(as.data.frame(income_lights_scaled[, 1:5]), income_lights_scaled$income_log, 
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


# Evaluate Classification -------------------------------------------------

# Logistic Regression (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(income_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(income_lights[, 1:5]), income_lights$label_class, 
                  method = "glmnet", 
                  family="binomial",
                  trControl = ctrl,
                  metric = "Accuracy",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = 10^seq(3, -2, by = -.1)))
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low income")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Logistic Regression (Scaled features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(income_lights_scaled$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(income_lights_scaled[, 1:5]), income_lights_scaled$label_class, 
                  method = "glmnet", 
                  family="binomial",
                  trControl = ctrl,
                  metric = "Accuracy",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = 10^seq(3, -2, by = -.1)))
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low income")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Decision Tree (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(income_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(income_lights[, 1:5]), income_lights$label_class, 
                  method = "rpart",
                  trControl = ctrl,
                  metric = "Accuracy")
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low income")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Decision Tree (Scaled features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(income_lights_scaled$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(income_lights_scaled[, 1:5]), income_lights_scaled$label_class, 
                  method = "rpart",
                  trControl = ctrl,
                  metric = "Accuracy")
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low income")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Boosted decision tree (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(income_lights$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(income_lights[, 1:5]), income_lights$label_class, 
                  method = "xgbTree", 
                  trControl = ctrl,
                  verbose = 0)
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low income")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}
