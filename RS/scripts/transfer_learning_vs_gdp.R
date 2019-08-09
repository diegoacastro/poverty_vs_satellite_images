### Created by: Diego Afonso de Castro
### Date: 25/07/2019
### Objective: evaluate transfer learning model on gdp per capita

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(parallel)
library(doParallel)


# Import data -------------------------------------------------------------

images_salary <- data.table::fread("input/model/google_image_features_cnn_transfer.csv") %>% 
  as.data.frame()

salary_data <- data.table::fread("input/model/average_salary_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code, average_wage) %>% 
  mutate(average_wage = as.character(average_wage))

gdp_data <- data.table::fread("input/model/gdp_per_capita_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code, gdp_per_capita)


# Join dataframes ---------------------------------------------------------

images_gdp <- images_salary %>% 
  mutate(salary_key = as.character(V4097)) %>%
  left_join(., salary_data, by = c("salary_key" = "average_wage")) %>% 
  left_join(., gdp_data, by = "city_code") %>% 
  select(-c("V4097", "salary_key", "city_code")) %>% 
  mutate(gdp_per_capita_log = log(gdp_per_capita))


# Exclude features with near zero var to apply PCA (avoiding overfitting) ---

near_zero_var <- nearZeroVar(images_gdp)

images_gdp <- images_gdp %>% 
  select(-near_zero_var) %>% 
  mutate(label_class = ifelse(gdp_per_capita <= 15.8695, # Median of gdp per capita
                              "low gdp", 
                              "high gdp"))


# Evaluate Regression -------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(images_gdp[1:4096], images_gdp$gdp_per_capita, 
                method = "glmnet", 
                preProcess = c("pca"),
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         savePredictions = "final",
                                         preProcOptions = list(pcaComp = 100)),
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

cv_fit <- train(images_gdp[1:4096], images_gdp$gdp_per_capita_log, 
                method = "glmnet", 
                preProcess = c("pca"),
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         savePredictions = "final",
                                         preProcOptions = list(pcaComp = 100)),
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

cv_fit <- train(images_gdp[1:4096], images_gdp$gdp_per_capita_log, 
                method = "glmnet", 
                preProcess = c("pca", "center", "scale"),
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         savePredictions = "final",
                                         preProcOptions = list(pcaComp = 100)),
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

cluster <- makeCluster(detectCores())
registerDoParallel(cluster)


for(i in c("no sampling", "down", "up", "smote")){
  # for(i in c("up")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_gdp$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         preProcOptions = list(pcaComp = 100),
                         savePredictions = "final",
                         allowParallel = TRUE)
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         preProcOptions = list(pcaComp = 100),
                         savePredictions = "final",
                         allowParallel = TRUE,
                         sampling = i)
    
  }
  
  
  cv_fit <- train(images_gdp[, 1:4096], images_gdp$label_class, 
                  method = "glmnet", 
                  family="binomial",
                  preProcess = c("pca"),
                  trControl = ctrl,
                  metric = "Accuracy",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = 10^seq(3, -2, by = -.1))
  )
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low gdp")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# # Logistic Regression (Scaled features)
# 
# for(i in c("no sampling", "down", "up", "smote")){
#   
#   set.seed(123)
#   
#   stratified_kfold <- createFolds(y = factor(images_gdp$label_class), 
#                                   k = 10, 
#                                   returnTrain = TRUE)
#   
#   if(i == "no sampling"){
#     
#     ctrl <- trainControl(index = stratified_kfold,
#                          method="cv", 
#                          number=10, 
#                          savePredictions = "final",
#                          allowParallel = TRUE)
#     
#   } else {
#     
#     ctrl <- trainControl(index = stratified_kfold,
#                          method="cv", 
#                          number=10, 
#                          savePredictions = "final",
#                          allowParallel = TRUE,
#                          sampling = i)
#     
#   }
#   
#   
#   cv_fit <- train(as.data.frame(images_gdp[, 1:4096]), images_gdp$label_class, 
#                   method = "glmnet", 
#                   family="binomial",
#                   preProcess = c("pca", "center", "scale"),
#                   trControl = ctrl,
#                   metric = "Accuracy",
#                   tuneGrid = expand.grid(alpha = 0,
#                                          lambda = 10^seq(3, -2, by = -.1)))
#   
#   results <- confusionMatrix(cv_fit$pred$pred, 
#                              cv_fit$pred$obs, 
#                              positive = "low gdp")
#   
#   print(paste("Accuracy", i, ":", results$overall[1]))
#   print(paste("F1 Score", i, ":", results$byClass[7]))  
#   
#   print(results$table)
#   cat("\n")
#   
# }


# Decision Tree (level features)

for(i in c("no sampling", "down", "up", "smote")){
  # for(i in c("down", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_gdp$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         allowParallel = TRUE)
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         allowParallel = TRUE,
                         sampling = i)
    
  }
  
  
  cv_fit <- train(as.data.frame(images_gdp[, 1:4096]), images_gdp$label_class, 
                  method = "rpart",
                  preProcess = c("pca"),
                  trControl = ctrl,
                  metric = "Accuracy")
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low gdp")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# # Decision Tree (Scaled features)
# 
# for(i in c("no sampling", "down", "up", "smote")){
#   
#   set.seed(123)
#   
#   stratified_kfold <- createFolds(y = factor(images_gdp$label_class), 
#                                   k = 10, 
#                                   returnTrain = TRUE)
#   
#   if(i == "no sampling"){
#     
#     ctrl <- trainControl(index = stratified_kfold,
#                          method="cv", 
#                          number=10, 
#                          savePredictions = "final",
#                          allowParallel = TRUE)
#     
#   } else {
#     
#     ctrl <- trainControl(index = stratified_kfold,
#                          method="cv", 
#                          number=10, 
#                          savePredictions = "final",
#                          allowParallel = TRUE,
#                          sampling = i)
#     
#   }
#   
#   
#   cv_fit <- train(as.data.frame(images_gdp[, 1:4096]), images_gdp$label_class, 
#                   method = "rpart",
#                   preProcess = c("pca", "center", "scale"),
#                   trControl = ctrl,
#                   metric = "Accuracy")
#   
#   results <- confusionMatrix(cv_fit$pred$pred, 
#                              cv_fit$pred$obs, 
#                              positive = "low gdp")
#   
#   print(paste("Accuracy", i, ":", results$overall[1]))
#   print(paste("F1 Score", i, ":", results$byClass[7]))  
#   
#   print(results$table)
#   cat("\n")
#   
# }


# Boosted decision tree (level features)

#for(i in c("no sampling", "down", "up", "smote")){
for(i in c("up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_gdp$label_class), 
                                  k = 10, 
                                  returnTrain = TRUE)
  
  if(i == "no sampling"){
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         allowParallel = TRUE)
    
  } else {
    
    ctrl <- trainControl(index = stratified_kfold,
                         method="cv", 
                         number=10, 
                         savePredictions = "final",
                         allowParallel = TRUE,
                         sampling = i)
    
  }
  
  
  cv_fit <- train(as.data.frame(images_gdp[, 1:4096]), images_gdp$label_class, 
                  method = "xgbTree", 
                  preProcess = c("pca"),
                  trControl = ctrl,
                  verbose = 0)
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low gdp")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}

stopCluster(cluster)
registerDoSEQ()