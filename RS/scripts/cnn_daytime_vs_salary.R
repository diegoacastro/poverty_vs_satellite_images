### Created by: Diego Afonso de Castro
### Date: 25/07/2019
### Objective: evaluate relation between CNN VGG16 features and average salary

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(parallel)
library(doParallel)


# Import data -------------------------------------------------------------

images_salary <- data.table::fread("input/model/google_image_features_cnn.csv") %>% 
  as.data.frame() %>% 
  rename(average_wage = V4097) %>% 
  mutate(average_wage = average_wage/12, # Change to monthly salary instead of annual
         average_wage_log = log(average_wage))


# Exclude features with near zero var to apply PCA (avoiding overfitting) ---

near_zero_var <- nearZeroVar(images_salary)

images_salary <- images_salary %>% 
  select(-near_zero_var) %>% 
  mutate(label_class = ifelse(average_wage <= 1562.858, # Median of average monthly wage
                              "low salary", 
                              "high salary"))

# Evaluate Regression -------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(images_salary[1:4034], images_salary$average_wage, 
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

cv_fit <- train(images_salary[1:4034], images_salary$average_wage_log, 
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

cv_fit <- train(images_salary[1:4034], images_salary$average_wage_log, 
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
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_salary$label_class), 
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
  
  
  cv_fit <- train(images_salary[, 1:4034], images_salary$label_class, 
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
                             positive = "low salary")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Logistic Regression (Scaled features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_salary$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(images_salary[, 1:4034]), images_salary$label_class, 
                  method = "glmnet", 
                  family="binomial",
                  preProcess = c("pca", "center", "scale"),
                  trControl = ctrl,
                  metric = "Accuracy",
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = 10^seq(3, -2, by = -.1)))
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low salary")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Decision Tree (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_salary$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(images_salary[, 1:4034]), images_salary$label_class, 
                  method = "rpart",
                  preProcess = c("pca"),
                  trControl = ctrl,
                  metric = "Accuracy")
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low salary")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Decision Tree (Scaled features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_salary$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(images_salary[, 1:4034]), images_salary$label_class, 
                  method = "rpart",
                  preProcess = c("pca", "center", "scale"),
                  trControl = ctrl,
                  metric = "Accuracy")
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low salary")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}


# Boosted decision tree (level features)

for(i in c("no sampling", "down", "up", "smote")){
  
  set.seed(123)
  
  stratified_kfold <- createFolds(y = factor(images_salary$label_class), 
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
  
  
  cv_fit <- train(as.data.frame(images_salary[, 1:4034]), images_salary$label_class, 
                  method = "xgbTree", 
                  preProcess = c("pca"),
                  trControl = ctrl,
                  verbose = 0)
  
  results <- confusionMatrix(cv_fit$pred$pred, 
                             cv_fit$pred$obs, 
                             positive = "low salary")
  
  print(paste("Accuracy", i, ":", results$overall[1]))
  print(paste("F1 Score", i, ":", results$byClass[7]))  
  
  print(results$table)
  cat("\n")
  
}

stopCluster(cluster)
registerDoSEQ()