### Created by: Diego Afonso de Castro
### Date: 11/08/2019
### Objective: evaluate all possible models and relation between variables

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(caret)
library(parallel)
library(doParallel)


######################### FUNCTIONS ########################################

# Create main functions ----------------------------------------------------

run_few_features_regression <- function(main_data, variable_name){
  
  set.seed(123)
  
  # Create parallel cluster
  
  cluster <- makeCluster(detectCores()-1)
  registerDoParallel(cluster)
  
  
  # Split data into folds
  
  fold_ids <- createFolds(main_data[[(dim(main_data)[2])]], k = 10, returnTrain = TRUE)
  
  
  ############################ REGRESSION #################################
  
  print("REGRESSION")
  
  # DF to save results
  results_regression <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                                 c("Variable", "Model", "Detail", "R2", "RMSE", "MAE"))
  
  
  # nested cv (algorithm in https://weina.me/nested-cross-validation/)
  
  # Define parameters grid
  
  grid_search <- expand.grid(alpha = c(0, 1), standardize = c(TRUE), y_idx = c(1, 2))
  
  # Loop to test combinations of grids
  
  for(idx in 1:nrow(grid_search)) {
    
    i <- grid_search[idx, ]
    
    
    # Variables to save results
    
    R2_temp <- c()
    RMSE_temp <- c() 
    MAE_temp <- c()
    
    
    # Parameters
    
    alpha <- i[[1]]
    standardize <- i[[2]]
    y_idx <- i[[3]]
    
    
    # Names for final results
    
    model_name <- ifelse(alpha == 0, "ridge regression", "lasso regression")
    
    detail_name <- ifelse(y_idx == 1, 
                          "Features standardised and Y in log", 
                          "Features standardised and Y in level")
    
    
    # Print to follow progress
    
    print(paste(model_name, detail_name, sep = " - "))
    
    
    # Loop for nested cv (used to tune lambda and then get evaluation metrics)
    
    for(indices in fold_ids){
      
      Xval <- main_data %>% 
        slice(indices) %>% 
        select(1:(dim(main_data)[2]-3)) %>% 
        as.matrix()
      
      Yval <- main_data %>% 
        slice(indices) %>% 
        select((dim(main_data)[2]-y_idx)) %>% 
        pull()
      
      
      # Tune lambda
      
      cv_fit_inner <- train(Xval, Yval, 
                            method = "glmnet", 
                            standardize = standardize,
                            trControl = trainControl(method="cv", 
                                                     number=5, 
                                                     allowParallel = TRUE,
                                                     savePredictions = "final"),
                            metric = "RMSE",
                            tuneGrid = expand.grid(alpha = alpha,
                                                   lambda = exp(seq(-1, 5, length.out = 10))))

      
      # Train the model on the complete training set
      
      fit_outer <- train(Xval, Yval, 
                         method = "glmnet", 
                         standardize = standardize,
                         metric = "RMSE",
                         tuneGrid = expand.grid(alpha = alpha,
                                                lambda = cv_fit_inner$bestTune[[2]]))
      
      
      # Make predictions
      
      Xtest <- main_data %>% 
        slice(-indices) %>% 
        select(1:(dim(main_data)[2]-3)) %>%
        as.matrix()
      
      Ytest <- main_data %>% 
        slice(-indices) %>% 
        select((dim(main_data)[2]-y_idx)) %>% 
        pull()
      
      predictions_outer <- fit_outer %>% predict(Xtest)
      
      
      # Evaluate
      
      R2_temp <-  c(R2_temp, R2(predictions_outer, Ytest))
      RMSE_temp <- c(RMSE_temp, RMSE(predictions_outer, Ytest))
      MAE_temp <- c(MAE_temp, MAE(predictions_outer, Ytest))
      
    }
    
    
    # Get average results
    if(sum(is.na(R2_temp)) > 0) {print(paste0("R2 NA's: ", sum(is.na(R2_temp))))}
    if(sum(is.na(RMSE_temp)) > 0) {print(paste0("RMSE NA's: ", sum(is.na(RMSE_temp))))}
    if(sum(is.na(MAE_temp)) > 0) {print(paste0("MAE NA's: ", sum(is.na(MAE_temp))))}
    
    results_regression <- bind_rows(results_regression,
                                    data.frame(Variable = variable_name, 
                                               Model = model_name, 
                                               Detail = detail_name,
                                               R2 = mean(R2_temp), 
                                               RMSE = mean(RMSE_temp),
                                               MAE = mean(MAE_temp)))
    
    # Remove models to avoid errors
    
    rm(cv_fit_inner, fit_outer)
    
  }
  
  # Stop parallel cluster
  
  stopCluster(cluster)
  
  
  # Return final data frames
  
  return(results_regression)
  
}

run_few_features_classification <- function(main_data, variable_name){
  
  set.seed(123)
  
  # Create parallel cluster
  
  cluster <- makeCluster(detectCores()-1)
  registerDoParallel(cluster)
  
  
  # Split data into folds
  
  fold_ids <- createFolds(main_data[[(dim(main_data)[2])]], k = 10, returnTrain = TRUE)
  
  
  ########################## CLASSIFICATION ################################
  
  print("CLASSIFICATION")
  
  results_classification <- setNames(data.frame(matrix(ncol = 11, nrow = 0)),
                                     c("Variable", "Model", "Detail", "Sampling", 
                                       "Accuracy", "F1_low", "F1_high", 
                                       "Precision_low", "Recall_low",
                                       "Precision_high", "Recall_high"))
  
  
  #### LOGISTIC REGRESSION ####
  
  print("LOGISTIC REGRESSION")
  
  # Define parameters grid
  
  grid_search <- expand.grid(alpha = c(0, 1), standardize = c(TRUE))
  
  # Loop to test combinations of grids
  
  for(idx in 1:nrow(grid_search)) {
    
    i <- grid_search[idx, ]
    
    
    # Parameters
    
    alpha <- i[[1]]
    standardize <- i[[2]]
    
    
    # Names for final results
    
    model_name <- ifelse(alpha == 0, "logistic ridge regression", "logistic lasso regression")
    
    detail_name <- ifelse(isTRUE(standardize), "Features standardised", "Features level")
    
    
    # Print to follow progress
    
    print(paste(model_name, detail_name, sep = " - "))
    
    
    # Loop over sampling methods
    
    #for(j in c("no sampling", "down", "up", "smote")){ # In case other sampling methods are wanted
    for(j in c("up")){
      
      print(j)
      
      predictions <- as.factor(c())
      correct_labels <- as.factor(c())
      
      
      # Loop for nested cv (used to tune lambda and then get evaluation metrics)
      
      for(indices in fold_ids){
        
        Xval <- main_data %>% 
          slice(indices) %>% 
          select(1:(dim(main_data)[2]-3)) %>% 
          as.matrix()
        
        Yval <- main_data %>% 
          slice(indices) %>% 
          select((dim(main_data)[2])) %>% 
          pull()
        
        stratified_kfold <- createFolds(y = Yval, k = 5, returnTrain = TRUE)
        
        if(j == "no sampling"){
          
          ctrl <- trainControl(index = stratified_kfold,
                               method="cv", 
                               number=5, 
                               allowParallel = TRUE,
                               savePredictions = "final")
          
        } else {
          
          ctrl <- trainControl(index = stratified_kfold,
                               method="cv", 
                               number=5, 
                               allowParallel = TRUE,
                               savePredictions = "final",
                               sampling = j)
          
        }
        
        
        # Tune lambda
        
        cv_fit_inner <- train(Xval, Yval, 
                              method = "glmnet", 
                              family="binomial",
                              standardize = standardize,
                              trControl = ctrl,
                              metric = "Accuracy",
                              tuneGrid = expand.grid(alpha = alpha,
                                                     lambda = exp(seq(-1, 5, length.out = 10))))
        
        
        # Train the model on the complete training set
        
        fit_outer <- train(Xval, Yval, 
                           method = "glmnet",
                           family="binomial",
                           standardize = standardize,
                           metric = "Accuracy",
                           tuneGrid = expand.grid(alpha = alpha,
                                                  lambda = cv_fit_inner$bestTune[[2]]))
        
        
        # Make predictions
        
        Xtest <- main_data %>% 
          slice(-indices) %>% 
          select(1:(dim(main_data)[2]-3)) %>%
          as.matrix()
        
        Ytest <- main_data %>% 
          slice(-indices) %>% 
          select((dim(main_data)[2])) %>% 
          pull() %>% 
          as.factor()
        
        predictions_outer <- fit_outer %>% predict(Xtest)
        
        correct_labels <- unlist(list(correct_labels, Ytest))
        predictions <- unlist(list(predictions, predictions_outer))
        
      }
      
      # Evaluate model
      
      results_low <- confusionMatrix(predictions, correct_labels, positive = "low")
      
      results_high <- confusionMatrix(predictions, correct_labels, positive = "high")
      
      results_classification <- bind_rows(results_classification,
                                          data.frame(Variable = variable_name, 
                                                     Model = model_name, 
                                                     Detail = detail_name,
                                                     Sampling = j,
                                                     Accuracy = results_high$overall[[1]],
                                                     F1_low = results_low$byClass[[7]],
                                                     F1_high = results_high$byClass[[7]],
                                                     Precision_low = results_low$byClass[[5]],
                                                     Precision_high = results_high$byClass[[5]],
                                                     Recall_low = results_low$byClass[[6]],
                                                     Recall_high = results_high$byClass[[6]]))
      
      
      # Remove models to avoid errors
      
      rm(cv_fit_inner, fit_outer)
      
    }
    
  }
  
  
  #### DECISION TREE AND GRADIENT BOOSTING ####
  
  print("DECISION TREE AND GRADIENT BOOSTING")
  
  # Define data
  
  Xval <- main_data %>%
    select(1:(dim(main_data)[2]-3)) %>% 
    as.matrix()
  
  Yval <- main_data %>%
    select((dim(main_data)[2])) %>% 
    pull()
  
  # Define new grid
  
  grid_search <- expand.grid(model_type = c("rpart", "xgbTree"), 
                             standardize = c(TRUE, FALSE))
  
  # Run models
  
  for(idx in 1:nrow(grid_search)) {
    
    i <- grid_search[idx, ]
    
    # Parameters
    model_type <- i[[1]]
    standardize <- i[[2]]
    pre_parameters <- if (isTRUE(standardize)) c("center", "scale") else c()
    
    # Names for final result
    model_name <- ifelse(model_type == "rpart", "Decision Tree", "Gradient Boosting")
    detail_name <- ifelse(isTRUE(standardize), 
                          "Features standardised", 
                          "Features level")
    
    # Print to follow progress
    print(paste(model_name, detail_name, sep = " - "))
    
    
    # Loop for each sampling method
    # for(j in c("no sampling", "down", "up", "smote")){ # In case other sampling methods are wanted
    for(j in c("up")){
      
      print(j)
      
      if(j == "no sampling"){
        
        ctrl <- trainControl(index = fold_ids,
                             method="cv", 
                             number=10, 
                             allowParallel = TRUE,
                             savePredictions = "final")
        
      } else {
        
        ctrl <- trainControl(index = fold_ids,
                             method="cv", 
                             number=10, 
                             allowParallel = TRUE,
                             savePredictions = "final",
                             sampling = j)
        
      }
      
      
      cv_fit <- train(Xval, Yval, 
                      method = model_type,
                      preProcess = pre_parameters,
                      trControl = ctrl,
                      metric = "Accuracy")
      
      
      # Get evaluation metrics
      
      results_low <- confusionMatrix(cv_fit$pred$pred, cv_fit$pred$obs, positive = "low")
      results_high <- confusionMatrix(cv_fit$pred$pred, cv_fit$pred$obs, positive = "high")
      
      results_classification <- bind_rows(results_classification,
                                          data.frame(Variable = variable_name, 
                                                     Model = model_name, 
                                                     Detail = detail_name,
                                                     Sampling = j,
                                                     Accuracy = results_high$overall[[1]],
                                                     F1_low = results_low$byClass[[7]],
                                                     F1_high = results_high$byClass[[7]],
                                                     Precision_low = results_low$byClass[[5]],
                                                     Precision_high = results_high$byClass[[5]],
                                                     Recall_low = results_low$byClass[[6]],
                                                     Recall_high = results_high$byClass[[6]]))
    }
    
  }
  
  # Stop parallel cluster
  
  stopCluster(cluster)
  
  
  # Return final data frames
  
  return(results_classification)
  
}

run_many_features_regression <- function(main_data, variable_name){
  
  set.seed(123)
  
  # Remove features columns with variance near zero
  
  main_data_Y <- main_data %>% 
    select((dim(main_data)[2]-2):(dim(main_data)[2]))
  
  main_data_features <- main_data %>% 
    select(1:(dim(main_data)[2]-3))
  
  near_zero_var <- nearZeroVar(main_data_features)
  
  main_data <- main_data_features %>% 
    select(-near_zero_var) %>% 
    bind_cols(main_data_Y)
  
  rm(main_data_Y, main_data_features)
  gc()
  
  
  # Split data into folds
  
  fold_ids <- createFolds(main_data[[(dim(main_data)[2])]], k = 10, returnTrain = TRUE)
  
  
  # Create parallel cluster
  
  cluster <- makeCluster(detectCores()-1)
  registerDoParallel(cluster)
  
  
  ############################ REGRESSION #################################
  
  print("REGRESSION")
  
  # DF to save results
  results_regression <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                                 c("Variable", "Model", "Detail", "R2", "RMSE", "MAE"))
  
  #### LASSO REGRESSION ####
  
  # Define parameters grid
  
  grid_search <- expand.grid(alpha = c(1), standardize = c(TRUE), y_idx = c(1, 2))
  
  
  # Loop to test combinations of grids
  
  for(idx in 1:nrow(grid_search)) {
    
    i <- grid_search[idx, ]
    
    
    # Variables to save results
    
    R2_temp <- c()
    RMSE_temp <- c() 
    MAE_temp <- c()
    
    
    # Parameters
    
    alpha <- i[[1]]
    standardize <- i[[2]]
    y_idx <- i[[3]]
    
    
    # Names for final results
    
    model_name <- "lasso regression"
    
    detail_name <- ifelse(y_idx == 1, 
                          "Features standardised and Y in log", 
                          "Features standardised and Y in level")
    
    
    # Print to follow progress
    
    print(paste(model_name, detail_name, sep = " - "))
    
    
    # Loop for nested cv (used to tune lambda and then get evaluation metrics)
    
    for(indices in fold_ids){
      
      Xval <- main_data %>% 
        slice(indices) %>% 
        select(1:(dim(main_data)[2]-3)) %>% 
        as.matrix()
      
      Yval <- main_data %>% 
        slice(indices) %>% 
        select((dim(main_data)[2]-y_idx)) %>% 
        pull()
      
      
      # Tune lambda
      
      cv_fit_inner <- train(Xval, Yval, 
                            method = "glmnet", 
                            standardize = standardize,
                            trControl = trainControl(method="cv", 
                                                     number=5, 
                                                     allowParallel = TRUE,
                                                     savePredictions = "final"),
                            metric = "RMSE",
                            tuneGrid = expand.grid(alpha = alpha, #lambda = 0))
                                                   lambda = exp(seq(-1, 5, length.out = 10))))

      
      # Train the model on the complete training set
      
      fit_outer <- train(Xval, Yval, 
                         method = "glmnet", 
                         standardize = standardize,
                         metric = "RMSE",
                         tuneGrid = expand.grid(alpha = alpha,
                                                lambda = cv_fit_inner$bestTune[[2]]))
      
      
      # Make predictions
      
      Xtest <- main_data %>% 
        slice(-indices) %>% 
        select(1:(dim(main_data)[2]-3)) %>%
        as.matrix()
      
      Ytest <- main_data %>% 
        slice(-indices) %>% 
        select((dim(main_data)[2]-y_idx)) %>% 
        pull()
      
      predictions_outer <- fit_outer %>% predict(Xtest)
      
      
      # Evaluate
      
      R2_temp <-  c(R2_temp, R2(predictions_outer, Ytest))
      RMSE_temp <- c(RMSE_temp, RMSE(predictions_outer, Ytest))
      MAE_temp <- c(MAE_temp, MAE(predictions_outer, Ytest))
      
    }
    
    
    # Get average results
    if(sum(is.na(R2_temp)) > 0) {print(paste0("R2 NA's: ", sum(is.na(R2_temp))))}
    if(sum(is.na(RMSE_temp)) > 0) {print(paste0("RMSE NA's: ", sum(is.na(RMSE_temp))))}
    if(sum(is.na(MAE_temp)) > 0) {print(paste0("MAE NA's: ", sum(is.na(MAE_temp))))}
    
    results_regression <- bind_rows(results_regression,
                                    data.frame(Variable = variable_name, 
                                               Model = model_name, 
                                               Detail = detail_name,
                                               R2 = mean(R2_temp), 
                                               RMSE = mean(RMSE_temp),
                                               MAE = mean(MAE_temp)))
    
    rm(cv_fit_inner, fit_outer)
    
  }
  
  
  #### RIDGE REGRESSION PCA ####
  
  # Define parameters grid
  
  grid_search <- expand.grid(alpha = c(0), standardize = c(TRUE), y_idx = c(1, 2))
  
  # Loop to test combinations of grids
  
  for(idx in 1:nrow(grid_search)) {
    
    i <- grid_search[idx, ]
    
    
    # Variables to save results
    
    R2_temp <- c()
    RMSE_temp <- c() 
    MAE_temp <- c()
    
    
    # Parameters
    
    alpha <- i[[1]]
    standardize <- i[[2]]
    y_idx <- i[[3]]
    
    
    # Names for final results
    
    model_name <- "ridge regression pca"
    
    detail_name <- ifelse(y_idx == 1, 
                          "Features standardised and Y in log", 
                          "Features standardised and Y in level")
    
    
    # Print to follow progress
    
    print(paste(model_name, detail_name, sep = " - "))
    
    
    # Loop for nested cv (used to tune lambda and then get evaluation metrics)
    
    for(indices in fold_ids){
      
      Xval <- main_data %>% 
        slice(indices) %>% 
        select(1:(dim(main_data)[2]-3)) %>% 
        as.matrix()
      
      Yval <- main_data %>% 
        slice(indices) %>% 
        select((dim(main_data)[2]-y_idx)) %>% 
        pull()
      
      
      # Tune lambda
      
      cv_fit_inner <- train(Xval, Yval, 
                            method = "glmnet", 
                            standardize = standardize,
                            preProcess = c("pca"),
                            trControl = trainControl(method="cv", 
                                                     number=5, 
                                                     allowParallel = TRUE,
                                                     savePredictions = "final"),
                            metric = "RMSE",
                            tuneGrid = expand.grid(alpha = alpha,
                                                   lambda = exp(seq(-1, 5, length.out = 10))))
      
      
      # Train the model on the complete training set
      
      fit_outer <- train(Xval, Yval, 
                         method = "glmnet", 
                         standardize = standardize,
                         preProcess = c("pca"),
                         metric = "RMSE",
                         tuneGrid = expand.grid(alpha = alpha,
                                                lambda = cv_fit_inner$bestTune[[2]]))
      
      
      # Make predictions
      
      Xtest <- main_data %>% 
        slice(-indices) %>% 
        select(1:(dim(main_data)[2]-3)) %>%
        as.matrix()
      
      Ytest <- main_data %>% 
        slice(-indices) %>% 
        select((dim(main_data)[2]-y_idx)) %>% 
        pull()
      
      predictions_outer <- fit_outer %>% predict(Xtest)
      
      
      # Evaluate
      
      R2_temp <-  c(R2_temp, R2(predictions_outer, Ytest))
      RMSE_temp <- c(RMSE_temp, RMSE(predictions_outer, Ytest))
      MAE_temp <- c(MAE_temp, MAE(predictions_outer, Ytest))
      
    }
    
    # Get average results
    if(sum(is.na(R2_temp)) > 0) {print(paste0("R2 NA's: ", sum(is.na(R2_temp))))}
    if(sum(is.na(RMSE_temp)) > 0) {print(paste0("RMSE NA's: ", sum(is.na(RMSE_temp))))}
    if(sum(is.na(MAE_temp)) > 0) {print(paste0("MAE NA's: ", sum(is.na(MAE_temp))))}
    
    results_regression <- bind_rows(results_regression,
                                    data.frame(Variable = variable_name, 
                                               Model = model_name, 
                                               Detail = detail_name,
                                               R2 = mean(R2_temp), 
                                               RMSE = mean(RMSE_temp),
                                               MAE = mean(MAE_temp)))
    
    rm(cv_fit_inner, fit_outer)
    
  }
  
  
  # Stop parallel cluster
  
  stopCluster(cluster)
  
  
  # Return final data frames
  
  return(results_regression)
  
}

run_many_features_classification <- function(main_data, variable_name){
  
  set.seed(123)
  
  # Remove features columns with variance near zero
  
  main_data_Y <- main_data %>% 
    select((dim(main_data)[2]-2):(dim(main_data)[2]))
  
  main_data_features <- main_data %>% 
    select(1:(dim(main_data)[2]-3))
  
  near_zero_var <- nearZeroVar(main_data_features)
  
  main_data <- main_data_features %>% 
    select(-near_zero_var) %>% 
    bind_cols(main_data_Y)
  
  rm(main_data_Y, main_data_features)
  gc()
  
  
  # Split data into folds
  
  fold_ids <- createFolds(main_data[[(dim(main_data)[2])]], k = 10, returnTrain = TRUE)
  
  
  # Create parallel cluster
  
  cluster <- makeCluster(detectCores()-1)
  registerDoParallel(cluster)
  
  
  ########################## CLASSIFICATION ################################
  
  print("CLASSIFICATION")
  
  results_classification <- setNames(data.frame(matrix(ncol = 11, nrow = 0)),
                                     c("Variable", "Model", "Detail", "Sampling", 
                                       "Accuracy", "F1_low", "F1_high", 
                                       "Precision_low", "Recall_low",
                                       "Precision_high", "Recall_high"))
  
  
  #### LOGISTIC REGRESSION ####
  
  print("LOGISTIC REGRESSION")
  
  # Define parameters grid
  
  grid_search <- expand.grid(alpha = c(0, 1), standardize = c(TRUE))
  
  # Loop to test combinations of grids
  
  for(idx in 1:nrow(grid_search)) {
    
    i <- grid_search[idx, ]
    
    
    # Parameters
    
    alpha <- i[[1]]
    standardize <- i[[2]]
    processe_type <- if(alpha == 0) {c("pca")} else {c()}
    
    
    # Names for final results
    
    model_name <- ifelse(alpha == 0, "logistic ridge regression with pca", "logistic lasso regression without pca")
    detail_name <- ifelse(isTRUE(standardize), "Features standardised", "Features level")
    
    
    # Print to follow progress
    
    print(paste(model_name, detail_name, sep = " - "))
    
    
    # Loop over sampling methods
    
    #for(j in c("no sampling", "down", "up", "smote")){ # If other sampling methods are wanted
    for(j in c("up")){
      
      print(j)
      
      predictions <- as.factor(c())
      correct_labels <- as.factor(c())
      
      
      # Loop for nested cv (used to tune lambda and then get evaluation metrics)
      
      for(indices in fold_ids){
        
        Xval <- main_data %>% 
          slice(indices) %>% 
          select(1:(dim(main_data)[2]-3)) %>% 
          as.matrix()
        
        Yval <- main_data %>% 
          slice(indices) %>% 
          select((dim(main_data)[2])) %>% 
          pull()
        
        stratified_kfold <- createFolds(y = Yval, k = 5, returnTrain = TRUE)
        
        if(j == "no sampling"){
          
          ctrl <- trainControl(index = stratified_kfold,
                               method="cv", 
                               number=5, 
                               allowParallel = TRUE,
                               savePredictions = "final")
          
        } else {
          
          ctrl <- trainControl(index = stratified_kfold,
                               method="cv", 
                               number=5, 
                               allowParallel = TRUE,
                               savePredictions = "final",
                               sampling = j)
          
        }
        
        # Tune lambda
        
        cv_fit_inner <- train(Xval, Yval, 
                              method = "glmnet", 
                              family="binomial",
                              preProcess = processe_type,
                              standardize = standardize,
                              trControl = ctrl,
                              metric = "Accuracy",
                              tuneGrid = expand.grid(alpha = alpha,
                                                     lambda = exp(seq(-1, 5, length.out = 10))))
        
        
        # Train the model on the complete training set
        
        fit_outer <- train(Xval, Yval, 
                           method = "glmnet", 
                           family="binomial",
                           preProcess = processe_type,
                           standardize = standardize,
                           metric = "Accuracy",
                           tuneGrid = expand.grid(alpha = alpha,
                                                  lambda = cv_fit_inner$bestTune[[2]]))
        
        # Make predictions
        
        Xtest <- main_data %>% 
          slice(-indices) %>% 
          select(1:(dim(main_data)[2]-3)) %>%
          as.matrix()
        
        Ytest <- main_data %>% 
          slice(-indices) %>% 
          select((dim(main_data)[2])) %>% 
          pull() %>% 
          as.factor()
        
        predictions_outer <- fit_outer %>% predict(Xtest)
        
        correct_labels <- unlist(list(correct_labels, Ytest))
        predictions <- unlist(list(predictions, predictions_outer))
        
      }
      
      # Evaluate model
      
      results_low <- confusionMatrix(predictions, correct_labels, positive = "low")
      
      results_high <- confusionMatrix(predictions, correct_labels, positive = "high")
      
      results_classification <- bind_rows(results_classification,
                                          data.frame(Variable = variable_name, 
                                                     Model = model_name, 
                                                     Detail = detail_name,
                                                     Sampling = j,
                                                     Accuracy = results_high$overall[[1]],
                                                     F1_low = results_low$byClass[[7]],
                                                     F1_high = results_high$byClass[[7]],
                                                     Precision_low = results_low$byClass[[5]],
                                                     Precision_high = results_high$byClass[[5]],
                                                     Recall_low = results_low$byClass[[6]],
                                                     Recall_high = results_high$byClass[[6]]))
      
      rm(cv_fit_inner, fit_outer)
      
    }
    
  }
  
  
  #### DECISION TREE AND GRADIENT BOOSTING ####
  
  print("DECISION TREE AND GRADIENT BOOSTING")
  
  # Define data
  
  Xval <- main_data %>%
    select(1:(dim(main_data)[2]-3)) %>% 
    as.matrix()
  
  Yval <- main_data %>%
    select((dim(main_data)[2])) %>% 
    pull()
  
  # Define new grid
  
  grid_search <- expand.grid(model_type = c("rpart", "xgbTree"), 
                             standardize = c(TRUE))
  
  # Run models
  
  for(idx in 1:nrow(grid_search)) {
    
    i <- grid_search[idx, ]
    
    # Parameters
    model_type <- i[[1]]
    standardize <- i[[2]]
    pre_parameters <- if (isTRUE(standardize)) {c("pca", "center", "scale")} else {c("pca")}
    
    # Names for final result
    model_name <- ifelse(model_type == "rpart", "Decision Tree", "Gradient Boosting")
    detail_name <- ifelse(isTRUE(standardize), 
                          "Features standardised", 
                          "Features level")
    
    # Print to follow progress
    print(paste(model_name, detail_name, sep = " - "))
    
    
    # Loop for each sampling method
    #for(j in c("no sampling", "down", "up", "smote")){ # If other sampling methods are wanted
    for(j in c("up")){
      
      print(j)
      
      if(j == "no sampling"){
        
        ctrl <- trainControl(index = fold_ids,
                             method="cv", 
                             number=10, 
                             allowParallel = TRUE,
                             savePredictions = "final")
        
      } else {
        
        ctrl <- trainControl(index = fold_ids,
                             method="cv", 
                             number=10, 
                             allowParallel = TRUE,
                             savePredictions = "final",
                             sampling = j)
        
      }
      
      
      cv_fit <- train(Xval, Yval, 
                      method = model_type,
                      preProcess = pre_parameters,
                      trControl = ctrl,
                      metric = "Accuracy")
      
      
      # Get evaluation metrics
      
      results_low <- confusionMatrix(cv_fit$pred$pred, cv_fit$pred$obs, positive = "low")
      results_high <- confusionMatrix(cv_fit$pred$pred, cv_fit$pred$obs, positive = "high")
      
      results_classification <- bind_rows(results_classification,
                                          data.frame(Variable = variable_name, 
                                                     Model = model_name, 
                                                     Detail = detail_name,
                                                     Sampling = j,
                                                     Accuracy = results_high$overall[[1]],
                                                     F1_low = results_low$byClass[[7]],
                                                     F1_high = results_high$byClass[[7]],
                                                     Precision_low = results_low$byClass[[5]],
                                                     Precision_high = results_high$byClass[[5]],
                                                     Recall_low = results_low$byClass[[6]],
                                                     Recall_high = results_high$byClass[[6]]))
      
      
      rm(cv_fit)
      
    }
    
  }
  
  # Stop parallel cluster
  
  stopCluster(cluster)
  
  
  # Return final data frames
  
  return(results_classification)
  
}


######################### RUN MODEL ########################################

# Import base data -------------------------------------------------------------

lights <- data.table::fread("input/model/download_coordinates.txt")
image_basic <- data.table::fread("input/model/google_image_features_basic.csv")
image_vgg <- data.table::fread("input/model/google_image_features_cnn.csv")
image_transfer <- data.table::fread("input/model/google_image_features_cnn_transfer.csv")


# Import "income" data ----------------------------------------------------

gdp_per_capita_data <- data.table::fread("../RS/input/model/gdp_per_capita_df.txt") %>% 
  filter(state == "BA") %>% 
  select(city_code, gdp_per_capita)

income_avg_data <- data.table::fread("../RS/input/model/income_avg_df.txt") %>% 
  filter(state == "BA") %>% 
  select(city_code = code, income)

water_data <- data.table::fread("../RS/input/model/water_index.txt") %>% 
  filter(state == "BA") %>% 
  select(city_code, water_index)


# Join all features -------------------------------------------------------

all_features <- image_basic %>% 
  inner_join(., lights %>% select(row_raster, col_raster, city_code, radiance), 
             by = c("V1" = "row_raster", "V2" = "col_raster")) %>% 
  group_by(city_code) %>% 
  summarise(V3 = mean(V3),
            V4 = mean(V4),
            V5 = mean(V5),
            V6 = mean(V6),
            V7 = mean(V7),
            V8 = mean(V8),
            V9 = mean(V9),
            V10 = mean(V10),
            V11 = mean(V11),
            V12 = mean(V12),
            V13 = mean(V13),
            V14 = mean(V14),
            V15 = mean(V15),
            V16 = mean(V16),
            V17 = mean(V17),
            radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance)) %>% 
  ungroup() %>% 
  inner_join(., image_vgg, by = c("city_code" = "V4097")) %>% 
  inner_join(., image_transfer, by = c("city_code" = "V4097"))


# Join DFs and get final DF ------------------------------------------------

#### GDP ####

lights_gdp <- lights %>%
  left_join(., gdp_per_capita_data, by = "city_code") %>%
  filter(!is.na(gdp_per_capita)) %>%
  group_by(city_code) %>%
  summarise(radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance),
            gdp_per_capita = min(gdp_per_capita)) %>%
  ungroup() %>%
  mutate(gdp_per_capita_log = log(gdp_per_capita),
         label_class = ifelse(gdp_per_capita <= 15.8695, # Median of gdp per capita
                              "low",
                              "high")) %>%
  select(-city_code)


images_basic_gdp <- image_basic %>% 
  inner_join(., lights %>% select(row_raster, col_raster, city_code), 
             by = c("V1" = "row_raster", "V2" = "col_raster")) %>% 
  select(-(1:2)) %>% 
  group_by(city_code) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  inner_join(., gdp_per_capita_data, by = "city_code") %>% 
  mutate(gdp_per_capita_log = log(gdp_per_capita),
         label_class = ifelse(gdp_per_capita <= 15.8695, # Median of gdp per capita
                              "low", 
                              "high")) %>% 
  select(-city_code)


images_vgg_gdp <- image_vgg %>% 
  rename(city_code = V4097) %>% 
  left_join(., gdp_per_capita_data, by = "city_code") %>% 
  filter(!is.na(gdp_per_capita)) %>% 
  mutate(gdp_per_capita_log = log(gdp_per_capita),
         label_class = ifelse(gdp_per_capita <= 15.8695, # Median of gdp per capita
                              "low", 
                              "high")) %>% 
  select(-city_code)


images_transfer_gdp <- image_transfer %>% 
  rename(city_code = V4097) %>% 
  left_join(., gdp_per_capita_data, by = "city_code") %>% 
  filter(!is.na(gdp_per_capita)) %>% 
  mutate(gdp_per_capita_log = log(gdp_per_capita),
         label_class = ifelse(gdp_per_capita <= 15.8695, # Median of gdp per capita
                              "low", 
                              "high")) %>% 
  select(-city_code)


all_gdp <- all_features %>% 
  inner_join(., gdp_per_capita_data, by = "city_code") %>% 
  mutate(gdp_per_capita_log = log(gdp_per_capita),
         label_class = ifelse(gdp_per_capita <= 15.8695, # Median of gdp per capita
                              "low", 
                              "high")) %>% 
  select(-city_code)


#### AVERAGE INCOME ####

lights_income_avg <- lights %>%
  left_join(., income_avg_data, by = "city_code") %>%
  filter(!is.na(income)) %>%
  group_by(city_code) %>%
  summarise(radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance),
            income = min(income)) %>%
  ungroup() %>%
  mutate(income_log = log(income),
         label_class = ifelse(income <= 537.16, # Median of average income
                              "low",
                              "high")) %>%
  select(-city_code)


images_basic_income_avg <- image_basic %>% 
  inner_join(., lights %>% select(row_raster, col_raster, city_code), 
             by = c("V1" = "row_raster", "V2" = "col_raster")) %>% 
  select(-(1:2)) %>% 
  group_by(city_code) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  inner_join(., income_avg_data, by = "city_code") %>% 
  mutate(income_log = log(income),
         label_class = ifelse(income <= 537.16, # Median of average income
                              "low", 
                              "high")) %>% 
  select(-city_code)


images_vgg_income_avg <- image_vgg %>% 
  rename(city_code = V4097) %>% 
  left_join(., income_avg_data, by = "city_code") %>% 
  filter(!is.na(income)) %>% 
  mutate(income_log = log(income),
         label_class = ifelse(income <= 537.16, # Median of average income
                              "low", 
                              "high")) %>% 
  select(-city_code)


images_transfer_income_avg <- image_transfer %>% 
  rename(city_code = V4097) %>% 
  left_join(., income_avg_data, by = "city_code") %>% 
  filter(!is.na(income)) %>% 
  mutate(income_log = log(income),
         label_class = ifelse(income <= 537.16, # Median of average income
                              "low", 
                              "high")) %>% 
  select(-city_code)


all_income_avg <- all_features %>% 
  inner_join(., income_avg_data, by = "city_code") %>% 
  mutate(income_log = log(income),
         label_class = ifelse(income <= 537.16, # Median of average income
                              "low", 
                              "high")) %>% 
  select(-city_code)


#### WATER ####

lights_water <- lights %>%
  mutate(city_code_reduced = as.integer(str_sub(city_code, 1, 6))) %>% 
  left_join(., water_data, by = c("city_code_reduced" = "city_code")) %>%
  filter(!is.na(water_index)) %>%
  group_by(city_code) %>%
  summarise(radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance),
            water_index = min(water_index)) %>%
  ungroup() %>%
  mutate(water_index_log = log(water_index),
         label_class = ifelse(water_index <= 0.667, # Median of water index
                              "low",
                              "high")) %>%
  select(-city_code)


images_basic_water <- image_basic %>% 
  inner_join(., lights %>% select(row_raster, col_raster, city_code), 
             by = c("V1" = "row_raster", "V2" = "col_raster")) %>% 
  select(-(1:2)) %>% 
  group_by(city_code) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>%
  mutate(city_code_reduced = as.integer(str_sub(city_code, 1, 6))) %>% 
  inner_join(., water_data, by = c("city_code_reduced" = "city_code")) %>% 
  mutate(water_index_log = log(water_index),
         label_class = ifelse(water_index <= 0.667, # Median of water index
                              "low", 
                              "high")) %>% 
  select(-c(city_code, city_code_reduced))


images_vgg_water <- image_vgg %>% 
  rename(city_code = V4097) %>% 
  mutate(city_code_reduced = as.integer(str_sub(city_code, 1, 6))) %>% 
  left_join(., water_data, by = c("city_code_reduced" = "city_code")) %>% 
  filter(!is.na(water_index)) %>% 
  mutate(water_index_log = log(water_index),
         label_class = ifelse(water_index <= 0.667, # Median of water index
                              "low", 
                              "high")) %>% 
  select(-c(city_code, city_code_reduced))


images_transfer_water <- image_transfer %>% 
  rename(city_code = V4097) %>% 
  mutate(city_code_reduced = as.integer(str_sub(city_code, 1, 6))) %>% 
  left_join(., water_data, by = c("city_code_reduced" = "city_code")) %>% 
  filter(!is.na(water_index)) %>% 
  mutate(water_index_log = log(water_index),
         label_class = ifelse(water_index <= 0.667, # Median of water index
                              "low", 
                              "high")) %>% 
  select(-c(city_code, city_code_reduced))


all_water <- all_features %>% 
  mutate(city_code_reduced = as.integer(str_sub(city_code, 1, 6))) %>%
  inner_join(., water_data, by = c("city_code_reduced"="city_code")) %>% 
  mutate(water_index_log = log(water_index),
         label_class = ifelse(water_index <= 0.667, # Median of water index
                              "low", 
                              "high")) %>% 
  select(-c(city_code, city_code_reduced))


# Clean environment

rm(lights, image_basic, image_vgg, image_transfer, gdp_per_capita_data,
   salary_data, income_avg_data, income_median_data, water_data)
gc()


# Run models --------------------------------------------------------------

results_regression <- data.frame()
results_classification <- data.frame()

#### REGRESSION ####

# GDP

try({
  print("Regression - lights vs gdp per capita")
  results <- run_few_features_regression(lights_gdp, "lights vs gdp per capita")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Images basic features vs gdp per capita")
  results <- run_few_features_regression(images_basic_gdp, "Images basic features vs gdp per capita")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Images VGG vs gdp per capita")
  results <- run_many_features_regression(images_vgg_gdp, "Images VGG vs gdp per capita")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Transfer learning vs gdp per capita")
  results <- run_many_features_regression(images_transfer_gdp, "Transfer learning vs gdp per capita")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - All features vs gdp per capita")
  results <- run_many_features_regression(all_gdp, "All features vs gdp per capita")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})
gc()


# AVERAGE INCOME

try({
  print("Regression - lights vs average income")
  results <- run_few_features_regression(lights_income_avg, "lights vs average income")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Images basic features vs average income")
  results <- run_few_features_regression(images_basic_income_avg, "Images basic features vs average income")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Images VGG vs average income")
  results <- run_many_features_regression(images_vgg_income_avg, "Images VGG vs average income")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Transfer learning vs average income")
  results <- run_many_features_regression(images_transfer_income_avg, "Transfer learning vs average income")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - All features vs average income")
  results <- run_many_features_regression(all_income_avg, "All features vs average income")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})
gc()


### WATER ###

try({
  print("Regression - lights vs water")
  results <- run_few_features_regression(lights_water, "lights vs water")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Images basic features vs water")
  results <- run_few_features_regression(images_basic_water, "Images basic features vs water")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Images VGG vs water")
  results <- run_many_features_regression(images_vgg_water, "Images VGG vs water")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - Transfer learning vs water")
  results <- run_many_features_regression(images_transfer_water, "Transfer learning vs water")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})

try({
  print("Regression - All features vs water")
  results <- run_many_features_regression(all_water, "All features vs water")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_regression.csv")
  rm(results)
})
gc()


#### CLASSIFICATION ####

### GDP ###

try({
  print("Classification - lights vs gdp per capita")
  results <- run_few_features_classification(lights_gdp, "lights vs gdp per capita")
  results_classification <- bind_rows(results_classification, results)
  rm(results, lights_gdp)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Images basic features vs gdp per capita")
  results <- run_few_features_classification(images_basic_gdp, "Images basic features vs gdp per capita")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_basic_gdp)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Images VGG vs gdp per capita")
  results <- run_many_features_classification(images_vgg_gdp, "Images VGG vs gdp per capita")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_vgg_gdp)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Transfer learning vs gdp per capita")
  results <- run_many_features_classification(images_transfer_gdp, "Transfer learning vs gdp per capita")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_transfer_gdp)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - All features vs gdp per capita")
  results <- run_many_features_classification(all_gdp, "All features vs gdp per capita")
  results_classification <- bind_rows(results_classification, results)
  rm(results, all_gdp)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

### AVERAGE INCOME ###

try({
  print("Classification - lights vs average income")
  results <- run_few_features_classification(lights_income_avg, "lights vs average income")
  results_classification <- bind_rows(results_classification, results)
  rm(results, lights_income_avg)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Images basic features vs average income")
  results <- run_few_features_classification(images_basic_income_avg, "Images basic features vs average income")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_basic_income_avg)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Images VGG vs average income")
  results <- run_many_features_classification(images_vgg_income_avg, "Images VGG vs average income")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_vgg_income_avg)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Transfer learning vs average income")
  results <- run_many_features_classification(images_transfer_income_avg, "Transfer learning vs average income")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_transfer_income_avg)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - All features vs average income")
  results <- run_many_features_classification(all_income_avg, "All features vs average income")
  results_classification <- bind_rows(results_classification, results)
  rm(results, all_income_avg)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})


### WATER ###

try({
  print("Classification - lights vs water")
  results <- run_few_features_classification(lights_water, "lights vs water")
  results_classification <- bind_rows(results_classification, results)
  rm(results, lights_water)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Images basic features vs water")
  results <- run_few_features_classification(images_basic_water, "Images basic features vs water")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_basic_water)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Images VGG vs water")
  results <- run_many_features_classification(images_vgg_water, "Images VGG vs water")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_vgg_water)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - Transfer learning vs water")
  results <- run_many_features_classification(images_transfer_water, "Transfer learning vs water")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_transfer_water)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})

try({
  print("Classification - All features vs water")
  results <- run_many_features_classification(all_water, "All features vs water")
  results_classification <- bind_rows(results_classification, results)
  rm(results, all_water)
  gc()
  data.table::fwrite(results_classification, "results/BA_classification.csv")
})