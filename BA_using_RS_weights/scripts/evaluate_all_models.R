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


# Join DFs and get final DF ------------------------------------------------

#### GDP ####

images_transfer_gdp <- image_transfer %>% 
  rename(city_code = V4097) %>% 
  left_join(., gdp_per_capita_data, by = "city_code") %>% 
  filter(!is.na(gdp_per_capita)) %>% 
  mutate(gdp_per_capita_log = log(gdp_per_capita),
         label_class = ifelse(gdp_per_capita <= 15.8695, # Median of gdp per capita
                              "low", 
                              "high")) %>% 
  select(-city_code)


#### AVERAGE INCOME ####

images_transfer_income_avg <- image_transfer %>% 
  rename(city_code = V4097) %>% 
  left_join(., income_avg_data, by = "city_code") %>% 
  filter(!is.na(income)) %>% 
  mutate(income_log = log(income),
         label_class = ifelse(income <= 537.16, # Median of average income
                              "low", 
                              "high")) %>% 
  select(-city_code)


#### WATER ####

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


# Clean environment

rm(lights, image_basic, image_vgg, image_transfer, gdp_per_capita_data,
   income_avg_data, water_data)
gc()


# Run models --------------------------------------------------------------

results_regression <- data.frame()
results_classification <- data.frame()

#### REGRESSION ####

# GDP

try({
  print("Regression - Transfer learning vs gdp per capita")
  results <- run_many_features_regression(images_transfer_gdp, "Transfer learning vs gdp per capita")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_using_RS_regression.csv")
  rm(results)
})
gc()


# AVERAGE INCOME

try({
  print("Regression - Transfer learning vs average income")
  results <- run_many_features_regression(images_transfer_income_avg, "Transfer learning vs average income")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_using_RS_regression.csv")
  rm(results)
})

gc()


### WATER ###

try({
  print("Regression - Transfer learning vs water")
  results <- run_many_features_regression(images_transfer_water, "Transfer learning vs water")
  results_regression <- bind_rows(results_regression, results)
  data.table::fwrite(results_regression, "results/BA_using_RS_regression.csv")
  rm(results)
})

gc()


#### CLASSIFICATION ####

### GDP ###

try({
  print("Classification - Transfer learning vs gdp per capita")
  results <- run_many_features_classification(images_transfer_gdp, "Transfer learning vs gdp per capita")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_transfer_gdp)
  gc()
  data.table::fwrite(results_classification, "results/BA_using_RS_classification.csv")
})

### AVERAGE INCOME ###

try({
  print("Classification - Transfer learning vs average income")
  results <- run_many_features_classification(images_transfer_income_avg, "Transfer learning vs average income")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_transfer_income_avg)
  gc()
  data.table::fwrite(results_classification, "results/BA_using_RS_classification.csv")
})

### WATER ###

try({
  print("Classification - Transfer learning vs water")
  results <- run_many_features_classification(images_transfer_water, "Transfer learning vs water")
  results_classification <- bind_rows(results_classification, results)
  rm(results, images_transfer_water)
  gc()
  data.table::fwrite(results_classification, "results/BA_using_RS_classification.csv")
})
