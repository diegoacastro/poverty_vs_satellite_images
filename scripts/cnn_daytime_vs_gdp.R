### Created by: Diego Afonso de Castro
### Date: 25/07/2019
### Objective: evaluate relation between CNN VGG16 features and gdp per capita

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------

images_salary <- data.table::fread("input/model/google_image_features_cnn.csv") %>% 
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


# Exclude features with 0 var to apply PCA to help avoiding overfitting ---

var_round <- function(x){
  x <- round(var(x), 8)
  return(x)
}

images_gdp <- images_gdp[ , apply(images_gdp, 2, var_round) != 0]


# Evaluate ----------------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(images_gdp[1:4077], images_gdp$gdp_per_capita, 
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

cv_fit <- train(images_gdp[1:4077], images_gdp$gdp_per_capita_log, 
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

cv_fit <- train(images_gdp[1:4077], images_gdp$gdp_per_capita_log, 
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