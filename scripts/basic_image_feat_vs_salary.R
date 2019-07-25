### Created by: Diego Afonso de Castro
### Date: 24/07/2019
### Objective: evaluate relation between daytime image features (mean, median,  
###            max, min, std for each RGB) and salary using only lights selected.

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------

features_basic = data.table::fread('input/model/google_image_features_basic.csv', 
                                   header = FALSE,
                                   sep = ' ')

lights <- data.table::fread("input/model/download_coordinates.txt") %>% 
  select(row_raster, col_raster, city_code)

salary_data <- data.table::fread("input/model/average_salary_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code, average_wage) %>% 
  # Change to monthly salary instead of annual
  mutate(average_wage = average_wage/12) 


# Join dataframes ---------------------------------------------------------

images_salary <- features_basic %>% 
  inner_join(., lights, by = c("V1" = "row_raster", "V2" = "col_raster")) %>% 
  select(-(1:2)) %>% 
  group_by(city_code) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  inner_join(., salary_data, by = "city_code") %>% 
  mutate(average_wage_log = log(average_wage)) %>% 
  select(-city_code)

images_salary_scaled <- images_salary %>%
  mutate_at(.vars = names(images_salary)[1:15], 
            .funs = list(~scale(., center = TRUE, scale = TRUE) %>% as.vector))


# Evaluate ----------------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(as.data.frame(images_salary[, 1:15]), images_salary$average_wage, 
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

cv_fit <- train(as.data.frame(images_salary[, 1:15]), images_salary$average_wage_log, 
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

cv_fit <- train(as.data.frame(images_salary_scaled[, 1:15]), images_salary_scaled$average_wage_log, 
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
