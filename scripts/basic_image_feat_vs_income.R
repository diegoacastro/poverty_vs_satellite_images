### Created by: Diego Afonso de Castro
### Date: 24/07/2019
### Objective: evaluate relation between daytime image features (mean, median,  
###            max, min, std for each RGB) and income using only lights selected.

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

income_data <- data.table::fread("input/model/income_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code = code, income)


# Join dataframes ---------------------------------------------------------

images_income <- features_basic %>% 
  left_join(., lights, by = c("V1" = "row_raster", "V2" = "col_raster")) %>% 
  select(-(1:2)) %>% 
  group_by(city_code) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  inner_join(., income_data, by = "city_code") %>% 
  mutate(income_log = log(income)) %>% 
  select(-city_code)

images_income_scaled <- images_income %>%
  mutate_at(.vars = names(images_income)[1:15], 
            .funs = list(~scale(., center = TRUE, scale = TRUE) %>% as.vector))


# Evaluate ----------------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(as.data.frame(images_income[, 1:15]), images_income$income, 
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

cv_fit <- train(as.data.frame(images_income[, 1:15]), images_income$income_log, 
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

cv_fit <- train(as.data.frame(images_income_scaled[, 1:15]), images_income_scaled$income_log, 
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
