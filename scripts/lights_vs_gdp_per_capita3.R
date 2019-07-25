### Created by: Diego Afonso de Castro
### Date: 15/07/2019
### Objective: evaluate relation between lights (mean, median, max, min, std) 
###            and gdp per capita using only lights selected

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/download_coordinates.txt")

gdp_per_capita_data <- data.table::fread("input/model/gdp_per_capita_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code, gdp_per_capita)


# Join DFs and get the mean radiance per city -----------------------------
gdp_lights <- lights %>% 
  left_join(., gdp_per_capita_data, by = "city_code") %>% 
  filter(!is.na(gdp_per_capita)) %>% 
  group_by(city_code) %>% 
  summarise(radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance),
            gdp_per_capita = min(gdp_per_capita),
            gdp_per_capita_log = log(gdp_per_capita)) %>% 
  select(-city_code)

gdp_lights_scaled <- gdp_lights %>% 
  mutate_each_(list(~scale(.) %>% as.vector), 
               vars=c("radiance_mean","radiance_median","radiance_max",
                      "radiance_min","radiance_std"))

# Check cities in the shapefile that dont have gdp data
setdiff(unique(lights$city_code), gdp_per_capita_data$city_code)

# Check cities that have gdp data and are not in the shapefile 
setdiff(gdp_per_capita_data$city_code, unique(lights$city_code))

# Clean enviroment
rm(lights, gdp_per_capita_data)
gc()


#### Obs: RS cities LAGOA MIRIM (4300001) and LAGOA DOS PATOS (4300002) are in the
####      shapefile but don't have GDP data. PINTO BANDEIRA (4314548) has GDP data
####      but is not in the shapefile


# Plot relation -----------------------------------------------------------

# Level variables
ggplot(gdp_lights, aes(x = radiance_mean, y = gdp_per_capita)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(gdp_lights, aes(x = radiance_median, y = gdp_per_capita)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

# Log income
ggplot(gdp_lights, aes(x = radiance_mean, y = gdp_per_capita_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(gdp_lights, aes(x = radiance_median, y = gdp_per_capita_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

# Scaled variables and log income
ggplot(gdp_lights_scaled, aes(x = radiance_mean, y = gdp_per_capita_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(gdp_lights_scaled, aes(x = radiance_median, y = gdp_per_capita_log)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Evaluate ----------------------------------------------------------------

# Level

set.seed(123)

cv_fit <- train(as.data.frame(gdp_lights[, 1:5]), gdp_lights$gdp_per_capita, 
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

cv_fit <- train(as.data.frame(gdp_lights[, 1:5]), gdp_lights$gdp_per_capita_log, 
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

cv_fit <- train(as.data.frame(gdp_lights_scaled[, 1:5]), gdp_lights_scaled$gdp_per_capita_log, 
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
