### Created by: Diego Afonso de Castro
### Date: 17/07/2019
### Objective: evaluate relation between lights (mean, median, max, min, std) 
###            and average salary using only lights selected

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(glmnet)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/download_coordinates.txt")

salary_data <- data.table::fread("input/model/average_salary_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code, average_wage)


# Join DFs and get the mean radiance per city -----------------------------
salary_lights <- lights %>% 
  left_join(., salary_data, by = "city_code") %>% 
  filter(!is.na(average_wage)) %>% 
  group_by(city_code) %>% 
  summarise(radiance_mean = mean(radiance),
            radiance_median = median(radiance),
            radiance_max = max(radiance),
            radiance_min = min(radiance),
            radiance_std = sd(radiance),
            average_wage = min(average_wage)) %>% 
  select(-city_code)

salary_lights_scaled <- salary_lights %>% 
  mutate_each_(list(~scale(.) %>% as.vector), 
               vars=c("radiance_mean","radiance_median","radiance_max",
                      "radiance_min","radiance_std","average_wage"))

# Check cities in the shapefile that dont have gdp data
setdiff(unique(lights$city_code), salary_data$city_code)

# Check cities that have gdp data and are not in the shapefile 
setdiff(salary_data$city_code, unique(lights$city_code))

# Clean enviroment
rm(lights, salary_data)
gc()


#### Obs: RS cities LAGOA MIRIM (4300001) and LAGOA DOS PATOS (4300002) are in the
####      shapefile but don't have GDP data. PINTO BANDEIRA (4314548) has GDP data
####      but is not in the shapefile


# Plot relation -----------------------------------------------------------

# Level variables
ggplot(salary_lights, aes(x = radiance_mean, y = average_wage)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

ggplot(salary_lights, aes(x = radiance_median, y = average_wage)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

# Scaled variables
ggplot(salary_lights_scaled, aes(x = radiance_mean, y = average_wage)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Evaluate ----------------------------------------------------------------

# Level variables
salary_lights <- salary_lights %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(salary_lights[, 1:5], salary_lights[, 6], alpha = 0, 
                    lambda = lambdas, nfolds = 10)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = salary_lights[, 1:5])
# Sum of Squares Total and Error
sst <- sum((salary_lights[, 6] - mean(salary_lights[, 6]))^2)
sse <- sum((y_predicted - salary_lights[, 6])^2)
# R squared
rsq <- 1 - sse / sst
rsq

# Scaled variables
salary_lights_scaled <- salary_lights_scaled %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(salary_lights_scaled[, 1:5], salary_lights_scaled[, 6], alpha = 0, 
                    lambda = lambdas, nfolds = 10)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = salary_lights_scaled[, 1:5])
# Sum of Squares Total and Error
sst <- sum((salary_lights_scaled[, 6] - mean(salary_lights_scaled[, 6]))^2)
sse <- sum((y_predicted - salary_lights_scaled[, 6])^2)
# R squared
rsq <- 1 - sse / sst
rsq
