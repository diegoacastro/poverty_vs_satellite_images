### Created by: Diego Afonso de Castro
### Date: 15/07/2019
### Objective: evaluate relation between lights (mean, median, max, min, std) 
###            and gdp per capita using only lights selected

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(glmnet)


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
            gdp_per_capita = min(gdp_per_capita)) %>% 
  select(-city_code)

gdp_lights_scaled <- gdp_lights %>% 
  mutate_each_(list(~scale(.) %>% as.vector), 
               vars=c("radiance_mean","radiance_median","radiance_max",
                      "radiance_min","radiance_std","gdp_per_capita"))

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

# Scaled variables
ggplot(gdp_lights_scaled, aes(x = radiance_mean, y = gdp_per_capita)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Evaluate ----------------------------------------------------------------

# Level variables
gdp_lights <- gdp_lights %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(gdp_lights[, 1:5], gdp_lights[, 6], alpha = 0, 
                    lambda = lambdas, nfolds = 10)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = gdp_lights[, 1:5])
# Sum of Squares Total and Error
sst <- sum((gdp_lights[, 6] - mean(gdp_lights[, 6]))^2)
sse <- sum((y_predicted - gdp_lights[, 6])^2)
# R squared
rsq <- 1 - sse / sst
rsq

# Scaled variables
gdp_lights_scaled <- gdp_lights_scaled %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(gdp_lights_scaled[, 1:5], gdp_lights_scaled[, 6], alpha = 0, 
                    lambda = lambdas, nfolds = 10)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = gdp_lights_scaled[, 1:5])
# Sum of Squares Total and Error
sst <- sum((gdp_lights_scaled[, 6] - mean(gdp_lights_scaled[, 6]))^2)
sse <- sum((y_predicted - gdp_lights_scaled[, 6])^2)
# R squared
rsq <- 1 - sse / sst
rsq
