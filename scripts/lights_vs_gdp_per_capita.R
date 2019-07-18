### Created by: Diego Afonso de Castro
### Date: 06/07/2019
### Objective: evaluate relation between lights and gdp per capita

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/nightlights_per_city.txt")

gdp_per_capita_data <- data.table::fread("input/model/gdp_per_capita_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code, gdp_per_capita)


# Join DFs and get the mean radiance per city -----------------------------
gdp_lights <- lights %>% 
  left_join(., gdp_per_capita_data, by = "city_code") %>% 
  filter(!is.na(gdp_per_capita)) %>% 
  group_by(city_code) %>% 
  summarise(radiance_mean = mean(radiance),
            gdp_per_capita = min(gdp_per_capita))

gdp_lights_scaled <- gdp_lights %>% 
  mutate_each_(list(~scale(.) %>% as.vector), 
               vars=c("radiance_mean","gdp_per_capita"))

# Check cities in the shapefile that dont have gdp data
setdiff(unique(lights$city_code), gdp_per_capita_data$city_code)

# Check cities that have gdp data and are not in the shapefile 
setdiff(gdp_per_capita_data$city_code, unique(lights$city_code))

# Clean enviroment
rm(lights, income_data)
gc()


#### Obs: RS cities LAGOA MIRIM (4300001) and LAGOA DOS PATOS (4300002) are in the
####      shapefile but don't have GDP data. PINTO BANDEIRA (4314548) has GDP data
####      but is not in the shapefile


# Plot relation -----------------------------------------------------------

# Level variables
ggplot(gdp_lights, aes(x = radiance_mean, y = gdp_per_capita)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

# Scaled variables
ggplot(gdp_lights_scaled, aes(x = radiance_mean, y = gdp_per_capita)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Evaluate ----------------------------------------------------------------

# Level variables
ols_cv_level <- train(gdp_per_capita ~ radiance_mean,
                      data = gdp_lights, 
                      method = "lm",
                      trControl=trainControl(method = "cv",
                                             number=10,
                                             savePredictions = TRUE,
                                             verboseIter = TRUE)
)

ols_cv_level$results

# Scaled variables
ols_cv_scale <- train(gdp_per_capita ~ radiance_mean,
                      data = gdp_lights_scaled, 
                      method = "lm",
                      trControl=trainControl(method = "cv",
                                             number=10,
                                             savePredictions = TRUE,
                                             verboseIter = TRUE)
)

ols_cv_scale$results