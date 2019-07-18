### Created by: Diego Afonso de Castro
### Date: 06/07/2019
### Objective: evaluate relation between lights and income

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(caret)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/nightlights_per_city.txt")

income_data <- data.table::fread("input/model/income_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code = code, income)


# Join DFs and get the mean radiance per city -----------------------------
income_lights <- lights %>% 
  left_join(., income_data, by = "city_code") %>% 
  filter(!is.na(income)) %>% 
  group_by(city_code) %>% 
  summarise(radiance_mean = mean(radiance),
            income = min(income))

income_lights_scaled <- income_lights %>% 
  mutate_each_(list(~scale(.) %>% as.vector), 
               vars=c("radiance_mean","income"))

rm(lights, income_data)
gc()

#### Obs: RS cities LAGOA MIRIM (4300001) and LAGOA DOS PATOS (4300002) are in the
####      shapefile but don't have income data.


# Plot relation -----------------------------------------------------------

# Level variables
ggplot(income_lights, aes(x = radiance_mean, y = income)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")

# Scaled variables
ggplot(income_lights_scaled, aes(x = radiance_mean, y = income)) + 
  geom_point(color = "#1E90FF") +
  geom_smooth(method = lm, color = "black", fill = "gray")


# Evaluate ----------------------------------------------------------------

# Level variables
ols_cv_level <- train(income ~ radiance_mean,
                      data = income_lights, 
                      method = "lm",
                      trControl=trainControl(method = "cv",
                                             number=10,
                                             savePredictions = TRUE,
                                             verboseIter = TRUE)
)

ols_cv_level$results

# Scaled variables
ols_cv_scale <- train(income ~ radiance_mean,
                      data = income_lights_scaled, 
                      method = "lm",
                      trControl=trainControl(method = "cv",
                                             number=10,
                                             savePredictions = TRUE,
                                             verboseIter = TRUE)
)

ols_cv_scale$results