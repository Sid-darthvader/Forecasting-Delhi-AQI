df = AQI_univar
library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(tidyverse)

aqi_tbl <- df %>%
  select(Date, AQI) %>%
  set_names(c("Date", "AQI")) 

aqi_tbl %>%
  plot_time_series(Date, AQI,.interactive=TRUE)

splits <- aqi_tbl %>%
  time_series_split(assess = "1 week", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, AQI, .interactive = FALSE)


# Modelling
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(AQI ~ Date, training(splits))

model_fit_prophet <- prophet_reg(seasonality_weekly = TRUE) %>%
  set_engine("prophet") %>%
  fit(AQI ~ Date, training(splits))
model_fit_prophet


# Model : ets ----
#model_fit_ets <- exp_smoothing() %>%
# set_engine(engine = "ets") %>%
#fit(AQI ~ Date, training(splits))
#model_fit_ets


## Machine Leanrning Models

recipe_spec <- recipe(AQI ~ Date, training(splits)) %>%
  step_timeseries_signature(Date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"), #Remove time features as they're absent in Date Column
          contains("second"), contains("xts")) %>%
  step_fourier(Date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

# Elastic Net

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")
workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(splits))



# Random Forest

model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(splits))

# Prophet Boost = Prophet + XGBoost
model_spec_prophet_boost <- prophet_boost(seasonality_weekly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost




model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost) 

model_table


calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

# Forecast
calibration_table %>%
  modeltime_forecast(actual_data = aqi_tbl) %>%
  plot_modeltime_forecast(.interactive = FALSE)

# Calculate Accuracy
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


## Refit and forecast future AQI
calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id != 2) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(aqi_tbl) %>%
  modeltime_forecast(h = "1 months", actual_data = aqi_tbl) %>%
  plot_modeltime_forecast(.interactive = FALSE)


