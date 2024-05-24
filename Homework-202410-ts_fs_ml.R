## ---------------------------
##
## Script name: ts_fs_ml
##
## Purpose of script:
## 1. Focusing on the data of fishes’ densities at the station of VERCah, selecting the species of VIA to create a timeseries object and visualizing it.
## 2. Extracting the features of this timeseries and building a forecast model with tidymodels package.
##
## Author: Weishan Tu
##
## Date Created: 2024-03-29
##
## Copyright (c) Timothy Farewell, 2024
## Email: weishan@mail.ustc.edu.cn
##
## ---------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables
#load package
library(tidyverse)
library(lubridate) # manipulating dates
library(timetk)
library(tidymodels)

###1. load doubs fish density
fish_density <- read_delim("doubs_paper/Prunier et al._RawBiomassData.txt")
head(fish_density)

##clean
fish_density_c <- fish_density |>
  drop_na() |>
  distinct()

##selecting station of VERCah, selecting the species of VIA
##beause a year have mutil data
VERCah_VAI_density <- fish_density_c|>
  filter(STATION=="VERCah" & SP == "VAI")|>
  group_by(DATE)|>
  summarise(BIOMASS=mean(BIOMASS),DENSITY=mean(DENSITY))

###2. create timeseries
### method1
# ts_VERCah_VAI_density <- ts(data = VERCah_VAI_density,
#              start = c(1994,1), # Start Year 1994
#              frequency = 1)  # freq = 1

### method 2
ts_VERCah_VAI_density <- VERCah_VAI_density|>
  tk_tbl()

###3. Plot data 
### method1
# library(forecast) # work with ggplot2 for autoplot()
# library(ggplot2)
# autoplot(ts_VERCah_VAI_density, facets = TRUE) +
#   ggtitle("VAI at VERCah station") +
#   ylab("Changes") + xlab("Year")
# ggsave("VAI_of_VERCah_station.png", width = 5, height = 5)

### method2
ts_VERCah_VAI_density|>
  pivot_longer( # convert to to long format
  cols = c("BIOMASS","DENSITY"))|>
  group_by(name)|>
  plot_time_series(DATE, value, 
                   .facet_ncol = 2, 
                   .interactive = FALSE,
                   .title = "VAI at VERCah station"
  )

### 4. Anomalize: breakdown, identify, and clean in 1 easy step
ts_anomalize_tbl <- ts_VERCah_VAI_density |>
  anomalize(
    .date_var      = DATE, 
    .value         = DENSITY,
    .iqr_alpha     = 0.05,
    .max_anomalies = 0.20,
    .message       = FALSE
  )

### 5. Fill in Gaps
# Check the regularity of the time series
ts_anomalize_tbl |>
  tk_summary_diagnostics(.date_var = DATE)
#
library(padr)
ts_anomalize_tbl_fill <- ts_anomalize_tbl |>
  select(DATE,observed)|>
  thicken( 'year')|> #aggregate the data first with thicken
  pad_by_time(DATE_year, .by = "year")|>
  mutate_at(vars(observed), .funs = ts_impute_vec, period = 1)
##plot fill gaps result
ts_anomalize_tbl_fill |> 
  plot_time_series(DATE_year, observed, .facet_ncol = 2, .interactive = FALSE) 
# 补的数据不会用到之后预测和特征提取
#### 6. Extracting the features
## A) Calendar-based features
ts_VERCah_VAI_density_features_C <- ts_VERCah_VAI_density |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(DENSITY =  log1p(x = DENSITY)) |>
  # Measure Transformation: standardization
  mutate(DENSITY =  standardize_vec(DENSITY)) |>
  # Add Calendar-based (or signature) features
  tk_augment_timeseries_signature(.date_var = DATE)
# Perform linear regression    
timetk::plot_time_series_regression(.date_var = DATE,
                                    .data = ts_VERCah_VAI_density_features_C,
                                    .formula = DENSITY ~ as.numeric(DATE) + index.num
                                    + year + half + quarter + month + month.lbl,
                                    .show_summary = TRUE)
# B) Fourier terms features

ts_VERCah_VAI_density_features_F <- ts_VERCah_VAI_density |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(DENSITY =  log1p(x = DENSITY)) |>
  # Measure Transformation: standardization
  mutate(DENSITY =  standardize_vec(DENSITY)) |>
  # Add Fourier features
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) 

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = ts_VERCah_VAI_density_features_F,
                            .formula = DENSITY ~ as.numeric(DATE) + 
                              DATE_sin5_K1 + DATE_cos5_K1,
                            .show_summary = TRUE)

# C) Lag features

ts_VERCah_VAI_density_L <- ts_VERCah_VAI_density |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(DENSITY =  log1p(x = DENSITY)) |>
  # Measure Transformation: standardization
  mutate(DENSITY =  standardize_vec(DENSITY)) |>
  # Add lag features
  tk_augment_lags(.value = DENSITY, .lags = c(4, 7))  

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = ts_VERCah_VAI_density_L,
                            .formula = DENSITY ~ as.numeric(DATE) + 
                              DENSITY_lag4 + DENSITY_lag7,
                            .show_summary = TRUE)

## D) Moving window statistics

ts_VERCah_VAI_density_M <- ts_VERCah_VAI_density |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(DENSITY =  log1p(x = DENSITY)) |>
  # Measure Transformation: standardization
  mutate(DENSITY =  standardize_vec(DENSITY)) |>
  tk_augment_lags(.value = DENSITY, .lags = c(4, 7)) |>
  # Add moving window statistics
  tk_augment_slidify(.value   = contains("DENSITY"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = ts_VERCah_VAI_density_M,
                            .formula = DENSITY ~ as.numeric(DATE) + 
                              DENSITY_roll_3 + DENSITY_roll_6,
                            .show_summary = TRUE)
#### 7. Building a forecast model with tidymodels package
# This enables the analysis to be reproducible when random numbers are used 
set.seed(2333)
# Put 3/4 of the data into the training set 
data_split <- initial_split(ts_VERCah_VAI_density, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

##
recipe_spec_final <- recipe(DENSITY ~ ., train_data) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

summary(prep(recipe_spec_final))
# 1) Training a boosted tree model

bt <- workflow() |>
  add_model(
    boost_tree("regression") |> set_engine("xgboost")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)
# evaluating model performance

bt_test <- bt |> 
  predict(test_data) |>
  bind_cols(test_data) 

bt_test

pbt <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = DENSITY, color = "Train"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = DENSITY, color = "Test"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "DENSITY") +
  theme_minimal()

# Calculating forecast error
bt_test |>
  metrics(DENSITY, .pred)

## 2) training a random forest model

rf <- workflow() |>
  add_model(
    spec = rand_forest("regression") |> set_engine("ranger")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)

rf

# evaluating model performance

rf_test <- rf |> 
  predict(test_data) |>
  bind_cols(test_data) 

rf_test

prf <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = DENSITY, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = DENSITY, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "DENSITY") +
  theme_minimal()


# Calculating forecast error
rf_test |>
  metrics(DENSITY, .pred)

library(patchwork)
pbt + prf

##-------------------------------------------------------
# D) comparing among different algorithms
library(modeltime)
# create a Modeltime Table

model_tbl <- modeltime_table(
  bt,
  rf
)

model_tbl

# Calibration table

calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_data)

calibrated_tbl 

# Model Evaluation

calibrated_tbl |>
  modeltime_accuracy(test_data) |>
  arrange(rmse)

##----------------------------------------------------------
# Forecast Plot

calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_data,
    actual_data = train_data,
    keep_data   = TRUE 
  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )
