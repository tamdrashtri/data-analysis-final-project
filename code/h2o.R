library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive",
              "nycflights13","gmodels", "stringr", "DataExplorer",
              "summarytools", "recipes", "broom", "modelr", "magrittr",
              "corrr", "viridis", "readr", "caret", "forcats", "h2o")
library_install(packages)

recipe_obj <- recipe(Churn ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  #step_num2factor(JobLevel, StockOptionLevel) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train)
test_tbl  <- bake(recipe_obj, new_data = test)

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "Churn"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)

typeof(automl_models_h2o)

slotNames(automl_models_h2o)

automl_models_h2o@leaderboard

automl_models_h2o@leader

# Saving & Loading

h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20181212_212229") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/")

h2o.getModel("StackedEnsemble_BestOfFamily_0_AutoML_20181212_212229") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/")

h2o.getModel("GBM_grid_0_AutoML_20181212_212229_model_4") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/")

deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_AllModels_0_AutoML_20181212_212229")

gbm_h2o <- h2o.loadModel("04_Modeling/h2o_models/GBM_grid_0_AutoML_20181212_212229_model_4")
