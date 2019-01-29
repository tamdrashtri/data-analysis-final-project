# References
# https://github.com/h2oai/h2o-tutorials/blob/master/h2o-world-2017/automl/R/automl_regression_powerplant_output.Rmd


source("code/setup.R")
churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

churn %<>%
  select(-customerID) %>%
  drop_na()

train.index <- createDataPartition(churn$Churn, p = .7, list = FALSE)
train <- churn[ train.index,]
test  <- churn[-train.index,]

# ML Preprocessing

recipe_obj <- recipe(Churn ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -Churn) %>%
  step_corr(all_predictors()) %>%
  prep(data = train)

recipe_obj


train_tbl <- bake(recipe_obj, new_data = train)
test_tbl  <- bake(recipe_obj, new_data = test)

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]

h2o.describe(train_h2o)

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
  max_runtime_secs = 60,
  nfolds = 5
)


automl_models_h2o@leaderboard

automl_models_h2o@leader

# why very bad results


# different methods -------------------------------------------------------

# start up h2o (I turn off progress bars when creating reports/tutorials)
h2o.no_progress()
h2o.init(max_mem_size = "5g")

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(20, 30, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x,
  y = y,
  training_frame = train_h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid",
  sort_by = "mse",
  decreasing = FALSE
)
print(grid_perf)

#---

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 150),
  mtries      = seq(15, 35, by = 10),
  max_depth   = seq(20, 40, by = 5),
  min_rows    = seq(1, 5, by = 2),
  nbins       = seq(10, 30, by = 5),
  sample_rate = c(.55, .632, .75)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 120
)

# build grid search
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x,
  y = y,
  training_frame = train_h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

model_path <- h2o.saveModel(object=random_grid, path=getwd(), force=TRUE)
# collect the results and sort by our model performance metric of choice
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid2",
  sort_by = "mse",
  decreasing = FALSE
)
print(grid_perf2)

h2o.shutdown()

