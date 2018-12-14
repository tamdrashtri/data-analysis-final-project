


# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive",
              "nycflights13","gmodels", "stringr", "DataExplorer",
              "summarytools", "recipes", "broom", "modelr", "magrittr",
              "corrr", "viridis", "readr", "caret", "forcats", "tidyquant",
              "h2o", "rsample", "randomForest", "ranger", "caret")
library_install(packages)

churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")


set.seed(123)

train.index <- createDataPartition(churn$Churn, p = .7, list = FALSE)
train <- churn[ train.index,]
test  <- churn[-train.index,]


# start up h2o (I turn off progress bars when creating reports/tutorials)
h2o.no_progress()
h2o.init(max_mem_size = "5g")

# create feature names
y <- "Churn"
x <- setdiff(names(churn), y)

# turn training set into h2o object
train.h2o <- as.h2o(train)

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
  training_frame = train.h2o,
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
  max_runtime_secs = 30*60
)

# build grid search
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x,
  y = y,
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

# collect the results and sort by our model performance metric of choice
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid2",
  sort_by = "mse",
  decreasing = FALSE
)
print(grid_perf2)

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf2@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a test set
ames_test.h2o <- as.h2o(ames_test)
best_model_perf <- h2o.performance(model = best_model, newdata = ames_test.h2o)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()

# randomForest
pred_randomForest <- predict(ames_randomForest, ames_test)
head(pred_randomForest)
##        1        2        3        4        5        6
## 128266.7 153888.0 264044.2 379186.5 212915.1 210611.4

# ranger
pred_ranger <- predict(ames_ranger, ames_test)
head(pred_ranger$predictions)
## [1] 128440.6 154160.1 266428.5 389959.6 225927.0 214493.1

# h2o
pred_h2o <- predict(best_model, ames_test.h2o)
head(pred_h2o)



# ----------------------------
# default RF model
m1 <- randomForest(
  formula = Churn ~ .,
  data    = train
)

plot(m1)

which.min(m1$mse)
## [1] 344

# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])

set.seed(123)
valid_split <- initial_split(ames_train, .8)

# training data
ames_train_v2 <- analysis(valid_split)

# validation data
ames_valid <- assessment(valid_split)
x_test <- ames_valid[setdiff(names(ames_valid), "Sale_Price")]
y_test <- ames_valid$Sale_Price

rf_oob_comp <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train_v2,
  xtest   = x_test,
  ytest   = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")

# names of features
features <- setdiff(names(ames_train), "Sale_Price")

set.seed(123)

m2 <- tuneRF(
  x          = ames_train[features],
  y          = ames_train$Sale_Price,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress
)

# randomForest speed
system.time(
  ames_randomForest <- randomForest(
    formula = Sale_Price ~ .,
    data    = ames_train,
    ntree   = 500,
    mtry    = floor(length(features) / 3)
  )
)
##    user  system elapsed
##  55.371   0.590  57.364

# ranger speed
system.time(
  ames_ranger <- ranger(
    formula   = Sale_Price ~ .,
    data      = ames_train,
    num.trees = 500,
    mtry      = floor(length(features) / 3)
  )
)

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 30, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {

  # train model
  model <- ranger(
    formula         = Sale_Price ~ .,
    data            = ames_train,
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )

  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>%
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# one-hot encode our categorical variables
one_hot <- dummyVars(~ ., ames_train, fullRank = FALSE)
ames_train_hot <- predict(one_hot, ames_train) %>% as.data.frame()

# make ranger compatible names
names(ames_train_hot) <- make.names(names(ames_train_hot), allow_ = FALSE)

# hyperparameter grid search --> same as above but with increased mtry values
hyper_grid_2 <- expand.grid(
  mtry       = seq(50, 200, by = 25),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE  = 0
)

# perform grid search
for(i in 1:nrow(hyper_grid_2)) {

  # train model
  model <- ranger(
    formula         = Sale.Price ~ .,
    data            = ames_train_hot,
    num.trees       = 500,
    mtry            = hyper_grid_2$mtry[i],
    min.node.size   = hyper_grid_2$node_size[i],
    sample.fraction = hyper_grid_2$sampe_size[i],
    seed            = 123
  )

  # add OOB error to grid
  hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid_2 %>%
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {

  optimal_ranger <- ranger(
    formula         = Sale_Price ~ .,
    data            = ames_train,
    num.trees       = 500,
    mtry            = 24,
    min.node.size   = 5,
    sample.fraction = .8,
    importance      = 'impurity'
  )

  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 20)

optimal_ranger$variable.importance %>%
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 important variables")

