library(simpleSetup)

# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive",
              "nycflights13","gmodels", "stringr", "DataExplorer",
              "summarytools", "recipes", "broom", "modelr", "magrittr",
              "corrr", "viridis", "readr", "caret", "forcats", "tidyquant",
              "rpart", "rpart.plot", "ipred", "rsample")

library_install(packages)


churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Load data
set.seed(123)
library(caret)

train.index <- createDataPartition(churn$Churn, p = .7, list = FALSE)
train <- churn[ train.index,]
test  <- churn[-train.index,]

test %<>%
  select(-customerID) %>%
  drop_na()

train %<>%
  select(-customerID) %>%
  drop_na()

set.seed(123)

# Feature Engineering -----------------------------------------------------

# use recipes to quickly turn features into formats that are favourable for logistic regression

recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_dummy(all_nominal()) %>% # convert string to factor
  step_BoxCox(all_numeric()) %>%
  #step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
  prep(data = train) # prepare for this data

bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)

m1 <- rpart(
  formula = Churn_Yes ~ .,
  data    = bakedTrain,
  method  = "anova"
)

rpart.plot(m1)

plotcp(m1)


m2 <- rpart(
  formula = Churn_Yes ~ .,
  data    = bakedTrain,
  method  = "anova",
  control = list(cp = 0, xval = 10)
)

plotcp(m2)
abline(v = 12, lty = "dashed")

m1$cptable

m3 <- rpart(
  formula = Churn_Yes ~ .,
  data    = bakedTrain,
  method  = "anova",
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
)

m3$cptable

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

head(hyper_grid)

nrow(hyper_grid)

models <- list()

for (i in 1:nrow(hyper_grid)) {

  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]

  # train a model and store in the list
  models[[i]] <- rpart(
    formula = Churn_Yes ~ .,
    data    = bakedTrain,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

optimal_tree <- rpart(
  formula = Churn_Yes ~ .,
  data    = bakedTrain,
  method  = "anova",
  control = list(minsplit = 11, maxdepth = 8, cp = 0.01)
)

pred <- predict(optimal_tree, newdata = bakedTest)
RMSE(pred = pred, obs = bakedTest$Churn_Yes)

# make bootstrapping reproducible
set.seed(123)

# train bagged model
bagged_m1 <- bagging(
  formula = Churn_Yes ~ .,
  data    = bakedTrain,
  coob    = TRUE
)

bagged_m1

# assess 10-50 bagged trees
ntree <- 10:50

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)

  # perform bagged model
  model <- bagging(
    formula = Churn_Yes ~ .,
    data    = bakedTrain,
    coob    = TRUE,
    nbagg   = ntree[i]
  )
  # get OOB error
  rmse[i] <- model$err
}

plot(ntree, rmse, type = 'l', lwd = 2)
abline(v = 25, col = "red", lty = "dashed")

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10)

# CV bagged model
bagged_cv <- train(
  Churn_Yes ~ .,
  data = bakedTrain,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv


# plot most important variables
plot(varImp(bagged_cv), 20)

pred <- predict(bagged_cv, bakedTest)

RMSE(pred, bakedTest$Churn_Yes)
