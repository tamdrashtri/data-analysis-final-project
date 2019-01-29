# https://afit-r.github.io/tree_based_methods

source("code/setup.R")

churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# create training and testing dataset
train.index <- createDataPartition(churn$Churn, p = .7, list = FALSE)
train <- churn[ train.index,]
test  <- churn[-train.index,]

# remove unnecessary columns and drop missing values for both training and testing
test %<>%
  select(-customerID) %>%
  drop_na()

train %<>%
  select(-customerID) %>%
  drop_na()

write_csv(train, "data/train.churn.csv")
write_csv(test, "data/test.churn.csv")

# Feature Engineering -----------------------------------------------------


recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -Churn) %>% # convert string to factor
  step_corr(all_predictors()) %>%
  prep(data = train) # prepare for this data

bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)

write_csv(bakedTrain, "data/bakedTrain.csv")
write_csv(bakedTest, "data/bakedTest.csv")
