# http://uc-r.github.io/svm



library(simpleSetup)

# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive",
              "nycflights13","gmodels", "stringr", "DataExplorer",
              "summarytools", "recipes", "broom", "modelr", "magrittr",
              "corrr", "viridis", "readr", "caret", "forcats", "tidyquant")
library_install(packages)

library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots

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

# Feature Engineering -----------------------------------------------------

# use recipes to quickly turn features into formats that are favourable for logistic regression

recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_dummy(all_nominal()) %>% # convert string to factor
  step_BoxCox(all_numeric()) %>%
  #step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
  prep(data = train) # prepare for this data

bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)


# svm ---------------------------------------------------------------------

# set pseudorandom number generator
set.seed(10)
svm_model <- svm(Churn_Yes ~ ., data=bakedTrain)
summary(svm_model)

plot(svm_model, data = bakedTrain)


# Fit Support Vector Machine model to data set
svmfit <- svm(Churn_Yes ~., data = bakedTrain, kernel = "linear", scale = FALSE)
# Plot Results
plot(svmfit, data = bakedTrain)

kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot')
plot(kernfit, data = x)


# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10)
# Plot Results
plot(svmfit, dat)

# Fit Support Vector Machine model to data set
kernfit <- ksvm(x,y, type = "C-svc", kernel = 'vanilladot', C = 100)
# Plot results
plot(kernfit, data = x)

# find optimal cost of misclassification
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
(bestmod <- tune.out$best.model)

# Create a table of misclassified observations
ypred <- predict(bestmod, dat)
(misclass <- table(predict = ypred, truth = dat$y))

et.seed(456)
cu.summary.complete <- cu.summary[complete.cases(cu.summary),
                                  ]
data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) * 0.7, replace = FALSE)
training.data <- cu.summary.complete[data.samples, ]
test.data <- cu.summary.complete[-data.samples, ]
fit.rf <- randomForest(Type ~ Price + Country + Reliability +
                         Mileage, data = training.data)
prediction.rf <- predict(fit.rf, test.data)
table(test.data$Type, prediction.rf)
