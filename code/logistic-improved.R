
library(simpleSetup)

# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive",
              "nycflights13","gmodels", "stringr", "DataExplorer",
              "summarytools", "recipes", "broom", "modelr", "magrittr",
              "corrr", "viridis", "readr", "caret", "forcats", "tidyquant",
              "caret")
library_install(packages)

churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Load data
set.seed(123)

# partition the data set into training and testing dataset, the testing data set has 0.7 of the whole data. The process of selecting entries is random.
train.index <- createDataPartition(churn$Churn, p = .7, list = FALSE)
train <- churn[ train.index,]
test  <- churn[-train.index,]

# clean the data set by removing customerID, not useful for analysis and drop missing values
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
  prep(data = train) # prepare for this data

bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)

# Train model --------------------------------------------------------------

# glm performs logistic regression to test the model

model1 <- glm(Churn_Yes ~ ., family = "binomial", data = bakedTrain)

# take a look at the model
summary(model1)

# organize variable importance
col_index <-
  data.frame(varImp(model1)) %>%
  mutate(names = factor(row.names(.))) %>%
  arrange(-Overall)

# draw varImp plot using ggplot
ggplot(col_index, aes(x = reorder(names), y = Overall)) +
  geom_segment(aes(xend = names, yend = 0)) +
  geom_point() +
  coord_flip() +
  scale_color_viridis() +
  theme_linedraw()

# Test model --------------------------------------------------------------
# using the function predict to test it on the test data set
test.predicted.m1 <- predict(model1, newdata = bakedTest, type = "response")

# get the RSME value
test %>%
  mutate(m1.pred = ifelse(test.predicted.m1 > 0.5, "Yes", "No")) %>%
  summarise(m1.error = mean(Churn != m1.pred))


# Model Performance -------------------------------------------------------

library(ROCR)

# draw ROC curve
prediction(test.predicted.m1, test$Churn) %>%
  performance(measure = "tpr", x.measure = "fpr") %>% #tpr specifies true positive rate, fpr represents false positive rate.
  plot()

# get the AUC value
prediction(test.predicted.m1, test$Churn) %>%
  performance(measure = "auc") %>%
  .@y.values #0.85, 0.8414





