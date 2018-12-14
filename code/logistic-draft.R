# TODO----
# - annotate
# - explain what everything does
# - twist variables a bit
# - try some samples to predict it
# - how to know if the ROC curve I'm having is correct?
# - understand the roc curve more
# - does strings2factor help or is it correct to use that for logistic regression?
# - learn caret
# - plot confusion matrix
# - changes: try different algorithms
# - do a SWOT analysis
# - Agile techniques
# - hyperparameter
# -

library(simpleSetup)

# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive",
              "nycflights13","gmodels", "stringr", "DataExplorer",
              "summarytools", "recipes", "broom", "modelr", "magrittr",
              "corrr", "viridis", "readr", "caret", "forcats")
library_install(packages)

churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Load data
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(churn), replace = T, prob = c(0.6,0.4))

#
train <- churn[sample, ]
test <- churn[!sample, ]

test %<>%
  select(-customerID) %>%
  drop_na()

train %<>%
  select(-customerID) %>%
  drop_na()


# Feature Engineering -----------------------------------------------------

# use recipes to quickly turn features into formats that are favourable for logistic regression
recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_string2factor(all_nominal()) %>% # convert string to factor
  prep(data = train) # prepare for this data

recObj

bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)


# Test model --------------------------------------------------------------

# glm performs logistic regression to test the model
model1 <- glm(Churn ~ ., family = "binomial", data = bakedTrain)
summary(model1)
tidy(model1)

col_index <-
  data.frame(varImp(model1)) %>%
  mutate(names = factor(row.names(.))) %>%
  arrange(-Overall)

ggplot(col_index, aes(x = names, y = Overall)) +
  geom_segment(aes(xend = names, yend = 0)) +
  geom_point() +
  coord_flip() +
  scale_color_viridis() +
  theme_linedraw()

#
test.predicted.m1 <- predict(model1, newdata = bakedTest, type = "response")

model1 = table(bakedTest$Churn, test.predicted.m1 > 0.5) %>% prop.table() %>% round(3)

test %>%
  mutate(m1.pred = ifelse(test.predicted.m1 > 0.5, "Yes", "No")) %>%
  summarise(m1.error = mean(Churn != m1.pred))


# Model Performance -------------------------------------------------------

library(ROCR)

prediction(test.predicted.m1, test$Churn) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test.predicted.m1, test$Churn) %>%
  performance(measure = "auc") %>%
  .@y.values #0.84





