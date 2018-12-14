library(rsample)  # data splitting
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o)

# conver some numeric variables to factors
attrition <- attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  )

# Create training (70%) and test (30%) sets for the attrition data.
# Use set.seed for reproducibility
set.seed(123)
split <- initial_split(attrition, prop = .7, strata = "Attrition")
train <- training(split)
test  <- testing(split)

# distribution of Attrition rates across train & test set
table(train$Attrition) %>% prop.table()
##
##       No      Yes
## 0.838835 0.161165
table(test$Attrition) %>% prop.table()
##
##        No       Yes
## 0.8386364 0.1613636

train %>%
  filter(Attrition == "Yes") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>%
  select(Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

# create response and feature data
features <- setdiff(names(train), "Attrition")
x <- train[, features]
y <- train$Attrition

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv",
  number = 10
)

# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

# results
confusionMatrix(nb.m1)

# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))
##   usekernel fL adjust  Accuracy     Kappa AccuracySD   KappaSD
## 1      TRUE  1      3 0.8737864 0.4435322 0.02858175 0.1262286
## 2      TRUE  0      2 0.8689320 0.4386202 0.02903618 0.1155707
## 3      TRUE  2      3 0.8689320 0.4750282 0.02830559 0.0970368
## 4      TRUE  2      4 0.8689320 0.4008608 0.02432572 0.1234943
## 5      TRUE  4      5 0.8689320 0.4439767 0.02867321 0.1354681

# plot search grid results
plot(nb.m2)


# results for best model
confusionMatrix(nb.m2)

pred <- predict(nb.m2, newdata = test)
confusionMatrix(pred, test$Attrition)

h2o.no_progress()
h2o.init()

# create feature names
y <- "Attrition"
x <- setdiff(names(train), y)

# h2o cannot ingest ordered factors
train.h2o <- train %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# train model
nb.h2o <- h2o.naiveBayes(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 10,
  laplace = 0
)

# assess results
h2o.confusionMatrix(nb.h2o)

# do a little preprocessing
preprocess <- preProcess(train, method = c("BoxCox", "center", "scale", "pca"))
train_pp   <- predict(preprocess, train)
test_pp    <- predict(preprocess, test)

# convert to h2o objects
train_pp.h2o <- train_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

test_pp.h2o <- test_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# get new feature names --> PCA preprocessing reduced and changed some features
x <- setdiff(names(train_pp), "Attrition")

# create tuning grid
hyper_params <- list(
  laplace = seq(0, 5, by = 0.5)
)

# build grid search
grid <- h2o.grid(
  algorithm = "naivebayes",
  grid_id = "nb_grid",
  x = x,
  y = y,
  training_frame = train_pp.h2o,
  nfolds = 10,
  hyper_params = hyper_params
)

# Sort the grid models by mse
sorted_grid <- h2o.getGrid("nb_grid", sort_by = "accuracy", decreasing = TRUE)
sorted_grid
## H2O Grid Details
## ================
##
## Grid ID: nb_grid
## Used hyper parameters:
##   -  laplace
## Number of models: 11
## Number of failed models: 0
##
## Hyper-Parameter Search Summary: ordered by decreasing accuracy
##    laplace        model_ids           accuracy
## 1      2.5  nb_grid_model_5 0.8572815533980582
## 2      5.0 nb_grid_model_10 0.8572815533980582
## 3      3.0  nb_grid_model_6 0.8533980582524272
## 4      2.0  nb_grid_model_4 0.8504854368932039
## 5      1.5  nb_grid_model_3 0.8466019417475728
## 6      4.5  nb_grid_model_9 0.8407766990291262
## 7      0.5  nb_grid_model_1 0.8398058252427184
## 8      4.0  nb_grid_model_8 0.8339805825242719
## 9      3.5  nb_grid_model_7 0.8281553398058252
## 10     1.0  nb_grid_model_2 0.8194174757281554
## 11     0.0  nb_grid_model_0 0.8009708737864077

# grab top model id
best_h2o_model <- sorted_grid@model_ids[[1]]
best_model <- h2o.getModel(best_h2o_model)

# confusion matrix of best model
h2o.confusionMatrix(best_model)
## Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.402989172890541:
##         No Yes    Error       Rate
## No     796  68 0.078704    =68/864
## Yes     59 107 0.355422    =59/166
## Totals 855 175 0.123301  =127/1030

# ROC curve
auc <- h2o.auc(best_model, xval = TRUE)
fpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.fpr() %>% .[['fpr']]
tpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.tpr() %>% .[['tpr']]
data.frame(fpr = fpr, tpr = tpr) %>%
  ggplot(aes(fpr, tpr) ) +
  geom_line() +
  ggtitle( sprintf('AUC: %f', auc) )

h2o.performance(best_model, newdata = test_pp.h2o)
h2o.predict(nb.h2o, newdata = test_pp.h2o)
h2o.shutdown(prompt = FALSE)


