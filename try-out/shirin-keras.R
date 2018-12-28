# Load libraries
library(tidyverse) # for tidy data analysis
library(readr)     # for fast reading of input files
library(caret)     # for convenient splitting
library(mice)      # mice package for Multivariate Imputation by Chained Equations (MICE)
library(keras)     # for neural nets
library(lime)      # for explaining neural nets
library(rsample)   # for splitting training and test data
library(recipes)   # for preprocessing
library(yardstick) # for evaluation
library(ggthemes)  # for additional plotting themes
library(corrplot)  # for correlation

theme_set(theme_minimal())

churn_data_raw <- read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

churn_data_raw %>%
  count(Churn)

churn_data_raw %>%
  mutate(SeniorCitizen = as.character(SeniorCitizen)) %>%
  select(-customerID) %>%
  select_if(is.character) %>%
  select(Churn, everything()) %>%
  gather(x, y, gender:PaymentMethod) %>%
  count(Churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

churn_data_raw %>%
  select(-customerID) %>%
  #select_if(is.numeric) %>%
  select(Churn, MonthlyCharges, tenure, TotalCharges) %>%
  gather(x, y, MonthlyCharges:TotalCharges) %>%
  ggplot(aes(x = y, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

churn_data <- churn_data_raw %>%
  select(-customerID)

md.pattern(churn_data, plot = FALSE)

imp <- mice(data = churn_data,  print = FALSE)
train_data_impute <- complete(imp, "long")

churn_data <- churn_data %>%
  drop_na()

set.seed(42)
index <- createDataPartition(churn_data$Churn, p = 0.7, list = FALSE)

train_data <- churn_data[index, ]
test_data  <- churn_data[-index, ]

index2 <- createDataPartition(test_data$Churn, p = 0.5, list = FALSE)

valid_data <- test_data[-index2, ]
test_data <- test_data[index2, ]

recipe_churn <- recipe(Churn ~ ., train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_data)

train_data <- bake(recipe_churn, new_data = train_data) %>%
  select(Churn, everything())

valid_data <- bake(recipe_churn, new_data = valid_data) %>%
  select(Churn, everything())

test_data <- bake(recipe_churn, new_data = test_data) %>%
  select(Churn, everything())

train_y_drop <- to_categorical(as.integer(as.factor(train_data$Churn)) - 1, 2)
colnames(train_y_drop) <- c("No", "Yes")

valid_y_drop <- to_categorical(as.integer(as.factor(valid_data$Churn)) - 1, 2)
colnames(valid_y_drop) <- c("No", "Yes")

test_y_drop <- to_categorical(as.integer(as.factor(test_data$Churn)) - 1, 2)
colnames(test_y_drop) <- c("No", "Yes")

# if training with binary crossentropy
train_y_drop <- train_y_drop[, 2, drop = FALSE]
head(train_y_drop)


valid_y_drop <- valid_y_drop[, 2, drop = FALSE]
test_y_drop <- test_y_drop[, 2, drop = FALSE]

train_data_bk <- select(train_data, -Churn)
head(train_data_bk)

valid_data_bk <- select(valid_data, -Churn)
test_data_bk <- select(test_data, -Churn)

train_data$Churn <- ifelse(train_data$Churn == "Yes", 1, 0)
valid_data$Churn <- ifelse(valid_data$Churn == "Yes", 1, 0)
test_data$Churn <- ifelse(test_data$Churn == "Yes", 1, 0)

model_keras <- keras_model_sequential()

model_keras %>%
  layer_dense(units = 32, kernel_initializer = "uniform", activation = "relu",
              input_shape = ncol(train_data_bk)) %>%
  layer_dropout(rate = 0.2) %>%

  layer_dense(units = 16, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%

  layer_dense(units = 8, kernel_initializer = "uniform", activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%

  layer_dense(units = 1,
              kernel_initializer = "uniform", activation = "sigmoid") %>%

  compile(
    optimizer = 'adamax',
    loss      = 'binary_crossentropy',
    metrics   = c("binary_accuracy", "mse")
  )

summary(model_keras)

fit_keras <- fit(model_keras,
                 x = as.matrix(train_data_bk),
                 y = train_y_drop,
                 batch_size = 32,
                 epochs = 20,
                 #validation_split = 0.30,
                 validation_data = list(as.matrix(valid_data_bk), valid_y_drop),
                 verbose = 2
)
plot(fit_keras) +
  scale_color_tableau() +
  scale_fill_tableau()
