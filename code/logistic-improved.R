
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
  prep(data = train) # prepare for this data

recObj

recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_dummy(all_nominal()) %>% # convert string to factor
  step_BoxCox(all_numeric()) %>%
  #step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
  prep(data = train) # prepare for this data

bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)


bakedTrain %>%
  get_correlation(formula = Churn_Yes ~ SeniorCitizen:PaymentMethod_Mailed.check)

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {

  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)

  data_cor <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)

  if (fct_reorder) {
    data_cor <- data_cor %>%
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }

  if (fct_rev) {
    data_cor <- data_cor %>%
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }

  return(data_cor)

}

bakedTrain %>%
  get_cor(Churn_Yes, fct_reorder = T, fct_rev = T)

data         <- bakedTrain
feature_expr <- quo(Churn_Yes)

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE,
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1,
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]) {

  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)

  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())

  g <- data_cor %>%
    ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    theme_tq() +
    scale_color_manual(values = c(color_neg, color_pos))

  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)

  return(g)

}

bakedTrain %>%
  plot_cor(target = Churn_Yes, fct_reorder = T, fct_rev = T)

# Test model --------------------------------------------------------------

# glm performs logistic regression to test the model

model1 <- glm(Churn_Yes ~ ., family = "binomial", data = bakedTrain)
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
  .@y.values #0.85, 0.8414





