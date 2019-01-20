source("code/setup.R")

train <- read_csv("data/bakedTrain.csv")

glimpse(dfm)

dfm <- train %>%
  rename("tag" = "Churn")

dfm %<>%
  mutate(tag = as.factor(tag))

seed <- 123

results <- lares::h2o_automl(df = dfm, seed = seed, max_time = 600)

lares::mplot_full(tag = results$scores_test$tag,
                  score = results$scores_test$score,
                  subtitle = "Churn dataset")

lares::mplot_importance(var = results$importance$variable,
                        imp = results$importance$importance,
                        subtitle = "Churn dataset")

lares::h2o_selectmodel(results = results, which_model = "GLM")

lares::rmse(results$tag, results$score)

lares::mplot_metrics(results)

lares::h2o_selectmodel(results, which_model = 1)

lares::mplot_full(tag = results$label,
                  score = results$pred,
                  splits = 10,
                  subtitle = "Salary Regression Model",
                  model_name = "simple_model_02",
                  save = T)

lares::mplot_lineal(tag = results$label,
                    score = results$pred,
                    subtitle = "Salary Regression Model",
                    model_name = "simple_model_02")
