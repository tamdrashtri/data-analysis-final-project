






source("code/setup.R")

train <- read_csv("data/train.churn.csv")
test <- read_csv("data/test.churn.csv")


recipe_obj <- recipe(Churn ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  prep(data = train)

recipe_obj


train_tbl <- bake(recipe_obj, new_data = train)
test_tbl  <- bake(recipe_obj, new_data = test)

# modeling

h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]

h2o.describe(train_h2o)

valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

y <- "Churn"
x <- setdiff(names(train_h2o), y)


automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 600,
  nfolds = 7,
  max_models = 5
)


automl_models_h2o@leaderboard

automl_models_h2o@leader

h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20190204_101345") #new

h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20190121_100814")


# Saving & Loading

h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20190121_100814") %>%
  h2o.saveModel(path = "models/h2o_models/")

h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20190204_101345") %>%
  h2o.saveModel(path = "models/h2o_models/") #new

stackEnsemble <- h2o.loadModel("models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190121_100814")

stackEnsemble <- h2o.loadModel("models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190204_101345") # neư

# Making Predictions

stacked_ensemble_h2o <- h2o.loadModel("models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190204_101345")

stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)

predictions_tbl <- predictions %>% as.tibble()
predictions_tbl

# 3. Visualizing The Leaderboard ----

data_transformed <- automl_models_h2o@leaderboard %>%
  as.tibble() %>%
  mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
  slice(1:10) %>%
  rownames_to_column() %>%
  mutate(
    model_id   = as_factor(model_id) %>% reorder(auc),
    model_type = as.factor(model_type)
  ) %>%
  gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>%
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())

data_transformed %>%
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  facet_wrap(~ key, scales = "free_x") +
  theme_linedraw() +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "H2O Leaderboard Metrics",
       subtitle = paste0("Ordered by: auc"),
       y = "Model Postion, Model ID", x = "")


h2o_leaderboard <- automl_models_h2o@leaderboard

plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"),
                                 n_max = 20, size = 4, include_lbl = TRUE) {

  # Setup inputs
  order_by <- tolower(order_by[[1]])

  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())

  # Transformation
  if (order_by == "auc") {

    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
      ) %>%
      gather(key = key, value = value,
             -c(model_id, model_type, rowname), factor_key = T)

  } else if (order_by == "logloss") {

    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as.factor(model_type)
      ) %>%
      gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)

  } else {
    stop(paste0("order_by = '", order_by, "' is not a permitted option."))
  }

  # Visualization
  g <- data_transformed_tbl %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~ key, scales = "free_x") +
    theme_linedraw() +
    scale_color_viridis(discrete = TRUE) +
    labs(title = "Leaderboard Metrics",
         subtitle = paste0("Ordered by: ", toupper(order_by)),
         y = "Model Postion, Model ID", x = "")

  if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))

  return(g)

}

automl_models_h2o@leaderboard %>%
  plot_h2o_leaderboard(order_by = "logloss")


# 4. Assessing Performance ----

stacked_ensemble_h2o <- h2o.loadModel("models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190121_100814")

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(performance_h2o)
performance_h2o %>% slotNames()

performance_h2o@metrics

# ROC Plot

path <- "models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190121_100814"

load_model_performance_metrics <- function(path, test_tbl) {

  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))

  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc)
}

model_metrics_tbl <- fs::dir_info(path = "models/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()

model_metrics_tbl %>%
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_linedraw() +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.direction = "vertical",
        legend.position = "none") +
  labs(
    title = "ROC Plot",
    subtitle = "Performance of the Best Performing Model - Stack Ensembles"
  )

# Gain & Lift

ranked_predictions_tbl <- predictions_tbl %>%
  bind_cols(test_tbl) %>%
  select(predict:Yes, Churn) %>%
  arrange(desc(Yes))

calculated_gain_lift_tbl <- ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarise(
    cases = n(),
    responses = sum(Churn == "Yes")
  ) %>%
  arrange(desc(ntile)) %>%
  mutate(group = row_number()) %>%
  select(group, cases, responses) %>%
  mutate(
    cumulative_responses = cumsum(responses),
    pct_responses        = responses / sum(responses),
    gain                 = cumsum(pct_responses),
    cumulative_pct_cases = cumsum(cases) / sum(cases),
    lift                 = gain / cumulative_pct_cases,
    gain_baseline        = cumulative_pct_cases,
    lift_baseline        = gain_baseline / cumulative_pct_cases
  )

calculated_gain_lift_tbl


gain_lift_tbl <- performance_h2o %>%
  h2o.gainsLift() %>%
  as.tibble()

gain_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain = cumulative_capture_rate) %>%
  gather(key = key, value = value, gain, baseline)

gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_linedraw() +
  scale_color_viridis(discrete = TRUE) +
  labs(
    title = "Gain Chart",
    x = "Cumulative Data Fraction",
    y = "Gain"
  )

lift_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  gather(key = key, value = value, lift, baseline)

lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_linedraw() +
  scale_color_viridis(discrete = TRUE) +
  labs(
    title = "Lift Chart",
    x = "Cumulative Data Fraction",
    y = "Lift"
  )
