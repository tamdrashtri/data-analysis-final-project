# 1. Setup ----


source("code/setup.R")

train       <- read_csv("data/train.churn.csv")
test       <- read_csv("data/test.churn.csv")

# ML Preprocessing Recipe
recipe_obj <- recipe(Churn ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  prep(data = train)

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train)
test_tbl  <- bake(recipe_obj, new_data = test)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190204_101345")

automl_leader


source("code/assess-churn.R")

# 3. Primer: Working With Threshold & Rates ----

performance_h2o <- automl_leader %>%
  h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
  h2o.confusionMatrix()

rates_by_threshold_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble()

rates_by_threshold_tbl %>% glimpse()

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr)

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1)) %>%
  slice(1)

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>%
  mutate(key = fct_reorder2(key, threshold, value)) %>%
  ggplot(aes(threshold, value, color = key)) +
  geom_point() +
  geom_smooth() +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "right") +
  labs(
    title = "Expected Rates",
    y = "Value", x = "Threshold"
  )

# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

predictions_with_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(InternetService, MonthlyCharges)
  )

predictions_with_OT_tbl

ev_with_OT_tbl <- predictions_with_OT_tbl %>%
  mutate(
    churn_cost = calculate_churn_cost(
      n = 1
    )
  ) %>%
  mutate(
    cost_of_no_discount = 0 # when no discount we lose nothing, churners still do the same behavior
  ) %>%
  mutate(
    expected_churn_cost =
      Yes * (churn_cost + cost_of_no_discount) + # when churn = YES -> expected churn cost = churn cost + cost of no discount
      No *  (cost_of_no_discount) # when there is no churn, there is only the cost of no discount: false positive.
  )

ev_with_OT_tbl

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
  summarise(
    total_expected_churn_cost_0 = sum(expected_churn_cost)
  )

total_ev_with_OT_tbl

## 776907

# 4.2 Calculating Expected Value With Targeted OT ----


max_f1_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1)) %>%
  slice(1)

max_f1_tbl

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr

threshold <- max_f1_tbl$threshold

test_targeted_OT_tbl <- test_tbl %>%
  add_column(Yes = predictions_with_OT_tbl$Yes) %>%
  mutate(
    InternetService = case_when(
      Yes >= threshold ~ factor("DSL", levels = levels(test_tbl$InternetService)),
      TRUE ~ InternetService
    )
  ) %>%
  select(-Yes)

test_targeted_OT_tbl

predictions_without_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_without_OT_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(MonthlyCharges, InternetService),
    test_without_OT_tbl %>%
      select(InternetService)
  ) %>%
  rename(
    InternetService_0 = InternetService,
    InternetService_1 = InternetService1
  )

predictions_without_OT_tbl


predictions_targeted_OT_tbl <- predictions_without_OT_tbl %>%
  mutate(
    churn_cost = calculate_churn_cost(
      n = 1
    )
  ) %>%
  mutate(
    cost_of_churn = case_when(
      InternetService_0 == "Fiber optic" & InternetService_1 == "DSL" ~ 0.019 * churn_cost,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    expected_churn_cost =
      Yes * (churn_cost + cost_of_churn) +
      No *  (cost_of_churn)
  )

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>%
  mutate(
    churn_cost = calculate_churn_cost(
      n = 1
    )
  ) %>%
  mutate(
    cost_of_churn = case_when(
      InternetService_0 == "Fiber optic" & InternetService_1 == "DSL" ~ 0.019 * churn_cost,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    cb_tn = cost_of_churn,
    cb_fp = cost_of_churn,
    cb_tp = cost_of_churn + churn_cost,
    cb_fn = cost_of_churn + churn_cost,
    expected_churn_cost =
      Yes * (tpr*cb_tp + fnr*cb_fn) +
      No *  (tnr*cb_tn + fpr*cb_fp)
  )

ev_targeted_OT_tbl

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>%
  summarize(
    total_expected_churn_cost_1 = sum(expected_churn_cost)
  )

total_ev_targeted_OT_tbl

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
  total_ev_with_OT_tbl,
  total_ev_targeted_OT_tbl
) %>%
  mutate(
    savings = total_expected_churn_cost_0 - total_expected_churn_cost_1,
    pct_savings = savings / total_expected_churn_cost_0
  )

savings_tbl

# 19.7%

