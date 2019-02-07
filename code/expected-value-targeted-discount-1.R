# 1. Setup ----


source("code/setup.R")

train       <- read_csv("data/train.churn.csv")
test       <- read_csv("data/test.churn.csv")

# ML Preprocessing Recipe
recipe_obj <- recipe(Churn ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -Churn) %>%
  step_corr(all_predictors()) %>%
  prep(data = train)

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train)
test_tbl  <- bake(recipe_obj, new_data = test)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190121_100814")

automl_leader

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

source("code/assess-churn.R")

predictions_with_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(MonthlyCharges)
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

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
  summarise(
    total_expected_churn_cost_0 = sum(expected_churn_cost)
  )

total_ev_with_OT_tbl

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

# if YES > threshold => mutate to a new column called discount with YES or NO variable
test_targeted_OT_tbl <- test_tbl %>%
  add_column(Yes = predictions_with_OT_tbl$Yes) %>%
  mutate(
    MonthlyCharges_threshold = case_when(
      Yes >= threshold ~ (MonthlyCharges = MonthlyCharges*0.85),
      TRUE ~ MonthlyCharges
    )
  ) %>%
  select(-Yes)

test_targeted_OT_tbl


predictions_without_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(MonthlyCharges),
    test_targeted_OT_tbl %>%
      select(MonthlyCharges_threshold)
  ) %>%
  rename(
    InitialMonthlyCharges = MonthlyCharges,
    DiscountedMonthlyCharges1 = MonthlyCharges_threshold
  )

predictions_without_OT_tbl


