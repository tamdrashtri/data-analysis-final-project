
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
automl_leader <-  h2o.loadModel("models/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190204_101345")


automl_leader

source("code/assess-churn.R")

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


test_without_OT_tbl <- test_tbl %>%
  mutate(InternetService = fct_recode(InternetService, "DSL" = "Fiber optic"))

glimpse(test_tbl)
glimpse(test_without_OT_tbl)


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


ev_without_OT_tbl <- predictions_without_OT_tbl %>%
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

ev_without_OT_tbl

total_ev_without_OT_tbl <- ev_without_OT_tbl %>%
  summarize(
    total_expected_churn_cost_1 = sum(expected_churn_cost)
  )

total_ev_without_OT_tbl

# 624093

bind_cols(
  total_ev_with_OT_tbl,
  total_ev_without_OT_tbl
) %>%
  mutate(
    savings = total_expected_churn_cost_0 - total_expected_churn_cost_1,
    pct_savings = savings / total_expected_churn_cost_0
  )

# save 20%
