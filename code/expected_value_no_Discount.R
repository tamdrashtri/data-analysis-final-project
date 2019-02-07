# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# NO OVERTIME POLICY ----

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

source("code/assess-churn.R")


# two ways we can calculate the potential loss and profits

# 1. if we predicted churn = YES -> false negative and true negatives --> don't give them discount --> loss revenue of 33 months.
# 1b. if predicted churn = NO --> true negatives and true positives --> give them discount --> profit = 1718.72


# 2. using confusion matrix - use percents we have from the confusion matrix and calculate revenues and loss based on that matrix


# first, when there is no discount then what happen
predictions_with_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(MonthlyCharges)
  )

glimpse(predictions_with_OT_tbl)

# 1. if we predicted churn = YES -> false negative and true negatives --> don't give them discount --> loss revenue of 33 months.
# if predict = YES --> loss revenue = monthlycharges*0.85*33
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
# 638533
# updated monthlycharges = 768518

# 3.2 Calculating Expected Value With discount ----
# the best scenario happens when we give discount to people who have to pay more than 70 dollars

test_without_OT_tbl <- test_tbl %>%
  mutate(DiscountedMonthlyCharges = MonthlyCharges*0.85)

test_without_OT_tbl


predictions_without_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_without_OT_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(MonthlyCharges),
    test_without_OT_tbl %>%
      select(DiscountedMonthlyCharges)
  ) %>%
  rename(
    InitialMonthlyCharges = MonthlyCharges
  )

predictions_without_OT_tbl

avg_overtime_pct <- 0.10

ev_without_OT_tbl <- predictions_without_OT_tbl %>%
  mutate(
    churn_cost = calculate_churn_cost(
      n = 1
    )
  ) %>%
  mutate(
    # how to determine false positives and false negatives?
    # if there is a discount, what can be lost?
    # when churn = YES -->
    # cost_of_discount = churn_cost + (monthcharge - monthlycharges_with_DC)
    # when churn = NO -->
    # cost_of_discount =  (monthcharge - monthlycharges_with_DC)
    cost_of_discount = InitialMonthlyCharges - DiscountedMonthlyCharges
  ) %>%
  mutate(
    expected_churn_cost =
      Yes * (churn_cost + cost_of_discount) +
      No *  (cost_of_discount)
  )

ev_without_OT_tbl

total_ev_without_OT_tbl <- ev_without_OT_tbl %>%
  summarize(
    total_expected_churn_cost_1 = sum(expected_churn_cost)
  )


total_ev_without_OT_tbl
# 789124
# 831027

bind_cols(
  total_ev_with_OT_tbl,
  total_ev_without_OT_tbl
) %>%
  mutate(
    savings = total_expected_churn_cost_0 - total_expected_churn_cost_1,
    pct_savings = savings / total_expected_churn_cost_0
  )


