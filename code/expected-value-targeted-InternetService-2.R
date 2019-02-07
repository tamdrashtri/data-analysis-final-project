
## 1. Setup ----


source("code/setup.R")

train       <- read_csv("data/train.churn.csv")
test       <- read_csv("data/test.churn.csv")

## ML Preprocessing Recipe
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

# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----


data <- test_tbl
h2o_model <- automl_leader


calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {


  data_0_tbl <- as.tibble(data)

  # 4. Expected Value

  # 4.1 Calculating Expected Value With OT

  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(InternetService, MonthlyCharges)
    )

  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      churn_cost = calculate_churn_cost(
        n = 1)
    ) %>%
    mutate(
      cost_of_churn = 0
    ) %>%
    mutate(
      expected_churn_cost =
        Yes * (churn_cost + cost_of_churn) +
        No *  (cost_of_churn)
    )


  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_churn_cost_0 = sum(expected_churn_cost)
    )

  # 4.2 Calculating Expected Value With Targeted OT

  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      InternetService = case_when(
        Yes >= threshold ~ factor("DSL", levels = levels(test_tbl$InternetService)),
        TRUE ~ InternetService
      )
    ) %>%
    select(-Yes)

  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(InternetService, MonthlyCharges),
      data_1_tbl %>%
        select(InternetService)
    ) %>%
    rename(
      InternetService_0 = InternetService,
      InternetService_1 = InternetService1
    )

  ev_1_tbl <- pred_1_tbl %>%
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



  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_churn_cost_1 = sum(expected_churn_cost)
    )


  # 4.3 Savings Calculation

  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_churn_cost_0 - total_expected_churn_cost_1,
      pct_savings = savings / total_expected_churn_cost_0
    )

  return(savings_tbl$savings)

}


# No OT Policy

test_tbl %>%
  calculate_savings_by_threshold(automl_leader, threshold = 0,
                                 tnr = 0, fnr = 0, tpr = 1, fpr = 1)

# 153762.1

# Do Nothing Policy

test_tbl %>%
  calculate_savings_by_threshold(automl_leader, threshold = 1,
                                 tnr = 1, fnr = 1, tpr = 0, fpr = 0)

# 0

# Threshold @ Max F1

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1))

max_f1_savings <- calculate_savings_by_threshold(test_tbl, automl_leader,
                                                 threshold = max_f1_tbl$threshold,
                                                 tnr = max_f1_tbl$tnr,
                                                 fpr = max_f1_tbl$fpr,
                                                 fnr = max_f1_tbl$fnr,
                                                 tpr = max_f1_tbl$tpr)

max_f1_savings

# 146643.5

# 5.2 Optimization ----

smpl <- seq(1, 220, length.out = 20) %>% round(digits = 0)

partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)

rates_by_threshold_optimized_tbl <- rates_by_threshold_tbl %>%
  select(threshold, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        threshold = threshold,
        tnr = tnr,
        fnr = fnr,
        fpr = fpr,
        tpr = tpr
      ),
      .f = partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)
    )
  )

rates_by_threshold_optimized_tbl


rates_by_threshold_optimized_tbl %>%
  ggplot(aes(threshold, savings)) +
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +

  # Optimal Point
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(savings == max(savings))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -1, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(savings == max(savings))) +

  # F1 Max
  geom_vline(xintercept = max_f1_tbl$threshold,
             color = palette_light()[[5]], size = 2) +
  annotate(geom = "label", label = scales::dollar(max_f1_savings),
           x = max_f1_tbl$threshold, y = max_f1_savings, vjust = -1,
           color = palette_light()[[1]]) +

  # No OT Policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == min(threshold))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == min(threshold))) +

  # Do Nothing Policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == max(threshold))) +
  geom_label(aes(label = scales::dollar(round(savings, 0))),
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == max(threshold))) +

  # Aesthestics
  theme_tq() +
  expand_limits(x = c(-.1, 1.1), y = 8e5) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Optimization Results: Expected Savings Maximized At 14.9%",
    x = "Threshold (%)", y = "Savings"
  )






# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

data <- test_tbl

h2o_model <- automl_leader

calculate_savings_by_threshold_2 <- function(data, h2o_model, threshold = 0,
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             ) {

  data_0_tbl <- as.tibble(data)


  # 4. Expected Value

  # 4.1 Calculating Expected Value With OT

  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime)
    )

  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        # Changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
    mutate(
      cost_of_policy_change = 0
    ) %>%
    mutate(
      expected_attrition_cost =
        Yes * (attrition_cost + cost_of_policy_change) +
        No *  (cost_of_policy_change)
    )


  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )

  # 4.2 Calculating Expected Value With Targeted OT

  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes)

  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>%
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime,
      OverTime_1 = OverTime1
    )


  avg_overtime_pct <- avg_overtime_pct # Changed in _2 ----

  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        # Changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
    mutate(
      cost_of_policy_change = case_when(
        OverTime_1 == "No" & OverTime_0 == "Yes"
        ~ attrition_cost * avg_overtime_pct,
        TRUE ~ 0
      ))%>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_fn = attrition_cost + cost_of_policy_change,
      cb_tp = attrition_cost + cost_of_policy_change,
      expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) +
        No * (tnr*cb_tn + fpr*cb_fp)
    )


  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )


  # 4.3 Savings Calculation

  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )

  return(savings_tbl$savings)

}

test_tbl %>%
  calculate_savings_by_threshold_2(automl_leader,
                                   avg_overtime_pct = 0.10,
                                   net_revenue_per_employee = 250000)

# 6.2 Sensitivity Analysis ----

max_savings_rates_tbl <- rates_by_threshold_optimized_tbl %>%
  filter(savings == max(savings))

max_savings_rates_tbl

calculate_savings_by_threshold_2(
  data = test_tbl,
  h2o_model = automl_leader,
  threshold = max_savings_rates_tbl$threshold,
  tnr = max_savings_rates_tbl$tnr,
  fnr = max_savings_rates_tbl$fnr,
  fpr = max_savings_rates_tbl$fpr,
  tpr = max_savings_rates_tbl$tpr
)

calculate_savings_by_threshold_2_preloaded <- partial(
  calculate_savings_by_threshold_2,
  # Function Arguments
  data = test_tbl,
  h2o_model = automl_leader,
  threshold = max_savings_rates_tbl$threshold,
  tnr = max_savings_rates_tbl$tnr,
  fnr = max_savings_rates_tbl$fnr,
  fpr = max_savings_rates_tbl$fpr,
  tpr = max_savings_rates_tbl$tpr
)

calculate_savings_by_threshold_2_preloaded(
  avg_overtime_pct = 0.10,
  net_revenue_per_employee = 250000)

sensitivity_tbl <- list(
  avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
  net_revenue_per_employee = seq(200000, 400000, by = 50000)
) %>%
  cross_df() %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        avg_overtime_pct = avg_overtime_pct,
        net_revenue_per_employee = net_revenue_per_employee
      ),
      .f = calculate_savings_by_threshold_2_preloaded
    )
  )

sensitivity_tbl

sensitivity_tbl %>%
  ggplot(aes(avg_overtime_pct, net_revenue_per_employee)) +
  geom_tile(aes(fill = savings)) +
  geom_label(aes(label = savings %>% round(0) %>% scales::dollar())) +
  theme_tq() +
  theme(legend.position = "none") +
  scale_fill_gradient2(
    low = palette_light()[[2]],
    mid = "white",
    high = palette_light()[[1]],
    midpoint = 0
  ) +
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0.05, 0.30, by = 0.05)
  ) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Profitability Heatmap: Expected Savings Sensitivity Analysis",
    subtitle = "How sensitive is savings to net revenue per emploee and average overtime percentage?",
    x = "Average Overtime Percentage",
    y = "Net Revenue Per Employee"
  )

