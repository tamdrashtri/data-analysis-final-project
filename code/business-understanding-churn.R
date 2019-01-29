

source("code/setup.R")

churnYes <- churn_clean %>%
  filter(Churn %in% c("Yes"))

mean(churnYes$MonthlyCharges)
mean(churnYes$TotalCharges) # 1531

# mean of monthly charge * number of churners
74.44133*1869
# 139130.8 = loss revenue of monthly charges

# or sum monthlycharges
sum(churnYes$MonthlyCharges)
# 139130.9

# Acquisition cost of a new customer is $315
1869*315
# 588735

# Industry KPI: Churn rate is 1.9% across top four carriers (AT&T, Verizon, T-Mobile, Sprint)


# functions ---------------------------------------------------------------

calculate_churn_cost <- function(

  # Employee
  n                    = 1,
  revenue_per_customer = 62,

  # from dataset
  monthly_charge       = 74.4,
  total_charges        = 1532,
  number_of_churners   = 1869,

  #
  churn_rate           = 1.9,
  customer_lifetime    = 19, # months
  CLV                  = 1782,

  # profit loss
  gross_margin             = 0.55,
  gross_profit             = 240,
  acquisition_cost         = 315

) {

  # Cost = Acquisition cost for a new customer + CLV

  # Total Cost of customer churn
  total_cost <- (CLV*0.68 + acquisition_cost)*n

  return(total_cost)

}

assess_churn <- function(data, churn_col, churn_value, baseline_pct) {

  churn_col_expr <- enquo(churn_col)

  data %>%
    filter((!! churn_col_expr) %in% churn_value) %>%
    arrange(desc(pct)) %>%
    mutate(
      above_industry_avg = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    )


}

count_to_pct <- function(data, ..., col = n) {

  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)

  ret <- data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()

  return(ret)

}


churn_clean %>%
  group_by(Churn) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))
# pct of churners are 0.265
# churners are 1869, non-churners are 5174

plot_churn <- function(data, ..., .value,
                           fct_reorder = TRUE,
                           fct_rev = FALSE,
                           include_lbl = TRUE,
                           color = palette_light()[[1]],
                           units = c("0", "K", "M")) {


  # Inputs

  group_vars_expr <- quos(...)
  if (length(group_vars_expr) == 0)
    group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))

  value_expr <- enquo(.value)
  value_name <- quo_name(value_expr)

  units_val <- switch(units[[1]],
                      "M" = 1e6,
                      "K" = 1e3,
                      "0"  = 1)
  if (units[[1]] == "0") units <- ""


  # Data Manipulation
  usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)

  data_manipulated <- data %>%
    mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>%
    mutate(value_text = str_c(usd(!! value_expr / units_val),
                              units[[1]], sep = ""))


  if (fct_reorder) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
      arrange(name)
  }

  if (fct_rev) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_rev(name)) %>%
      arrange(name)
  }

  # Visualization

  g <- data_manipulated %>%
    ggplot(aes_string(x = value_name, y = "name")) +
    geom_segment(aes(xend = 0, yend = name), color = color) +
    geom_point(aes_string(size = value_name), color = color) +
    scale_x_continuous(labels = scales::dollar) +
    theme_tq() +
    scale_size(range = c(3, 5)) +
    theme(legend.position = "none")


  if (include_lbl) {
    g <- g +
      geom_label(aes_string(label = "value_text", size = value_name),
                 hjust = "inward", color = color)
  }

  return(g)

}




# Payment Method versus Paperless Billing versus Contract ----
churn_clean %>%

  count(PaymentMethod, PaperlessBilling, Churn) %>%

  count_to_pct(PaymentMethod, PaperlessBilling) %>%

  assess_churn(Churn, churn_value = "Yes", baseline_pct = 0.019) %>%

  mutate(
    cost_of_churn = calculate_churn_cost(n = n)
  )

# plot

churn_clean %>%

  count(PaymentMethod, PaperlessBilling, Churn) %>%

  count_to_pct(PaymentMethod, PaperlessBilling) %>%

  assess_churn(Churn, churn_value = "Yes", baseline_pct = 0.019) %>%

  mutate(
    cost_of_churn = calculate_churn_cost(n = n)
  ) %>%

  plot_churn(PaymentMethod, PaperlessBilling, .value = cost_of_churn,
                 units = "M") +
  labs(
    title = "Estimated Cost of Attrition by Department & Job Role",
    x = "Cost of Attrition", y = "",
    subtitle = "Looks like Sales Executive and Laboratory Technician are the biggest drivers of cost"
  )





#
churn_clean %>%

  count(PaymentMethod, PaperlessBilling, Contract, Churn) %>%

  count_to_pct(PaymentMethod, PaperlessBilling, Contract) %>%

  assess_churn(Churn, churn_value = "Yes", baseline_pct = 0.019) %>%

  mutate(
    cost_of_churn = calculate_churn_cost(n = n)
  )
