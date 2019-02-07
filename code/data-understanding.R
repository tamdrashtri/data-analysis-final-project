

source("code/setup.R")

glimpse(churn_clean)

churn_clean %>%
  select_if(is.character) %>%
  glimpse()

churn_clean %>%
  select_if(is.character) %>%
  map(unique)

churn_clean %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())


# Step 2: Data Visualization ----

churn_clean %>%
  select(Churn, tenure, gender, Partner, Dependents) %>%
  ggpairs(aes(color = Churn),
          lower = "blank", #remove lower diagonal
          legend = 1,
          diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) + # if continuous alpha is 0.5
  theme(legend.position = "bottom")

churn_clean %>%
  select(Churn, MonthlyCharges, TotalCharges) %>%
  ggpairs(aes(color = Churn),
          lower = "blank", #remove lower diagonal
          legend = 1,
          diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) + # if continuous alpha is 0.5
  theme(legend.position = "bottom")



