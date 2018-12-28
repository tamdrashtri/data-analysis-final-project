library(readr)
churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv",
                  col_types = cols(customerID = col_skip()))
View(churn)

write_csv(churn, "data/churn.clean.csv")
