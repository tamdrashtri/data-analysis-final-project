#install.packages("simpleSetup")
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'viridis')
library_install(packages)


library(readr)
churn <- read_csv("../data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

churn %>%
  mutate(SeniorCitizen = as.character(SeniorCitizen)) %>%
  select(-customerID) %>%
  select_if(is.character) %>%
  select(Churn, everything()) %>%
  gather(x, y, gender:PaymentMethod) %>%
  count(Churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = Churn)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  ylab("Variables") + xlab("Count") +
  scale_fill_viridis(discrete = TRUE) +
  scale_colour_viridis(discrete = TRUE, alpha = 0.1) +
  theme_linedraw(base_size = 7, base_line_size = 0, base_rect_size = 0.1) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 4))

churn %>%
  select(-customerID) %>%
  select(Churn, MonthlyCharges, tenure, TotalCharges) %>%
  gather(x, y, MonthlyCharges:TotalCharges) %>%
  ggplot(aes(x = y, fill = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  scale_colour_viridis( alpha = 0) +
  scale_fill_viridis(discrete = TRUE, alpha = 0) +
  ylab("Variables") + xlab("Density") +
  theme_linedraw(base_size = 7, base_line_size = 0, base_rect_size = 0.1)
