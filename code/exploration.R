# Load libraries
library(tidyverse) # for tidy data analysis
library(readr)     # for fast reading of input files
library(caret)     # for convenient splitting
library(mice)      # mice package for Multivariate Imputation by Chained Equations (MICE)
library(keras)     # for neural nets
library(lime)      # for explaining neural nets
library(rsample)   # for splitting training and test data
library(recipes)   # for preprocessing
library(yardstick) # for evaluation
library(ggthemes)  # for additional plotting themes
library(corrplot)  # for correlation
library(magrittr)
theme_set(theme_minimal())

churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

churn %<>% str_replace("Bank transfer (automatic)", "[[:punct:]]auto[[:punct:]]")

names(churn$PaymentMethod)[names(churn$PaymentMethod) == "Bank transfer (automatic)"] <- "auto"


churn_data_raw %>%
  count(Churn)

churn_data_raw %>%
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
  theme(axis.text.x = element_text(size = 3))

theme(axis.text.x = element_text(size = 5),
      legend.position = "top") +
  scale_fill_viridis(discrete = TRUE) +
  scale_colour_viridis(discrete = TRUE, alpha = 0.1) +
