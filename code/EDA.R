library(readr)
churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# SETUP-------

# this package helps to install and load packages at the same time.
require(simpleSetup)
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive", "nycflights13",
              "gmodels", "stringr", "DataExplorer", "viridis")
library_install(packages)
?plot.histogram

library(readr)
churn <- read_csv("~/Google Drive/QUEST/myThesisProject/data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# EDA ---------------------------------------------------------------------
introduce(churn)
plot_intro(churn)
plot_bar(churn)
plot_histogram(churn, ggtheme = theme_minimal()) +scale_fill_viridis()
plot_correlation(na.omit(churn), maxcat = 5L)
descr(churn)
view(dfSummary(churn))
print(dfSummary(churn, style = 'grid', plain.ascii = FALSE, graph.magnif = 0.85),
      method = 'render', omit.headings = TRUE)

ggpairs(churn)

churn %>%
  select(PhoneService, MonthlyCharges, OnlineSecurity, InternetService, MultipleLines, Churn) %>%
  ggpairs()

ggsave("output/churn1.png")


# Train -------------------------------------------------------------------

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(churn), replace = T, prob = c(0.6,0.4))
train <- churn[sample, ]
test <- churn[!sample, ]
model1 <- glm(Churn ~ MonthlyCharges, family = "binomial", data = train)

