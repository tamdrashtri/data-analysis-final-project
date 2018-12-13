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
churn %>%
  mutate(SeniorCitizen = as.character(SeniorCitizen)) %>%
  select(-customerID) %>%
  select_if(is.character) %>%
  select(Churn, everything()) %>%
  gather(x, y, gender:PaymentMethod) %>%
  count(Churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.1, begin = 0, end = 0.5, direction = 1) +
  scale_color_viridis(discrete = TRUE, alpha = 0.1)

churn %>%
  select(-customerID) %>%
  #select_if(is.numeric) %>%
  select(Churn, MonthlyCharges, tenure, TotalCharges) %>%
  gather(x, y, MonthlyCharges:TotalCharges) %>%
  ggplot(aes(x = y, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_viridis(discrete = TRUE, alpha = 0.1, begin = 0, end = 0.5, direction = 1) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.1) +
  theme_linedraw()

ggcorr(churn) + scale_fill_viridis(alpha = 1, begin = 0, end = 0.5, direction = 1)

introduce(churn)
plot_intro(churn)
plot_bar(churn)
plot_density(churn)
plot_qq(churn)
plot_scatterplot(churn)
plot_histogram(churn, ggtheme = theme_minimal()) +scale_fill_viridis()
plot_correlation(na.omit(churn), maxcat = 5L)
descr(churn)
view(dfSummary(churn))
print(dfSummary(churn, style = 'grid', plain.ascii = FALSE, graph.magnif = 0.85),
      method = 'render', omit.headings = TRUE)

ggpairs(churn)

churn %>%
  select(PhoneService:PaymentMethod) %>%
  ggpairs()

ggsave("output/churn1.png")


# Train -------------------------------------------------------------------

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(churn), replace = T, prob = c(0.6,0.4))
train <- churn[sample, ]
test <- churn[!sample, ]
model1 <- glm(Churn ~ MonthlyCharges, family = "binomial", data = train)

