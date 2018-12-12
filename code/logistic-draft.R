library(simpleSetup)

library(readr)
churn <- read_csv("../data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive", "nycflights13",
              "gmodels", "stringr", "DataExplorer", "summarytools", "recipes", "broom", "modelr", "magrittr", "corrr", "viridis")
library_install(packages)

library(readr)
churn <- read_csv("../data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Load data
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(churn), replace = T, prob = c(0.6,0.4))

train <- churn[sample, ]
test <- churn[!sample, ]
train %<>%
  select(-customerID) %>%
  drop_na()

test %<>%
  select(-customerID) %>%
  drop_na()

recObj <- recipe(Churn ~ ., data = train) %>%
  step_string2factor(all_nominal()) %>%
  prep(data = train)

recObj

bakedTrain <- bake(recObj, new_data = train)
bakedTest <- bake(recObj, new_data = test)

model1 <- glm(Churn ~ MonthlyCharges, family = "binomial", data = bakedTrain)
summary(model1)
tidy(model1)

model3 <- glm(Churn ~ ., family = "binomial", data = bakedTrain)
summary(model3)
tidy(model3)

caret::varImp(model3)
anova(model1, model3, test = "Chisq")

list(model1 = pscl::pR2(model1)["McFadden"],
     model2 = pscl::pR2(model2)["McFadden"],
     model3 = pscl::pR2(model3)["McFadden"])

model1_data <- augment(model1) %>%
  mutate(index = 1:n())

ggplot(model1_data, aes(index, .std.resid, color = Churn)) +
  geom_point(alpha = .5) +
  geom_ref_line(h = 3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.1, begin = 0, end = 0.5, direction = 1) +
  theme_classic()

plot(model1, which = 4, id.n = 5)

model1_data %>%
  top_n(5, .cooksd)


test.predicted.m1 <- predict(model1, newdata = bakedTest, type = "response")
test.predicted.m2 <- predict(model2, newdata = bakedTest, type = "response")
test.predicted.m3 <- predict(model3, newdata = bakedTest, type = "response")

test.predicted.m1

model1 = table(bakedTest$Churn, test.predicted.m1)
na.omit(bakedTest)
na.omit(test.predicted.m1)
introduce(bakedTrain)

list(
  model1 = table(bakedTest$Churn, test.predicted.m1 > 0.5) %>% prop.table() %>% round(3),

  model3 = table(bakedTest$Churn, test.predicted.m3 > 0.5) %>% prop.table() %>% round(3)
)


test %>%
  mutate(m1.pred = ifelse(test.predicted.m1 > 0.5, "Yes", "No"),

         m3.pred = ifelse(test.predicted.m3 > 0.5, "Yes", "No")) %>%
  summarise(m1.error = mean(Churn != m1.pred),

            m3.error = mean(Churn != m3.pred))

table(test$default, test.predicted.m1 > 0.5)

library(ROCR)

par(mfrow=c(1, 2))

prediction(test.predicted.m1, test$Churn) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test.predicted.m3, test$Churn) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test.predicted.m1, test$Churn) %>%
  performance(measure = "auc") %>%
  .@y.values

prediction(test.predicted.m3, test$Churn) %>%
  performance(measure = "auc") %>%
  .@y.values

anova(model3, test="Chisq")

y_train_vec <- ifelse(pull(train, Churn) == "Yes", 1, 0)
y_test_vec  <- ifelse(pull(test, Churn) == "Yes", 1, 0)

corrr_analysis <- bakedTrain %>%
  mutate(Churn = y_train_vec) %>%
  correlate() %>%
  focus(Churn) %>%
  rename(feature = rowname) %>%
  arrange(abs(Churn)) %>%
  mutate(feature = as_factor(feature))
corrr_analysis
