# https://afit-r.github.io/tree_based_methods

source("code/setup.R")

churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# create training and testing dataset
train.index <- createDataPartition(churn$Churn, p = .7, list = FALSE)
train <- churn[ train.index,]
test  <- churn[-train.index,]

write_csv(train, "train.churn.csv")
write_csv(test, "test.churn.csv")


# remove unnecessary columns and drop missing values for both training and testing
test %<>%
  select(-customerID) %>%
  drop_na()

train %<>%
  select(-customerID) %>%
  drop_na()

# Feature Engineering -----------------------------------------------------


recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_string2factor(Churn) %>%
  step_dummy(all_predictors()) %>% # convert string to factor
  step_BoxCox(all_numeric()) %>%
  #step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
  prep(data = train) # prepare for this data


bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)

write_csv(bakedTrain, "data/bakedTrain.csv")
write_csv(bakedTest, "data/bakedTest.csv")

# for reproduciblity
set.seed(123)

#use Random Forest
# default RF model
m1 <- randomForest(
  formula = Churn ~ .,
  data    = bakedTrain
)

vip(m1, bar = FALSE)
m1 # show error rate and performance

# draw the importance graph
ggplot(col_index, aes(x = names, y = Overall)) +
  geom_segment(aes(xend = names, yend = 0)) +
  geom_point() +
  coord_flip() +
  theme(text = element_text(size=10)) +
  scale_color_viridis() +
  theme_linedraw()

# using it to predict
Prediction <- predict(m1, bakedTest, OOB=TRUE, type = "response")





