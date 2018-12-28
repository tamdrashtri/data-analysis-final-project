library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(caret)      # ML model building
library(h2o)        # ML model building

# initialize h2o
h2o.init()

h2o.no_progress()

df <- rsample::attrition %>%
  dplyr::mutate_if(is.ordered, factor, ordered = FALSE) %>%
  dplyr::mutate(Attrition = factor(Attrition, levels = c("Yes", "No")))

index <- 1:5
train_obs <- df[-index, ]
local_obs <- df[index, ]

# create h2o objects for modeling
y <- "Attrition"
x <- setdiff(names(train_obs), y)
train_obs.h2o <- as.h2o(train_obs)
local_obs.h2o <- as.h2o(local_obs)

# Create Random Forest model with ranger via caret
fit.caret <- train(
  Attrition ~ .,
  data = train_obs,
  method = 'ranger',
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneLength = 1,
  importance = 'impurity'
)

# create h2o models
h2o_rf <- h2o.randomForest(x, y, training_frame = train_obs.h2o)
h2o_glm <- h2o.glm(x, y, training_frame = train_obs.h2o, family = "binomial")
h2o_gbm <- h2o.gbm(x, y, training_frame = train_obs.h2o)

# ranger model --> model type not built in to LIME
fit.ranger <- ranger::ranger(
  Attrition ~ .,
  data = train_obs,
  importance = 'impurity',
  probability = TRUE
)

vip(fit.ranger) + ggtitle("ranger: RF")

# built-in PDP support in H2O
h2o.partialPlot(h2o_rf, data = train_obs.h2o, cols = "MonthlyIncome")

fit.ranger %>%
  partial(pred.var = "MonthlyIncome", grid.resolution = 25, ice = TRUE) %>%
  autoplot(rug = TRUE, train = train_obs, alpha = 0.1, center = TRUE)

explainer_caret <- lime(train_obs, fit.caret, n_bins = 5)

class(explainer_caret)
## [1] "data_frame_explainer" "explainer"            "list"

summary(explainer_caret)

explanation_caret <- explain(
  x = local_obs,
  explainer = explainer_caret,
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = .75,
  n_features = 10,
  feature_select = "highest_weights",
  labels = "Yes"
)

tibble::glimpse(explanation_caret)

plot_features(explanation_caret)

plot_explanations(explanation_caret)

# tune LIME algorithm
explanation_caret <- explain(
  x = local_obs,
  explainer = explainer_caret,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = 3,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

plot_features(explanation_caret)

explainer_h2o_rf  <- lime(train_obs, h2o_rf, n_bins = 5)
explainer_h2o_glm <- lime(train_obs, h2o_glm, n_bins = 5)
explainer_h2o_gbm <- lime(train_obs, h2o_gbm, n_bins = 5)

explanation_rf <- explain(local_obs, explainer_h2o_rf, n_features = 5, labels = "Yes", kernel_width = .1, feature_select = "highest_weights")
explanation_glm <- explain(local_obs, explainer_h2o_glm, n_features = 5, labels = "Yes", kernel_width = .1, feature_select = "highest_weights")

explanation_gbm <- explain(local_obs, explainer_h2o_gbm, n_features = 5, labels = "Yes", kernel_width = .1, feature_select = "highest_weights")

p1 <- plot_features(explanation_rf, ncol = 1) + ggtitle("rf")
p2 <- plot_features(explanation_glm, ncol = 1) + ggtitle("glm")
p3 <- plot_features(explanation_gbm, ncol = 1) + ggtitle("gbm")
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

explainer_ranger <- lime(train, fit.ranger, n_bins = 5)
