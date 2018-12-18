
# For later use -----------------------------------------------------------

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

anova(model1, model3, test = "Chisq")


corrr_analysis <- bakedTrain %>%
  mutate(Churn = y_train_vec) %>%
  correlate() %>%
  focus(Churn) %>%
  rename(feature = rowname) %>%
  arrange(abs(Churn)) %>%
  mutate(feature = as_factor(feature))
corrr_analysis

y_train_vec <- ifelse(pull(train, Churn) == "Yes", 1, 0)
y_test_vec  <- ifelse(pull(test, Churn) == "Yes", 1, 0)


anova(model3, test="Chisq")


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


test.predicted.m1 <- predict(model1, newdata = bakedTest, type = "response")

table(test$default, test.predicted.m1 > 0.5)

model1 = table(bakedTest$Churn, test.predicted.m1 > 0.5) %>% prop.table() %>% round(3)

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {

  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)

  data_cor <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)

  if (fct_reorder) {
    data_cor <- data_cor %>%
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }

  if (fct_rev) {
    data_cor <- data_cor %>%
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }

  return(data_cor)

}

bakedTrain %>%
  get_cor(Churn_Yes, fct_reorder = T, fct_rev = T)

data         <- bakedTrain
feature_expr <- quo(Churn_Yes)

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE,
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1,
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]) {

  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)

  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())

  g <- data_cor %>%
    ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    theme_tq() +
    scale_color_manual(values = c(color_neg, color_pos))

  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)

  return(g)

}

bakedTrain %>%
  plot_cor(target = Churn_Yes, fct_reorder = T, fct_rev = T)

bakedTrain %>%
  get_correlation(formula = Churn_Yes ~ SeniorCitizen:PaymentMethod_Mailed.check)

recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_dummy(all_nominal()) %>% # convert string to factor for all nominal variables
  prep(data = train) # prepare for this data
