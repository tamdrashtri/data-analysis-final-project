
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
