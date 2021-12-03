home_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

library(tidyverse)

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

#Data diagnostics
home_sample_1 %>%
  summary()

home_sample_1 %>%
  ggplot() +
  aes(x = IQ) +
  geom_histogram()
home_sample_1 %>%
  ggplot() +
  aes(x = weight) +
  geom_histogram()
home_sample_1 %>%
  ggplot() +
  aes(x = household_income) +
  geom_histogram()

home_sample_1["34", "STAI_trait"] <- 42
home_sample_1["88", "pain"] <- 5

#backward regression initial model

model31 = lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = home_sample_1)
drop1(model31)
summary(model31)
#High leverage and cook's distance
model31 %>%
  plot(which = 5)
model31 %>%
  plot(which = 4)
home_sample_1 %>%
  slice(c(47,85,104))

# Assumptions

#Q-Q for normality
model31 %>%
  plot(which = 2)
#Normality of residuals
residuals_model31 = enframe(residuals(model2))
residuals_model31 %>%
  ggplot() + aes(x = value) + geom_histogram()
#skew and kurtosis
describe(residuals(model31))
#Linearity
model31%>%
  residualPlots()
model31 %>%
  plot(which = 3)
#Homoscedasticty
model31 %>%
  ncvTest() 
model31 %>%
  bptest()
#Multicollineaerity
model31 %>%
  vif()

#Backward regression
model32 = lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight + household_income, data = home_sample_1)
drop1(model32)
model32

summary(model31)$adj.r.squared
summary(model32)$adj.r.squared

model33 = lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = home_sample_1)
drop1(model33)

summary(model33)$adj.r.squared

model34 = lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness, data = home_sample_1)
summary(model34)$adj.r.squared

drop1(model34)

model35 = lm(pain ~ age + pain_cat + cortisol_serum + mindfulness, data = home_sample_1)
summary(model35)$adj.r.squared

drop1(model35)

model36 = lm(pain ~ pain_cat + cortisol_serum + mindfulness, data = home_sample_1)
summary(model36)$adj.r.squared

# R square not better anymore, model36 < model35, 0.4942 < 0.5067

backward_model <- model35
backward_model
summary(backward_model)

theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_sample_1)
AIC(model31)
AIC(backward_model)
AIC(theory_based_model)

anova(backward_model,theory_based_model)

home_sample_2 = read.csv("https://tinyurl.com/87v6emky")

#  predicted values 	
pred_theory <- predict(theory_based_model, home_sample_2)	
pred_back <- predict(backward_model, home_sample_2)	

# the sum of squared residuals 	
RSS_theory = sum((home_sample_2[,"pain"] - pred_theory)^2)	
RSS_back = sum((home_sample_2[,"pain"] - pred_back)^2)	
RSS_theory	
RSS_back

# confidence intervals and beta
coef_table(backward_model)
coef_table(theory_based_model)
coef_table(model31)
