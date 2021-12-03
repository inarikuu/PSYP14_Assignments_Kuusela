home_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
library(car)
library(lmtest)
library(lm.beta) 
library(psych)
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

View(home_sample_1)

#Checking for misscoded values

str(home_sample_1)

home_sample_1 %>%
  summary()

#Checking the plots for the ones that look like have a suspicious value
home_sample_1 %>%
  ggplot() +
  aes(x = pain) +
  geom_histogram()
home_sample_1 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram()

#recode errors in data

home_sample_1["34", "STAI_trait"] <- 42
home_sample_1["88", "pain"] <- 5

# Linear regression model 1

model1 = lm(pain ~ age + sex, data = home_sample_1)
model1
summary(model1)

AIC(model1)

#Model visualisation
home_sample_1 %>%
  ggplot() + aes(x = age, y = pain) + geom_point() +
  geom_smooth(method = "lm")

home_sample_1 %>%
  ggplot() + aes(x = sex, y = pain) + geom_point() +
  geom_smooth(method = "lm")

home_sample_1 %>%
  mutate(rownum = row.names(home_sample_1)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_label()
#High leverage and cook's distance
model1 %>%
  plot(which = 5)
model1 %>%
  plot(which = 4)


# Model 2

model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = home_sample_1)
model2
summary(model2)

#High leverage and cook's distance
model2 %>%
  plot(which = 5)
model2 %>%
  plot(which = 4)
home_sample_1 %>%
  slice(c(47,74,86))

# Assumptions

#Q-Q for normality
model2 %>%
  plot(which = 2)
#Normality of residuals
residuals_model2 = enframe(residuals(model2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()
#skew and kurtosis
describe(residuals(model2))
#Linearity
model2%>%
  residualPlots()

model2 %>%
  plot(which = 3)
#Homoscedasticty
model2 %>%
  ncvTest() 
model2 %>%
  bptest()
#Multicollineaerity
model2 %>%
  vif()

# New model 2 
model21 = lm(pain ~ age + sex + STAI_trait + pain_cat  + cortisol_serum + mindfulness, data = home_sample_1)
model21
summary(model21)
AIC(model21)

model22 = lm(pain ~ age + sex + STAI_trait + pain_cat  + cortisol_saliva + mindfulness, data = home_sample_1)
model22
summary(model22)
AIC(model22)

#High leverage and cook's distance
model21 %>%
  plot(which = 5)
model21 %>%
  plot(which = 4)
home_sample_1 %>%
  slice(c(47,65,86))

# Assumptions

#Q-Q for normality
model21 %>%
  plot(which = 2)
#Normality of residuals
residuals_model21 = enframe(residuals(model21))
residuals_model21 %>%
  ggplot() + aes(x = value) + geom_histogram()
#skew and kurtosis
describe(residuals(model21))
#Linearity
model21%>%
  residualPlots()

model21 %>%
  plot(which = 3)
#Homoscedasticty
model21 %>%
  ncvTest() 
model21 %>%
  bptest()
#Multicollineaerity
model21 %>%
  vif()

# beta-values and confidence intervals

coef_table(model1)
coef_table(model21)

#anova
anova(model1,model21)

