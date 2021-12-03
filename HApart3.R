home_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
data3 = read.csv("https://tinyurl.com/b385chpu")
data4 = read.csv("https://tinyurl.com/4f8thztv")

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

library(psych)
library(tidyverse)
library(cAIC4)
library(r2glmm) 
library(lme4)
library(lmerTest) 
library(MuMIn)

view(data3)
view(data4)
str(data3)
str(data4)
summary(data3)
summary(data4)

describe(data3)
str(data3$sex)
sex.factor <- factor(data3$sex)
str(sex.factor)
summary(sex.factor)

data3["25", "sex"] <- "female"

sex <- as.factor(data3$sex)
class(sex)

#random intercept

mixedmodel1 = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1 | hospital), data = data3)
mixedmodel1
summary(mixedmodel1)

#theory based model
model21 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_sample_1)

#Model fit
AIC(model21)
cAIC(mixedmodel1)$caic
cAIC(newmixedmodel)$caic

#confidence intervals and R2 of predictors
r2beta(mixedmodel1)

#marginal and conditional R2
r.squaredGLMM(mixedmodel1)

#confidence intervals for the model coefficients
confint(mixedmodel1)
#standardised beta for each predictor
stdCoef.merMod(mixedmodel1)

#  predicted values 	
pred_mixed <- predict(mixedmodel1, data4 = data4, allow.new.levels = TRUE)	
pred_original <- predict(model21, data4)	

# the sum of squared residuals and predict
RSS = sum((data4$pain - predict(mixedmodel1))^2)
RSS

RSS_mixed = sum((data4[,"pain"] - pred_mixed)^2)	
RSS_original = sum((data4[,"pain"] - pred_original)^2)	
RSS_mixed
RSS_original

sum(residuals(model21)^2)
sum(residuals(mixedmodel1)^2)

# TSS
model_mean <- lm(pain ~ 1, data = data4)
TSS = sum((data4$pain - predict(model_mean))^2)
TSS


#R2
R2 = 1 - (RSS/TSS)
abs(R2)

# New mixed model
newmixedmodel = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data3)
newmixedmodel

pred_newmix <- predict(newmixedmodel, data3)

#anova
anova(newmixedmodel, mixedmodel1)

#plots

slope_plot = data3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE) + xlim(-1, 10) + geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
slope_plot

intercept_plot = data3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
intercept_plot

data3 = data3 %>%
  mutate(pred_int = predict(mixedmodel1), pred_slope = predict(newmixedmodel))

#random intercept
data3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
 aes(y = pred_int, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

#random slope
data3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
  aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
