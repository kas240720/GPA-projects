---
  title: "stat-420-final-project"
author: "Hyunjun Lee, Seulhui Lee, Minji Song"
date: "2019??? 8??? 1???"
output: html_document
---
  
  ##modifying the data##
  

stat.data = read.csv("C:/Users/user/Desktop/stat 420 proj/stat.data.csv")
proj.data = stat.data[ , c(2, 12, 13, 14, 19, 20, 25, 30)]
proj.data = as.data.frame(proj.data)
proj.data$Gender = as.factor(proj.data$Gender)
is.factor(proj.data$Gender)
nrow(proj.data)

set.seed(1)
proj_trn_idx = sample(1:nrow(proj.data), 165)
proj_trn = proj.data[proj_trn_idx, ]
proj_tst = proj.data[-proj_trn_idx, ]



##Single Linear Regression Model##

slr_model = lm(GPA ~ ACT, data = proj_trn)
summary(slr_model)


##Multiple Linear Regression Model##

mlr_model = lm(GPA ~ ., data = proj_trn)
summary(mlr_model)



anova(slr_model, mlr_model)

In order to find the best model between the two models, we used ANOVA F-test to compare both models. Based on this test, it turns out that the p-value is 0.03243, so we do not reject the null. Thus, we prefer sigle linear regression model.


##Interaction Model##


inter_model = lm(GPA ~ Gender * (. - Gender), data = proj_trn)
summary(inter_model)


anova(mlr_model, inter_model)


##Normality & Homoscedasticity Assumptions##


library(lmtest)
bptest(mlr_model)


plot(fitted(mlr_model), resid(mlr_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals 1")
abline(h = 0, col = "darkorange", lwd = 2)






shapiro.test(resid(mlr_model))




**fixing heteroscedasticity**
  
  

log_mlr_model = lm(log(GPA) ~ ., data = proj_trn)
bptest(log_mlr_model)




sqrt_inter_model = lm(sqrt(GPA) ~ ., data = proj_trn)
bptest(sqrt_inter_model)




library(MASS)
boxcox(mlr_model, plotit = TRUE, lambda = seq(5, 6, by = 0.1))


boxcox_model = lm((((GPA^5.4)-1)/5.4) ~ ., data = proj_trn)
bptest(boxcox_model)





plot(fitted(boxcox_model), resid(boxcox_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals 1")
abline(h = 0, col = "darkorange", lwd = 2)



shapiro.test(resid(boxcox_model))



##Unusual Observation##


**leverage**
hatvalues(boxcox_model)[hatvalues(boxcox_model)> 2 * mean(hatvalues(boxcox_model))]
sum(hatvalues(boxcox_model)> 2 * mean(hatvalues(boxcox_model)))


**outliers**
  
rstandard(boxcox_model)[abs(rstandard(boxcox_model)) > 2]
sum(abs(rstandard(boxcox_model)) > 2)

**influence**
  
  
cooks.distance(boxcox_model)[cooks.distance(boxcox_model) > 4 / length(cooks.distance(boxcox_model))]
sum(cooks.distance(boxcox_model) > 4 / length(cooks.distance(boxcox_model)))





##Collinearity##


pairs(proj_trn, col = "dodgerblue")



library(faraway)
vif(boxcox_model)




##Model Selection##


AIC_boxcox = step(boxcox_model, direction = "backward", trace = 0)
summary(AIC_boxcox)


BIC_boxcox = step(boxcox_model, direction = "backward", trace = 0, k = log(nrow(proj_trn)))
summary(BIC_boxcox)
summary(boxcox_model)$adj.r.squared
summary(AIC_boxcox)$adj.r.squared
summary(BIC_boxcox)$adj.r.squared

calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

calc_loocv_rmse(boxcox_model)
calc_loocv_rmse(AIC_boxcox)
calc_loocv_rmse(BIC_boxcox)
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


rmse(proj_tst$GPA, predict(boxcox_model,proj_tst))
rmse(proj_tst$GPA, predict(AIC_boxcox,proj_tst))
rmse(proj_tst$GPA, predict(BIC_boxcox,proj_tst))

bptest(BIC_boxcox)
shapiro.test(resid(BIC_boxcox))


bptest(AIC_boxcox)
shapiro.test(resid(AIC_boxcox))


bptest(boxcox_model)
shapiro.test(resid(boxcox_model))




##Interpretation##

summary(AIC_boxcox)
































