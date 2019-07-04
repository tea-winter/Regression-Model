library (datasets)
library(ggplot2)
library("Hmisc")
library(readr)
library(dplyr)
data(mtcars)
head(mtcars)
str(mtcars)
#Firstly vs and am should be modelled as categorical variables
mtcars <- mtcars %>%
  mutate(vs = as.factor(vs),
         am = as.factor(am),
         cyl = as.ordered(cyl),
         gear = as.ordered(gear),
         carb = as.ordered(carb))
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))
# plot box mpg along with am
boxplot(mpg ~ am, data = mtcars, xlab = "Transmission type", ylab = "Miles Per Gallon")
plot(mpg ~ am, data = mtcars)
#base model: mpg along with am
fit_base <- lm(mpg ~ am, data=mtcars)
summary(fit_base)
# multivariable regression fit_all mpg along with all feature in the mtcars data
fit_all <- lm(mpg ~ . ,data=mtcars)
summary(fit_all)
#step_wise to choose variance selection
fit_final <- step(fit_all, direction = "both", k = log(nrow(mtcars)))
summary(fit_final)
#re check better_fit and simple_fit with anova test
anova(fit_base, fit_final)
# visualization
par(mfrow = c(2, 2))
plot(better_fit)
