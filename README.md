# Polynomial Regression using ggplot

data("pressure")
names(pressure)

View(pressure)
plot(pressure)

mod11 <- lm(pressure~temperature, data = pressure) 
summary(mod11)
#We came to know from summary that the R2 value is very low and 
#the intercept value is -147.89 that indicates that for some values of temperature 
#the model will predict the negative value.

#plotting the model using linear regression
ggplot(pressure, aes(temperature, pressure))+
  geom_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE)+geom_point()+theme_bw()

#Not all the datapoints are touching the regression line and they are far from the regression line.
-147.8989+1.5124*(240)
## [1] 215.0771

# we found out the prediction value for pressure is 215.0771 when the temperature is 
#240 which is more than the actual value. We can say that the model is non-linear 
#in nature and we will apply polynomial regression in order to check
#that the model is going to fit or not.
poly2 <- pressure$temperature^2 
poly3 <- pressure$temperature^3 
poly4 <- pressure$temperature^4
fit3 <- lm(pressure~temperature+poly2+poly3+poly4, data = pressure)
summary(fit3)

#from summary of model, we come to know that model has all significant variables and
#R2 value is 99.96%.

#plotting polynomial model
p1 <- ggplot(pressure, aes(temperature, pressure)) + geom_point() + theme_bw()+
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial x2'), se= FALSE)+
  stat_smooth(method = 'lm', formula = y ~ poly(x,3), aes(colour = 'polynomial x3'), se= FALSE)+
  stat_smooth(method = 'lm', formula = y ~ poly(x,4), aes(colour = 'polynomial x4'), se= FALSE)
ggplotly(p1)

# now we will predict the value of temperature i.e. 240 with polynomial model
pred_pressure1 <- predict(fit3)
