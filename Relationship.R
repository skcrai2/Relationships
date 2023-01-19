#let's load the library and the data and take a look at it

library(faraway)
require(faraway)
data(sat)
head(sat)

#now we have to make some changes to the data to check our assumptions

by.math <- sat[order(sat$math),]
by.verbal <- sat[order(sat$verbal),]
by.salary <- sat[order(sat$salary),]
attach(sat)
out <- lm(total ~ expend + ratio + salary + takers)
summary(out)

#Definitely if you don’t have too many predictor variables, try plotting the response vs 
#each variable individually first- this is a good exploratory first step.

windows()
par(mfrow=c(2,2))
plot(total ~ expend)
plot(total ~ ratio)
plot(total ~ salary)
windows()
par(mfrow=c(2,2))
plot(out)

#Partial regression/added variable plots… check the takers variable

windows()
par(mfrow=c(1,1))
d <- residuals(lm(total ~ expend + ratio + salary))
m <- residuals(lm(takers ~ expend + ratio + salary))
plot(d ~ m, xlab = "takers residuals", ylab="total residuals")

#No sign of non-linearity here. These partial regression/added variable plots allow us to examine 
#the marginal effects of predictor variables on the response after the other predictor variables 
#have been removed. These plots indicate how a predictor variable tracks with the response 
#versus the other variables.

#For instance, try examining the added value/marginal effects of “ratio”. 

windows()
par(mfrow=c(1,1))
d2 <- residuals(lm(total ~ expend + salary + takers))
m2 <- residuals(lm(ratio ~ expend + salary + takers))
plot(d2 ~ m2, xlab = "ratio residuals", ylab="total residuals") 

#There is no real sign of linearity here, and so there is evidence that the ratio variable is not so 
#important. 

#Partial residual plots are alternatives to added variable/partial regression plots:

windows()
par(mfrow=c(2,2))
termplot(out, partial.resid=TRUE, terms=1)
termplot(out, partial.resid=TRUE, terms=2)
termplot(out, partial.resid=TRUE, terms=3)
termplot(out, partial.resid=TRUE, terms=4)

#Could be two groups in the takers variable, so let’s investigate:

mod1 <- lm(total ~ expend + ratio + salary + takers, subset=(takers < 40))
mod2 <- lm(total ~ expend + ratio + salary + takers, subset=(takers > 40))
sumary(mod1)
sumary(mod2)

#the r-squared value is very different for the two taker groups, so there probably is two groups