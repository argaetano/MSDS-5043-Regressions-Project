library(ggplot2)
library(car)

comm_prop <- read.csv("comm_prop.csv")
comm_prop$W2MiDT <- factor(comm_prop$W2MiDT, labels = c("No", "Yes"))
hist(comm_prop$RentRate)
hist(comm_prop$Age)
hist(comm_prop$OperExp)
hist(comm_prop$VacRate)
hist(comm_prop$SqFt)
hist(comm_prop$Taxes)
hist(comm_prop$W2MiDT)

"VacRate's distrobution contains strong Right/positive skewness. SqFt contains a slight amount of 
right skewness. OperExp contains a slight amount of left/negative skewness. Age has an unusual 
ditrobutional shape to it as well."

round(cor(comm_prop),2)
library(corrplot)
corrplot(cor(comm_prop[,c(1:6)]), method="ellipse")

"RentRate and OperExp have a moderate correlation. RentRate and SqFt have a moderate correlation as well.
RentRate and Taxes share the same correlation with .58. W2MiDT has the strongest 
correlation with RentRate with a correlation coefficent of.70. SqFt and Taxes have a perfect correlatino
with 1.00. "

library(leaps)
subsets <- regsubsets(RentRate ~ ., data=comm_prop)
plot(subsets, scale="adjr2")
#^^ Based on the plot, our variable selection should be Age, OperExp, Taxes, and W2MiDT.

"Model's attributes:
Adjusted R^2: .73 - Fairly high adjusted R-squared value. 73% of the RentRate's variation can be 
explained by the Age, OperExp, W2MiDT, and Taxes variables.

F-statistic: 68.36

P-values:
Age: 1.73e-07
OperExp: 5.18e-07
W2MiDTYes: 1.66e-07

Model Coeficients:
Age:        -0.10398
OperExp:    0.23731
W2MiDTYes:  1.43475
Taxes:      0.36898
"
m <- lm(RentRate ~ Age+OperExp+W2MiDT+Taxes, data=comm_prop)
summary(m)
car::ncvTest(m) #pass
shapiro.test(m$residuals) #low p-valud but pass

"Tails are a bit out, this could suggest non-constant variance. However, we formally tested via the
ncvTest and did not reject the null hypothesis."
qqnorm(m$residuals, pch=16)
qqline(m$residuals)

# Does not suggest Multicollinearity.
car::vif(m)

"Start on Predict: We are 95% confidence that the value of RentRate will be between 
14.17338 and 18.20759"
new_obs = data.frame(Age=9, SqFt=40,OperExp=13,Taxes=.0540,W2MiDT="No")
predict(m, newdata = new_obs, interval="predict")


