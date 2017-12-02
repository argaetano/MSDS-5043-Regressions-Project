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

m <- lm(RentRate ~ Age+OperExp+W2MiDT+Taxes, data=comm_prop)
summary(m)
car::ncvTest(m) #pass
shapiro.test(m$residuals) #low p-valud but pass

#Tails are a bit out
qqnorm(fit.2$residuals, pch=16)
qqline(fit.2$residuals)
