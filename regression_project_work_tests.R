library(ggplot2)
library(car)

comm_prop <- read.csv("comm_prop.csv")

hist(comm_prop$RentRate)
hist(comm_prop$Age)
hist(comm_prop$OperExp)
hist(comm_prop$VacRate)
hist(comm_prop$SqFt)
hist(comm_prop$Taxes)
hist(comm_prop$W2MiDT)

cor(comm_prop)

fit.1 <- lm(RentRate ~ OperExp, data=comm_prop)
summary(fit.1)
ncvTest(fit.1) #Significant p-value
shapiro.test(fit.1$residuals)

# No apparant pattern in data, Does not appear to be a violation of nonconstant variance
fit.1.df <- fortify(fit.1)
ggplot(fit.1.df, aes(x=.fitted, y=.resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(x="Fitted Values", y="Residuals")

#Residuals appears to be normal
ggplot(fit.1.df, aes(x=.resid)) + 
  geom_histogram(aes(y=..density..), bins=20, color="black", fill="light grey") + 
  
  stat_function(fun=dnorm, color="red", size=1.2, 
                 args=list(mean=mean(fit.1.df$.resid), sd=sd(fit.1.df$.resid))) + labs(x="Residuals")

#Confirmed with a non-significant p-value
shapiro.test(fit.1.df$.resid)

fit.2 <- lm(RentRate ~ SqFt, data=comm_prop)
fit.2.df <- fortify(fit.2)
summary(fit.2)
#Confirmed with a significant p-value that the residuals are not normal
shapiro.test(fit.2.df$.resid)

# No apparant pattern in data, Does not appear to be a violation of nonconstant variance

ggplot(fit.2.df, aes(x=.fitted, y=.resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(x="Fitted Values", y="Residuals")

#Residuals do not appear to be normal
ggplot(fit.2.df, aes(x=.resid)) + 
  geom_histogram(aes(y=..density..), bins=20, color="black", fill="light grey") + 
  stat_function(fun=dnorm, color="red", size=1.2, 
                args=list(mean=mean(fit.2.df$.resid), sd=sd(fit.2.df$.resid))) + labs(x="Residuals")



#let's transform the data
fit.2.2 <- lm(RentRate ~ SqFt+I(SqFt^2), data=comm_prop)
fit.2.2.df <- fortify(fit.2.2)
ggplot(fit.2.2.df, aes(x=.fitted, y=.resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(x="Fitted Values", y="Residuals")

#Residuals do not appear to be normal
ggplot(fit.2.2.df, aes(x=.resid)) + 
  geom_histogram(aes(y=..density..), bins=20, color="black", fill="light grey") + 
  
  stat_function(fun=dnorm, color="red", size=1.2, 
                args=list(mean=mean(fit.2.2.df$.resid), sd=sd(fit.2.2.df$.resid))) + labs(x="Residuals")

#Confirmed with a significant p-value that the residuals are not normal
shapiro.test(fit.2.2.df$.resid)

#model with Taxes & SqFT
fit.3 <- lm(RentRate ~ SqFt+Taxes+OperExp, data=comm_prop)
fit.3 <- lm(RentRate ~ OperExp, data=comm_prop)
summary(fit.3)
ncvTest(fit.3)
leveragePlots(fit.3, pch=16)

#####################################################
#                 OperExp & W2MiDT                  #
#####################################################
comm_prop$W2MiDT <- factor(comm_prop$W2MiDT, labels = c("Not Close", "Close"))
fit.4 <- lm(log(RentRate) ~ OperExp+W2MiDT, data=comm_prop)
summary(fit.4)
ncvTest(fit.4) #No significant p-value
leveragePlots(fit.4, pch=16)
shapiro.test(fit.4$residuals) #Significant p-value - Need to figure out why

#summary(anova(RentRate ~ OperExp+W2MiDT, data=comm_prop))

fit.4.df <- fortify(fit.4)
ggplot(fit.4.df, aes(x=.resid)) + 
  geom_histogram(aes(y=..density..), bins=20, color="black", fill="light grey") + 
  stat_function(fun=dnorm, color="red", size=1.2, 
                args=list(mean=mean(fit.4.df$.resid), sd=sd(fit.4.df$.resid))) + labs(x="Residuals")

ggplot(fit.4.df, aes(x=.fitted, y=.resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(x="Fitted Values", y="Residuals")

qqPlot(fit.4, pch=16)


ggplot(comm_prop, aes(x=W2MiDT, y=RentRate)) + 
  geom_point()

