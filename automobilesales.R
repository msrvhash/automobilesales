setwd(“SET YOUR WORKING DIRECTORY HERE“);

data <- read.csv("Prestige.csv"); attach(data);names(data);
library(mgcv);
mod.gam1 <- gam(Prestige ~ s(Income) + s(Education))
plot(mod.gam1); ##this plots the slope-curve of income
summary(mod.gam1); ##this returns the rest of the regression output, e.g. intercept, R-squared, etc
mod1 <- lm(Prestige ~ (Income) + (Education))
summary(mod1);

data <- read.csv("Spam.csv"); attach(data);names(data);
mod1 <- glm(Spam ~ lab + conference + credit + money,family = binomial);
summary(mod1);

data <- read.csv("AutomobileMarket.csv");attach(data);names(data);
mod2 <- kmeans(dist(data[,2:11]),2)
clus <- as.numeric(mod2$cluster);
by(data[,2:11],as.factor(clus),summary);
by(data[,2:11],as.factor(clus),cor);

data <- read.csv("Enrollments.csv");attach(data);names(data);
Roll    <- ROLL[2:29];
Roll.l1 <- ROLL[1:28];
Year    <- YEAR[2:29];

mod3 <- lm(Roll~Roll.l1);
summary(mod3);
