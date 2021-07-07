### Add packages we use in Assignment 3

library("plyr")
library("tidyverse")
library("nlme")
library("multcomp")
library("lmerTest")
library("foreign")


###########################################################################################################################################################

#### 1. Graphical Analysis

## Import the Veneer data into R.

Veneer <- read.csv(file = 'Veneer.csv')
View(Veneer)

## Factor the variables Tooth and T.

Veneer <- mutate(Veneer, Tooth.f = factor(Tooth), T.f = factor(T))
View(Veneer)

## Figure 1: Plot of the values of G as a function of MA grouped by T.

ggplot(data = Veneer, aes(x = MA, y = G)) +
  geom_point(aes(MA, G, colour = T.f), size = 3, shape = 15) +
  geom_smooth(aes(MA, G, colour = T.f), method = lm, se = FALSE) +
  scale_x_continuous(breaks=seq(-25, 25, 5)) +
  scale_y_continuous(breaks=seq(0, 90, 10)) +
  labs(x = "MA", y = "G")

## Figure 2: Plot of the values of G as a function of T for each Tooth.

ggplot(data = Veneer, aes(x = T, y = G)) +
  geom_line(aes(T, G, colour = Tooth.f), size = 1) +
  geom_point(aes(T, G, colour = Tooth.f), size = 3, shape = 15) +
  scale_x_discrete(limits = c(0, 1)) +
  scale_y_continuous(breaks=seq(0, 90, 10)) +
  labs(x = "T", y = "G") +
  theme(legend.position="none")

######################################################################################################################################################

#### 3. Variance-covariance estimates of the final linear mixed model

Model1 <- lme(G ~ MA + MBG + MC + T + MA:T + MBG:T + MC:T, random = ~ T | Tooth.f, method = "REML", data = Veneer)

## R matrix

R_hat <- getVarCov(Model1, type = "conditional")[[1]]
round(R_hat, 2)

## Var(Y)

Var_Y <- getVarCov(Model1, type = "marginal")[[1]]
round(Var_Y, 2)


######################################################################################################################################################

#### 4. Predicted values and residuals of the final linear mixed model

## Table 1: The estimates of the fixed effects in model 1.

Table_1 <- summary(Model1)$tTable
round(Table_1, 2)

## Table 2: The random effect predictions in model 1.

Table_2 <- ranef(Model1)[1:10,]
round(Table_2, 2)

