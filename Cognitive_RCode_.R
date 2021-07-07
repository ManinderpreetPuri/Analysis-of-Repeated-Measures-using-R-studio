### Add packages we use in Assignment 2

library("plyr")
library("tidyverse")
library("nlme")
library("multcomp")
library("lmerTest")
library("foreign")

###################################################################################################################################################################################

#### 1. Graphical analysis

## Import the Cognitive data into R.

Cognitive <- read.csv(file = 'Cognitive.csv')
View(Cognitive)

## Factor the variables W, Cue, D and Subject.

Cognitive <- mutate(Cognitive, W.f = factor(W), Cue.f = factor(Cue), D.f = factor(D), Subject.f = factor(Subject))
View(Cognitive)

## Figure 1: Plot of the values of Time as a function of Cue grouped by D.

Table_1 <- ddply(Cognitive, c("Cue", "D.f"), summarise, n = length(Time), Time_mean = mean(Time), Time_stdev = sd(Time))
Table_1

ggplot(data = Table_1, aes(x = Cue, y = Time_mean)) +  
  geom_line(aes(Cue, Time_mean, colour = D.f), size = 1) +
  geom_point(aes(Cue, Time_mean, colour = D.f), size = 3, shape = 15) +
  scale_x_discrete(limits = c(1, 2, 3)) +
  scale_y_continuous(breaks=seq(160, 220, 20)) +
  ylim(160, 220) + 
  labs(x = "Cue", y = "Time")

## Create the C2 and C3 variables

Cognitive <- mutate(Cognitive, C2 = if_else(Cue == 2, 1, 0), C3 = if_else(Cue == 3, 1, 0))  
View(Cognitive)


############################################################################################################################################################

#### 3. Testing for random effects

## Testing the null hypothesis that the variance of the random effect of W is zero, using REML-based likelihood ratio test.

Model1 <- lme(Time ~ W + C2 + C3 + D + W:C2 + W:C3 + W:D + 
                C2:D + C3:D, random = ~ W | Subject.f, 
              weights = varIdent(form = ~ 1 | W), method = "REML", data = Cognitive)

Model2 <- lme(Time ~ W + C2 + C3 + D + W:C2 + W:C3 + W:D + 
                C2:D + C3:D, random = ~ 1 | Subject.f, 
              weights = varIdent(form = ~ 1 | W), method = "REML", data = Cognitive) # Fitting model 2 using REML

# Obtaining the likelihood ratio observed test statistic, LRT = 2ln(LRR) - 2ln(LNR).

anova(Model1, Model2) # Obtain ln(LRR) and ln(LNR)
LRT <- anova(Model1, Model2)$L.Ratio[-1] # Obtain LRT = 2ln(LRR) - 2ln(LNR)

# Calculating the p-value for the REML based likelihood ratio test

p_value <- (0.5 * pchisq(LRT, df = 1, lower.tail = FALSE)) + (0.5 * pchisq(LRT, df = 2, lower.tail = FALSE))
p_value

############################################################################################################################################################

#### 4. Variance-covariance estimates of the final linear mixed model

## D matrix

D_hat <- getVarCov(Model1)[-3,] # Gives the D matrix
D_hat

## R matrix

R_hat <- getVarCov(Model1, type = "conditional")[[1]]
R_hat


######################################################################################################################################################

#### 5. Fixed effect estimates of the final linear mixed model

## Table 2: The estimates of the fixed effects in model 1.

Table_2 <- summary(Model1)$tTable
round(Table_2, 4)

## beta9 - beta8

C <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, -1, 1), 1)
beta_Contrast <- glht(Model1, linfct = C)
summary(beta_Contrast)

