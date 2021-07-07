### Add packages we use in Assignment 1

library("foreign")
library("plyr")
library("tidyverse")
library("nlme")
library("car")
library("multcomp")
library("lmerTest")


###################################################################################################################################################################################

### 1. Graphical analysis 

## Import the Antisocial.csv data into R.

Antisocial <- read.csv(file = "Antisocial.csv")

## Factor the variables Child and Race and add these variables to the data frame.

Antisocial <- mutate(Antisocial, Child.f = factor(Child), Race.f = factor(Race))
View(Antisocial)

## A numerical and graphical summary for the Antisocial data that provides an indication for whether the interaction effects of YOI * Race 
## should be included in our linear mixed model.

# Table 1: Summary of ASB grouped by Race.

Table_1 <- ddply(Antisocial, c("Race.f", "YOI"), summarise, n = length(ASB), ASB_mean = mean(ASB), ASB_stdev = sd(ASB))
Table_1

# Figure 1: Plot of the values of mean ASB vs YOI grouped by Race.

ggplot(data = Table_1, aes(x = YOI, y = ASB_mean)) +
  geom_point(aes(colour = Race.f), size = 4, shape = 15) +
  geom_line(aes(colour = Race.f)) +
  scale_x_discrete(limits = c(90, 92, 94)) +
  ylim(1.3, 1.8)

## A graphical summary for the Antisocial data that provides an indication for whether random effects of YOI specific to 
## child should be included in our linear mixed model.

# Figure 2: Plot of the values of ASB vs YOI for each Child grouped within Race.

ggplot(data = Antisocial[1:60, ], aes(x = YOI, y = ASB)) +
  geom_point(aes(colour = Child.f), size = 4) +
  geom_line(aes(colour = Child.f)) +
  scale_x_discrete(limits = c(90, 92, 94)) +
  scale_y_continuous(breaks = seq(0, 6, 1)) +
  facet_grid(.~ Race) +
  theme(legend.position="none") 

##############################################################################################################################################################

#### 3. Testing for fixed effects

### Testing the null hypothesis that the fixed effects of Y94 * Race2 and Y94 * Race3 are equal to zero, using ML-based LRT. Test at 12% sig level.

ctrl <- lmeControl(opt = 'optim')

Model1 <- lme(ASB ~ Race + Y92 + Y94 + Race:Y92 + Race:Y94, random = ~ Y92 + Y94 | Child.f, 
              method = "ML", control = ctrl, data = Antisocial) # Fitting model 1 using ML
Model2 <- lme(ASB ~ Race + Y92 + Y94, random = ~ Y92 + Y94 | Child.f, 
              method = "ML", control = ctrl, data = Antisocial) # Fitting model 2 using ML

anova(Model1, Model2)

# We choose Model 1

##############################################################################################################################################################

#### 4. Diagnostics of our final linear mixed model

## Fit Model 1 using REML.

Model1 <- lme(ASB ~ Race + Y92 + Y94 + Race:Y92 + Race:Y94, random = ~ Y92 + Y94 | Child.f, 
              method = "REML", control = ctrl, data = Antisocial) # Fitting model 1 using REML

## Checking the agreement between the predicted marginal values of ASB and the observed mean 
## values of ASB as a function of YOI grouped by Race. 

# Generating the predicted marginal values of ASB.

Antisocial <- mutate(Antisocial, Marg_ASB_hat = fitted(Model1, level = 0))
View(Antisocial)

# Figure 3: Plot of the predicted marginal values of ASB and the observed mean values of ASB 
# as a function of YOI grouped by Race.

ggplot(data = Antisocial, aes(x = YOI, y = Marg_ASB_hat)) +
  geom_point(aes(colour = Race.f), size = 8, shape = 22) +
  geom_point(data = Table_1, aes(YOI, ASB_mean, colour = Race.f), size = 4, shape = 15) +
  scale_x_discrete(limits = c(90, 92, 94)) +
  ylim(1.3, 1.8) +
  labs(x = "YOI", y = "ASB")

## Checking the agreement between the predicted conditional values of ASB and the observed 
## values of ASB as a function of YOI grouped for the first 5 children in the data set.  

# Generating the predicted conditional values of ASB.

Antisocial <- mutate(Antisocial, Cond_ASB_hat = fitted(Model1, level = 1))
View(Antisocial)

# Figure 4: Plot of the predicted conditional values of ASB and the observed values of ASB 
# as a function of YOI for the first five children in the data set.

ggplot(data = Antisocial[c(1:9), ], aes(x = YOI, y = Cond_ASB_hat)) +
  geom_line(aes(colour = Child.f), size = 1) +
  geom_point(aes(YOI, ASB, colour = Child.f), size = 4) +
  scale_x_discrete(limits = c(90, 92, 94)) +
  ylim(0, 6) +
  facet_grid(Child.f ~.) +
  theme(legend.position="none") +
  labs(x = "YOI", y = "ASB")  

#######################################################################################################################################################

#### 5. Fixed effect estimates and random effect predictions for your final linear mixed model

## Table 2: The estimates of the fixed effects in model 1.

Table_2 <- summary(Model1)$tTable
round(Table_2, 4)

## Table 3: The predictions for the random effects for the first five children in the data set.

Table_3 <- ranef(Model1)[1:5,]
round(Table_3, 4)

