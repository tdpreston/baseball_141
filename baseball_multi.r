# Multinomial Regression Model

library(tidyverse)

sub_data <- read.csv("sub_data.csv")


# install.packages("foreign")

library(foreign)

# install.packages("nnet")

library(nnet)


sub_data$inj_cat <- factor(sub_data$inj_cat)

sub_data$inj_cat <- relevel(sub_data$inj_cat, ref = "Shoulder")

mtest <- multinom(inj_cat ~ heightInches + weightLbs + x0_mean +  z0_mean + z0_diff +
                    SS_mean + main_pitch_type + pitch_count_cat, data = sub_data)

# These are tables of the confidence interval of odds ratios for the coefficient

# Each table is in reference to Shoulder since that is specified as the reference

# If the odds ratio bound contains 1, then that predictor is not significant since you are 1 times likely
# to get one injury from the other

data.frame("Coefficient" = names(confint(mtest)[, , "Back"][, 1]),
           "Lower_Bound" = as.numeric(round(exp(confint(mtest)[, , "Back"][, 1]), 3)), 
           "Upper_Bound" = as.numeric(round(exp(confint(mtest)[, , "Back"][, 2]), 3)))[-1, ]

data.frame("Coefficient" = names(confint(mtest)[, , "Elbow"][, 1]),
           "Lower_Bound" = as.numeric(round(exp(confint(mtest)[, , "Elbow"][, 1]), 3)), 
           "Upper_Bound" = as.numeric(round(exp(confint(mtest)[, , "Elbow"][, 2]), 3)))[-1, ]

# Finger has zero odds for off speed because there are no off speed finger injuries in the subset

data.frame("Coefficient" = names(confint(mtest)[, , "Finger"][, 1]),
           "Lower_Bound" = as.numeric(round(exp(confint(mtest)[, , "Finger"][, 1]), 3)), 
           "Upper_Bound" = as.numeric(round(exp(confint(mtest)[, , "Finger"][, 2]), 3)))[-1, ]

data.frame("Coefficient" = names(confint(mtest)[, , "Forearm"][, 1]),
           "Lower_Bound" = as.numeric(round(exp(confint(mtest)[, , "Forearm"][, 1]), 3)), 
           "Upper_Bound" = as.numeric(round(exp(confint(mtest)[, , "Forearm"][, 2]), 3)))[-1, ]

data.frame("Coefficient" = names(confint(mtest)[, , "Knee"][, 1]),
           "Lower_Bound" = as.numeric(round(exp(confint(mtest)[, , "Knee"][, 1]), 3)), 
           "Upper_Bound" = as.numeric(round(exp(confint(mtest)[, , "Knee"][, 2]), 3)))[-1, ]


# We need separation for any of these variables to work, which we do not have

ggplot(sub_data, aes(heightInches, color = inj_cat)) + geom_density()

ggplot(sub_data, aes(weightLbs, color = inj_cat)) + geom_density()

ggplot(sub_data, aes(x0_mean, color = inj_cat)) + geom_density()

ggplot(sub_data, aes(z0_mean, color = inj_cat)) + geom_density()

ggplot(sub_data, aes(z0_diff, color = inj_cat)) + geom_density()

ggplot(sub_data, aes(SS_mean, color = inj_cat)) + geom_density()


# We can try to predict the injury type but it is not going to go well

library(caret)

set.seed(11221)

trainids <- createDataPartition(sub_data$inj_cat, p = .75)

baseball.multi <- multinom(inj_cat ~ heightInches + weightLbs + x0_mean +  z0_mean + z0_diff +
                             SS_mean + main_pitch_type + pitch_count_cat, data = sub_data[trainids$Resample1, ])

table(sub_data$inj_cat[-trainids$Resample1], predict(baseball.multi, sub_data[-trainids$Resample1, ]))

table(sub_data$inj_cat)

# Because Shoulder and Elbow have the highest frequency, this is where most predictions will fall
# since there are no predictors that are able to separate the different types of injuries







# End