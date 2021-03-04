# Baseball Results



# Model subset from the main baseball data

inj_subset <- read.csv("inj_subset.csv")

# About this data set:
# This dataset is a combination from the datasets provided and filtered by: 
# 1) Pitchers who have More than 50 games in the dataset
# 2) Pitchers who have not been on the DL to date
# 3) Pitcher's first injury to go on DL
# 4) Pitchers who either throw mainly Fastball or Offspeed

# and the pitcher's statistics are based on the last game played before going on the DL


# The injury categories

table(inj_subset$inj_cat)

# Get rid of other since we are not worried about it

inj_subset <- inj_subset %>% 
  filter(inj_cat != "other")



# Initial ANOVA

inj.aov <- aov(z0_diff ~ inj_cat, data = inj_subset)

summary(inj.aov)

# We thought that the relative release point would be a main indicator for pitching injury types but the 
# initial test did not prove significant so we decided to move onto a more complex model so if other
# variables were able to help with injury prediction

# Multinomial

# install.packages("foreign")

library(foreign)

# install.packages("nnet")

library(nnet)


inj_subset$inj_cat <- factor(inj_subset$inj_cat)

inj_subset$inj_cat <- relevel(inj_subset$inj_cat, ref = "Shoulder")

inj.multi <- multinom(inj_cat ~ ageYrs + heightInches + weightLbs + x0_mean +  z0_mean + z0_diff +
                    SS_mean + main_pitch_type + pitch_count_cat, data = inj_subset)


# These are tables of the confidence interval of odds ratios for the coefficient

# Each table is in reference to Shoulder since that is specified as the reference

# If the odds ratio bound contains 1, then that predictor is not significant since you are 1 times likely
# to get one injury from the other


elbow_coef <- data.frame("Name" = names(confint(inj.multi)[, , "Elbow"][, 1]),
           "Lower_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Elbow"][, 1]), 3)), 
           "Coefficient" = as.numeric(exp(coef(inj.multi))["Elbow", ]),
           "Upper_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Elbow"][, 2]), 3)))[-1, ]

elbow_coef

# Elbow and Shoulder injuries are some of the most common injuries so this comparison seems like 
# it would be one the more important comparisons. As you can see, most of the variables appear
# to be insignificant but height barely makes the cut as a significant predictor. What the coefficient
# is saying is that you are .86 times likely to have an elbow injury vs a shoulder injury for 
# an increment in height, the upper bound though is very close to 1 so it is not something that
# stands out as extreme. Height outliers in the data could be causing this as well but it would be 
# interesting to investigate further since we did not initially think height would play a roll.



arm_coef <- data.frame("Name" = names(confint(inj.multi)[, , "Arm_other"][, 1]),
                         "Lower_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Arm_other"][, 1]), 3)), 
                         "Coefficient" = as.numeric(exp(coef(inj.multi))["Arm_other", ]),
                         "Upper_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Arm_other"][, 2]), 3)))[-1, ]

arm_coef


# Insignificant 

lower_coef <- data.frame("Name" = names(confint(inj.multi)[, , "Lower_Body"][, 1]),
                       "Lower_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Lower_Body"][, 1]), 3)), 
                       "Coefficient" = as.numeric(exp(coef(inj.multi))["Lower_Body", ]),
                       "Upper_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Lower_Body"][, 2]), 3)))[-1, ]

lower_coef

# Insignificant

trunk_coef <- data.frame("Name" = names(confint(inj.multi)[, , "Trunk"][, 1]),
                         "Lower_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Trunk"][, 1]), 3)), 
                         "Coefficient" = as.numeric(exp(coef(inj.multi))["Trunk", ]),
                         "Upper_Bound" = as.numeric(round(exp(confint(inj.multi)[, , "Trunk"][, 2]), 3)))[-1, ]

trunk_coef

# Insignificant 

# Using this to predict data does not seem promising but we'll give it a shot anyways


library(caret)

set.seed(11221)

trainids <- createDataPartition(inj_subset$inj_cat, p = .75)

inj.multi.train <- multinom(inj_cat ~ ageYrs + heightInches + weightLbs + x0_mean +  z0_mean + z0_diff +
                             SS_mean + main_pitch_type + pitch_count_cat, data = inj_subset[trainids$Resample1, ])

table(inj_subset$inj_cat[-trainids$Resample1], predict(inj.multi.train, inj_subset[-trainids$Resample1, ]))

mean(inj_subset$inj_cat[-trainids$Resample1] == predict(inj.multi.train, inj_subset[-trainids$Resample1, ]), na.rm = TRUE)

# We achieve 22% accuracy but what does that really mean?

# Mixing up the injuries between players and running the model many times should provide a little more clarity
# if the model is actually doing anything

# copy data first

inj_copy <- inj_subset

# empty vector

multi_means <- numeric(1000)

set.seed(23231)

for(i in 1:1000) {
  
  # mix up the injuries
  
  inj_copy$inj_cat <- sample(inj_copy$inj_cat, length(inj_copy$inj_cat))
  
  # run the  model
  
  trainids <- createDataPartition(inj_copy$inj_cat, p = .75)
  
  inj.rand.multi.train <- multinom(inj_cat ~ ageYrs + heightInches + weightLbs + x0_mean +  z0_mean + z0_diff +
                                SS_mean + main_pitch_type + pitch_count_cat, data = inj_copy[trainids$Resample1, ])
  
  table(inj_copy$inj_cat[-trainids$Resample1], predict(inj.rand.multi.train, inj_copy[-trainids$Resample1, ]))
  
  multi_means[i] <- mean(inj_copy$inj_cat[-trainids$Resample1] == predict(inj.rand.multi.train, inj_copy[-trainids$Resample1, ]), na.rm = TRUE)
}

# Our original accuracy was 22% and looking at the histogram of the accuracy for random injuries we do not do any better

hist(multi_means, xlab = "Random Injury Accuracy",
     main = "Actual Accuracy Compared to Random Injury Assignment", density = 80)
abline(v = .22, col = "red", lty = 1, lwd = 3)

# Although our model makes some correct predictions, they are basically just random guesses



# Plot Ideas

ggplot(inj_subset, aes(inj_cat, z0_diff)) + geom_boxplot() + 
  ylab("Pitcher Relative Release Point") + 
  xlab("Injury Type") + 
  ggtitle("Pitcher Injury Type vs Relative Release Point")

ggplot(inj_subset, aes(inj_cat, heightInches)) + geom_boxplot() + 
  ylab("Pitcher Height") + 
  xlab("Injury Type") + 
  ggtitle("Pitcher Injury Type vs Height")

ggplot(inj_subset, aes(inj_cat, ageYrs)) + geom_boxplot() + 
  ylab("Pitcher Age") + 
  xlab("Injury Type") + 
  ggtitle("Pitcher Injury Type vs Age")


# We can do whatever, these are just some basic ones



# Tables

table(inj_subset$inj_cat, inj_subset$main_pitch_type)

table(inj_subset$inj_cat, inj_subset$pitch_count_cat)

tapply(inj_subset$heightInches, list(inj_subset$inj_cat), mean, na.rm = TRUE)

tapply(inj_subset$ageYrs, list(inj_subset$inj_cat), mean, na.rm = TRUE)



# Pitcher Release plot (don't worry I will update this with Zes' face)


# install.packages("jpeg")
library(jpeg)

# install.packages("png")
library(png)

new_df <- inj_subset %>% 
  filter(RHP == 1) %>% 
  filter(inj_cat != "other") %>% 
  select(x0_mean, z0_mean, inj_cat)


my_image <- readPNG('pitcher.png')

palette(c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99'))


# Set up a plot area with no plot
plot(1:2, type='n', xlim = c(-5, 5),
     ylim = c(0, 7), xlab = "Mound Position", ylab = "Release Height",
     main = "Pitcher Release Points and Injury Type")

# Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(my_image, 
            xleft=-2, xright=4, 
            ybottom=-2, ytop=6.7)
# grid()

points(new_df[, c(1:2)], cex = .7, pch = 16,
       col = as.numeric(factor(new_df$inj_cat)))
legend("bottomleft", levels(factor(new_df$inj_cat)),inset = .05,
       cex = .7, fill = 1:5, border = "white", bty = "n")


# reset your pallete

palette("default")






# End