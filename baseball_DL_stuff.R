# ------------------------------
# more baseball stuff
# ------------------------------


# load the data

main_data <- read.csv("baseball_main_data.csv")


load("built_pitcherDate_DL_data.RData")

load("compiled_pitcherGame.RData")

load("compiled_DL.RData")


# you need this variable in main_data, future date of next DL

main_data$fut_DL_1 <- as.Date(substr(xfullmx_XY_xtra[, 4], 1, 8), "%Y%m%d")

# ------------------------------
# high frequency pitchers
# ------------------------------

# you will need this variable

# get all pitcher names

p_names <- sort(unique(main_data$name))

# number of games per pitcher

p_nGames <- numeric(length(p_names)) # empty 

for(i in seq_len(length(p_names))) {
  p_nGames[i] <- main_data %>% 
    filter(name == p_names[i]) %>% 
    nrow()
}

# histogram of games per pitcher

# hist(p_nGames, breaks = 100) # many pitchers with few games

# pitcher total games data frame

tot_games <- data.frame(name = p_names,
                        games = p_nGames)

head(tot_games)

# filter pitchers by # of games

hi_freq_p <- tot_games %>% 
  filter(games > 50) %>% # adjust number of games here
  select(name) %>% 
  pull()

# names of pitcher with more than x games

hi_freq_p



# ------------------------------
# pitcher DL subset
# ------------------------------

# you can adjust this how ever you want but here I have high frequency pitchers who have not appeared
# on the DL yet and will in the next 14 days

# I get one example row from the main data that we'll get rid of after

# you need hi_freq_p for this to work

upcoming_dl_1 <- main_data[1, ]

for(i in seq_len(length(hi_freq_p))) {
  p_data <- main_data %>% 
    filter(y14 == 1 & cum_occur_dl == 0) %>% # here is where you can adjust if they have been on DL or not
    filter(name == hi_freq_p[i])
  
  if(nrow(p_data) == 0) {
    # skip it boo
  } else {
    upcoming_dl_1 <- rbind(upcoming_dl_1, p_data[1, ])
  }
}

upcoming_dl_1 <- upcoming_dl_1[-1, ]


#### new, get last game pitcher pitched before going on DL

new_dl <- upcoming_dl_1[1, ]

for(i in seq_len(nrow(upcoming_dl_1))) {
  new_dl[i, ] <- main_data %>% 
    filter(name == upcoming_dl_1$name[i]) %>% 
    filter(timestamp <= upcoming_dl_1$fut_DL_1[i]) %>% 
    arrange(desc(timestamp)) %>% 
    head(1)
}

# replace upcoming DL with this

upcoming_dl_1 <- new_dl

# Find the main pitch for each pitcher

main_pitch_type <- character(nrow(upcoming_dl_1))

for(i in seq_len(length(main_pitch_type))) {
  main_pitch_type[i] <- names(which.max(upcoming_dl_1[i, 59:79]))
}

# re-categorize main pitch types

sort(table(main_pitch_type), decreasing = TRUE) / length(main_pitch_type)


# Fastball (4 seam and 2 seam), Sinker, Slider, Other

# There's probably an easier way to do this but whatever

main_pitch_type[which(main_pitch_type == "c_FF")] <- "Fastball"
main_pitch_type[which(main_pitch_type == "c_FT")] <- "Fastball"
main_pitch_type[which(main_pitch_type == "c_SI")] <- "Fastball"
main_pitch_type[which(main_pitch_type == "c_SL")] <- "Offspeed"

main_pitch_type[which(main_pitch_type == "c_NA")] <- "Other"
main_pitch_type[which(main_pitch_type == "c_KC")] <- "Offspeed"
main_pitch_type[which(main_pitch_type == "c_FS")] <- "Fastball"
main_pitch_type[which(main_pitch_type == "c_FC")] <- "Fastball"
main_pitch_type[which(main_pitch_type == "c_CU")] <- "Offspeed"
main_pitch_type[which(main_pitch_type == "c_CH")] <- "Offspeed"



table(main_pitch_type)

upcoming_dl_1$main_pitch_type <- main_pitch_type

# get rid of NA pitchers

upcoming_dl_1 <- upcoming_dl_1 %>% 
  filter(main_pitch_type != "Other")

# assign average pitch count category

upcoming_dl_1$pitch_count_cat <- factor(ifelse(upcoming_dl_1$pitch_roll_avg >= 50, "over50pg", "under50pg"))

table(upcoming_dl_1$pitch_count_cat)

# average injury age by pitch type and pitch frequency

tapply(upcoming_dl_1$ageYrs, list(upcoming_dl_1$main_pitch_type, upcoming_dl_1$pitch_count_cat), mean)


# ------------------------------
# injury categories
# ------------------------------


inj_type_vector <- c("shoulder", "elbow", "forearm", "back")

# inj_type_vector <- c("shoulder", "elbow")


# inj_type_vector <- c("strain")


upcoming_dl_1$inj_cat <- ""

for(i in seq_len(length(inj_type_vector))) {
  upcoming_dl_1$inj_cat[which(str_detect(upcoming_dl_1$yn14_why, inj_type_vector[i]))] <- inj_type_vector[i]
}

upcoming_dl_1$inj_cat[which(upcoming_dl_1$inj_cat == "")] <- "other"

table(upcoming_dl_1$inj_cat)


# quick chi-squared test

table(upcoming_dl_1$inj_cat, upcoming_dl_1$main_pitch_type)

chisq.test(table(upcoming_dl_1$inj_cat, upcoming_dl_1$main_pitch_type))

# ------------------------------
# Average data for z0, x0 and SS
# ------------------------------

# here we are going to get some summary stats between a certain date for each pitcher


dl_names <- upcoming_dl_1$name

dl_timestamp <- upcoming_dl_1$fut_DL_1

seq_mean_df <- as.data.frame(matrix(0, nrow = nrow(upcoming_dl_1), ncol = length(83:145)))

colnames(seq_mean_df) <- paste(colnames(main_data)[83:145], "_mean", sep="")

for(i in seq_len(length(dl_names))) {
  seq_mean_df[i, ] <- main_data %>% 
    filter(name == dl_names[i]) %>% 
    filter(timestamp <= dl_timestamp[i]) %>% 
    select(83:145) %>% 
    colMeans(na.rm = TRUE) %>% 
    as.numeric()
}

head(seq_mean_df)

# add this stuff to upcoming_dl_1

upcoming_dl_1 <- cbind(upcoming_dl_1, seq_mean_df)


# there is a lot of missing stuff so you will get NaN and NA's throughout but I think we can work with it

tapply(upcoming_dl_1$cPitch, list(upcoming_dl_1$main_pitch_type, upcoming_dl_1$inj_cat), mean, na.rm = TRUE)


# some new variables 

# for fastball pitches

upcoming_dl_1$p_h_diff_z0_FF <-  (upcoming_dl_1$z0_FF_mean * 12)  - upcoming_dl_1$heightInches

upcoming_dl_1$p_h_diff_z0_FT <- (upcoming_dl_1$z0_FT_mean * 12) - upcoming_dl_1$heightInches

upcoming_dl_1$p_h_diff_z0_SI <- (upcoming_dl_1$z0_SI_mean * 12) - upcoming_dl_1$heightInches

upcoming_dl_1$p_h_diff_z0_FC <- (upcoming_dl_1$z0_FC_mean * 12) - upcoming_dl_1$heightInches

upcoming_dl_1$p_h_diff_z0_FS <- (upcoming_dl_1$z0_FS_mean * 12) - upcoming_dl_1$heightInches

# mean release of fastball pitches

upcoming_dl_1$mean_release_fastball <- apply(upcoming_dl_1[, 213:217], 1, mean, na.rm = TRUE)

# mean velocity of fastball pitches

upcoming_dl_1$mean_velocity_fastball <- apply(upcoming_dl_1[, c(194, 196, 199, 200, 198)], 1, mean, na.rm = TRUE)

# mean whatever x0 is

# 154, 156, 152, 158, 157

upcoming_dl_1$mean_x0_fastball <- apply(upcoming_dl_1[, c(154, 156, 152, 158, 157)], 1, mean, na.rm = TRUE)


# ------------------------------
# Basic tests
# ------------------------------

# There aren't enough offspeed dominant pitchers so it wouldn't really
# make sense to include them so I am just going to filter by Fastball

table(upcoming_dl_1$main_pitch_type)

sub_df <- upcoming_dl_1 %>% 
  filter(main_pitch_type == "Fastball")

# Is there a difference between release points and injury types?

baseball.aov <- aov(mean_release_fastball ~ inj_cat, data = sub_df)

summary(baseball.aov)


# Do low volume pitchers have different injuries than high volume pitchers?

table(sub_df$pitch_count_cat, sub_df$inj_cat)

chisq.test(table(sub_df$pitch_count_cat, sub_df$inj_cat))


# random forest

library(randomForest)

library(caret)

# 14, 16, 17, 37, 81, 149, 147, 148, 218:220

rf_data <- upcoming_dl_1 %>% 
  select(c(149, 14, 16, 17, 37, 81, 147, 148, 218:220))

nrow(na.omit(rf_data))

rf_data <- na.omit(rf_data)

nrow(rf_data)

trainids <- createDataPartition(rf_data$inj_cat, p = .75)

baseball.rf <- randomForest(factor(inj_cat) ~ ., data = rf_data, subset = trainids$Resample1,
                            mtry = 4, ntrees = 1000)

baseball.rf.predict <- predict(baseball.rf, rf_data[-trainids$Resample1, ])

# this thing sucks but it makes sense

table(rf_data$inj_cat[-trainids$Resample1], baseball.rf.predict)







# end