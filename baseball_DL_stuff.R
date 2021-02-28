# ------------------------------
# more baseball stuff
# ------------------------------


# load the data

main_data <- read.csv("baseball_main_data.csv")

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
main_pitch_type[which(main_pitch_type == "c_SI")] <- "Sinker"
main_pitch_type[which(main_pitch_type == "c_SL")] <- "Slider"

main_pitch_type[which(main_pitch_type == "c_NA")] <- "Other"
main_pitch_type[which(main_pitch_type == "c_KC")] <- "Other"
main_pitch_type[which(main_pitch_type == "c_FS")] <- "Other"
main_pitch_type[which(main_pitch_type == "c_FC")] <- "Other"
main_pitch_type[which(main_pitch_type == "c_CU")] <- "Other"
main_pitch_type[which(main_pitch_type == "c_CH")] <- "Other"



table(main_pitch_type)

upcoming_dl_1$main_pitch_type <- main_pitch_type



# assign average pitch count category

upcoming_dl_1$pitch_count_cat <- factor(ifelse(upcoming_dl_1$pitch_roll_avg >= 50, "over50pg", "under50pg"))

table(upcoming_dl_1$pitch_count_cat)

# average injury age by pitch type and pitch frequency

tapply(upcoming_dl_1$ageYrs, list(upcoming_dl_1$main_pitch_type, upcoming_dl_1$pitch_count_cat), mean)


# ------------------------------
# injury categories
# ------------------------------


inj_type_vector <- c("shoulder", "elbow", "forearm", "back")

upcoming_dl_1$inj_cat <- ""

for(i in seq_len(length(inj_type_vector))) {
  upcoming_dl_1$inj_cat[which(str_detect(upcoming_dl_1$yn14_why, inj_type_vector[i]))] <- inj_type_vector[i]
}

upcoming_dl_1$inj_cat[which(upcoming_dl_1$inj_cat == "")] <- "other"

table(upcoming_dl_1$inj_cat, upcoming_dl_1$main_pitch_type)


# ------------------------------
# Average data for z0, x0 and SS
# ------------------------------

# here we are going to get some summary stats between a certain date for each pitcher


dl_names <- upcoming_dl_1$name

dl_timestamp <- upcoming_dl_1$timestamp

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

tapply(upcoming_dl_1$SS_FF_mean, list(upcoming_dl_1$main_pitch_type, upcoming_dl_1$inj_cat), mean)






# end