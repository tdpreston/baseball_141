# BASEBALL PROJECT 141SL

# Required libraries

library(tidyverse)

# Load data

load("built_pitcherDate_DL_data.RData")
load("compiled_DL.RData")
load("compiled_pitcherGame.RData")

# xdf_full main dataset
# xfullmx_XY prior appearance and DL stuff
# xfullmx_XY_PxP pitch stuff
# xfullmx_XY_xtra DL description
# xfullmx_XY2 birth country
# xfullmx_XY3 days to season end

main_data <- cbind(xdf_full, xfullmx_XY)

# need a timestamp for later

main_data$timestamp <- as.Date(main_data$agg_Date, "%Y%m%d")


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

hist(p_nGames, breaks = 100) # many pitchers with few games

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
# get pitch count
# ------------------------------

pitchCount <- numeric(nrow(main_data))

biglist <- str_split(main_data$isPitch_seq, ";")

pitchCount <- unlist(lapply(biglist, function(x) {
  sum(x == "T")
}))


main_data$pitch_count <- pitchCount

# 7 day pitch count rolling average

# to save time I will only do this for high freq pitchers

main_data$pitch_roll_avg <- 0


for(i in seq_len(length(hi_freq_p))) {
  p1 <- main_data %>% 
    filter(name == hi_freq_p[i]) %>% 
    select(pitch_count) %>% 
    pull()
  
  p1mean <- numeric(length(p1))
  
  p1mean[1:6] <- 0
  
  for(j in seq_len(length(p1)-6)) {
    p1mean[j+6] <- mean(p1[j:(j+6)])
  }
  
  # length(p1mean)
  
  main_data$pitch_roll_avg[which(main_data$name == hi_freq_p[i])] <- p1mean
} 

# check to see if it worked, the first 6 will be zero

main_data %>% 
  filter(name == hi_freq_p[1]) %>% 
  select(pitch_roll_avg) %>% 
  head(25)

# ------------------------------
# cumulative sum of pitches per pitcher
# ------------------------------

main_data$cPitch <- 0

for(i in seq_len(length(p_names))) {
  main_data$cPitch[main_data$name == p_names[i]] <- main_data %>% 
    filter(name == p_names[i]) %>% 
    select(pitch_count) %>% 
    cumsum() %>% 
    pull()
}

# check cumulative

main_data %>% 
  filter(name == hi_freq_p[1]) %>% 
  select(cPitch) %>% 
  head(25)

# ------------------------------
# get pitch types
# ------------------------------

pitch_type_list <- str_split(main_data$Ptype_seq, ";")

pitch_types <- names(table(unlist(pitch_type_list)))

# there is an NA pitch, you can remove it if you want but I think some pitches are actually NA
# instead of no pitch thrown

# ------------------------------
# pitch type data
# ------------------------------

# here I am going to create an empty data frame to bind to the main data

blank_pitch_df <- as.data.frame(matrix(0, nrow = nrow(main_data), ncol = length(pitch_types)))

colnames(blank_pitch_df) <- pitch_types

head(blank_pitch_df)

# append to main_data

main_data <- cbind(main_data, blank_pitch_df)

# cycle through each pitch type and sum each type of pitch and append to new data
# that we attached to the main data

# this one could take a minute

for(i in seq_len(length(pitch_types))) {
  main_data[, pitch_types[i]] <- unlist(lapply(main_data$Ptype_seq, function(x) {
    sum(unlist(str_split(x, ";")) == pitch_types[i])
  }))
}

# ------------------------------
# cumulative pitch type data
# ------------------------------

# here I am going to do a cumulative sum of each pitch type

# again, start with a blank data frame and append to the main data

# cumulative pitch types

blank_c_pitch_df <- as.data.frame(matrix(0, nrow = nrow(main_data), ncol = length(pitch_types)))

colnames(blank_c_pitch_df) <- paste("c_", pitch_types, sep = "")

head(blank_c_pitch_df)

# append to main_data

main_data <- cbind(main_data, blank_c_pitch_df)

# to save some time I am going to do this to only high frequency pitchers,
# make sure you have hi_freq_p in your environment

for(i in seq_len(length(hi_freq_p))) {
  
  for(j in seq_len(length(pitch_types))) {
    
    main_data[main_data$name == hi_freq_p[i], paste("c_", pitch_types[j], sep = "")] <- main_data %>% 
      filter(name == hi_freq_p[i]) %>% 
      select(pitch_types[j]) %>% 
      cumsum() %>% 
      pull()
    
  }
  
}


# check a sample

main_data %>% 
  filter(name == hi_freq_p[1]) %>% 
  select(59:79) %>% 
  head(25)

# ------------------------------
# x0, z0, SS stuff
# ------------------------------

# make sure this is "x0_seq"  "z0_seq"  "SS_seq"  "Ptype_seq" "isPitch_seq"

colnames(main_data[, 19:23])

pitch_sub <- main_data[, 19:23]

colnames(pitch_sub[, 3:4])

pitch_types <- unique(unlist(str_split(pitch_sub$Ptype_seq, ";")))

# if there are any warnings check the summary data after the loop, might just be some NA's but still works

# SS data

SS_df <- as.data.frame(matrix(0, nrow = nrow(pitch_sub), ncol = length(pitch_types)))

colnames(SS_df) <- paste("SS_", pitch_types, sep = "")

head(SS_df)

for(i in seq_len(length(pitch_types))) {
  
  
  SS_df[, i] <- apply(pitch_sub[, 3:4, drop = FALSE], 1, function(x) {  
    
    ids <- which(unlist(str_split(x[2], ";")) == pitch_types[i])
    
    if(length(ids)==0 | pitch_types[i] == "NA") {
      NA
    } else {
      mean(as.numeric(str_split(x[1], ";")[[1]][ids]), na.rm = TRUE)
    }
    
  })
  
  
}

summary(SS_df)

# z0 data

z0_df <- as.data.frame(matrix(0, nrow = nrow(pitch_sub), ncol = length(pitch_types)))

colnames(z0_df) <- paste("z0_", pitch_types, sep = "")

head(z0_df)

for(i in seq_len(length(pitch_types))) {
  
  
  z0_df[, i] <- apply(pitch_sub[, c(2,4), drop = FALSE], 1, function(x) {  
    
    ids <- which(unlist(str_split(x[2], ";")) == pitch_types[i])
    
    if(length(ids)==0 | pitch_types[i] == "NA") {
      NA
    } else {
      mean(as.numeric(str_split(x[1], ";")[[1]][ids]), na.rm = TRUE)
    }
    
  })
  
  
}

summary(z0_df)


# x0 data

x0_df <- as.data.frame(matrix(0, nrow = nrow(pitch_sub), ncol = length(pitch_types)))

colnames(x0_df) <- paste("x0_", pitch_types, sep = "")

head(x0_df)

for(i in seq_len(length(pitch_types))) {
  
  
  x0_df[, i] <- apply(pitch_sub[, c(1,4), drop = FALSE], 1, function(x) {  
    
    ids <- which(unlist(str_split(x[2], ";")) == pitch_types[i])
    
    if(length(ids)==0 | pitch_types[i] == "NA") {
      NA
    } else {
      mean(as.numeric(str_split(x[1], ";")[[1]][ids]), na.rm = TRUE)
    }
    
  })
  
  
}

summary(x0_df)

# combine to make one big ass data frame

main_data <- cbind(main_data, x0_df, z0_df, SS_df)

# ------------------------------
# injury info for y14
# ------------------------------

main_data$yn14_why <- xfullmx_XY_xtra[, 7]

# save this data so you don't have to do it again

write.csv(main_data, "baseball_main_data.csv", row.names = FALSE)

# clear your workspace and load the csv to get rid of all the crap you don't need

rm(list=ls())

# I hope that all worked...



# End