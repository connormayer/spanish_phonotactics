library(tidyverse)
# I didn't use this library here, but you could
library(googlesheets4)

#df <- read_csv("C:/Users/conno/Dropbox/ling/research/nsf_megha/berta/spanish_stimuli.csv")
df <- read_csv("E:/Dropbox/ling/research/nsf_megha/berta/spanish_stimuli.csv")
# Alternative version using paths relative to the current working directory
# setwd("C:/Users/conno/Dropbox/ling/research/nsf_megha/berta/")
# df <- read_csv("spanish_stimuli.csv")

# Filter out non-na results
df <- df %>%
  filter(is.na(result)) %>%
  select(word)

# Alternative version without pipes
# df <- filter(df, is.na(result))
# df <- select(df, word)

# Save file
write_csv(df, "C:/Users/conno/Dropbox/ling/research/nsf_megha/berta/filtered_stimuli.csv", 
          col_names = FALSE)

# This is scrap code we were discussing
# How to replace all NAs with 0s
# df %>%
#   mutate(result = ifelse(is.na(result), 0, 1))

# Syntax for the ifelse function
# ifelse(boolean, do_if_true, do_if_false)
#
# Example of an if statement in RRR
# if (var > 0) {
#   do something
# } else if (var < 0) {
#   do something
# }
