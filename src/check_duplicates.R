# This script compares two CSVs and returns rows in the second that are not
# in the first
library(tidyverse)

#setwd("C:/Users/conno/git_repos/spanish_phonotactics")
setwd("E:/git_repos/spanish_phonotactics")

df1 <- read_csv("data/stimuli_candidates_final_v3.csv")
df2 <- read_csv("data/stimuli_candidates_final_v4.csv")

new_df <- anti_join(df2, df1)

new_df %>% 
  write_csv('differences_v4.csv')
