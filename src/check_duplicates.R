library(tidyverse)

setwd("C:/Users/conno/git_repos/spanish_phonotactics")

df1 <- read_csv("data/stimuli_candidates_v2.csv")
df2 <- read_csv("data/stimuli_candidates_v3.csv")

df2 <- df2 %>%
  mutate(word=str_replace(word, 't͡ʃ', 'tʃ'))

new_df <- anti_join(df2, df1)

new_df %>% 
  write_csv('differences.csv')
