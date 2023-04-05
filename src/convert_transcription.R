library(tidyverse)

setwd("C:/Users/conno/git_repos/spanish_phonotactics")
filename <- "data/training.csv"

df <- read_csv(filename) 

filtered_df <- df %>%
  select(-X) %>%
  mutate(transcription = map_chr(
    str_split(sa_phon_structure, pattern = ""), 
    str_flatten, collapse=" "
  )) %>%
  # Add more of the same row below but changing the mapping
  mutate(transcription=str_replace_all(transcription, 'B', 'b')) %>%
  mutate(transcription=str_replace_all(transcription, 'D', 'd')) %>%
  mutate(transcription=str_replace_all(transcription, 's', 's')) %>%
  mutate(transcription=str_replace_all(transcription, 'S', 'ʃ')) %>%
  mutate(transcription=str_replace_all(transcription, 'J', 'ʝ')) %>%
  mutate(transcription=str_replace_all(transcription, 'y', 'j')) %>%
  mutate(transcription=str_replace_all(transcription, 'r', 'ɾ')) %>%
  mutate(transcription=str_replace_all(transcription, 'R', 'r')) %>%
  mutate(transcription=str_replace_all(transcription, 'N', 'ɲ')) %>%
  mutate(transcription=str_replace_all(transcription, 'G', 'g')) %>%
  mutate(transcription=str_replace_all(transcription, 'L', 'ʝ')) %>%
  mutate(transcription=str_replace_all(transcription, 'C', 'tʃ'))

write_csv(filtered_df, 'data/transcribed_training.csv')
