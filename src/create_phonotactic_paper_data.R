library(tidyverse)

responses <- read_csv('data/spanish_responses_phonotactics_paper.csv') 
scores <- read_csv("data/wug_word_scores_stress_2.csv")

left_join(responses, scores, by='word') %>%
  write_csv('data/spanish_responses_phonotactics_paper_scored_2.csv')
