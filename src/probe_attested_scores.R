library(tidyverse)

setwd("E:/git_repos/spanish_phonotactics")

df <- read_csv("data/attested_word_scores_stress.csv") %>%
  select(word, uni_prob, bi_prob_smoothed, word_len)
