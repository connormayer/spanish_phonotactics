library(tidyverse)

#setwd("C:/Users/conno/git_repos/spanish_phonotactics")
setwd("E:/git_repos/spanish_phonotactics")

df <- read_csv("data/stimuli_candidates_final.csv")

# Verify we have the right amounts, should be 36 in each
df %>%
  group_by(bucket_q) %>%
  count()

num_groups <- 8

# Set seed for reproducibility
set.seed(123456789)

# Randomly re-order rows within buckets
df_randomized <- df %>% 
  group_by(bucket_q) %>%
  slice(sample(1:n())) %>%
  ungroup()

# Assign block number
grouped_df <- df_randomized %>%
  mutate(group = rep(seq_len(num_groups), length.out=n()))

grouped_df %>% 
  group_by(group) %>%
  summarize(mean_uni = mean(uni_prob),
         mean_bi = mean(bi_prob_smoothed))

grouped_df %>%
  write_excel_csv('data/grouped_stimuli.csv')
