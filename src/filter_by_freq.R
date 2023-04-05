library(tidyverse)

df <- read_csv("E:/Dropbox/ling/research/nsf_megha/berta/training_data_stress.csv",
               col_names = FALSE)

df %>%
  filter(X2 >= 1) %>%
  write_csv("E:/Dropbox/ling/research/nsf_megha/berta/training_data_stress_high_freq.csv",
            col_names = FALSE)

df <- read_csv("E:/Dropbox/ling/research/nsf_megha/berta/training_data_no_stress.csv",
               col_names = FALSE)

df %>%
  filter(X2 >= 1) %>%
  write_csv("E:/Dropbox/ling/research/nsf_megha/berta/training_data_no_stress_high_freq.csv",
            col_names = FALSE)

