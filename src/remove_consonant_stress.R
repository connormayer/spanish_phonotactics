library(tidyverse)

full_stress <- read_csv('data/training/training_data_stress.csv', col_names=FALSE)
vowel_stress <- full_stress %>%
  mutate(X1 = str_replace_all(X1, "([^aeiou])1", "\\1"))

vowel_stress %>%
  write_csv('data/training/training_data_vowel_stress.csv', col_names=FALSE)

vowel_stress %>%
  mutate(X1 = str_replace_all(X1, "([aeiou])1", "\\1")) %>%
  write_csv('data/training/training_data_no_stress.csv', col_names=FALSE)

full_stimuli_stress <- read_csv('data/stimuli_candidates_final_v4.csv')
vowel_stress_stimuli <- full_stimuli_stress %>%
  select(word) %>%
  mutate(word = str_replace_all(word, '^i', 'j'),
         word = str_replace_all(word, '^u', 'w'),
         word = str_replace_all(word, "^([^ ]+ [^ ]+ )u(1? [aieou])", "\\1w\\2"),
         word = str_replace_all(word, "^([^ ]+ [^ ]+ )i(1? [aieou])", "\\1j\\2"),
         word = str_replace_all(word, "([^aeiou])1", "\\1"),
         word = str_replace_all(word, "t͡ʃ", "tʃ"),
         ) 

vowel_stress_stimuli %>%
  write_csv('data/testing/testing_data_vowel_stress.csv', col_names=FALSE)

vowel_stress_stimuli %>%
  mutate(word = str_replace_all(word, "([aeiou])1", "\\1")) %>%
  write_csv('data/testing/testing_data_no_stress.csv', col_names=FALSE)
