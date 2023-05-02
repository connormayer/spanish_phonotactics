library(tidyverse)

# Our data come from SUBTLEX-ESP, a database of word frequency counts taken
# from a large corpus of Spanish subtitles. We don't actually use the 
# frequencies, but we'll use the word list as a starting point

# This word list has been fed through EsPal (https://www.bcbl.eu/databases/espal/)
# which gets us a number of word properties related to frequency, neighborhood
# density, phonological form, etc.

# The file containing this data is data/subtlex_esp_espal_data.csv

# The first step is to use the Python script src/syllabify_and_transcribe.py
# to syllabify and transcribe the orthographic representations into IPA

# data <- read_csv('E:/git_repos/spanish_phonotactics/data/syllabified_spanish.csv')
data <- read_csv('C:/Users/conno/git_repos/spanish_phonotactics/data/syllabified_spanish.csv')

# Keep only high frequency items (> 1 per millon words). Do this in advance to 
# avoid dealing with weird loanwords
# Remove some weird words and duplicate entries
data <- data %>%
  select(-1,-2, -X) %>%
  filter(frq >= 1) %>%
  filter(!(word %in% c("nietzsche", "software", "etc", "little"))) %>%
  distinct()

# Replace IPA g character
data <- data %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 'ɡ', 'g'),
         stressed_ipa = str_replace_all(stressed_ipa, 'ɡ', 'g'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'ɡ', 'g')) 

data <- data %>%
  # Remove a few annoying characters from epitrans transcriptions
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 't ͡ ʃ', 'tʃ'),
         stressed_ipa = str_replace_all(stressed_ipa, 't ͡ ʃ', 'tʃ'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 't͡ʃ', 'tʃ') ) %>%
  mutate(stressed_ipa = str_replace_all(stressed_ipa, 't1 ͡1 ʃ1', 'tʃ1')) %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, '([^t]|^)ʃ', '\\1s'),
         stressed_ipa = str_replace_all(stressed_ipa, '([^t]|^)ʃ', '\\1s'),
         syllabified_ipa = str_replace_all(syllabified_ipa, '([^t]|^)ʃ', '\\1s')) %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 'ü', 'u'),
         stressed_ipa = str_replace_all(stressed_ipa, 'ü', 'u'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'ü', 'u')) %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 'j', 'ʝ'),
         stressed_ipa = str_replace_all(stressed_ipa, 'j', 'ʝ'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'j', 'ʝ')) %>%
  
  # Fix trill/tap allophony
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, '^ɾ', 'r'),
         stressed_ipa = str_replace_all(stressed_ipa, '^ɾ', 'r'),
         syllabified_ipa = str_replace_all(syllabified_ipa, '^ɾ', 'r')) %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 'ɾɾ', 'r'),
         stressed_ipa = str_replace_all(stressed_ipa, 'ɾɾ', 'r'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'ɾɾ', 'r')) %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, '([lnsz])ɾ', '\\1r'),
         stressed_ipa = str_replace_all(stressed_ipa, '([lnsz])ɾ', '\\1r'),
         syllabified_ipa = str_replace_all(syllabified_ipa, '([lnsz])ɾ', '\\1r')) %>%
  
  # Glide mistranscription
  # Convert all glides to corresponding vowel phonemes
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 'll', 'ʝ'),
         stressed_ipa = str_replace_all(stressed_ipa, 'll', 'ʝ'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'll', 'ʝ')) %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 'ʝ', 'i'),
         stressed_ipa = str_replace_all(stressed_ipa, 'ʝ', 'i'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'ʝ', 'i')) %>%
  mutate(unsyllabified_ipa = str_replace_all(unsyllabified_ipa, 'w', 'u'),
         stressed_ipa = str_replace_all(stressed_ipa, 'w', 'u'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'w', 'u')) 

data %>%
  select(unsyllabified_ipa, frq) %>%
  write_csv("C:/Users/conno/git_repos/spanish_phonotactics/data/training/training_data_no_stress.csv", col_names=FALSE)

data %>%
  select(stressed_ipa, frq) %>%
  write_csv("C:/Users/conno/git_repos/spanish_phonotactics/data/training/training_data_stress.csv", col_names=FALSE)