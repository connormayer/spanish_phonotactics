library(tidyverse)

espal <- read_csv("data/subtlex_esp_espal_data.csv")

transcribed_data <- espal %>%
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
  mutate(transcription=str_replace_all(transcription, 'J', 'j')) %>%
  mutate(transcription=str_replace_all(transcription, 'y', 'j')) %>%
  mutate(transcription=str_replace_all(transcription, 'r', 'ɾ')) %>%
  mutate(transcription=str_replace_all(transcription, 'R', 'r')) %>%
  mutate(transcription=str_replace_all(transcription, 'N', 'ɲ')) %>%
  mutate(transcription=str_replace_all(transcription, 'G', 'g')) %>%
  mutate(transcription=str_replace_all(transcription, 'L', 'j')) %>%
  mutate(transcription=str_replace_all(transcription, 'C', 'tʃ')) %>%
  mutate(transcription_str = str_replace(transcription, str_glue("^(([^aieou]*[aeiou]){{{sa_syll_accent}}})"), "\\11")) %>%
  filter(!(word %in% c("nietzsche", "software", "etc", "little"))) %>%
  distinct() %>%
  mutate(freq_per_mil = (cnt / sum(cnt)) * 1000000) %>%
  select(-frq)

# Combine all strings in column R into a single string
combined_string <- paste0(transcribed_data$transcription, collapse = "")
# Split the combined string into individual characters
all_characters <- strsplit(combined_string, split = "")[[1]]
# Get the unique characters
unique_characters <- unique(all_characters)
# Print the unique characters
print(unique_characters)
