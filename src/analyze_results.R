library(lme4)
library(tidyverse)

#Read read in the filenames of all files from the naming task
filenames <- list.files("results/final/")

# Create tibbles to hold experimental data
task <- tibble()
guided_test_run <- tibble()
test_run <- tibble()
consent <- tibble()
background <- tibble()
audio_check <- tibble()

# Read in each experimental data file, force reaction time to be numeric,
# and add it to our task tibble
for (filename in filenames) {
  result <- read_csv(paste("results/final/", filename, sep=""), show_col_types = FALSE) %>%
    filter(`Event Index` != "END OF FILE")
  if (str_detect(filename, 'p3ei')) {
    consent <- rbind(consent, result)
  } else if (str_detect(filename, 'erm5')) {
    # Did not consent, discard
  } else if (str_detect(filename, 'faqc')) {
    background <- rbind(background, result)
  } else if (str_detect(filename, 'gbnv')) {
    audio_check <- rbind(audio_check, result)
  } else if (str_detect(filename, 'znzu')) {
    guided_test_run <- rbind(guided_test_run, result)
  } else if (str_detect(filename, 'tl3i')) {
    test_run <- rbind(test_run, result)
  } else if (str_detect(filename, 'yepb')) {
    # Final screen file, don't need
  } else{
    task <- rbind(task, result) 
  }
}

# Clean up column names
task <- task %>% 
  rename(ID = `Participant Private ID`,
         timestamp = `UTC Timestamp`,
         zone = `Zone Type`,
         response = Response,
         RT = `Reaction Time`,
         trial = `Trial Number`,
         orthography = Orthography,
         filename = filename,
         word = word,
         bucket_q = bucket_q,
         uni_prob = uni_prob,
         bi_prob_smoothed = bi_prob_smoothed,
         group = group)

task_responses <- task %>%
  filter(zone == "response_slider_endValue") %>%
  mutate(response = as.numeric(response))

# task_responses %>%
#   select(ID, response, word) %>%
#   write_csv('data/spanish_responses_phonotactics_paper.csv')

task_responses %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

task_responses %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

model_s <- lmer(response ~ scale(uni_prob, scale=FALSE) * scale(bi_prob_smoothed, scale=FALSE) + (1|ID), data=task_responses)
