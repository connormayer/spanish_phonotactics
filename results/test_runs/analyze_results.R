library(lme4)
library(tidyverse)
library(lmerTest)
library(emmeans)

# Read in experimental results
setwd("C:/Users/conno/git_repos/spanish_phonotactics/results/real/")

#Read read in the filenames of all files from the naming task
filenames <- list.files(".")

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
  result <- read_csv(paste("./", filename, sep=""), show_col_types = FALSE) %>%
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
  mutate(response = as.numeric(response),
         word = str_replace_all(word, 't͡ʃ', 'tʃ'),
         no_stress_word = str_replace_all(word, '\\d', ''),
         vowel_stress_word = str_replace_all(word, '([^aeiou])\\d', '\\1'))

stress_scores <- read_csv('../../phonotactic_scores/testing_data_stress_scores.csv') %>%
  select(word, uni_prob_smoothed, bi_prob_smoothed, pos_uni_score, pos_bi_score) %>%
  mutate(stress_uni_prob_smoothed = uni_prob_smoothed,
         stress_bi_prob_smoothed = bi_prob_smoothed,
         stress_pos_uni_score = pos_uni_score,
         stress_pos_bi_score = pos_bi_score)

nonstress_scores <- read_csv('../../phonotactic_scores/testing_data_no_stress_scores.csv') %>%
  select(word, uni_prob_smoothed, bi_prob_smoothed, pos_uni_score, pos_bi_score) %>%
  mutate(nostress_uni_prob_smoothed = uni_prob_smoothed,
         nostress_bi_prob_smoothed = bi_prob_smoothed,
         nostress_pos_uni_score = pos_uni_score,
         nostress_pos_bi_score = pos_bi_score) %>%
  distinct()

vowel_stress_scores <- read_csv('../../phonotactic_scores/testing_data_vowel_stress_scores.csv') %>%
  select(word, uni_prob_smoothed, bi_prob_smoothed, pos_uni_score, pos_bi_score) %>%
  mutate(vowel_stress_uni_prob_smoothed = uni_prob_smoothed,
         vowel_stress_bi_prob_smoothed = bi_prob_smoothed,
         vowel_stress_pos_uni_score = pos_uni_score,
         vowel_stress_pos_bi_score = pos_bi_score) %>%
  distinct()

scored_task_responses <- task_responses %>%
  select(ID, timestamp, zone, response, RT, trial, orthography, filename,
         word, no_stress_word, vowel_stress_word, bucket_q, group) %>%
  inner_join(nonstress_scores, by=join_by(no_stress_word == word)) %>%
  inner_join(stress_scores, by=join_by(word == word)) %>%
  inner_join(vowel_stress_scores, by=join_by(vowel_stress_word == word)) %>%
  mutate(initial_stress = str_detect(word, '^\\w+\\d'))

# # Remove the non-native speakers who snuck by. This is based on the questionnaire
# # but the filtering is implemented manually here.
# background %>%
#   select(`Participant Private ID`, `response-native`) %>%
#   filter(!str_detect(`response-native`, '([sS][Iií])|(espanol|Espanol|Español|español)'))
# 
# background %>%
#   select(`Participant Private ID`, `response-aoa`) %>%
#   arrange(-`response-aoa`)

# task_responses %>%
#   # People who said Spanish was not their native language
#   filter(!(ID %in% c(9642250, 9602917, 9544827, 9524953, 9751912, 976608, 9864223, 9989475))

scored_task_responses %>%
  group_by(word) %>%
  summarize(response = mean(response),
            uni_prob = mean(vowel_stress_uni_prob_smoothed),
            bi_prob_smoothed = mean(vowel_stress_bi_prob_smoothed)) %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

scored_task_responses %>%
  group_by(word) %>%
  summarize(response = mean(response),
            uni_prob = mean(vowel_stress_uni_prob_smoothed),
            bi_prob_smoothed = mean(vowel_stress_bi_prob_smoothed)) %>%
  # mutate(response = log(response + 0.01)) %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

model_data <- scored_task_responses %>%
  mutate(stress_uni_prob_smoothed = scale(stress_uni_prob_smoothed)[,1],
         stress_bi_prob_smoothed = scale(stress_bi_prob_smoothed)[,1],
         stress_pos_uni_score = scale(stress_pos_uni_score)[,1],
         stress_pos_bi_score = scale(stress_pos_bi_score,)[,1],
         nostress_uni_prob_smoothed = scale(nostress_uni_prob_smoothed)[,1],
         nostress_bi_prob_smoothed = scale(nostress_bi_prob_smoothed)[,1],
         nostress_pos_uni_score = scale(nostress_pos_uni_score)[,1],
         nostress_pos_bi_score = scale(nostress_pos_bi_score,)[,1],
         vowel_stress_uni_prob_smoothed = scale(vowel_stress_uni_prob_smoothed)[,1],
         vowel_stress_bi_prob_smoothed = scale(vowel_stress_bi_prob_smoothed)[,1],
         vowel_stress_pos_uni_score = scale(vowel_stress_pos_uni_score)[,1],
         vowel_stress_pos_bi_score = scale(vowel_stress_pos_bi_score,)[,1]
  )

model_stress_nonpos <- lmer(
  response ~ stress_uni_prob_smoothed * stress_bi_prob_smoothed + 
    (1 |ID) + 
    (1|word), data=model_data)
summary(model_stress_nonpos)

model_stress_pos <- lmer(
  response ~ stress_pos_uni_score * stress_pos_bi_score + 
    (1|ID) + 
    (1|word), data=model_data)
summary(model_stress_pos)

model_nostress_nonpos <- lmer(
  response ~ nostress_uni_prob_smoothed * nostress_bi_prob_smoothed + 
    (1|ID) + 
    (1|word), data=model_data)
summary(model_nostress_nonpos)

model_nostress_pos <- lmer(
  response ~ nostress_pos_uni_score * nostress_pos_bi_score + 
    (1|ID) + 
    (1|word), data=model_data)
summary(model_nostress_pos)

# Interaction between stress position and scores doesn't improve model
# model_sep_stress_nonpos_int <- lmer(
#   response ~ nostress_uni_prob_smoothed * nostress_bi_prob_smoothed * initial_stress +
#     (1|ID) + 
#     (1|word), data=model_data)
# summary(model_sep_stress_nonpos_int)
# 
# model_sep_stress_pos_int <- lmer(
#   response ~ nostress_pos_uni_score * nostress_pos_bi_score * initial_stress +
#     (1|ID) + 
#     (1|word), data=model_data)
# summary(model_sep_stress_pos_int)

model_sep_stress_nonpos <- lmer(
  response ~ nostress_uni_prob_smoothed * nostress_bi_prob_smoothed + initial_stress +
    (1|ID) + 
    (1|word), data=model_data)
summary(model_sep_stress_nonpos)

model_sep_stress_pos <- lmer(
  response ~ nostress_pos_uni_score * nostress_pos_bi_score + initial_stress +
    (1|ID) + 
    (1|word), data=model_data)
summary(model_sep_stress_pos)

# VOWEL STRESS

model_vowel_stress_nonpos <- lmer(
  response ~ vowel_stress_uni_prob_smoothed * vowel_stress_bi_prob_smoothed +
    (1+ vowel_stress_bi_prob_smoothed|ID) + 
    (1|word), data=model_data)
summary(model_vowel_stress_nonpos)

model_vowel_stress_nonpos_no_int <- lmer(
  response ~ vowel_stress_uni_prob_smoothed + vowel_stress_bi_prob_smoothed +
    (1+ vowel_stress_bi_prob_smoothed|ID) + 
    (1|word), data=model_data)
summary(model_vowel_stress_nonpos_no_int)

model_vowel_stress_nonpos_uni <- lmer(
  response ~ vowel_stress_uni_prob_smoothed +
    (1|ID) + 
    (1|word), data=model_data)
summary(model_vowel_stress_nonpos_uni)

model_vowel_stress_pos <- lmer(
  response ~ vowel_stress_pos_uni_score * vowel_stress_pos_bi_score +
    (1|ID) + 
    (1|word), data=model_data)
summary(model_vowel_stress_pos)

anova(model_stress_nonpos, model_stress_pos, model_nostress_nonpos, 
      model_nostress_pos, model_sep_stress_nonpos, model_sep_stress_pos,
      model_vowel_stress_pos, model_vowel_stress_nonpos, model_vowel_stress_nonpos_no_int)

summary(model_vowel_stress_nonpos)

# WTF is up with the negative unigram coefficient?


# Figure out interaction for stress non-pos
uni_median <- median(model_data$stress_uni_prob_smoothed)
bi_median <- median(model_data$stress_bi_prob_smoothed)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(stress_uni_prob_smoothed >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(stress_bi_prob_smoothed >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ stress_uni_prob_smoothed * bi_prob_class + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "stress_uni_prob_smoothed")

model_uni <- lmer(response ~ uni_prob_class * stress_bi_prob_smoothed + (1|ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "stress_bi_prob_smoothed")

interaction_data %>%
  group_by(word, uni_prob_class) %>%
  summarize(response = mean(response), 
            bi_prob_smoothed=mean(bi_prob_smoothed)) %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~uni_prob_class)

interaction_data %>%
  group_by(word, bi_prob_class) %>%
  summarize(response = mean(response), 
            uni_prob=mean(uni_prob)) %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~bi_prob_class)

# Figure out interaction for stress pos
uni_median <- median(model_data$stress_pos_uni_score)
bi_median <- median(model_data$stress_pos_bi_score)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(stress_pos_uni_score >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(stress_pos_bi_score >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ stress_pos_uni_score * bi_prob_class + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "stress_pos_uni_score")

model_uni <- lmer(response ~ uni_prob_class * stress_pos_bi_score + (1|ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "stress_pos_bi_score")

interaction_data %>%
  group_by(word, uni_prob_class) %>%
  summarize(response = mean(response), 
            stress_pos_bi_score=mean(stress_pos_bi_score)) %>%
  ggplot(aes(x=stress_pos_bi_score, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~uni_prob_class)

interaction_data %>%
  group_by(word, bi_prob_class) %>%
  summarize(response = mean(response), 
            stress_pos_uni_score=mean(stress_pos_uni_score)) %>%
  ggplot(aes(x=stress_pos_uni_score, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~bi_prob_class)

# Figure out interaction for no stress non-pos
uni_median <- median(model_data$nostress_uni_prob_smoothed)
bi_median <- median(model_data$nostress_bi_prob_smoothed)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(nostress_uni_prob_smoothed >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(nostress_bi_prob_smoothed >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ nostress_uni_prob_smoothed * bi_prob_class + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "nostress_uni_prob_smoothed")

model_uni <- lmer(response ~ uni_prob_class * nostress_bi_prob_smoothed + (1 |ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "nostress_bi_prob_smoothed")

interaction_data %>%
  group_by(word, uni_prob_class) %>%
  summarize(response = mean(response), 
            bi_prob_smoothed=mean(bi_prob_smoothed)) %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~uni_prob_class)

interaction_data %>%
  group_by(word, bi_prob_class) %>%
  summarize(response = mean(response), 
            uni_prob=mean(uni_prob)) %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~bi_prob_class)

# Figure out interaction for no stress pos
uni_median <- median(model_data$nostress_pos_uni_score)
bi_median <- median(model_data$nostress_pos_bi_score)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(nostress_pos_uni_score >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(nostress_pos_bi_score >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ nostress_pos_uni_score * bi_prob_class + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "nostress_pos_uni_score")

model_uni <- lmer(response ~ uni_prob_class * nostress_pos_bi_score + (1|ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "nostress_pos_bi_score")

interaction_data %>%
  group_by(word, uni_prob_class) %>%
  summarize(response = mean(response), 
            bi_prob_smoothed=mean(bi_prob_smoothed)) %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~uni_prob_class)

interaction_data %>%
  group_by(word, bi_prob_class) %>%
  summarize(response = mean(response), 
            uni_prob=mean(uni_prob)) %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~bi_prob_class)

# Figure out interaction for sep stress pos
uni_median <- median(model_data$nostress_pos_uni_score)
bi_median <- median(model_data$nostress_pos_bi_score)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(nostress_pos_uni_score >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(nostress_pos_bi_score >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ nostress_pos_uni_score * bi_prob_class + initial_stress + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "nostress_pos_uni_score")

model_uni <- lmer(response ~ uni_prob_class * nostress_pos_bi_score + initial_stress + (1|ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "nostress_pos_bi_score")


# Figure out interaction for sep stress nonpos
uni_median <- median(model_data$nostress_uni_prob_smoothed)
bi_median <- median(model_data$nostress_bi_prob_smoothed)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(nostress_uni_prob_smoothed >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(nostress_bi_prob_smoothed >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ nostress_uni_prob_smoothed * bi_prob_class + initial_stress + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "nostress_uni_prob_smoothed")

model_uni <- lmer(response ~ uni_prob_class * nostress_bi_prob_smoothed + initial_stress + (1|ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "nostress_bi_prob_smoothed")

# Figure out interaction for vowel stress non-pos
uni_median <- median(model_data$vowel_stress_uni_prob_smoothed)
bi_median <- median(model_data$vowel_stress_bi_prob_smoothed)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(vowel_stress_uni_prob_smoothed >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(vowel_stress_bi_prob_smoothed >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ vowel_stress_uni_prob_smoothed * bi_prob_class + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "vowel_stress_uni_prob_smoothed")

model_uni <- lmer(response ~ uni_prob_class * vowel_stress_bi_prob_smoothed + (1|ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "vowel_stress_bi_prob_smoothed")

interaction_data %>%
  group_by(word, uni_prob_class) %>%
  summarize(response = mean(response), 
            bi_prob_smoothed=mean(vowel_stress_bi_prob_smoothed)) %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~uni_prob_class)

interaction_data %>%
  group_by(word, bi_prob_class) %>%
  summarize(response = mean(response), 
            uni_prob=mean(vowel_stress_uni_prob_smoothed)) %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~bi_prob_class)

# Figure out interaction for vowel stress pos
uni_median <- median(model_data$vowel_stress_pos_uni_score)
bi_median <- median(model_data$vowel_stress_pos_bi_score)
interaction_data <- model_data %>%
  mutate(uni_prob_class = ifelse(vowel_stress_pos_uni_score >= uni_median, 'high', 'low'),
         bi_prob_class = ifelse(vowel_stress_pos_bi_score >= bi_median, 'high', 'low'))

model_bi <- lmer(response ~ vowel_stress_pos_uni_score * bi_prob_class + (1|ID) + (1|word), data=interaction_data)
emtrends(model_bi, "bi_prob_class", "vowel_stress_pos_uni_score")

model_uni <- lmer(response ~ uni_prob_class * vowel_stress_pos_bi_score + (1|ID) + (1|word), data=interaction_data)
emtrends(model_uni, "uni_prob_class", "vowel_stress_pos_bi_score")

interaction_data %>%
  group_by(word, uni_prob_class) %>%
  summarize(response = mean(response), 
            stress_pos_bi_score=mean(vowel_stress_pos_bi_score)) %>%
  ggplot(aes(x=stress_pos_bi_score, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~uni_prob_class)

interaction_data %>%
  group_by(word, bi_prob_class) %>%
  summarize(response = mean(response), 
            stress_pos_uni_score=mean(vowel_stress_pos_uni_score)) %>%
  ggplot(aes(x=stress_pos_uni_score, y=response)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~bi_prob_class)
