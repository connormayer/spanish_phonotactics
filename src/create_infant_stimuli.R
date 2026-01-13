library(lme4)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(anticlust)

# Read in experimental results
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

vowel_stress_scores <- read_csv('phonotactic_scores/testing_data_vowel_stress_scores.csv') %>%
  select(word, uni_prob_smoothed, bi_prob_smoothed, pos_uni_score_freq_weighted, pos_bi_score_freq_weighted) %>%
  mutate(vowel_stress_uni_prob_smoothed = uni_prob_smoothed,
         vowel_stress_bi_prob_smoothed = bi_prob_smoothed,
         vowel_stress_pos_uni_score = pos_uni_score_freq_weighted,
         vowel_stress_pos_bi_score = pos_bi_score_freq_weighted) %>%
  distinct()

task_responses <- task %>%
  filter(zone == "response_slider_endValue") %>%
  mutate(word = str_replace_all(word, '^u', 'w')) %>%
  mutate(word = str_replace_all(word, '^(\\w+\\d? \\w+\\d? )u', '\\1w')) %>%
  mutate(word = str_replace_all(word, '^i', 'j')) %>%
  mutate(word = str_replace_all(word, '^(\\w+\\d? \\w+\\d? )i', '\\1j')) %>%
  mutate(response = as.numeric(response),
         word = str_replace_all(word, 't͡ʃ', 'tʃ'),
         no_stress_word = str_replace_all(word, '\\d', ''),
         vowel_stress_word = str_replace_all(word, '([^aeiou])\\d', '\\1')) %>%
  # People who said Spanish was not their native language
  filter(!(ID %in% c(9642250, 9602917, 9544827, 9524953, 9751912, 976608, 9864223, 9989475)))

task_responses %>%
  select(ID, timestamp, zone, response, RT, trial, orthography, filename,
         word, no_stress_word, vowel_stress_word, bucket_q, group) %>%
  anti_join(vowel_stress_scores, by=join_by(vowel_stress_word == word))


# Join task responses and phonotactic scores
scored_task_responses <- task_responses %>%
  select(ID, timestamp, zone, response, RT, trial, orthography, filename,
         word, no_stress_word, vowel_stress_word, bucket_q, group) %>%
  inner_join(vowel_stress_scores, by=join_by(vowel_stress_word == word))

# Scale responses within participants
model_data <- scored_task_responses %>%
  mutate(vowel_stress_uni_prob_smoothed = scale(vowel_stress_uni_prob_smoothed)[,1],
         vowel_stress_bi_prob_smoothed = scale(vowel_stress_bi_prob_smoothed)[,1],
         vowel_stress_pos_uni_score = scale(vowel_stress_pos_uni_score)[,1],
         vowel_stress_pos_bi_score = scale(vowel_stress_pos_bi_score,)[,1]
  ) %>%
  group_by(ID) %>%
  mutate(z_response = scale(response))

# Plot scaled uni/bigram scores against each other
model_data %>% 
  ggplot(aes(x=vowel_stress_uni_prob_smoothed, y=vowel_stress_bi_prob_smoothed)) +
  geom_point() +
  geom_smooth(method='lm')

# Plot scaled responses against scaled uni and bi probs
model_data %>%
  group_by(word) %>%
  summarize(bi_prob = mean(vowel_stress_bi_prob_smoothed),
            resp = mean(z_response)) %>%
  ggplot(aes(x=bi_prob, y=resp)) +
  geom_point() +
  geom_smooth(method='lm')


model_data %>%
  group_by(vowel_stress_word) %>%
  summarize(response = mean(z_response),
            uni_prob = mean(vowel_stress_uni_prob_smoothed),
            bi_prob_smoothed = mean(vowel_stress_bi_prob_smoothed)) %>%
  # mutate(response = log(response + 0.01)) %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

# Build data for clustering
cluster_data <- model_data %>%
  group_by(vowel_stress_word) %>%
  summarize(response = mean(z_response),
            uni_prob = mean(vowel_stress_uni_prob_smoothed),
            bi_prob_smoothed = mean(vowel_stress_bi_prob_smoothed),
            uni_pos = mean(vowel_stress_pos_uni_score),
            bi_pos = mean(vowel_stress_pos_bi_score))

# Regress out bigram scores from responses and unigram scores
m_resp_no_bi <- lm(response ~ bi_prob_smoothed, data=cluster_data)
m_uni_no_bi <- lm(uni_prob ~ bi_prob_smoothed, data=cluster_data)

m_resp_no_bi_resd <- resid(m_resp_no_bi)
m_uni_no_bi_resd <- resid(m_uni_no_bi)

# Predict responses from unigram scores with bigram regressed out
model_no_bi_resd <- lm(m_resp_no_bi_resd ~ m_uni_no_bi_resd )
summary(model_no_bi_resd)

# Add residuals to cluster df
no_bi <- tibble(resp_no_bi=m_resp_no_bi_resd, uni_no_bi=m_uni_no_bi_resd)
cluster_data <- cbind(cluster_data, no_bi)

cluster_data %>%
  ggplot(aes(x=uni_no_bi, y=resp_no_bi)) +
  geom_point() +
  geom_smooth(method='lm') + 
  geom_text(aes(label = vowel_stress_word))

clusters <- no_bi %>%
  select(resp_no_bi, uni_no_bi) %>%
  kmeans(2)

cluster_data$uni_cluster <- as_factor(clusters$cluster)

cluster_data %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, group=uni_cluster, color=uni_cluster)) +
  geom_point(aes(color=uni_cluster), size = 3)

cluster_data %>%
  ggplot(aes(x=uni_no_bi, y=resp_no_bi, color = uni_cluster)) +
  geom_point() +
  geom_smooth(method='lm') + 
  geom_text(aes(label = vowel_stress_word))

# Regress out unigram scores
m_resp_no_uni <- lm(response ~ uni_prob, data=cluster_data)
m_bi_no_uni <- lm(bi_prob_smoothed ~ uni_prob, data=cluster_data)

m_resp_no_uni_resd <- resid(m_resp_no_uni)
m_bi_no_uni_resd <- resid(m_bi_no_uni)

no_uni <- tibble(resp_no_uni=m_resp_no_uni_resd, bi_no_uni=m_bi_no_uni_resd)
no_uni %>%
  ggplot(aes(x=resp_no_uni, y=bi_no_uni)) +
  geom_point()


# Get bigram clusters
cluster_data <- cluster_data %>%
  cbind(no_uni)

clusters <- no_uni %>%
  select(resp_no_uni, bi_no_uni) %>%
  kmeans(2)

cluster_data$bi_cluster <- as_factor(clusters$cluster)

cluster_data %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, group=bi_cluster)) +
  geom_point(size=3, aes(color=bi_cluster))


clusters <- cluster_data %>%
  select(uni_prob, bi_prob_smoothed, response) %>%
  kmeans(2)

cluster_data$both_cluster <- as_factor(clusters$cluster)

cluster_data %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, group=both_cluster)) +
  geom_point(size=3, aes(color=both_cluster))

cluster_data %>%
  group_by(bi_cluster) %>%
  summarize(mean(uni_prob), mean(bi_prob_smoothed), mean(response), n())

cluster_data %>%
  group_by(uni_cluster) %>%
  summarize(mean(uni_prob), mean(bi_prob_smoothed), mean(response), n())


cluster_data %>%
  ggplot(aes(x=bi_prob_smoothed, y=response, group=bi_cluster, color=bi_cluster)) +
  geom_point(aes(color=bi_cluster), size = 3)

no_uni_pca <- cluster_data %>%
  select(resp_no_uni, bi_no_uni) %>%
  prcomp()

cluster_data$no_uni_pca <- no_uni_pca$x[,1]

no_bi_pca <- cluster_data %>%
  select(resp_no_bi, uni_no_bi) %>%
  prcomp()

cluster_data$no_bi_pca <- no_bi_pca$x[,1]
full_pca <- cluster_data %>%
  select(response, uni_prob, bi_prob_smoothed) %>%
  prcomp()
summary(full_pca)

cluster_data$full_pca <- full_pca$x[,1]

low_uni_contrast <- cluster_data %>%
  slice_max(order_by=no_bi_pca, n=66) %>%
  mutate(exp="uni_contrast",
         condition="high")

high_uni_contrast <- cluster_data %>%
  slice_max(order_by=-no_bi_pca, n=66) %>%
  mutate(exp="uni_contrast",
         condition="low")

uni_contrast = high_uni_contrast %>% 
  rbind(low_uni_contrast)

high_bi_contrast <- cluster_data %>%
  slice_max(order_by=-no_uni_pca, n=66) %>%
  mutate(exp="bi_contrast",
         condition="high")

low_bi_contrast <- cluster_data %>%
  slice_max(order_by=no_uni_pca, n=66) %>%
  mutate(exp="bi_contrast",
         condition="low")

bi_contrast = high_bi_contrast %>% 
  rbind(low_bi_contrast)

low_both_contrast <- cluster_data %>%
  slice_max(order_by=full_pca, n=66) %>%
  mutate(exp="full_contrast",
         condition="low")

high_both_contrast <- cluster_data %>%
  slice_max(order_by=-full_pca, n=66) %>%
  mutate(exp="full_contrast",
         condition="high")

both_contrast <- high_both_contrast %>% 
  rbind(low_both_contrast)

both_contrast %>%
  inner_join(uni_contrast, by="vowel_stress_word") %>%
  nrow()

both_contrast %>%
  inner_join(bi_contrast, by="vowel_stress_word") %>%
  nrow()

uni_contrast %>%
  inner_join(bi_contrast, by="vowel_stress_word") %>%
  nrow()

infant_data <- both_contrast %>%
  rbind(bi_contrast) %>%
  rbind(uni_contrast) %>%
  select(vowel_stress_word, uni_prob, bi_prob_smoothed, response, exp, condition) %>%
  mutate(vowel_stress_word = str_replace_all(vowel_stress_word, '(\\w+ \\w)1', 'ˈ\\1'),
         vowel_stress_word = str_replace_all(vowel_stress_word, ' ', '')) %>%
  write_csv("data/infant_stimuli.csv")

all_both <- both_contrast %>%
  right_join(cluster_data) %>%
  replace_na(list(condition="None",
                  exp="full_contrast"))

all_uni <- uni_contrast %>%
  right_join(cluster_data) %>%
  replace_na(list(condition="None",
                  exp="uni_contrast"))

all_bi <- bi_contrast %>%
  right_join(cluster_data) %>%
  replace_na(list(condition="None",
                  exp="bi_contrast"))

all_total <- all_both %>%
  rbind(all_uni) %>%
  rbind(all_bi)

all_total %>%
  filter(condition != "None") %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, color=condition)) +
  geom_point(size = 3) +
  facet_grid(~ exp)

all_total %>%
  filter(condition != "None") %>%
  ggplot(aes(x=uni_pos, y=bi_pos, color=condition)) +
  geom_point(size = 3) +
  facet_grid(~ exp)
