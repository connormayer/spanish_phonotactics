library(lme4)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(ggrepel)
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

task_responses <- task %>%
  filter(zone == "response_slider_endValue") %>%
  mutate(response = as.numeric(response),
         word = str_replace_all(word, 't͡ʃ', 'tʃ'),
         no_stress_word = str_replace_all(word, '\\d', ''),
         vowel_stress_word = str_replace_all(word, '([^aeiou])\\d', '\\1')) %>%
  mutate(word = str_replace_all(word, '^i', 'ʝ'),
         word = str_replace_all(word, '^u', 'w'),
         word = str_replace_all(word, "^([^ ]+ [^ ]+ )u(1? [aieou])", "\\1w\\2"),
         word = str_replace_all(word, "^([^ ]+ [^ ]+ )i(1? [aieou])", "\\1j\\2"),
         word = str_replace_all(word, 'ʝ', 'j'),
         word = str_replace_all(word, 'ɾ', 'r'),
         word = str_replace_all(word, "([^aeiou])1", "\\1"),
         final_stress = str_detect(word, "1$"),
         no_stress_word = str_replace_all(word, '\\d', ''))

# task_responses %>%
#   select(ID, response, word) %>%
#   write_csv('data/spanish_responses_phonotactics_paper.csv')

# # Remove the non-native speakers who snuck by. This is based on the questionnaire
# # but the filtering is implemented manually here.
# background %>%
#   select(`Participant Private ID`, `response-native`) %>%
#   filter(!str_detect(`response-native`, '([sS][Iií])|(espanol|Espanol|Español|español)'))
# 
# background %>%
#   select(`Participant Private ID`, `response-aoa`) %>%
#   arrange(-`response-aoa`)

task_responses <- task_responses %>%
  # People who said Spanish was not their native language
  filter(!(ID %in% c(9642250, 9602917, 9544827, 9524953, 9751912, 976608, 9864223, 9989475)))


vowel_stress_scores <- read_csv('phonotactic_scores/testing_data_vowel_stress_scores.csv') %>%
  select(word, uni_prob_smoothed, bi_prob_smoothed, pos_uni_score, pos_bi_score) %>%
  mutate(vowel_stress_uni_prob_smoothed = uni_prob_smoothed,
         vowel_stress_bi_prob_smoothed = bi_prob_smoothed,
         vowel_stress_pos_uni_score = pos_uni_score,
         vowel_stress_pos_bi_score = pos_bi_score) %>%
  distinct()

# no_stress_scores <- read_csv('phonotactic_scores/testing_data_no_stress_scores.csv') %>%
#   select(word, uni_prob_smoothed, bi_prob_smoothed, pos_uni_score, pos_bi_score) %>%
#   mutate(no_stress_word = word,
#          no_stress_uni_prob_smoothed = uni_prob_smoothed,
#          no_stress_bi_prob_smoothed = bi_prob_smoothed,
#          no_stress_pos_uni_score = pos_uni_score,
#          no_stress_pos_bi_score = pos_bi_score) %>%
#   distinct()

scored_task_responses <- task_responses %>%
  select(ID, timestamp, zone, response, RT, trial, orthography, filename,
         word, no_stress_word, vowel_stress_word, bucket_q, group) %>%
  inner_join(vowel_stress_scores, by=join_by(word == word)) #%>%
  #inner_join(no_stress_scores, by=join_by(no_stress_word == no_stress_word))

scored_task_responses %>%
  group_by(word) %>%
  summarize(response = mean(response),
            uni_prob = mean(vowel_stress_uni_prob_smoothed),
            bi_prob_smoothed = mean(vowel_stress_bi_prob_smoothed),
            pos_uni_prob = mean(vowel_stress_pos_uni_score),
            pos_bi_prob_smoothed = mean(vowel_stress_pos_bi_score)) %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
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
  mutate(vowel_stress_uni_prob_smoothed = scale(vowel_stress_uni_prob_smoothed)[,1],
         vowel_stress_bi_prob_smoothed = scale(vowel_stress_bi_prob_smoothed)[,1],
         vowel_stress_pos_uni_score = scale(vowel_stress_pos_uni_score)[,1],
         vowel_stress_pos_bi_score = scale(vowel_stress_pos_bi_score,)[,1]
         # no_stress_uni_prob_smoothed = scale(no_stress_uni_prob_smoothed)[,1],
         # no_stress_bi_prob_smoothed = scale(no_stress_bi_prob_smoothed)[,1],
  )

model_vowel_stress_nonpos <- lmer(
  response ~ vowel_stress_uni_prob_smoothed * vowel_stress_bi_prob_smoothed +
    (1 + vowel_stress_bi_prob_smoothed|ID) + 
    (1|word), data=model_data)
summary(model_vowel_stress_nonpos)

model_vowel_stress_nonpos_no_int <- lmer(
  response ~ vowel_stress_uni_prob_smoothed + vowel_stress_bi_prob_smoothed +
    (1 + vowel_stress_bi_prob_smoothed|ID) + 
    (1|word), data=model_data)
summary(model_vowel_stress_nonpos_no_int)

anova(model_vowel_stress_nonpos, model_vowel_stress_nonpos_no_int)

summary(model_vowel_stress_nonpos)

# WTF is up with the negative unigram coefficient?

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



# Try to figure out weird unigram issues
plot_data <- model_data %>%
  group_by(ID) %>%
  mutate(z_response = scale(response)) %>%
  group_by(word) %>%
  summarize(response = mean(z_response),
            uni_prob = mean(vowel_stress_uni_prob_smoothed),
            bi_prob_smoothed = mean(vowel_stress_bi_prob_smoothed),
            pos_uni_prob = mean(vowel_stress_pos_uni_score),
            pos_bi_prob = mean(vowel_stress_pos_bi_score))

m_resp_no_bi <- lm(response ~ bi_prob_smoothed, data=plot_data)
m_uni_no_bi <- lm(uni_prob ~ bi_prob_smoothed, data=plot_data)

plot_data$response_no_bi <- resid(m_resp_no_bi)
plot_data$uni_no_bi <- resid(m_uni_no_bi)

plot_data %>%
  ggplot(aes(x=uni_no_bi, y=response_no_bi)) +
  geom_point() +
  geom_text_repel(aes(label=word)) +
  geom_smooth(method='lm')

uni_sp <- read_csv('data/unigram_investigation/unigram_probs_sp.csv') %>%
  mutate(vowel = str_detect(segment, '[aeiou]'))
uni_en <- read_csv('data/unigram_investigation/unigram_probs_en.csv') %>%
  mutate(vowel = str_detect(segment, "(OY|UH|AW|UW|AO|OW|AY|EY|AA|AE|ER|EH|IY|IH|AH)"))

uni_sp %>%
  ggplot(aes(x=reorder(segment, probability), y=probability, fill=vowel)) +
  geom_bar(stat='identity')

uni_en %>%
  ggplot(aes(x=reorder(segment, probability), y=probability, fill=vowel)) +
  geom_bar(stat='identity')

model_no_stress_nonpos <- lmer(
  response ~ no_stress_uni_prob_smoothed * no_stress_bi_prob_smoothed +
    (1 + no_stress_bi_prob_smoothed|ID) + 
    (1|no_stress_word), data=model_data)
summary(model_no_stress_nonpos)

model_pos <- lmer(
  response ~ vowel_stress_pos_uni_score * vowel_stress_pos_bi_score +
    (1 + vowel_stress_pos_bi_score|ID) + 
    (1|word), data=model_data)
summary(model_pos)

anova(model_no_stress_nonpos, model_vowel_stress_nonpos, model_pos)


# Generate infant stimuli


m_resp_no_bi <- lm(response ~ bi_prob_smoothed, data=plot_data)
m_uni_no_bi <- lm(uni_prob ~ bi_prob_smoothed, data=plot_data)
plot_data$resp_no_bi <- resid(m_resp_no_bi)
plot_data$uni_no_bi <- resid(m_uni_no_bi)

# 
# model_no_bi_resd <- lm( m_resp_no_bi_resd ~ m_uni_no_bi_resd )
# summary(model_no_bi_resd)

plot_data %>%
  ggplot(aes(x=uni_no_bi, y=resp_no_bi)) +
  geom_point() +
  geom_text(aes(label = word))


# clusters <- foo %>%
#   select(m_resp_no_bi_resd, m_uni_no_bi_resd) %>%
#   kmeans(2)
# 
# cluster_data$uni_cluster <- as_factor(clusters$cluster)
# 
# cluster_data %>%
#   ggplot(aes(x=uni_prob, y=bi_prob_smoothed, group=uni_cluster, color=uni_cluster)) +
#   geom_point(aes(color=uni_cluster), size = 3)


m_resp_no_uni <- lm(response ~ uni_prob, data=plot_data)
m_bi_no_uni <- lm(bi_prob_smoothed ~ uni_prob, data=plot_data)
plot_data$resp_no_uni <- resid(m_resp_no_uni)
plot_data$bi_no_uni <- resid(m_bi_no_uni)

# m_resp_no_uni_resd <- resid(m_resp_no_uni)
# m_bi_no_uni_resd <- resid(m_bi_no_uni)
# 
# bar <- tibble(resp_no_uni=m_resp_no_uni_resd, bi_no_uni=m_bi_no_uni_resd)
# bar %>%
#   ggplot(aes(x=resp_no_uni, y=bi_no_uni)) +
#   geom_point()

# cluster_data <- cluster_data %>%
#   cbind(bar)
# 
# clusters <- bar %>%
#   select(m_resp_no_uni_resd, m_bi_no_uni_resd) %>%
#   kmeans(2)
# 
# cluster_data$bi_cluster <- as_factor(clusters$cluster)
# 
# cluster_data %>%
#   ggplot(aes(x=uni_prob, y=bi_prob_smoothed, group=bi_cluster)) +
#   geom_point(size=3, aes(color=bi_cluster))
# 
# 
# 
# clusters <- cluster_data %>%
#   select(uni_prob, bi_prob_smoothed, response) %>%
#   kmeans(2)
# 
# cluster_data$both_cluster <- as_factor(clusters$cluster)
# 
# cluster_data %>%
#   ggplot(aes(x=uni_prob, y=bi_prob_smoothed, group=both_cluster)) +
#   geom_point(size=3, aes(color=both_cluster))
# clusters <- cluster_data %>%
#   select(response, uni_prob) %>%
#   kmeans(2)
# 
# cluster_data$cluster <- clusters$cluster
# 
# cluster_data %>%
#   group_by(resid_cluster) %>% 
#   summarize(mean(uni_prob),
#             mean(bi_prob_smoothed))
# 
# cluster_data %>%
#   ggplot(aes(x=uni_prob, y=response, group=cluster, color=cluster)) +
#   geom_point() +
#   geom_smooth(method='lm')
# 
# anticlustering(cluster_data$bi_prob_smoothed, 2, categories=cluster_data$cluster)

# cluster_data %>%
#   group_by(bi_cluster) %>%
#   summarize(mean(uni_prob), mean(bi_prob_smoothed), mean(response), n())
# 
# 
# cluster_data %>%
#   ggplot(aes(x=bi_prob_smoothed, y=response, group=bi_cluster, color=bi_cluster)) +
#   geom_point(aes(color=bi_cluster), size = 3)

no_uni_pca <- plot_data %>%
  select(resp_no_uni, bi_no_uni) %>%
  prcomp()

plot_data$no_uni_pca <- no_uni_pca$x[,1]

no_bi_pca <- plot_data %>%
  select(resp_no_bi, uni_no_bi) %>%
  prcomp()

plot_data$no_bi_pca <- no_bi_pca$x[,1]

full_pca <- plot_data %>%
  select(response, uni_prob, bi_prob_smoothed) %>%
  prcomp()
summary(full_pca)

plot_data$full_pca <- full_pca$x[,1]

low_uni_contrast <- plot_data %>%
  slice_max(order_by=-no_bi_pca, n=66) %>%
  mutate(exp="uni_contrast",
         condition="low")

high_uni_contrast <- plot_data %>%
  slice_max(order_by=no_bi_pca, n=66) %>%
  mutate(exp="uni_contrast",
         condition="high")

uni_contrast = high_uni_contrast %>% 
  rbind(low_uni_contrast)

high_bi_contrast <- plot_data %>%
  slice_max(order_by=-no_uni_pca, n=66) %>%
  mutate(exp="bi_contrast",
         condition="high")

low_bi_contrast <- plot_data %>%
  slice_max(order_by=no_uni_pca, n=66) %>%
  mutate(exp="bi_contrast",
         condition="low")

bi_contrast = high_bi_contrast %>% 
  rbind(low_bi_contrast)

low_both_contrast <- plot_data %>%
  slice_max(order_by=full_pca, n=66) %>%
  mutate(exp="full_contrast",
         condition="low")

high_both_contrast <- plot_data %>%
  slice_max(order_by=-full_pca, n=66) %>%
  mutate(exp="full_contrast",
         condition="high")

both_contrast <- high_both_contrast %>% 
  rbind(low_both_contrast)

both_contrast %>%
  inner_join(uni_contrast, by="word") %>%
  nrow()

both_contrast %>%
  inner_join(bi_contrast, by="word") %>%
  nrow()

uni_contrast %>%
  inner_join(bi_contrast, by="word") %>%
  nrow()

infant_data <- both_contrast %>%
  rbind(bi_contrast) %>%
  rbind(uni_contrast) %>%
  mutate(condition=as_factor(condition),
         exp=as_factor(exp ),
         word = str_replace(word, "(\\w+ \\w+)1", "ˈ\\1"),
         word = str_replace_all(word, ' ', '')) %>%
  select(word, uni_prob, bi_prob_smoothed, response, exp, condition) %>%
  write_csv("data/infant_stimuli.csv")

all_both <- both_contrast %>%
  right_join(plot_data) %>%
  replace_na(list(condition="None",
                  exp="full_contrast"))

all_uni <- uni_contrast %>%
  right_join(plot_data) %>%
  replace_na(list(condition="None",
                  exp="uni_contrast"))

all_bi <- bi_contrast %>%
  right_join(plot_data) %>%
  replace_na(list(condition="None",
                  exp="bi_contrast"))

all_total <- all_both %>%
  rbind(all_uni) %>%
  rbind(all_bi)

all_total %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, color=condition)) +
  geom_point(size = 3) +
  facet_grid(~ exp)

all_total %>%
  arrange(condition) %>%
  ggplot(aes(x=pos_uni_prob, y=pos_bi_prob, color=condition)) +
  geom_point(size = 3) +
  facet_grid(~ exp)


# CHECK THAT INFANT STIMULI ARE DISCRIMINABLE BASED ON Z-SCORED PROBABILITIES

# If you train gold standard Spanish corpus using n-grams and positional models,
# can you distinguish these stimuli?

# Choose new English stimuli based on normal n-grams, send to Megha
# - do in exactly the same way as Spanish

full <- infant_data %>% 
  filter(exp=='full_contrast') %>%
  mutate(condition = as_factor(condition))

bi <- infant_data %>% 
  filter(exp=='bi_contrast') %>%
  mutate(condition = as_factor(condition))

uni <- infant_data %>% 
  filter(exp=='uni_contrast') %>%
  mutate(condition = as_factor(condition))


infant_m_full <- glm(condition ~ response, family='binomial', data = full)
summary(infant_m_full)

full <- full %>%
  mutate(model_probs = predict(infant_m_full, full, type="response"),
         model_pred = (model_probs > 0.5),
         accurate = 1 * ((model_pred == 0 & full$condition == 'high') | (model_pred == 1 & condition == 'low')))

sum(full$accurate / nrow(full))

infant_m_bi <- glm(condition ~ response, family='binomial', data = bi)
summary(infant_m_bi)

bi <- bi %>%
  mutate(model_probs = predict(infant_m_bi, bi, type="response"),
         model_pred = (model_probs > 0.5),
         accurate = 1 * ((model_pred == 0 & full$condition == 'high') | (model_pred == 1 & condition == 'low')))

sum(bi$accurate / nrow(bi))

infant_m_uni <- glm(condition ~ response, family='binomial', data = uni)
summary(infant_m_uni)

uni <- uni %>%
  mutate(model_probs = predict(infant_m_uni, uni, type="response"),
         model_pred = (model_probs > 0.5),
         accurate = 1 * ((model_pred == 0 & full$condition == 'high') | (model_pred == 1 & condition == 'low')))

sum(uni$accurate / nrow(uni))

# Example for a binary classification problem (p = 0.5) with 100 observations
n <- 132
p <- 0.5 # Probability of correct guess for a random binary classifier
x <- round(n * p) # Expected number of correct guesses

# Calculate the confidence interval
ci <- prop.test(x, n, p = p, conf.level = 0.95)

# Print the confidence interval
print(ci$conf.int)

infant_data %>%
  ggplot() + 
  geom_boxplot(aes(y=response, x=condition)) +
  facet_wrap(~exp)

infant_data %>%
  ggplot() + 
  geom_boxplot(aes(x=condition, y=bi_prob_smoothed)) +
  facet_wrap(~exp)

big_test_data <- infant_data %>%
  inner_join(model_data, by="word")

big_full <- big_test_data %>% 
  filter(exp=='full_contrast') %>%
  mutate(condition = as_factor(condition))

big_bi <- big_test_data %>% 
  filter(exp=='bi_contrast') %>%
  mutate(condition = as_factor(condition))

big_uni <- big_test_data %>% 
  filter(exp=='uni_contrast') %>%
  mutate(condition = as_factor(condition))


big_m_full <- lmer(response.y  ~ condition + (1 * condition|ID) + (1|word), data = big_full)
summary(big_m_full)

big_m_bi <- lmer(response.y  ~ condition + (1 * condition|ID) + (1|word), data = big_bi)
summary(big_m_bi)

big_m_uni <- lmer(response.y  ~ condition + (1 * condition|ID) + (1|word), data = big_uni)
summary(big_m_uni)
