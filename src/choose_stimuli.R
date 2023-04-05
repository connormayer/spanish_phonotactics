library(tidyverse)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
               "green4",
               "#6A3D9A", # purple
               "#FF7F00", # orange
               "black", "gold1",
               "skyblue2", "#FB9A99", # lt pink
               "palegreen2",
               "#CAB2D6", # lt purple
               "#FDBF6F", # lt orange
               "gray70", "khaki2",
               "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
               "darkturquoise", "green1", "yellow4", "yellow3",
               "darkorange4", "brown"
)

setwd("C:/Users/conno/git_repos/spanish_phonotactics")

df <- read_csv("data/test_scores/test_scores_stress_high_freq.csv")

df %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed)) +
  geom_point()

df %>%
  ggplot(aes(x=pos_uni_score_freq_weighted, y=pos_bi_score_freq_weighted)) +
  geom_point()

uni_quants = quantile(df$uni_prob)
bi_quants = quantile(df$bi_prob_smoothed)
uni_range = seq(range(df$uni_prob)[[1]], range(df$uni_prob)[[2]], length.out=5)
bi_range = seq(range(df$bi_prob_smoothed)[[1]], range(df$bi_prob_smoothed)[[2]], length.out=5)

df <- df %>%
  mutate(uni_bucket_q=ifelse(uni_prob < uni_quants[[2]], "uni1",
                      ifelse(uni_prob < uni_quants[[3]], "uni2",
                             ifelse(uni_prob < uni_quants[[4]], "uni3", "uni4"))),
         bi_bucket_q=ifelse(bi_prob_smoothed < bi_quants[[2]], "bi1",
                          ifelse(bi_prob_smoothed < bi_quants[[3]], "bi2",
                                 ifelse(bi_prob_smoothed < bi_quants[[4]], "bi3", "bi4"))),
         bucket_q=str_c(uni_bucket_q, bi_bucket_q, sep='-'),
         uni_bucket_r=ifelse(uni_prob < uni_range[[2]], "uni1",
                             ifelse(uni_prob < uni_range[[3]], "uni2",
                                    ifelse(uni_prob < uni_range[[4]], "uni3", "uni4"))),
         bi_bucket_r=ifelse(bi_prob_smoothed < bi_range[[2]], "bi1",
                            ifelse(bi_prob_smoothed < bi_range[[3]], "bi2",
                                   ifelse(bi_prob_smoothed < bi_range[[4]], "bi3", "bi4"))),
         bucket_r=str_c(uni_bucket_r, bi_bucket_r, sep='-'),
  )

df %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, color=bucket_q)) +
  geom_point() +
  scale_color_manual(values=c25)

df %>%
  ggplot(aes(x=pos_uni_score_freq_weighted, y=pos_bi_score_freq_weighted, color=bucket_q)) +
  geom_point(alpha=0.25) +
  scale_color_manual(values=c25)

df %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, color=bucket_r)) +
  geom_point() +
  scale_color_manual(values=c25)

df %>%
  ggplot(aes(x=pos_uni_score_freq_weighted, y=pos_bi_score_freq_weighted, color=bucket_r)) +
  geom_point() +
  scale_color_manual(values=c25)

df %>%
  group_by(bucket_r) %>%
  count()

df %>%
  group_by(bucket_q) %>%
  count()

# P(x1)

# P(x,y)
# P(y|x)
#
# P(y|x) = P(x,y) / P(x)
#
# P(y|x)P(x) = P(x,y)
