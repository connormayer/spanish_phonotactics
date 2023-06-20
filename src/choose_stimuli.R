library(tidyverse)
library(RANN)

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

df <- read_csv("data/wug_word_scores_stress.csv")

# Filter out words we don't want
df <- df %>% 
  filter(!word %in% c(
    # Real words that snuck in
    "b1 a1 ɾ e",
    "b a ɾ1 e1",
    "b1 i1 s i",
    "b i s1 i1",
    "d1 a1 l o",
    "d a l1 o1",
    "m i i1 a1",
    "m1 i1 i a",
    "r1 e1 g o",
    "r e g1 o1",
    "s1 i1 d o",
    "x1 a1 x a",
    "x a x1 a1",
    "b a i1 e1",
    "b1 a1 i e",
    "l1 i1 n o",
    "l i n1 o1",
    "m a ɲ1 a1",
    "m1 a1 ɲ a",
    "m e m1 o1",
    "m1 e1 m o",
    "n1 a1 s e",
    "n a s1 e1",
    "s1 e1 ɾ e",
    "s e ɾ1 e1",
    "s e r1 e1",
    "s1 e1 r e",
    "s e ɲ1 a1",
    "s1 e1 ɲ a",
    "s1 e1 t o",
    "s e t1 o1",
    "b1 i1 g o",
    "b i g1 o1",
    "l1 u1 g o",
    "l u g1 o1",
    "l o l1 i1",
    "l1 o1 l i",
    "s1 a1 r a",
    "s a r1 a1",
    # Words that are stress minimal pairs with real words
    "l a t1 a1",
    "l a g1 o1",
    "k o k1 o1",
    "l e x1 a1",
    "p a ɾ1 a1",
    "s a k1 a1",
    "s a n1 a1",
    "s o p1 a1",
    "s u b1 e1",
    "x u x1 e1",
    "b1 e1 b i",
    "f o f1 a1",
    "f o s1 a1",
    "k o r1 a1",
    "m o r1 o1",
    "n e n1 e1",
    "n o ɲ1 a1",
    "p a r1 a1",
    "r1 o1 ɲ o",
    "t i p1 o1",
    "tʃ u r1 o1",
    # Potentially offensive
    "p u t1 o1"
  ))

# fix tS transcription
df <- df %>%
  mutate(word = str_replace_all(word, 'tʃ', 't͡ʃ'))

# Get rid of ii/uu sequences, which sound strange in the
# synthesizer
df <- df %>%
  filter(!str_detect(word, "(i1? i)|(u1? u)"))

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

df %>%
  select(word, bucket_q, uni_prob, bi_prob_smoothed) %>%
  arrange(bucket_q, -uni_prob, -bi_prob_smoothed) %>%
  write_csv('data/stimuli_with_buckets.csv')

df %>%
  ggplot(aes(x=bi_prob)) +
  geom_histogram() +
  facet_wrap(~ bucket_q, scale='free')

tokens <- data.frame()
for (bucket in unique(df$bucket_q)) {
  print(bucket)
  temp_df <- df %>% 
    filter(bucket_q == bucket) %>%
    select(word, bucket_q, uni_prob, bi_prob_smoothed)
  
  uni_range <- range(temp_df$uni_prob)
  bi_range <- range(temp_df$bi_prob_smoothed)
  
  uni_diff <- uni_range[2] - uni_range[1]
  bi_diff <- bi_range[2] - bi_range[1]
  
  uni_range_start <- uni_range[1] + uni_diff / 10
  uni_range_end <- uni_range[2] - uni_diff / 10
  
  bi_range_start <- bi_range[1] + bi_diff / 10
  bi_range_end <- bi_range[2] - bi_diff / 10
  
  uni_seq <- seq(
    uni_range_start,
    uni_range_end, 
    by=(uni_range_end - uni_range_start) / 5
  )
  bi_seq <- seq(
    bi_range_start, 
    bi_range_end, 
    by=(bi_range_end - bi_range_start) / 5
  )
  for (i in uni_seq) {
    for (j in bi_seq) {
      point = c(i, j)
      closest_idx <- nn2(
        temp_df %>% select(uni_prob, bi_prob_smoothed), 
        as.data.frame(t(point)), k=100)$nn.idx
      found_point <- FALSE
      closest_i <- 1
      while (!found_point) {
        closest <- temp_df[closest_idx[closest_i],]
        if (nrow(tokens) > 0 && 
            nrow(anti_join(closest, tokens, by="word")) == 0) {
          # Closest point is already in our sample
          closest_i = closest_i + 1
        } 
        else {
          found_point <- TRUE
        }
      }
      tokens <- rbind(tokens, closest)
    }
  }
}

tokens %>%
  ggplot(aes(x=uni_prob, y=bi_prob_smoothed, color=bucket_q)) +
  geom_point() +
  scale_color_manual(values=c25)

tokens %>%
  write_csv('data/stimuli_candidates_v3.csv')
