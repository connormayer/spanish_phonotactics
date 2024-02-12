# Script for generating experimental stimuli. To be tidied.
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
#setwd("E:/git_repos/spanish_phonotactics")

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
  ggplot(aes(x=pos_uni_score, y=pos_bi_score, color=bucket_q)) +
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

# df %>%
#   select(word, bucket_q, uni_prob, bi_prob_smoothed) %>%
#   arrange(bucket_q, -uni_prob, -bi_prob_smoothed) %>%
#   write_csv('data/stimuli_with_buckets_v4.csv')

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
  write_csv('data/stimuli_candidates_v4.csv')

# Final sanitation
bogus_words <- c(
  # Real words that crept in
  "k e r1 a1",
  "s1 a1 ɾ a",
  "s1 a1 d o",
  "s1 i1 t e",
  "t1 a1 k i",
  "m o r1 i1",
  "b1 a1 d o",
  "d1 o1 l o",
  "x e m1 i1",
  "f1 i1 f i",
  "f1 o1 f a",
  "f1 o1 s a",
  "f1 o1 ɾ o",
  "r1 o1 ɲ a",
  "s1 i1 s a",
  "p1 e1 ɲ a",
  "m1 a1 l i",
  "p e r1 u1",
  #"k e b1 a1",
  "n1 o1 s e",
  # Stress minimal pairs
  "k i u1 i1",
  "n a t͡ʃ1 o1",
  "s e n1 a1",
  "t a p1 a1",
  "t o ɾ1 o1",
  "x o t1 a1",
  "b a i1 a1",
  "b e i1 a1",
  "k a ɾ1 o1",
  "k a k1 a1",
  "i a n1 o1",
  "k u i1 a1",
  "m e t1 o1",
  "m i s1 a1",
  "p i p1 a1",
  "t a x1 o1",
  "t͡ʃ u t͡ʃ1 e1",
  "n a n1 a1"
)

# Real words that are already in the initial data set but not filtered out above
more_real_words <- c(
  "t1 a1 ɾ a",
  "u1 i1 k i",
  "g1 i1 s a",
  "r1 a1 k e",
  "i o d1 a1",
  "t1 a1 ɾ o",
  "s1 i1 t o",
  "l u s1 i1",
  "k e b1 a1",
  "m u d1 a1",
  "n o t1 a1",
  "b o r1 a1",
  "g i s1 o1",
  "p1 a1 x e",
  "p a i1 o1",
  "i e b1 o1",
  "m1 e1 s e",
  "l e b1 a1",
  "t1 a1 l e",
  "f a m1 a1",
  "b u ɾ1 o1"
)

new_tokens <- tibble(tokens)

# Remove bogus words that crept in
for (word_str in bogus_words) {
  word <- new_tokens %>% filter(word == word_str)
  new_tokens <- new_tokens %>% filter(word != word_str)
  point = c(word$uni_prob, word$bi_prob_smoothed)
  closest_idx <- nn2(
    df %>% select(uni_prob, bi_prob_smoothed), 
    as.data.frame(t(point)), k=100)$nn.idx
  found_point <- FALSE
  # Start from 2 because closest point will always
  # be the point itself
  closest_i <- 2
  while (!found_point) {
    closest <- df[closest_idx[closest_i],]
    if (nrow((anti_join(closest, new_tokens, by="word"))) == 0 | (closest$word %in% more_real_words)) {
      # Closest point is already in our sample or is 
      # real word that snuck through
      closest_i = closest_i + 1
    } 
    else {
      found_point <- TRUE
    }
  }
  print(str_glue("Replacing ", word$word, " with ", closest$word))
  new_tokens <- rbind(new_tokens, closest %>% select(word, bucket_q, uni_prob, bi_prob_smoothed))
  print("Done")
}

new_tokens %>%
  write_csv('data/stimuli_candidates_final_v4.csv')

# Find poik equivalent
poik_df <- df %>%
  select(
    word, uni_prob, bi_prob_smoothed, pos_uni_score_smoothed, 
    pos_bi_score_smoothed
  ) %>%
  mutate(
    uni_prob = cume_dist(uni_prob),
    bi_prob_smoothed = cume_dist(bi_prob_smoothed),
    pos_uni_score_smoothed = cume_dist(pos_uni_score_smoothed),
    pos_bi_score_smoothed = cume_dist(pos_bi_score_smoothed)
  ) %>%
  filter(word )

closest_idx_poik <- nn2(
  poik_df %>% 
    select(uni_prob, bi_prob_smoothed, pos_uni_score_smoothed, 
           pos_bi_score_smoothed), 
  as.data.frame(t(c(0.484, 0.676, 0.834, 0.296))), k=10)$nn.idx
