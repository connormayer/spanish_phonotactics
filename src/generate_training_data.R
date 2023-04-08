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

data <- read_csv('E:/git_repos/spanish_phonotactics/data/syllabified_spanish.csv')

# Keep only high frequency items (> 1 per millon words). Do this in advance to 
# avoid dealing with weird loanwords
data <- data %>% 
  filter(frq >= 1) %>%
  filter(!(word %in% c("nietzsche", "software")))

# Remove odd g character and extra column
data <- data %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 'ɡ', 'g'),
         stressed_ipa = str_replace_all(stressed_ipa, 'ɡ', 'g'),
         syllabified_ipa = str_replace_all(syllabified_ipa, 'ɡ', 'g')) %>%
  select(-X)

data <- data %>%
  # Convert transcription from EsPal to IPA
  mutate(transcription = str_replace_all(sa_phon_structure, 'B', 'b')) %>%
  mutate(transcription = str_replace_all(transcription, 'D', 'd')) %>%
  mutate(transcription = str_replace_all(transcription, 'r', 'ɾ')) %>%
  mutate(transcription = str_replace_all(transcription, 'R', 'r')) %>%
  mutate(transcription = str_replace_all(transcription, 'G', 'g')) %>%
  mutate(transcription = str_replace_all(transcription, 'C', 'tʃ')) %>%
  mutate(transcription = str_replace_all(transcription, 'J', 'ɲ')) %>%
  mutate(transcription = str_replace_all(transcription, 'N', 'ŋ')) %>%
  # Collapse palatal lateral and fricative
  mutate(transcription = str_replace_all(transcription, 'L', 'ʝ')) %>%
  mutate(transcription = str_replace_all(transcription, 'H', 'ʝ')) %>%
  mutate(transcription = str_replace_all(transcription, 'j', 'ʝ')) %>%
  
  # Remove a few annoying characters from epitrans transcriptions
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 't͡ʃ', 'tʃ')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 't͡ɬ', 'tl')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '([^t]|^)ʃ', '\\1s')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 'ü', 'u')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 'j', 'ʝ')) %>%
  # Fix trill/tap allophony
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '^ɾ', 'r')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '([lnsz])ɾ', '\\1r')) %>%
  # Nasal place assimilation
  # mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 'n([mfb])', 'm\\1')) %>%
  # mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 'n([gxk])', 'ŋ\\1')) %>%
  # # Obstruent voicing assimilation
  # mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 's([mln])', 'z\\1')) %>%
  # mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 't([mln])', 'd\\1')) %>%
  # Glide mistranscription
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '([^aeiou])w([^aeiou])', '\\1u\\2')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '([^aeiou])ʝ([^aeiou])', '\\1i\\2')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 'ɾɾ', 'r')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '^ʝ([^aeoiu])', 'i\\1')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '([^aeoiu])ʝ$', '\\1i')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '^w([^aeoiu])', 'u\\1')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, '([^aeoiu])w$', '\\1u')) %>%
  mutate(no_spaces_ipa = str_replace_all(no_spaces_ipa, 'll', 'ʝ'))

data <- data %>% 
  filter(!(word %in% c('etc')),)

data %>%
  select(transcription, no_spaces_ipa) %>%
  filter(transcription != no_spaces_ipa 
         # Voicing assimilation discrepancies
         & !(str_detect(no_spaces_ipa, 'sm') & str_detect(transcription, 'zm'))
         & !(str_detect(no_spaces_ipa, 'sl') & str_detect(transcription, 'zl'))
         & !(str_detect(no_spaces_ipa, 'kn') & str_detect(transcription, 'gn'))
         & !(str_detect(no_spaces_ipa, 'kd') & str_detect(transcription, 'gd'))
         & !(str_detect(no_spaces_ipa, 'tm') & str_detect(transcription, 'dm'))
         & !(str_detect(no_spaces_ipa, 'sn') & str_detect(transcription, 'zn'))
         & !(str_detect(no_spaces_ipa, 'sb') & str_detect(transcription, 'zb'))
         & !(str_detect(no_spaces_ipa, 'tb') & str_detect(transcription, 'db'))
         & !(str_detect(no_spaces_ipa, 'sd') & str_detect(transcription, 'zd'))
         & !(str_detect(no_spaces_ipa, 'sg') & str_detect(transcription, 'zg'))
         & !(str_detect(no_spaces_ipa, 'sr') & str_detect(transcription, 'zr'))
         & !(str_detect(no_spaces_ipa, 'tn') & str_detect(transcription, 'dn'))
         
         # Miscellaneous
         & !(str_detect(no_spaces_ipa, 'ks') & str_detect(transcription, 'gs'))
         & !(str_detect(no_spaces_ipa, 't$') & str_detect(transcription, 'd$'))
         & !(str_detect(no_spaces_ipa, 'ks') & str_detect(transcription, 's'))
         & !(str_detect(no_spaces_ipa, 'ts') & str_detect(transcription, 'ds'))
         & !(str_detect(no_spaces_ipa, 'tt') & str_detect(transcription, 'dt'))
         & !(str_detect(no_spaces_ipa, '[aeiou]w[aeiou]') & str_detect(transcription, '[aeiou]b[aeiou]'))
         & !(str_detect(no_spaces_ipa, '^w') & str_detect(transcription, '^b'))
         & !(str_detect(no_spaces_ipa, '^w') & str_detect(transcription, '^gw'))
         & !(str_detect(no_spaces_ipa, 'dw') & str_detect(transcription, 'db'))
         & !(str_detect(no_spaces_ipa, 'nt') & str_detect(transcription, 'nd'))
         & !(str_detect(no_spaces_ipa, 'oti') & str_detect(transcription, 'odi'))
         
         # Place assimilation
         & !(str_detect(no_spaces_ipa, 'nm') & str_detect(transcription, 'mm'))
         & !(str_detect(no_spaces_ipa, 'nk') & str_detect(transcription, 'ŋk'))
         & !(str_detect(no_spaces_ipa, 'nf') & str_detect(transcription, 'mf'))
         & !(str_detect(no_spaces_ipa, 'nx') & str_detect(transcription, 'ŋx'))
         & !(str_detect(no_spaces_ipa, 'ng') & str_detect(transcription, 'ŋg'))
         & !(str_detect(no_spaces_ipa, 'nb') & str_detect(transcription, 'mb'))
         
         # glide-vowel differences
         # Off-glides transcribed as vowels 
         & !(str_detect(no_spaces_ipa, 'si') & str_detect(transcription, 'sʝ'))
         & !(str_detect(no_spaces_ipa, 'tu') & str_detect(transcription, 'tw'))
         & !(str_detect(no_spaces_ipa, 'gi') & str_detect(transcription, 'gʝ'))
         & !(str_detect(no_spaces_ipa, 'xi') & str_detect(transcription, 'xʝ'))
         & !(str_detect(no_spaces_ipa, 'li') & str_detect(transcription, 'lʝ'))
         & !(str_detect(no_spaces_ipa, 'ni') & str_detect(transcription, 'nʝ'))
         & !(str_detect(no_spaces_ipa, 'ɾi') & str_detect(transcription, 'ɾʝ'))
         & !(str_detect(no_spaces_ipa, 'ri') & str_detect(transcription, 'rʝ'))
         & !(str_detect(no_spaces_ipa, 'pi') & str_detect(transcription, 'pʝ'))
         & !(str_detect(no_spaces_ipa, 'fi') & str_detect(transcription, 'fʝ'))
         & !(str_detect(no_spaces_ipa, 'ki') & str_detect(transcription, 'kʝ'))
         & !(str_detect(no_spaces_ipa, 'di') & str_detect(transcription, 'dʝ'))
         & !(str_detect(no_spaces_ipa, 'ɾu') & str_detect(transcription, 'ɾw'))
         & !(str_detect(no_spaces_ipa, 'bu') & str_detect(transcription, 'bw'))
         & !(str_detect(no_spaces_ipa, 'ku') & str_detect(transcription, 'kw'))
         & !(str_detect(no_spaces_ipa, 'nu') & str_detect(transcription, 'nw'))
         & !(str_detect(no_spaces_ipa, 'ru') & str_detect(transcription, 'rw'))
         & !(str_detect(no_spaces_ipa, 'lu') & str_detect(transcription, 'lw'))
         & !(str_detect(no_spaces_ipa, 'du') & str_detect(transcription, 'dw'))
         & !(str_detect(no_spaces_ipa, 'gu') & str_detect(transcription, 'w'))
         
         # Intervocalic glide transcribed as vowel
         # & !(str_detect(no_spaces_ipa, '[auoe]i[auoe]') & str_detect(transcription, '[auoe]ʝ[auoe]'))
         # & !(str_detect(no_spaces_ipa, '[aeiou]w[aeiou]') & str_detect(transcription, '[aeiou]u[aeiou]'))
         
         # Coda glides transcribed as glides
         & !(str_detect(no_spaces_ipa, 'aʝ') & str_detect(transcription, 'ai'))
         & !(str_detect(no_spaces_ipa, 'oʝ') & str_detect(transcription, 'oi'))
         & !(str_detect(no_spaces_ipa, 'aʝ') & str_detect(transcription, 'ai'))
         & !(str_detect(no_spaces_ipa, 'eʝ') & str_detect(transcription, 'ei'))
         & !(str_detect(no_spaces_ipa, 'ew') & str_detect(transcription, 'eu'))
         & !(str_detect(no_spaces_ipa, 'aw') & str_detect(transcription, 'au'))
         
         # Miscellaneous
         & !(str_detect(no_spaces_ipa, 'kua') & str_detect(transcription, 'ktwa'))
         & !(str_detect(no_spaces_ipa, 'w') & str_detect(transcription, 'gu'))
         & !(str_detect(no_spaces_ipa, 'wi') & str_detect(transcription, 'uʝ'))
         & !(str_detect(no_spaces_ipa, 'tʝu') & str_detect(transcription, 'tinw'))
         & !(str_detect(no_spaces_ipa, '^ʝ') & str_detect(transcription, '^i'))
         & !(str_detect(no_spaces_ipa, '^i') & str_detect(transcription, '^ʝ'))
         & !(str_detect(no_spaces_ipa, 'tuɾ') & str_detect(transcription, 'tɾw'))
  )
         # & !str_detect(no_spaces_ipa, 
         #               'ia|io|ie|iu|wa|wo|we|ks|kd|kb|kn|sb|ii|zm|zn|tb|sd|sɾ|sl|zl|ŋk|sg')
         # & !str_detect(transcription, 'ia|io|ie|ai|d$|dt|ds')
         # & !str_detect(no_spaces_ipa, "[wʝ][^aeiou]"))

         