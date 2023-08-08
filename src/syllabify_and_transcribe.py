# This script syllabifies and transcribes to IPA a list of words
# in Spanish orthography
import epitran
import re
import silabeador
import pandas as pd

df = pd.read_csv("../data/subtlex_esp_espal_data.csv")
epi = epitran.Epitran('spa-Latn')

syllabified_ipa = []

for x in df.word:
    if x == 'y':
        # silabeador can't syllabify 'y' for some reason...
        syl = ['y']
    elif x == 'yvonne':
        syl = ['y', 'vonne']
    else:
        syl = silabeador.syllabify(x)
    syllabified_ipa.append(epi.transliterate('.'.join(syl)).split('.'))

# syllabified_ipa = [
#     epi.transliterate('.'.join(x)).split('.') 
#     for x in map(silabeador.syllabify, df.word)
# ]

stressed_ipa = []

for i, syllables in enumerate(syllabified_ipa):
    stress_num = df.sa_syll_accent[i]
    syllables = [list(x) for x in syllables]
    syllables[stress_num - 1] = [x + '1' for x in syllables[stress_num - 1]]
    stressed_ipa.append(' '.join([' '.join(y) for y in syllables]))

syllabified_ipa = ['.'.join(x) for x in syllabified_ipa]
unsyllabified_ipa = [re.sub('1', '', x) for x in stressed_ipa]

df['stressed_ipa'] = stressed_ipa
df['unsyllabified_ipa'] = unsyllabified_ipa
df['syllabified_ipa'] = syllabified_ipa
df['no_spaces_ipa'] = [''.join(x.split(' ')) for x in unsyllabified_ipa]
df.to_csv('../data/syllabified_spanish.csv')