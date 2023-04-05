import epitran
import re
import silabeador
import pandas as pd

df = pd.read_csv("/home/connor/git_repos/spanish_phonotactics/transcribed_training.csv")
epi = epitran.Epitran('spa-Latn')

syllabified_ipa = []
for x in df.word:
    try:
        if x == 'y':
            # silabeador can't syllabify 'y' for some reason...
            syl = ['y']
        elif x == 'yvonne':
            syl = ['y', 'vonne']
        else:
            syl = silabeador.syllabify(x)
        syllabified_ipa.append(epi.transliterate('.'.join(syl)).split('.'))
    except:
        breakpoint()

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
unstressed_ipa = [re.sub('1', '', x) for x in stressed_ipa]

df['stressed_ipa'] = stressed_ipa
df['unstressed_ipa'] = unstressed_ipa
df['syllabified_ipa'] = syllabified_ipa
df.drop('transcription', axis=1)
df.to_csv('syllabified_spanish.csv')