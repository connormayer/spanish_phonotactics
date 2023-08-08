# This script generates all possible Spanish CVCV words
import csv

consonants_1 = [
    'p',
    't',
    'k',
    'b',
    'd',
    'ɡ',
    'f',
    's',
    'x',
    'm',
    'n',
    'ɲ',
    'r',
    'tʃ',
    'ʝ',
    'w',
    'l'   
]

consonants_2 = consonants_1 + ['ɾ']

vowels = [
    'a',
    'i',
    'u',
    'e',
    'o'
]

results = []

for c1 in consonants_1:
    for v1 in vowels:
        for c2 in consonants_2:
            for v2 in vowels:
                initial_stress = "'{}{}.{}{}".format(
                    c1, v1, c2, v2
                )
                final_stress = "{}{}.'{}{}".format(
                    c1, v1, c2, v2
                )

                results.extend([[initial_stress], [final_stress]])

with open('spanish_stimuli.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerows(results)
