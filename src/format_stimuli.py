import csv

with open("/mnt/e/Dropbox/ling/research/nsf_megha/berta/spanish_stimuli.csv") as f:
	with open('/mnt/e/Dropbox/ling/research/nsf_megha/berta/testing_data.csv', 'w') as g:
		reader = csv.reader(f)
		writer = csv.writer(g)

		headers = next(reader)

		for line in reader:
			if line[1] == '1':
				continue

			word = line[0].split('.')

			if word[1][0] == "'":
				word[0] = ' '.join(word[0])
				word[1] = ' '.join([x + '1' for x in word[1][1:]])
			else:
				word[0] = ' '.join([x + '1' for x in word[0]])
				word[1] = ' '.join(word[1])

			new_word = ' '.join(word)
			writer.writerow([new_word])
			