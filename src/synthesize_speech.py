# This script takes a list of stimuli in IPA format and synthesizes it
# using Amazon Polly
from boto3 import Session
from botocore.exceptions import BotoCoreError, ClientError
from contextlib import closing
import csv
import os
import pandas as pd
import re
import sys
import subprocess
from tempfile import gettempdir

def change_glides(word_list):
    foo = ' '.join(word_list)
    foo = re.sub(r"(^)i( [aeoiu])",r"\1j\2", foo)
    foo = re.sub(r"(^)u( [aeoiu])",r"\1w\2", foo)
    foo = re.sub(r"([aeoiu] )i( [aeoiu])",r"\1j\2", foo)
    foo = re.sub(r"([aeoiu] )u( [aeoiu])",r"\1w\2", foo)
    return foo.split(' ')

def convert_stress(word):
    split_word = word.split(' ')
    stress_idx = [idx for idx, s in enumerate(split_word) if '1' in s][0]
    split_word = [re.sub('1', '', s) for s in split_word]
    split_word = change_glides(split_word)
    split_word.insert(stress_idx, 'ˈ')
    return ''.join(split_word)

# Create a client using the credentials and region defined in the [adminuser]
# section of the AWS credentials file (~/.aws/credentials).
session = Session(profile_name="default")
polly = session.client("polly")

file = "../data/grouped_stimuli.csv"
with open(file) as f:
    reader = csv.DictReader(f)
    df = pd.DataFrame(reader)

filenames = []

for _, row in df.iterrows():
    word = convert_stress(row['word'])
    print(word)
    try:
        # Request speech synthesis
        response = polly.synthesize_speech(
            Engine = 'neural',
            LanguageCode = 'es-US',
            Text="<speak><phoneme alphabet='ipa' ph='{}'>Blah</phoneme></speak>".format(word), 
            OutputFormat="mp3",
            VoiceId="Lupe",
            TextType="ssml"
        )
    except (BotoCoreError, ClientError) as error:
        # The service returned an error, exit gracefully
        print(error)
        sys.exit(-1)

    # Access the audio stream from the response
    if "AudioStream" in response:
        # Note: Closing the stream is important because the service throttles on the
        # number of parallel connections. Here we are using contextlib.closing to
        # ensure the close method of the stream object will be called automatically
        # at the end of the with statement's scope.
        with closing(response["AudioStream"]) as stream:
            root_file = "{}.mp3".format("_".join(row['word'].split(" ")))
            output = os.path.join('../audio', root_file)

            try:
            # Open a file for writing the output as a binary stream
                with open(output, "wb") as file:
                   file.write(stream.read())
            except IOError as error:
                # Could not write to file, exit gracefully
                print(error)
                sys.exit(-1)
            filenames.append(root_file)
    else:
        # The response didn't contain audio data, exit gracefully
        print("Could not stream audio")
        sys.exit(-1)

df['filename'] = filenames
df.to_csv('../data/stimuli_with_filenames.csv')