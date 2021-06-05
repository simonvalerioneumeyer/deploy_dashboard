import gzip
from google.cloud import storage
from oauth2client.client import GoogleCredentials
import os
import google.auth
from sqlalchemy import create_engine
import time
import pymysql
from gensim.test.utils import common_texts
from gensim.models import Word2Vec
from re import sub
import re
import gensim
from gensim.parsing.preprocessing import preprocess_string, split_alphanum, strip_tags, strip_punctuation, strip_multiple_whitespaces, strip_numeric, remove_stopwords, strip_short, stem_text
from gensim.parsing.preprocessing import STOPWORDS
from nltk.corpus import stopwords
import spacy
import de_core_news_sm
import json
import yaml
import pandas as pd
from sqlalchemy import create_engine
pd.options.display.max_columns = 99


class HonestPieceOfWork:
    def __init__(self, last_update=None):  # We set default value of last_update to None so we can initialize it!
        with open(r'Python/config.yml') as file:
            self.configs = yaml.load(file)
            # Load connection settings:
        self.ip_address = self.configs['ip_address']
        self.port = self.configs['port']
        self.password = self.configs['password']
        self.username = self.configs['username']

        # define engine:
        self.engine = create_engine('mysql+pymysql://'
                               + self.username + ':'
                               + self.password + '@'
                               + self.ip_address + ':'
                               + self.port
                               + '/tweets',
                               echo=False)
        self.engine.connect()

        def remove_URL(sample):
        #Remove URLs from a sample string
            return re.sub(r"http\S+", "", sample)

        self.data_path = 'Data/'
        self.last_update = last_update
        self.not_alphanumeric_or_space = re.compile('[^(\w|\s|\d)]')
        self.CUSTOM_FILTERS = [lambda x: x.lower(), remove_URL, strip_tags, strip_punctuation, strip_multiple_whitespaces, 
                          strip_numeric, strip_short]  # ,stem_text, remove_stopwords] GERMAN LANGUAGE!!
        self.nlp = spacy.load('de_core_news_sm')

        #Load some stop word lists and combine them to create comprehensive list
        
        #Git list of german stop words
        git_german_list = []
#        with open('stopwords-de.json') as f:
#            git_german_list.append(json.load(f))
#            git_german_list = [i for i in git_german_list[0]]

        #nltk list of german stop words

#        german_stop_words_nltk = stopwords.words('german')

        #spacy stop word list

#        nlp_stop_word_list_spacy = [i for i in self.nlp.Defaults.stop_words]

        # Combine the two stop word list to make it more comprehensive

        def combining_stopwords(list_A, list_B):
            for i in list_B:
                if i not in list_A:
                    list_A.append(i)
            return list_A

        #Combine to final stop word list

#        german_stop_words = combining_stopwords(git_german_list, german_stop_words_nltk)
#        self.geman_stop_words = combining_stopwords(german_stop_words, nlp_stop_word_list_spacy)

    def preprocess(self, doc):
        words = preprocess_string(doc, filters=self.CUSTOM_FILTERS)
        #words = [i for i in words if i not in self.german_stop_words]
        doc = ' '.join(words).lower()
        doc_lem = self.nlp(doc)
        result = ' '.join([x.lemma_ for x in doc_lem])
        return sub(self.not_alphanumeric_or_space, '', result)

    def update_necessary(self, update_request):
        if self.last_update is None:  # If we have a none value to initialize it we return yes right away!
            return("Yes")
        else:
            last_update = time.strptime(self.last_update, '%Y-%m-%d')
            update_request = time.strptime(update_request, '%Y-%m-%d')
            if last_update[0] < update_request[0]:  # different year
                return("Yes")
            elif last_update[1] < update_request[1]:  # Different month
                return("Yes")
            elif last_update[1] == update_request[1]:  # Same month
                if last_update[2] < update_request[2]:  # different day
                    return("Yes")
            else:
                return('No')
   
    def load_tweets(self, rel_path):
        with gzip.open(rel_path, 'r') as f:
            lines = [line for line in f.readlines() if line.strip()]
            return [json.loads(line) for line in lines]

    def load_data(self, json_file, date_of_tweet):
        # load data:
        data = self.load_tweets(self.data_path + json_file)

        # load selection of columns to be considered
        columns = self.configs['columns']
        tw_list = []

        # Load list of user_ids that we want to get the mentions from
        target_mention_id = self.configs['target_mention_id']
        # Load dictionary that has all relevant states so we can create a state variable
        state_dict = self.configs['state_dict']
        # Load list for states in East Germany
        east = self.configs['east']
        # Load entity info (party or politician)
        party_politician = self.configs['party_politician']
        # extract and collect tweet texts and features:
        for tweet in data:
            # early stoppage for tweets that are neither party nor politicians:
            if tweet['user']['id'] not in party_politician.keys():
                continue
            print(f'Politician/Party {tweet["id"]} found!')
            # extract the features:
            screen_name = tweet['user']['screen_name']
            user_id = tweet['user']['id']
            tweet_date = date_of_tweet
            try:
                text = tweet['full_text']
            except KeyError:  # As there are different API, it seems as if there is Full Text and just text as a key of the output
                text = tweet['text']  # Streaming data it looks like this!

            # preprocessed text:
            preprocessed_text = self.preprocess(text)

            # hashtags:
            hashtags = ' '
            for htag in tweet['entities']['hashtags']:
                hashtags += htag['text'] + str(' ')

            favorite_count = tweet['favorite_count']
            retweet_count = tweet['retweet_count']
            reply_count = tweet['reply_count']
            quote_count = tweet['quote_count']
            location = tweet['user']['location']
            followers_count = tweet['user']['followers_count']
            friends_count = tweet['user']['friends_count']
            statuses_count = tweet['user']['statuses_count']

            try:
                longitude = float(tweet['agrius_search']['geocode'].split(',')[1])
            except KeyError:
                longitude = float(0)
            try:
                latitude = float(tweet['agrius_search']['geocode'].split(',')[0])
            except KeyError:
                latitude = float(0)
            try: 
                radius_km = float(tweet['agrius_search']['geocode'].split(',')[2][:-2])
            except KeyError:
                radius_km = float(0)
            
            # Again, as we have different APIs we need to build in this mechanims. 0 means here that we do not have the geo data!
            # Streaming API does not have location! We need to be aware of none type location!
            # Getting the mentions, but only those that are of interest to us
            
            user_mentioned = ''
            
            for mention in tweet['entities']['user_mentions']:
                if mention['id'] in target_mention_id:
                    user_mentioned += mention['id_str'] + str(' ')

            # Getting the state variable

            state = 'Germany'  # Set germany as a default value and overwrite it if we have more detailed information!
            east_dummy = 0  # Set default east dummy variable as 0, and overwrite it if we have better information!
            west_east_berlin = 0

            if latitude != float(0):  # So we have a certain latitude value
                if latitude in state_dict.keys():
                    state = state_dict[latitude]
                    if state in east:
                        east_dummy = 1
                        west_east_berlin = 1
                    elif state == 'Berlin':
                        west_east_berlin = 2

            # We have no latitude value before due to the streaming API, we can try to find it via location and match!
            else:
                try:
                    #Location could be None type, so we need to have this!
                    loc_element = location.replace(' ', '').split(',')
                    for el in loc_element:
                        for value in state_dict.values():
                            if value == el:
                                state = value          
                                if state in east:
                                    east_dummy = 1
                                    west_east_berlin = 1
                                elif state == 'Berlin':
                                    west_east_berlin = 2
                except AttributeError:
                    pass
            try:
                user_role = party_politician[user_id][0]
                party = party_politician[user_id][1]
            except KeyError:
                user_role = 'normal_user'
                party = 'no'           
            
            row = [
                screen_name, user_id, user_role, party,
                tweet_date, text, preprocessed_text, hashtags,
                favorite_count, retweet_count, reply_count, quote_count,
                location, latitude, longitude, radius_km, followers_count,
                friends_count, statuses_count, state, east_dummy, west_east_berlin, user_mentioned
                   ]
            
            tw_list.append(row)
        return pd.DataFrame(tw_list, columns=columns)

    def download_bucket(self, bucket_name, stored_data_destination):

        '''     bucket_name should be in the form of:       'name_of_bucket'            '''
        '''     stored_data_destination should be in the form of:       '/path/to/storage/location/'            '''

        #storage_client = storage.Client.from_service_account_json(self.configs['GOOGLE_APPLICATION_CREDENTIALS'])
        storage_client = storage.Client.from_service_account_json(os.getenv('GOOGLE_APPLICATION_CREDENTIALS'))
        bucket = storage_client.bucket(bucket_name)

        # NB: I have to call each bucket as bucket_1, bucket_2... I cannot call the files like the
        # source_blob_name because it includes / and python reads it as a different file.
        i = 0

        for blob_iterator in storage_client.list_blobs(bucket_name):

            # blob name is the datastamp of each unique blob:
            source_blob_name = str(blob_iterator.name)
            print('This is source blob name ' + str(source_blob_name))

            # Extract the date of the tweet so we can create a string for the tweet
            date_of_tweet = source_blob_name[source_blob_name.index("2"):(-1)]
            date_of_tweet = date_of_tweet.split("/")[0]
            print(date_of_tweet)

            if self.update_necessary(date_of_tweet) == "Yes":
                # location where i want the current blob to be stored
                # NB: need to call the file .json.gz since if not it would not be readable.
                # destination_file_name = stored_data_destination + 'blob_' + str(i) + '.json.gz'

                blob = bucket.blob(source_blob_name)
                blob.download_to_filename('Data/temp.json.gz')

                data = self.load_data('temp.json.gz', date_of_tweet)

                # drop duplicates:
                data = data.drop_duplicates(['tweet_date', 'text', 'user_id'])

                # create dynamic index
                diff = len(data)
                data['id'] = range(i, i+diff)
                i += diff
                print(data)

                # deleting the temp file
                try:
                    os.remove('Data/temp.json.gz')
                except FileNotFoundError:
                    pass

                # sql:
                data.to_sql('seb_table', con=self.engine, if_exists='append', index=False)
                print('Downloaded: ' + source_blob_name)
            
            #i += 1

        if self.update_necessary(date_of_tweet) == "Yes":
            print(
                "Bucket {} downloaded to {}.".format(
                    bucket_name, stored_data_destination
                )
            )
