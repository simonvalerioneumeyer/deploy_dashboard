import streamlit as st
import yaml
from sqlalchemy import create_engine
import pandas as pd
import datetime
import altair as alt
from PIL import Image

with open(r'Python/config.yml') as file:
    configs = yaml.load(file)
party_color_dict = configs['party_color_dict']
months_dict = configs['months_dict']

# Change layout wide
st.set_page_config(layout="wide")

# Title:
st.title("The German parliament election 2021 - the political climate on Twitter")

st.write("""
### This dashboard is a pilot version and aims to give an overview of the political debate on Twitter with respect to the upcoming parliament elections in 2021
by Buelent Uendes, Simon Neumeyer and Elio Abi Karam
""")

# settings:
engine = create_engine('mysql+pymysql://bes:seb_1_bes@3.140.248.246:3306/tweets', echo=False)
chunks = pd.read_sql('SELECT party, followers_count, favorite_count, retweet_count, reply_count,'
                     ' quote_count, state, east_dummy, text, tweet_date, user_id '
                     'FROM seb_table WHERE party != "no";', con=engine, chunksize=1000)

def chunks_to_lists(chunks):
    party = []
    followers_count = []
    favorite_count = []
    retweet_count = []
    reply_count = []
    quote_count = []
    state = []
    east_dummy = []
    text = []
    tweet_date = []
    user_id = []

    for i in chunks:
        party.append(list(i.party))
        favorite_count.append(list(i.favorite_count))
        retweet_count.append(list(i.retweet_count))
        reply_count.append(list(i.reply_count))
        quote_count.append(list(i.quote_count))
        state.append(list(i.state))
        east_dummy.append(list(i.east_dummy))
        text.append(list(i.text))
        tweet_date.append(list(i.tweet_date))
        user_id.append(list(i.user_id))
        followers_count.append(list(i.followers_count))

    # flatten out lists
    party = [item for sublist in party for item in sublist]
    favorite_count = [item for sublist in favorite_count for item in sublist]
    retweet_count = [item for sublist in retweet_count for item in sublist]
    reply_count = [item for sublist in reply_count for item in sublist]
    quote_count = [item for sublist in quote_count for item in sublist]
    state = [item for sublist in state for item in sublist]
    east_dummy = [item for sublist in east_dummy for item in sublist]
    text = [item for sublist in text for item in sublist]
    tweet_date = [item for sublist in tweet_date for item in sublist]
    user_id = [item for sublist in user_id for item in sublist]
    followers_count = [item for sublist in followers_count for item in sublist]

    return party, favorite_count, retweet_count, reply_count, quote_count, state, east_dummy, text, tweet_date, user_id, followers_count


@st.cache
def preprocess():
    # create dataframe
    data_dic = {'party': party, 'favorites': favorite_count,
                'retweets': retweet_count, 'replies': reply_count,
                'quotes': quote_count, 'state': state, 'east_dummy': east_dummy,
                'text': text, 'tweet_date': tweet_date, 'user_id': user_id,
                'followers_count': followers_count}
    data = pd.DataFrame(data_dic)
    data['party'] = data['party'].replace({'Bündnis90/Die Grünen': 'Grünen', 'Die Linke': 'Linke', 'AFD': 'AFD'})

    # create month column
    dates = [datetime.date(*(int(s) for s in t.split('-'))) for t in data.tweet_date]
    data['month'] = [x.month for x in dates]

    # convert columns from str to int
    cols_to_int = ['favorites', 'retweets', 'replies', 'quotes', 'east_dummy', 'followers_count']
    for col in cols_to_int:
        data[col] = data[col].astype(int)

    # drop duplicate tweets
    data = data.drop_duplicates(['tweet_date', 'text', 'user_id'])

    return data

# Get chunks into separate lists
party, favorite_count, retweet_count, reply_count, quote_count, state, \
east_dummy, text, tweet_date, user_id, followers_count = chunks_to_lists(chunks)

# Get all relevant info into a dataframe
data = preprocess()

# sub-selection with dropdown
state_filter = st.sidebar.selectbox("Select state you are interested in", ('All',) + tuple(data.state.unique()))
month_filter = st.sidebar.selectbox("Select month are you interested in?", ('All',  'April', 'May', 'June'))
party_filter = st.sidebar.selectbox("Select party you are interested in?", ('All', 'AFD', 'CDU', 'CSU', 'FDP',
                                                                          'Grünen', 'Linke', 'SPD', 'other'))
popularity_filter = st.sidebar.selectbox("Popularity measure: favorites or retweets?", ('Favorites',  'Retweets'))
popularity_filter = popularity_filter.lower()

if state_filter != 'All':
    data = data[data.state == state_filter]

if month_filter != 'All':
    data = data[data.month == months_dict[month_filter]]

# plot overall frequency:
overall_frequence = data['party'].value_counts().reset_index()
overall_frequence = overall_frequence.rename(columns={'party': 'frequency', 'index': 'party'})
overall_frequence = overall_frequence.sort_values('frequency', ascending=False)
overall_frequence = overall_frequence.reset_index(drop=True)

# plot popularity:
data_metrics = data.groupby(['party']).sum().reset_index()
data_metrics = data_metrics.merge(overall_frequence, how='left', on='party', validate='1:1')

data_metrics['favorites per tweet'] = data_metrics.favorites/data_metrics.frequency
data_metrics['retweets per tweet'] = data_metrics.retweets/data_metrics.frequency

# frequency chart
frequency = alt.Chart(overall_frequence).mark_bar().encode(
    x=alt.X('party', sort='-y'), y='frequency', color=alt.Color(
        'party', scale=alt.Scale(domain=list(party_color_dict.keys()),
                                 range=list(party_color_dict.values())), sort='-y'
                                         )
).properties(width=240)
#.properties(width=400, height=300)

# favorites chart
favs = alt.Chart(data_metrics).mark_bar().encode(
    x='party',
    y='favorites per tweet',
    color=alt.Color(
        'party', scale=alt.Scale(domain=list(party_color_dict.keys()), range=list(party_color_dict.values())), sort='-y'
                                         )
).properties(width=240)

# retweets chart
retws = alt.Chart(data_metrics).mark_bar().encode(
    x='party',
    y='retweets per tweet',
    color=alt.Color(
        'party', scale=alt.Scale(domain=list(party_color_dict.keys()), range=list(party_color_dict.values())), sort='-y'
                                         )
).properties(width=240)

subchart = alt.hconcat(frequency, favs, retws).configure_mark(
    opacity=0.5,
    color='red'
)
#st.write(frequency)
st.write(subchart)

# favorites chart
favs_overall = alt.Chart(data_metrics).mark_bar().encode(
    x='party',
    y='favorites',
    color=alt.Color(
        'party', scale=alt.Scale(domain=list(party_color_dict.keys()), range=list(party_color_dict.values())), sort='-y'
                                         )
).properties(width=350)

# retweets chart
retws_overall = alt.Chart(data_metrics).mark_bar().encode(
    x='party',
    y='retweets',
    color=alt.Color(
        'party', scale=alt.Scale(domain=list(party_color_dict.keys()), range=list(party_color_dict.values())), sort='-y'
                                         )
).properties(width=350)

subchart_overall = alt.hconcat(favs_overall, retws_overall).configure_mark(
    opacity=0.5,
    color='red'
)
st.write(subchart_overall)



# text box:
# most favorite tweet (/ per # of followers)
if party_filter != 'All':
    data = data[data.party == party_filter]



try:
    highest = data[popularity_filter].max()
    text = data[data[popularity_filter] == highest].text.iloc[0]
    date = data[data[popularity_filter] == highest].tweet_date.iloc[0]
    favorites = data[data[popularity_filter] == highest][popularity_filter].iloc[0]
    user_id = data[data[popularity_filter] == highest].user_id.iloc[0]
except:
    highest = text = date = favorites = ''

# per followers count:
try:
    highest_2 = (data[popularity_filter] / data.followers_count).max()
    text_2 = data[(data[popularity_filter] / data.followers_count) == highest_2].text.iloc[0]
    date_2 = data[(data[popularity_filter] / data.followers_count) == highest_2].tweet_date.iloc[0]
    favorites_2 = data[(data[popularity_filter] / data.followers_count) == highest_2][popularity_filter].iloc[0]
    user_id_2 = data[(data[popularity_filter] / data.followers_count) == highest_2].user_id.iloc[0]
except:
    highest_2 = text_2 = date_2 = favorites_2 = ''



st.title("Most favorite/retweeted tweets")

st.write(f'### Top tweet (in terms of {popularity_filter}):')
st.write(f'{text}  - tweeted by {configs["party_politician"][int(user_id)][2]} on {date}, with a total of {favorites} {popularity_filter}.')
#st.write(f'Tweeted by {configs["party_politician"][int(user_id)][2]} on {date}, with a total of {favorites} {popularity_filter}.')

st.write(f'### Top tweet (in terms of {popularity_filter}) per number of followers:')
st.write(f'{text_2}  - tweeted by {configs["party_politician"][int(user_id_2)][2]} on {date_2}, with a total of {favorites_2} {popularity_filter} per followers.')
#st.write(f'Tweeted by {configs["party_politician"][int(user_id_2)][2]} on {date_2}, with a total of {favorites_2} {popularity_filter} per followers.')


topic_dist_1 = Image.open('favs.png')
topic_dist_2 = Image.open('retweets.png')

if popularity_filter == 'favorites':
    st.write(f'### Topic distribution of most favorite tweet per party:')
    st.image(topic_dist_1)
else:
    st.write(f'### Topic distribution of most retweeted tweet per party:')
    st.image(topic_dist_2)

