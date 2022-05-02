!pip3 install snscrape

import snscrape.modules.twitter as sntwitter

import pandas as pd
# Creating list to append tweet data. Not entirely sure why or what this does 
tweets_list1 = []

# Using TwitterSearchScraper to scrape data and append tweets to list. I can tell this is a for loop, but I don't get the syntax
# this enumerate function will give me all the tweets 
for i,tweet in enumerate(sntwitter.TwitterSearchScraper('NFT since:2021-01-01 until:2021-05-31').get_items()):
   #i>5000 and then break means that it will stop scraping at 5001
    if i>200:
        break
    tweets_list1.append([tweet.date, tweet.id, tweet.content, tweet.username])
    #this last part is giving 
    #us the data that we are scraping, in this case date, id, content and username

print(tweets_list1)

# Creating a dataframe from the tweets list above
tweets_df1 = pd.DataFrame(tweets_list1, columns=['Datetime', 'Tweet Id', 'Text', 'Username'])

print(tweets_df1)
