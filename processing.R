######################################################################################################################################################
# Packages
######################################################################################################################################################

# packages
require(rtweet)
require(ggplot2)
require(dplyr)
library(tidytext)
require(stringr)

######################################################################################################################################################
# Auth for Twitter
######################################################################################################################################################

# keys and secrets for auth
consumer_key = ""
consumer_secret = ""
access_token = ""
access_secret = ""

# do auth
nr18token = create_token(app = "nr18", 
                         consumer_key, 
                         consumer_secret, 
                         access_token, 
                         access_secret)

######################################################################################################################################################
# Getting the Tweets
######################################################################################################################################################

# get the tweets
rt = search_tweets(
  "#nr18", n = 20000, include_rts = FALSE, token = nr18token
)

######################################################################################################################################################
# Process the Tweets
######################################################################################################################################################

# save names of tweeters
p_names = unique(rt$screen_name)

# remove ad shit
rt = filter(rt, source != "Twitter Ads Composer")

# remove tweet with more than 9 hashtags (moslty shit bots)
rt$hashtags_count = str_count(rt$hashtags, ",")
rt = filter(rt, hashtags_count < 10)

# round by hour and format
rt$rounded = round(rt$created_at, units = "hours")
rt$rounded = as.character(rt$rounded)

# remove tweet names from text
rt$text = gsub("[^\\s]*@[^\\s]*", "", rt$text, perl=T) 

# remove retweets
rt = rt %>% filter(!is_retweet)

# remove links from text
rt = rt %>% mutate(text = gsub("https?://[\\w\\./]+", "", text, perl = TRUE))

# filter by time
rt = filter(rt, created_at >= "2018-06-28 00:00:00" & created_at < "2018-07-03 00:00:00")

######################################################################################################################################################
# When were people tweeting?
######################################################################################################################################################

# group by hour
rt_group = rt %>% group_by(rounded) %>% summarise(count = n())

# format
rt_group$rounded = as.POSIXct(rt_group$rounded,"%Y-%m-%d %h:%m:%s")

# viz
time = ggplot(data = rt_group, aes(x = rounded, y = count)) +
  geom_line() +
  labs(x = "",y = "Tweets", caption = "All tweets (n = 1439) for the hashtag #nr18 between 2018-06-28 and 2018-07-03.") +
  ggtitle("#nr18: Tweets aggregated by hour") +
  theme_minimal()

######################################################################################################################################################
# Who were tweeting?
######################################################################################################################################################

# who is tweeting
people = rt %>%
  count(screen_name, sort = TRUE) %>% slice(1:20) %>%
  ggplot(aes(x = reorder(screen_name, n, function(n) -n), y = n)) + 
  labs(x = "",y = "Tweets", caption = "All tweets (n = 1439) for the hashtag #nr18 between 2018-06-28 and 2018-07-03.") +
  geom_bar(stat = "identity") + 
  ggtitle("#nr18: Most active twitter users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################################################################################################################################################
# What are people tweeting about?
######################################################################################################################################################

# what is being said
tweet_words = dplyr::select(rt, user_id, text)
tweet_words = tweet_words %>% unnest_tokens(word, text)

# stemming
#tweet_words$word = wordStem(tweet_words$word)

# remove name of tweeters
tweet_words = filter(tweet_words, !(word %in% p_names))
tweet_words = tweet_words %>% group_by(word) %>% summarise(count = n())

# remove stop words (ENG/GER)
setwd("C:/Users/akruse/Desktop/")
stops = read.table("stopwords-de.txt", encoding = "UTF-8")
tweet_words = filter(tweet_words, !(word %in% stops$V1))

# remove shiddy words and numbers
tweet_words = filter(tweet_words, !(word %in% c("amp","nr18")))
tweet_words = tweet_words[-grep('^\\d+$', tweet_words$word),]

# get top 20
word = tweet_words %>%
  arrange(-count) %>%
  slice(1:20)

# reorder factors from plotting
word$word = factor(word$word, levels = word$word)

# ggplot
word = word %>%
  ggplot(aes(x = word, y = count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "",y = "Word Occurrence", caption = "All tweets (n = 1439) for the hashtag #nr18 between 2018-06-28 and 2018-07-03.") +
  ggtitle("#nr18: Most used words in tweets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################################################################################################################################################
# What hashtags were people using?
######################################################################################################################################################

# extract hashtags
hash_analysis = rt
hash_analysis$hashtags = gsub("c\\(|)|\"|([\n])","", hash_analysis$hashtags)
hash_analysis$hashtags = gsub(",","", hash_analysis$hashtags)
hash_analysis = as.data.frame(hash_analysis$hashtags)
colnames(hash_analysis) = c("hashtags")
hash_analysis$hashtags = as.character(hash_analysis$hashtags)
hash_analysis = unnest_tokens(tbl = hash_analysis, output = checker, input = hashtags)

# count hashtags
hash_analysis = hash_analysis %>% group_by(checker) %>% summarise(count = n())

# remove shiddy hashtags
hash_analysis = filter(hash_analysis, !(checker %in% c("nr18","u")))

# get top 20
hash_analysis = hash_analysis %>%
  arrange(-count) %>%
  slice(1:20)

# reorder factor levels for plotting
hash_analysis$checker = factor(hash_analysis$checker, levels = hash_analysis$checker)

# ggplot
hash_analysis = hash_analysis %>%
  ggplot(aes(x = checker, y = count)) + 
  geom_bar(stat = "identity") + 
  labs(x = "",y = "Hashtag Occurrence", caption = "All tweets (n = 1439) for the hashtag #nr18 between 2018-06-28 and 2018-07-03.") +
  ggtitle("#nr18: Most used hashtags in tweets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
