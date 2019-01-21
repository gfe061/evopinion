library(quanteda)
library(SnowballC)
library(stm)
library(foreign)
library(gridExtra)
library(feather)
library(tidyverse)
library(tidytext)
library(text2vec)
library(lubridate)

options(stringsAsFactors = FALSE)
rm(list=ls(all=TRUE))
## Personalized -- wd path
path <- '~/pCloudDrive/Stellen/UiB - CET/Various/evopinion'
# path <- "/home/ghfc/pCloudDrive/Stellen/UiB - CET/Various/evopinion"
setwd(path)

evtweets.full <- read.csv2("norwayKeyword_2018-09-06.csv", sep=",")
evtweets1 <- evtweets.full[,c(1,4:8,14)]
evtweets1 <- evtweets1 %>%
  arrange(created)
# 141 days of tweets
seconds_to_period(interval(evtweets1$created[1], evtweets1$created[length(evtweets1$created)]))
evtweets1 <- evtweets1 %>%
  mutate(wc = str_count(text, '[\\w]+'))
evtweets1 <- evtweets1 %>%
  mutate(wc_ot = str_count(original_text, '[\\w]+'))
# Filter: question is how to do this and be reasonably sure that these are the ones talking about elbil?
# definately need all forms of 'elbil' -- elbiler, elbilene, el-bil, etc, etc. As opposed to also filtering in tesla, leaf, etc. -- 
# when we have elbil we assume talk is about general class of cars rather than just their own (perhaps something completely unrelated)
# to it being an elbil?
# Here: filter for all: elbil*, el-bil* 
## THIS AND TIDY VERSION HAVE DIFFERENT RESULTS -- COMPARE THE TWO TO FIGURE OUT WHATS GOING ON
#evtweets <- evtweets1[grepl("el[-]?bil", evtweets1$text, ignore.case = TRUE),]
#dim(evtweets)
#dplyr easier here I think
evtweets <- evtweets1 %>%
  filter(str_detect(text, '[Ee]l[-]?bil')|str_detect(original_text, '[Ee]l[-]?bil'))
# filter brings number down from ~42k to ~4k.
dim(evtweets)

# function to identify and label retweet or original
retweet_replace <- function(x) {
  if (x == 'TRUE') {
    out <- 'retweet'
  }
  else if (x == 'FALSE') {
    out <- 'non-retweet'}
  return(out)
}
# change time zone and ID retweets
evtweets <- evtweets %>%
  mutate(timestamp = with_tz(ymd_hms(created), "Europe/Oslo")) %>% 
  mutate(month = month(created)) %>%
  mutate(retweet = str_detect(text, '^(RT)'))
evtweets$retweet <- sapply(evtweets$retweet, retweet_replace)

# Stemming (via tidy) of both text and original_text
evtweets_stm <- evtweets %>%
  unnest_tokens(word, text) %>%
  mutate(stem = wordStem(word, language='no')) %>%
  ungroup()
evtweets_stm <- evtweets_stm %>%
  group_by(id) %>%
  summarise(text = str_c(stem, collapse = ' '))
col <- which(colnames(evtweets) == 'text')
evtweets_merge <- evtweets[-col]
evtweets_stemmed <- left_join(evtweets_merge, evtweets_stm, by = 'id')
dim(evtweets_stemmed)

# Stemming for original_text, same process
evtweets_stm <- evtweets %>%
  unnest_tokens(word, original_text) %>%
  mutate(stem = wordStem(word, language='no')) %>%
  ungroup()
evtweets_stm <- evtweets_stm %>%
  group_by(id) %>%
  summarise(original_text = str_c(stem, collapse = ' '))
col <- which(colnames(evtweets) == 'original_text')
evtweets_merge <- evtweets[-col]
evtweets_stemmed <- left_join(evtweets_merge, evtweets_stm, by = 'id')
dim(evtweets_stemmed)

# Tried to streamline the manual stemming process, not sure that I really did....



# Create a manual stemming dictionary
######### NEED TO BE CAREFUL WITH THIS: STEMMING OF ROOTS MUST HAPPEN BEFORE STEMMING OF SUFFIXES (IE EL-BIL -> ELBIL BEFORE
######### ELBILEN -> ELBIL) OTHERWISE YOU GET INCONSISTENT RESULTS THAT CHANGE EVERYTIME YOU RUN THE LOOP OR NOTE WITH 
######### REGEX AS DONE HERE IN COL#2 OF DF

# this is a manual stemming df. Additional stems desired can be added: 'c(word to be stemmed, stem)'
stem_dict <- data.frame(c('#', ' '), c("el-bil[en]?", "elbil"), c("avgiften", "avgift"), c("nyeste", "ny"), 
      c("nyest", "ny"), c('nye', 'ny'), c('elbil[a-z]+', 'elbil'))
stem_dict_tw <- data.frame(c('@[A-z]*', ''))
# Stemming function to make it easy to add to dict -- maybe this doesn't actually make things easier???
# Function to add stems to the dataframe, but doesn't actually make things any easier, let's just add to df
#add_to_stem_dict <- function(x,y) {
#  stem_dict <- cbind(stem_dict, Entry = c(x, y))
#  return(stem_dict)
#}
# makes things look nicer but not needed
#colnames(stem_dict) <- 1:ncol(stem_dict)

# Actual stemming performed here
for (i in 1:ncol(stem_dict)) {
  evtweets_stemmed$text <- gsub(stem_dict[[i]][1], stem_dict[[i]][2], evtweets_stemmed$text)
  evtweets_stemmed$original_text <- gsub(stem_dict[[i]][1], stem_dict[[i]][2], evtweets_stemmed$original_text)}

#CHECK -- looks like it worked...
search <- c('#', 'elbilen', 'elbil', 'avgiften', 'nyeste', 'el-bil')
for (i in search) {
  print(sum(str_count(evtweets_stemmed$text, i)))
}

# Create feather file and read from here when starting in the future
write_feather(evtweets_stemmed, './evtweets_df_stemmed.feather')
write_feather(evtweets, './evtweets.feather')

#### Stemming of twitter handles
look <- '@[A-z]*'
# ~6500 handles in corpus
sum(str_count(evtweets_stemmed$text, look))
stem_dict_tw <- data.frame(c('@[A-z]*', ''))
for (i in 1:ncol(stem_dict_tw)) {
  evtweets_stemmed$text <- gsub(stem_dict_tw[[i]][1], stem_dict[[i]][2], evtweets_stemmed$text)
  evtweets_stemmed$original_text <- gsub(stem_dict_tw[[i]][1], stem_dict[[i]][2], evtweets_stemmed$original_text)}

# checking, no matches, stem worked
sum(str_count(evtweets_stemmed$text, look))

# create feather file
write_feather(evtweets_stemmed, './evtweets_stemmed_nohandles.feather')

###################################################
###### START FROM HERE WITH PRE-PROCESSED DFS
###################################################

evtweets <- read_feather('./evtweets.feather')
evtweets_stemmed <- read_feather('./evtweets_df_stemmed.feather')
# stemmed including of twitter handles
evtweets_nohand <- read_feather('./evtweets_stemmed_nohandles.feather')

####################################################

# taking an initial look at things
# very different distribution of word counts than citizens panel (cp)
# total word count ~105k, original_text ~48k, 
# so we potentially have 6 times more text to work with. mean ~25, but for original text only ~11.
sum(evtweets$wc)
sum(evtweets$wc_ot)
print(c(mean(evtweets$wc), mean(evtweets$wc_ot)))

# histogram of word counts of tweets
evtweets %>%
  ggplot(aes(wc)) +
  geom_histogram(bins = 30)

evtweets$following <- as.numeric(evtweets$friends_count)
evtweets$followers <- as.numeric(evtweets$followers_count)
# hist of number of accounts each account has PER individual tweet (ie. individual users counted multiple 
# times for multiple tweets)
evtweets %>%
  ggplot(aes(following)) +
  geom_histogram(bins = 60) +
  xlim(c(0,3000))
# hist of number of followers per tweet
evtweets %>%
  ggplot(aes(followers)) +
  geom_histogram(bins = 60) +
  xlim(c(0,10000))
# number of followers of every handle in the evtweets dataframe (looks geometric)
evtweets %>%
  group_by(user_id) %>%
  summarize(follow = mean(followers)) %>%
  ggplot(aes(follow)) +
  geom_histogram(bins = 60) +
  xlim(c(0,10000))
# relationship between followers of person tweeting and words writing about elbiler. Slight.
evtweets %>%
  group_by(user_id) %>%
  summarize(follow = mean(followers), wordtotal = sum(wc)) %>%
  ggplot(aes(x = follow, y = wordtotal)) +
  geom_point() +
  geom_smooth(method = loess) +
  xlim(c(0,7000))
# time of tweets
evtweets %>%
  mutate(hour = hour(timestamp)) %>%
  ggplot(aes(hour)) +
  geom_histogram(bins = 24)
# days of the week of tweets
evtweets %>%
  mutate(day = wday(created)) %>%
  ggplot(aes(day)) +
  geom_histogram(bins = 7)
# dates of tweets in the dataframe (each bar is one day) 
evtweets %>%
  mutate(numday = date(created)) %>%
  ggplot(aes(numday)) +
  geom_histogram(bins = 141)
# do these peaks correspond to obvious things?

# ~1700 are retweets. These essentially, then, just duplicates, though reaching different audiences, 
# of course. to be aware of...
sum(evtweets$retweet == 'retweet')

## Word frequency by month
corp <- corpus(evtweets_stemmed, text_field = 'text')
corp <- tokens(corp, remove_twitter = TRUE, remove_numbers = TRUE, remove_punc = TRUE, 
       remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
corp <- tokens_remove(corp, pattern = c('rt', 't.co', 'https'))

d <- dfm(corp, remove = stopwords('norwegian'), stem = FALSE, remove_punct = TRUE, remove_numbers = T)

dfm <- dfm_weight(d, scheme = 'prop')
freq_weight <- textstat_frequency(dfm, n = 20, groups = 'month')
ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency") +
  ggtitle("Word frequency counts by month (of 2017(")
  
# tf-idf by month
tfidf <- dfm_tfidf(d, scheme_tf = "prop", scheme_df = "inverse", base = 10)
rel_freq <- textstat_frequency(tfidf, n = 20, groups = 'month')
ggplot(data = rel_freq, aes(x = nrow(rel_freq):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(rel_freq):1,
                     labels = rel_freq$feature) +
  labs(x = NULL, y = "Total frequency - inverse document frequency")

corp <- corpus(evtweets_stemmed, text_field = 'text')
d <- dfm(corp, remove = stopwords('no'), stem = F, remove_punct = T)
dfm <- dfm_weight(d, scheme = 'prop')
freq <- textstat_frequency(dfm, n = 30)
ggplot(freq, aes(x = nrow(freq):1, y = frequency)) +
  geom_point() +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq):1,
                   labels = freq$feature) +
  labs(x = NULL, y = "Relative frequency") +
  ggtitle("Most frequent words total tweets")


freq_weight <- textstat_frequency(dfm, n = 20, groups = evtweets_stemmed$retweet)

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency") +
  ggtitle(paste("Most frequent words by", evtweets_stemmed$retweet))

# anything else here before we get into NLP???

######################
## NLP
######################

# ngrams -- creating 2-4-grams of the whole text, to later view, sort, search, etc.
# not interested here in handles so using handle-less df
corp <- corpus(evtweets_stemmed, text_field = 'text')
toks_notw <- tokens_remove(tokens(corp, remove_twitter = TRUE, remove_numbers = TRUE, remove_punc = TRUE, 
        remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE), c(stopwords('norwegian'), 'rt', 
        't.co', '*bil*', 'el', valuetype = 'glob'))
two_gram <- tokens_ngrams(toks_notw, n=2)
dfm_twogram <- dfm(two_gram)
#dfm <- dfm_weight(two_gram, scheme = 'prop')
freq <- textstat_frequency(dfm_twogram, n = 30)
ggplot(freq, aes(x = nrow(freq):1, y = frequency)) +
  geom_point() + 
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq):1,
                     labels = freq$feature) +
  labs(x = NULL, y = "Frequency") +
  ggtitle("Most frequent bi-grams total tweets")

# tri-grams
three_gram <- tokens_ngrams(toks_notw, n=3)
dfm_threegram <- dfm(three_gram)
#dfm <- dfm_weight(two_gram, scheme = 'prop')
freq <- textstat_frequency(dfm_threegram, n = 30)
ggplot(freq, aes(x = nrow(freq):1, y = frequency)) +
  geom_point() +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq):1,
                     labels = freq$feature) +
  labs(x = NULL, y = "Frequency") +
  ggtitle("Most frequent tri-grams total tweets")

# quad-grams
four_gram <- tokens_ngrams(toks_notw, n=4)
dfm_fourgram <- dfm(four_gram)
#dfm <- dfm_weight(two_gram, scheme = 'prop')
freq <- textstat_frequency(dfm_fourgram, n = 30)
ggplot(freq, aes(x = nrow(freq):1, y = frequency)) +
  geom_point() +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq):1,
                     labels = freq$feature) +
  labs(x = NULL, y = "Frequency") +
  ggtitle("Most frequent four-grams total tweets")

# selective n-grams
# keeping 'ikke_*' compounds
corp <- corpus(evtweets_stemmed, text_field = 'text')
toks_ikke <- tokens_remove(tokens(corp, remove_twitter = TRUE, remove_numbers = TRUE, remove_punc = TRUE, 
            remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE, ngrams = 3))
toks_ikke <- tokens_select(toks_ikke, '^ikk[e|(je)]_[A-z]*', selection = 'keep', valuetype = 'regex')
dfm_ikketrigram <- dfm(toks_ikke)
#dfm <- dfm_weight(two_gram, scheme = 'prop')
freq <- textstat_frequency(dfm_ikketrigram, n = 30)
ggplot(freq, aes(x = nrow(freq):1, y = frequency)) +
  geom_point() +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq):1,
                     labels = freq$feature) +
  labs(x = NULL, y = "Frequency") +
  ggtitle("Most frequent ikke-compounds in total tweets")

# contructing a fcm with fcm() - from either tokens or dfm.
corp <- corpus(evtweets_stemmed, text_field = 'text')
# stemming et al -- and taking out certain words manually: rt, t.co(what is this?), and elbil
toks_notw <- tokens_remove(tokens(corp, remove_twitter = TRUE, remove_numbers = TRUE, remove_punc = TRUE, 
  remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE), c(stopwords('norwegian'), 
  'rt', 't.co', 'elbil', 'el', 'biler', 'bilen', 'bil'))
d <- dfm(toks_notw, remove = stopwords('no'), stem = F, remove_punct = T)
# play around with this to get a good picture - min_termfreq = 40 gives very crowded, but potentially int
d_trim <- dfm_trim(d, min_termfreq = 70, docfreq_type = 'rank')
dfm_freq <- dfm_weight(d_trim, scheme = 'prop')
fcm_freq <- fcm(dfm_freq, window = 5)
fcm_select(fcm_freq, c('miljø', 'bompenger', 'tesla', 'ikke*'))
# Using an FCM to visualize a semantic network
feat <- names(topfeatures(fcm_freq, dim(fcm_freq)[1]))
size <- log(colSums(dfm_select(fcm_freq, feat)))
textplot_network(fcm_freq, min_freq = 0.8, vertex_size = size / max(size) * 3)
