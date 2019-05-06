library(quanteda)
library(SnowballC)
library(stm)
library(foreign)
library(gridExtra)
library(feather)
library(tidyverse)
library(tidytext)
library(text2vec)


rm(list=ls(all=TRUE))
## Personalized -- wd path
path <- '~/pCloudDrive/Stellen/UiB - CET/Various/evopinion'
# path <- "/home/ghfc/pCloudDrive/Stellen/UiB - CET/Various/evopinion"
setwd(path)


################################################################
## Start from here generally with already prepared and saved dataframe
################################################################

# stemmed
evdata_stemmed <-read_feather('./evdata_df_stemmed.feather') 
# non-stemmed
evdata <- read_feather('./evdata_df.feather')

##########################
#   Initial tests to see what we have
##########################

# looking at relationship between place on norwegian political spectrum and party voted for in 2017
# Seems to map pretty closely
plot(table(evdata$r10k8_1), xlim = c(0,11))
pol <- evdata[, c('r10k8_1', 'r10k3')]
pol <- filter(pol, r10k3 < 10 & r10k8_1 < 15)
ggplot(pol, aes(x=r10k8_1, y=r10k3)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method=loess)
summary(lm(data = pol, r10k3 ~ r10k8_1))

## Gender
evdata %>%
  filter(Evopinion < 9) %>%
  ggplot(aes(x = Gender, y = Evopinion)) +
  geom_point(position = 'jitter') +
  geom_smooth(method=lm)

evdata %>%
  filter(Evopinion < 9) %>%
  ggplot(aes(Gender, Evopinion)) +
  geom_point(position ='jitter', color = 'light blue') +
  stat_summary(geom = "point", fun.y = mean, position = "dodge", color='red') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = .3, color = 'red') +
  theme_bw()
summary(lm(data = pol, r10k3 ~ r10k8_1))

## is it surprising how weak the relationship is? Or just to me because I'm american?
pol1 <- evdata[, c('r10k8_1', 'Evopinion')]
pol1 <- filter(pol1, Evopinion < 10 & r10k8_1 < 12)
ggplot(pol1, aes(x=r10k8_1, y=Evopinion)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method=lm) +
  ylab("Positiv/negativ oppfatning om elbiler") +
  xlab("Personlig plassering på politisk akse venstre-høyre") +
  ggtitle('Opinion about EVs (low to high) regressed on political leanings (high=right, low=left)')
summary(lm(data = evdata, Evopinion ~ r10k8_1))

# create a word count variable of opensanswer
evdata <- evdata %>%
  mutate(wc = str_count(openanswer, '[\\w]+'))

# hisogram of word counts - looks like a nice geometric distribution
evdata %>%
  ggplot(aes(wc)) +
  geom_histogram(bins = 30) +
  xlim(c(0,110)) +
  ylim(c(0,500))

## total word count: ~25,000
sum(evdata$wc)

## word counts and responses by several categories
# by gender
# word count by gender -- almost exactly equal
evdata %>%
  group_by(r10P1) %>%
  summarize(total = sum(wc)) %>%
  arrange(desc(total))
# response by gender -- also almost exactly equal 
evdata %>%
  group_by(r10P1) %>%
  count()

# wc and response by region - no big differences
evdata %>%
  group_by(r10P2) %>%
  summarize(total = sum(wc))
evdata %>%
  group_by(r10P2) %>%
  count()

# by age -- here there are some differences with total survey respondents
(one <- evdata %>%
    group_by(r10P5_1) %>%
    summarize(total = sum(wc)) %>%
    ggplot(aes(r10P5_1, total)) +
    geom_col())
(two <- evdata %>%
    group_by(r10P5_1) %>%
    count() %>%
    ggplot(aes(r10P5_1, n)) +
    geom_col())
## Plotted together - by age x axis, word count left y-axis and total respondents right y-axis
## 66-75 disproportionate number of words in openanswer
grid.arrange(one, two, ncol=2)

# by education level
evdata %>%
  group_by(r10P4_1) %>%
  mutate(total = sum(wc)) %>%
  summarize(total)


##############################################3
## Ways of putting confidence bars in - Using different, manual way that is just easier

# Summary function, from R cookbook: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

############################################

one <- evdata %>%
  group_by(Age) %>%
  summarize(total = sum(wc)/n()) %>%
  ggplot(aes(Age, total)) +
  geom_col() +
  labs(y = 'Word total by category') +
  theme_bw()

(two <- evdata %>%
    group_by(Age) %>%
    #count() %>%
    summarise(mean = mean(wc),  # calculates the mean of each group
              sd = sd(wc), # calculates the standard deviation of each group
              n = n(),  # calculates the sample size per group
              se = sd(wc)/sqrt(n())) %>% # calculates the standard error of each group
    ggplot(aes(Age, mean)) +
    geom_col() +
    geom_errorbar(aes(ymin = mean - se*1.96, ymax = mean + se*1.96), width=0.2) +
    labs(y = 'Average response length in words') +
    theme_bw())

## Plotted together - by age x axis, word count left y-axis and total respondents right y-axis
## 66-75 disproportionate number of words in openanswer
grid.arrange(one, two, ncol=2)


# other things that might influence/covary with length of answers
# by education level
evdata %>%
  group_by(r10P4_1) %>%
  summarize(total = sum(wc)) 

# other things that might influence/covary with length of answers
evdata %>%
  group_by(r10browsertype) %>%
  summarise(mean = mean(wc),  # calculates the mean of each group
            sd = sd(wc), # calculates the standard deviation of each group
            n = n(),  # calculates the sample size per group
            se = sd(wc)/sqrt(n())) %>% # calculates the standard error of each group
  ggplot(aes(r10browsertype, mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean - se*1.96, ymax = mean + se*1.96), width=0.2) +
  labs(y = 'Average response length in words') +
  theme_bw()
###I had no idea and am slightly disappointed how little used Firefox is in Norway...
###I had no idea and am slightly disappointed how little used Firefox is in Norway...
# Obviously non-mobile/mobile corellated with how much people write -- 2:1 ration for respondents, 
# but 4:1 in words written.

# wc slightly skewed to those with negative opinion compared to total respondents, but this doesn't look 
# too significant
one <- evdata %>%
  group_by(r10kmelbil2) %>%
  summarize(total = sum(wc)) %>%
  ggplot(aes(r10kmelbil2, total)) +
  geom_col() +
  xlim(c(0,8)) +
  labs(y = 'Word Counts', x = '') +
  theme_bw()
two <- evdata %>%
  group_by(r10kmelbil2) %>%
  count() %>%
  ggplot(aes(r10kmelbil2, n)) +
  geom_col() +
  xlim(c(0,8)) +
  labs(y = 'Total Number of Responses', x = '') +
  theme_bw()
grid.arrange(one, two, ncol=2, bottom = 'Opinion on EVs')

# Perhaps most important: those who think negatively of EVs write more
wc_group <- evdata %>%
  group_by(Evopinion) %>%
  summarize(total = sum(wc))
evdata %>%
  group_by(Evopinion) %>%
  count() %>%
  ggplot(aes(Evopinion, wc_group$total/n)) +
  geom_col() +
  ylab("Response length (words) per respondent") +
  xlab('Positiv/negativ oppfatning om elbiler') +
  xlim(c(0,10)) 

# Statistically significant
pol <- evdata[, c('wc', 'Evopinion')]
pol <- filter(pol, Evopinion < 8)
ggplot(pol, aes(x=Evopinion, y=wc)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method=lm) +
  ylim(c(0,100)) +
  labs(y = 'Word Count', x = 'Positiv/negativ oppfatning om elbiler') +
  theme_bw()
summary(lm(data = pol, wc ~ Evopinion)) 


########### word frequencies by covariate

# total word counts
evdata_no97 <- evdata %>%
  filter(Evopinion < 9)
corp <- corpus(evdata_no97, text_field = 'openanswer')
d <- dfm(corp, remove = stopwords('norwegian'), 
         stem = FALSE, remove_punct = TRUE)
dfm <- dfm_weight(d, scheme = 'count')
freq_weight <- textstat_frequency(dfm, n = 30)
ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Frequency") +
  theme_bw()


plot_freq <- function(group, df=evdata_stemmed) {
  c <- which(colnames(df) == group)
  
  if (sum(is.na(df[,c])) > 0) {
    evdata1 <- df[!is.na(df[,c]),]
    corp <- corpus(evdata1, text_field = 'openanswer')
  }
  else {
    corp <- corpus(df, text_field = 'openanswer')
  }
  d <- dfm(corp, remove = stopwords('norwegian'), 
           stem = FALSE, remove_punct = TRUE)
  dfm <- dfm_weight(d, scheme = 'prop')
  freq_weight <- textstat_frequency(dfm, n = 20, groups = group)
  
  ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
    geom_point() +
    facet_wrap(~ group, scales = "free") +
    coord_flip() +
    scale_x_continuous(breaks = nrow(freq_weight):1,
                       labels = freq_weight$feature) +
    labs(x = NULL, y = "Relative frequency") +
    ggtitle(paste("Most frequent words by", group))
}

plot_freq('Region')
plot_freq('Age')
plot_freq('Evopinion')
plot_freq('Party')
plot_freq('Gender')

## Looking more closely though -- still a problem here with stemming, see:
kwic(evdata$openanswer, 'snylt*')
kwic(evdata_stemmed$openanswer, 'tesl*')

## plot certain words across groups

# total word counts
corp <- corpus(evdata, text_field = 'openanswer')
d <- dfm(corp, remove = stopwords('norwegian'), 
         stem = FALSE, remove_punct = TRUE)

freq_grouped <- textstat_frequency(d, groups = "Age")

# Filter the term "tesla"
freq_tesla <- subset(freq_grouped, freq_grouped$feature %in% "tesla")  
# plot ferquency of 'tesla' across age groups
ggplot(freq_tesla, aes(x = group, y = frequency)) +
  geom_bar(stat = 'identity') + 
  #scale_y_continuous(limits = c(0, 14), breaks = c(seq(0, 14, 2)))
  xlab(NULL) + 
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Frequency of the word "Tesla"')


# Proportional word frequency (apperances per 100 words)
prop_freq <- function(var, word) {
  dfm_rel_freq <- dfm_weight(dfm(corp), scheme = "prop")
  rel_freq <- textstat_frequency(dfm_rel_freq, groups = var)
  rel_freq_word <- subset(rel_freq, feature %in% word)
  ggplot(rel_freq_word, aes(x = group, y = frequency)) +
    geom_bar(stat = 'identity') + 
    #scale_y_continuous(limits = c(0, 0.7), breaks = c(seq(0, 0.7, 0.1))) +
    xlab(var) + 
    ylab("Relative frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste('Relative frequency (per 100 words) of the word "', word, '"', sep = ''))
}

a <- prop_freq("Age", "tesla")
b <- prop_freq("Evopinion", 'billig')
c <- prop_freq('Region', 'rekkevidde')
d <- prop_freq('Evopinion', 'snyltere')

grid.arrange(a, b, c, d, ncol=2)

#### selecting just word/phrase and x number of words surrounding it

toks <- tokens(corp)
tesla_group <- tokens_select(toks, phrase('tesl*'), padding=T, window = 10)
tesla_dfm <- dfm(tesla_group, remove = stopwords('norwegian'), 
                                stem = FALSE, remove_punct = TRUE)
tesla_dfm <- dfm_weight(tesla_dfm, scheme = 'prop')
freq_weight <- textstat_frequency(tesla_dfm, n = 20, groups = 'Evopinion')
freq_weight <- freq_weight[-c(1,2,21,22,41,42,61,62,81,82,101,102,121,122),]

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency") +
  ggtitle(paste("Most frequent words occurring together with Tesla by EV opinion category"))

# Compounding tokens so they will still be aroud in bag-of-words analysis
# tokens_compound()
corp1 <- corpus(evdata, text_field = 'openanswer')
toks <- tokens(corp1)
mystop_no <- stopwords('no')[-which(stopwords('no') == 'ikke')]
toks <- tokens_remove(toks, mystop_no)
pattern <- list(c('ikke', '[A-z]*'))
comp_tokens <- tokens_compound(toks, pattern, valuetype = 'regex', concatenator = '_')
comp_tokens <- tokens_select(comp_tokens, 'ikke_[A-z]*', valuetype = 'regex')
ikke_dfm <- dfm(comp_tokens, remove = stopwords('norwegian'), 
                 stem = FALSE, remove_punct = TRUE)
ikke_dfm <- dfm_weight(ikke_dfm, scheme = 'prop')
freq_weight <- textstat_frequency(ikke_dfm, n = 20, groups = 'Evopinion')
ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency") +
  ggtitle(paste("Most frequent non-stopwords in compound with 'ikke'"))


comp_tokens <- tokens_compound(toks, pattern, valuetype = 'regex', concatenator = '_')
d <- dfm(comp_tokens, remove = stopwords('norwegian'), 
         stem = FALSE, remove_punct = TRUE)
dfm <- dfm_weight(d, scheme = 'prop')
group <- 'Evopinion'
freq_weight <- textstat_frequency(dfm, n = 20, groups = group)

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency") +
  ggtitle(paste("Most frequent words by", group))


# TF-IDF
evdat_no97 <- evdata %>%
  filter(Evopinion < 9 & Evopinion > 1)
corp <- corpus(evdat_no97, text_field = 'openanswer')
dfm <- dfm(corp, remove = stopwords('norwegian'), 
              stem = FALSE, remove_punct = TRUE)
tfidf <- dfm_tfidf(dfm, scheme_tf = "prop", scheme_df = "inverse", base = 10)
rel_freq <- textstat_frequency(tfidf, n = 15, groups = 'Evopinion')
ggplot(data = rel_freq, aes(x = nrow(rel_freq):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(rel_freq):1,
                     labels = rel_freq$feature) +
  labs(x = NULL, y = "Relative frequency") 
  #ggtitle("TF-IDF by Opinion of EVs")


plot_tfidf <- function(group, label = '') {
  c <- which(colnames(evdata) == group)
  
  if (sum(is.na(evdata[,c])) > 0) {
    evdata1 <- evdata[!is.na(evdata[,c]),]
    corp <- corpus(evdata1, text_field = 'openanswer')
  }
  else {
    corp <- corpus(evdata, text_field = 'openanswer')
  }
  d <- dfm(corp, remove = stopwords('norwegian'), 
           stem = FALSE, remove_punct = TRUE)
  dfm <- dfm_tfidf(d, scheme_tf = "prop", scheme_df = "inverse", base = 10)
  freq_weight <- textstat_frequency(dfm, n = 20, groups = group)
  
  ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
    geom_point() +
    facet_wrap(~ group, scales = "free") +
    coord_flip() +
    scale_x_continuous(breaks = nrow(freq_weight):1,
                       labels = freq_weight$feature) +
    labs(x = NULL, y = "TF-IDF") +
    ggtitle(label) +
    theme_bw()
}

plot_tfidf('Evopinion')


# similarities between documents
sim_matrix <- function(var) {
  grouped_dfm <- dfm(corp, remove = stopwords('norwegian'), stem = FALSE, remove_punct = TRUE, groups = var)
  c <- which(colnames(evdata) == var)
  l <- names(table(evdata[,c]))
  simil <- textstat_simil(grouped_dfm, l[1:length(l)], margin = 'documents', method = 'cosine')
  return(simil)  
}

sim_matrix('Party')
sim_matrix('Region')
sim_matrix('Age')
sim_matrix('Evopinion')
sim_matrix('Gender')

### Compare to tweets - doc similarity

evdata1 <- evdata %>%
  filter(Evopinion < 9)
corp <- corpus(evdata1, text_field = 'openanswer')
evtweets_stemmed <- read_feather('./evtweets_df_stemmed.feather')
evtweets_stemmed$tweets <- 'tweets'
tw_corp <- corpus(evtweets_stemmed, text_field = 'text')
tw_corp <- tokens(tw_corp, remove_twitter = TRUE, remove_numbers = TRUE, remove_punc = TRUE, 
                  remove_symbols = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
tw_corp <- tokens_remove(tw_corp, pattern = c('rt', 't.co', 'https'))


var <- 'Evopinion'
grouped_dfm <- dfm(corp, remove = stopwords('norwegian'), stem = FALSE, remove_punct = TRUE, groups = var)
var1 <- 'Gender'
grouped1_dfm <- dfm(corp, remove = stopwords('norwegian'), stem = FALSE, remove_punct = TRUE, groups = var1)
var2 <- 'month'
tweets_dfm_bymonth <- dfm(corp, remove = stopwords('norwegian'), stem = FALSE, remove_punct = TRUE, remove_numbers = T, 
                          groups = var2)
#var2 <- 'tweets'
#tweets_dfm_total <- dfm(tw_corp, remove = stopwords('norwegian'), stem = FALSE, remove_punct = TRUE, remove_numbers = T,
groups = var2
merged <- rbind(grouped_dfm, grouped1_dfm, tweets_dfm_bymonth)
#merged <- rbind(grouped_dfm, grouped1_dfm, tweets_dfm_total)
c <- which(colnames(evdata) == var)
c1 <- which(colnames(evdata) == var1)
c2 <- which(colnames(evtweets_stemmed) == var2)
l <- c(names(table(evdata1[,c])), names(table(evdata1[,c1])), names(table(evtweets_stemmed[,c2])))
l <- c(names(table(evdata1[,c])), names(table(evdata1[,c1])), c('Apr', 'May', 'June', 'July', 'Aug', 'Sept'))
#l[10:15] <- c('Apr', 'May', 'June', 'July', 'Aug', 'Sept')
#l <- c(names(table(evdata1[,c])), names(table(evdata1[,c1])), names(table(evtweets_stemmed[,c2])))
out <- textstat_simil(merged, l[1:length(l)], margin = 'documents', method = 'cosine')
#out[12:19,]
#out1 <- out[12:19,c(1,2,3,8,9,10,12,13,14)]
#xtable(as.data.frame(as.matrix(out[12:19,])))
#out1 <- out[nrow(out),1:9]
#xtable(as.data.frame(as.matrix(t(out1))))

out[,10:15]

## Using xtable to get latex code for cos similarity matrix
library(xtable)
xtable(as.data.frame(as.matrix(sim_matrix('Evopinion'))))

xtable(as.data.frame(as.matrix(sim_matrix('Age'))))

xtable(as.data.frame(as.matrix(sim_matrix('Party'))))

# frequency co-occurance matrixes/networks

createnetwork <- function(fcm_freq) {
  feat <- names(topfeatures(fcm_freq, dim(fcm_freq)[1]))
  size <- log(colSums(dfm_select(fcm_freq, feat)))
  textplot_network(fcm_freq, min_freq = 0.8, vertex_size = size / max(size) * 3)
}

var <- 'Region'
c <- which(colnames(evdata) == var)
l <- names(table(evdata[,c]))
plot_list <- list()
for(i in 1:length(l)) {
    sub <- corpus_subset(corp, docvars(corp)[var] == l[i])
    dfm <- dfm(sub, tolower = TRUE, remove = stopwords('norwegian'), remove_punct = TRUE)
    dfm <- dfm_select(dfm, names(topfeatures(dfm, n = 30)))
    fcm <- fcm(dfm, window = 10)
    out <- createnetwork(fcm)
    assign(paste("plot", i, 'net', sep=''), out)
    plot_list[i] <- paste("plot", i, 'net', sep='')
}
## not quite sure how to label these
#They are from top left regions
grid.arrange(plot1net, plot2net, plot3net, plot4net, plot5net, plot6net, nrow = 3,
             top = "Word networks by region: \n Norge-Norge, Oslo/Akershus \n Sørlandet, Trøndelag \n Vestlandet, Østlandet")


var <- 'Gender'
c <- which(colnames(evdata) == var)
l <- names(table(evdata[,c]))
plot_list <- list()
for(i in 1:length(l)) {
  sub <- corpus_subset(corp, docvars(corp)[var] == l[i])
  dfm <- dfm(sub, tolower = TRUE, remove = stopwords('norwegian'), remove_punct = TRUE)
  dfm <- dfm_select(dfm, names(topfeatures(dfm, n = 30)))
  fcm <- fcm(dfm, window = 10)
  out <- createnetwork(fcm)
  assign(paste("plot", i, 'net', sep=''), out)
  plot_list[i] <- paste("plot", i, 'net', sep='')
}
plot1net <- plot1net + 
  ggtitle('Female') +
  theme(plot.title = element_text(hjust = 0.5))
plot2net <- plot2net +
  ggtitle('Male') +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1net, plot2net, ncol = 2)



var <- 'Age'
c <- which(colnames(evdata) == var)
l <- names(table(evdata[,c]))
plot_list <- list()
for(i in 1:length(l)) {
  sub <- corpus_subset(corp, docvars(corp)[var] == l[i])
  dfm <- dfm(sub, tolower = TRUE, remove = stopwords('norwegian'), remove_punct = TRUE)
  dfm <- dfm_select(dfm, names(topfeatures(dfm, n = 30)))
  fcm <- fcm(dfm, window = 10)
  out <- createnetwork(fcm)
  assign(paste("plot", i, 'net', sep=''), out)
  plot_list[i] <- paste("plot", i, 'net', sep='')
}

grid.arrange(plot1net, plot2net, plot3net, plot4net, plot5net, plot6net, plot7net,
             top = '18-25 --- 26-35 --- 36-45 \n 46-55 --- 56-65 --- 66-75 \n 75 år og oppover')

########### THESE TOO SMALL TO GIVE ANYTHING INTERESTING
var <- 'Evopinion'
evdata1 <- evdata[!is.na(evdata[,c]),]
corp1 <- corpus(evdata1, text_field = 'openanswer')
c <- which(colnames(evdata1) == var)
l <- names(table(evdata1[,c]))
plot_list <- list()
for(i in 1:length(l)) {
  sub <- corpus_subset(corp1, docvars(corp1)[var] == l[i])
  dfm <- dfm(sub, tolower = TRUE, remove = stopwords('norwegian'), remove_punct = TRUE)
  dfm <- dfm_select(dfm, names(topfeatures(dfm, n = 30)))
  fcm <- fcm(dfm, window = 30)
  out <- createnetwork(fcm)
  assign(paste("plot", i, 'net', sep=''), out)
  plot_list[i] <- paste("plot", i, 'net', sep='')
}
grid.arrange(plot1net, plot2net, plot2net, plot3net, plot4net, plot5net, plot6net, plot7net, plot8net)

#############
# Modal verb usage -- nothing very interesting here I don't think.

## Not using this, instead use method below
# modals <- function(x) {
#   if (x == "måtte")
#   {out <- 'må'}
#   else if (x == "måttet")
#   {out <- 'må'}
#   else if (x == "burde")
#   {out <- 'bør'}
#   else if (x == "burdet")
#   {out <- 'bør'}
#   else if (x == "kunne")
#   {out <- 'kan'}
#   else if (x == "kunnet")
#   {out <- 'kan'}
#   else if (x == "ville")
#   {out <- 'vill'}
#   else if (x == "villet")
#   {out <- 'vill'}
#   else if (x == "skulle")
#   {out <- 'skal'}
#   else if (x == "skullet")
#   {out <- 'skal'}
#   else {out <- ""}
#   return(out)
# }

# faster and easier
ma <- '(må)|(måtte)|(måttet)'
boer <-  '(bør)|(burde)|(burdet)'
vil <- '(vil)|(ville)|(villet)'
skal <- '(skal)|(skulle)|(skullet)'
kan <- '(kan)|(kunne)|(kunnet)'

dt_words <- evdata %>%
  mutate(ma = str_count(openanswer, ma)) %>%
  mutate(boer = str_count(openanswer, boer)) %>%
  mutate(vil = str_count(openanswer, vil)) %>%
  mutate(skal = str_count(openanswer, skal)) %>%
  mutate(kan = str_count(openanswer, kan))
# small sample, not sure this is significant, but still potentially interesting
dt_words %>%
  group_by(Age) %>%
  select(Party, ma, boer, vil, skal, kan) %>%
  summarize(ma_summ = sum(ma), boer_summ = sum(boer), vil_sum = sum(vil), kan_sum = sum(kan))

## Plot word averages with confidence intervals
## making also columns for modalword/word, for this I'm taking out response length == 0
dt_words <- 
  dt_words %>%
  filter(wc > 0)
maa <- dt_words %>%
    filter(Evopinion < 9) %>%
    group_by(Evopinion) %>%
    summarize(mean = mean(ma),  # calculates the mean of each group
              mean_per = mean(ifelse(wc==0, 0, (ma*100)/wc)),
              se_per = sd(ifelse(wc==0, 0, (100*ma)/wc))/sqrt(n()),
              sd = sd(ma), # calculates the standard deviation of each group
              n = n(),  # calculates the sample size per group
              se = sd(ma)/sqrt(n())) %>%
    mutate(mword = 'må')
boer <- dt_words %>%
  filter(Evopinion < 9) %>%
  group_by(Evopinion) %>%
  summarize(mean = mean(boer),  # calculates the mean of each group
            mean_per = mean(ifelse(wc==0, 0, (boer*100)/wc)),
            se_per = sd(ifelse(wc==0, 0, (100*boer)/wc))/sqrt(n()),
              sd = sd(boer), # calculates the standard deviation of each group
              n = n(),  # calculates the sample size per group
              se = sd(boer)/sqrt(n())) %>%
  mutate(mword = 'bør')
kan <- dt_words %>%
  filter(Evopinion < 9) %>%
  group_by(Evopinion) %>%
  summarize(mean = mean(kan),  # calculates the mean of each group
            mean_per = mean(ifelse(wc==0, 0, (kan*100)/wc)),
            se_per = sd(ifelse(wc==0, 0, (100*kan)/wc))/sqrt(n()),
              sd = sd(kan), # calculates the standard deviation of each group
              n = n(),  # calculates the sample size per group
              se = sd(kan)/sqrt(n())) %>%
  mutate(mword = 'kan')
vil <- dt_words %>%
  filter(Evopinion < 9) %>%
  group_by(Evopinion) %>%
  summarize(mean = mean(vil),  # calculates the mean of each group
            mean_per = mean(ifelse(wc==0, 0, (vil*100)/wc)),
            se_per = sd(ifelse(wc==0, 0, (100*vil)/wc))/sqrt(n()),
              sd = sd(vil), # calculates the standard deviation of each group
              n = n(),  # calculates the sample size per group
              se = sd(vil)/sqrt(n())) %>%
  mutate(mword = 'vil')
skal <- dt_words %>%
  filter(Evopinion < 9) %>%
  group_by(Evopinion) %>%
  summarize(mean = mean(skal),  # calculates the mean of each group
            mean_per = mean(ifelse(wc==0, 0, (skal*100)/wc)),
            se_per = sd(ifelse(wc==0, 0, (100*skal)/wc))/sqrt(n()),
              sd = sd(skal), # calculates the standard deviation of each group
              n = n(),  # calculates the sample size per group
              se = sd(skal)/sqrt(n())) %>%
  mutate(mword = 'skal')
merged <- rbind(maa, boer, kan, vil, skal)
# to dodge ci bars
pd <- position_dodge(0.1)
merged %>%
  ggplot(aes(x = Evopinion, y= mean, group = mword, color = mword)) +
    geom_point(position=pd) +
    geom_line(position=pd) +
    geom_errorbar(aes(ymin = mean - se*1.96, ymax = mean + se*1.96), width=0.2, position=pd) +
    labs(y = 'Average response length in words') +
    theme_bw()
# less busy version of same thing  
rbind(boer, kan) %>%
  ggplot(aes(x = Evopinion, y= mean, group = mword, color = mword)) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin = mean - se*1.96, ymax = mean + se*1.96), width=0.2, position=pd) +
  labs(y = 'Average appearace of modal verb per response') +
  theme_bw()
# per words
rbind(boer, kan) %>%
  ggplot(aes(x = Evopinion, y= mean_per, group = mword, color = mword)) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin = mean_per - se_per*1.96, ymax = mean_per + se_per*1.96), width=0.2, position=pd) +
  labs(y = 'Average appearace of modal verb per word/response') +
  theme_bw()


## modal verbs by opinion of evs
p1 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(ma), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, Evopinion)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using må-')
p2 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(boer), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, Evopinion)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using boer-')
p3 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(vil), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, Evopinion)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using vil-')
p4 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(skal), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, Evopinion)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using skal-')
p5 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(kan), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, Evopinion)) %>%
  ggplot(aes(factor(Evopinion), maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using kan-')
grid.arrange(p1, p2, p3, p4, p5)

## modal verbs by age
p1 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(ma), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, Age)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using må-')
p2 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(boer), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, Age)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using boer-')
p3 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(vil), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, Age)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using vil-')
p4 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(skal), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, Age)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using skal-')
p5 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(kan), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, Age)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using kan-')
grid.arrange(p1, p2, p3, p4, p5)

## modal verbs by party
p1 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(ma), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, Party)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using må-')
p2 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(boer), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, Party)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using bør-')
p3 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(vil), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, Party)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using vil-')
p4 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(skal), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, Party)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using skal-')
p5 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(kan), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, Party)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using kan-')
grid.arrange(p1, p2, p3, p4, p5)


# THE SAME THING AS ABOVE WITH TIDYTEXT
##### GENDER
wf_gender <- evdata %>%
  unnest_tokens(word, openanswer) %>%
  filter(!(word %in% stopwords('norwegian'))) %>%
  count(r10P1, word, sort = TRUE) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ungroup()

wf_gender_m <- wf_gender %>%
  filter(r10P1 == 1) %>%
  top_n(20, word) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  ylim(c(0,450)) +
  labs(x = NULL, y = "male n") +
  coord_flip()
wf_gender_f <- wf_gender %>%
  filter(r10P1 == 2) %>%
  top_n(20, word) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  ylim(c(0,450)) +
  labs(x = NULL, y = "female n") +
  coord_flip()
grid.arrange(wf_gender_m, wf_gender_f, ncol = 2)

#tf-idfs
tfidf_gender <- evdata %>%
  unnest_tokens(word, openanswer) %>%
  filter(!(word %in% stopwords('norwegian'))) %>%
  count(r10P1, word, sort = TRUE) %>%
  bind_tf_idf(word, r10P1, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ungroup()

tfidf_gender_m <- tfidf_gender %>%
  filter(r10P1 == 1) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "male tf-idf") +
  coord_flip()
tfidf_gender_f <- tfidf_gender %>%
  filter(r10P1 == 2) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "female tf-idf") %>%
  coord_flip()
grid.arrange(tfidf_gender_m, tfidf_gender_f, ncol = 2)


######################## POL PARTY
wf_polparty <- evdata %>%
  unnest_tokens(word, openanswer) %>%
  filter(!(word %in% stopwords('norwegian'))) %>%
  count(Party, word, sort = TRUE) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ungroup()

hoyre <- wf_polparty %>%
  filter(Party == 'Høyre') %>%
  top_n(20, word) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = Party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Høyre n") +
  coord_flip()


wf_create(Party, 'SV')

wf_ <- wf_gender %>%
  filter(r10P1 == 1) %>%
  top_n(20, word) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  ylim(c(0,450)) +
  labs(x = NULL, y = "male n") +
  coord_flip()
wf_gender_f <- wf_gender %>%
  filter(r10P1 == 2) %>%
  top_n(20, word) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  ylim(c(0,450)) +
  labs(x = NULL, y = "female n") +
  coord_flip()
grid.arrange(wf_gender_m, wf_gender_f, ncol = 2)

tfidf_gender %>%
  bind_tf_idf(word, r10P1, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_tfidf %>% 
  group_by(r10P1) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~r10P1, ncol = 2, scales = "free") +
  coord_flip()


plot_gender %>% 
  group_by(r10P1) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = r10P1)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~r10P1, ncol = 2, scales = "free") +
  coord_flip()

