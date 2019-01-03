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

#################################### DF Preparation -- Only need to run once

evdata <- read.spss(file="Norwegian Citizen Panel - wave 10 - EN.sav",
                    use.value.labels=FALSE,
                    to.data.frame=TRUE,
                    trim.factor.names=TRUE)
raw_openansw <- as.data.frame(read.csv2("NCP-text-responses-wave-10.csv", header=TRUE, sep=",", stringsAsFactor=FALSE))

elbil_df <- raw_openansw[, c("responseid", "r10kmelbil")]
colnames(elbil_df) <- c("responseid", "openanswer")
evdata<-merge(evdata, elbil_df,
              by.x="responseid",
              by.y="responseid",
              all.x=TRUE,
              all.y=TRUE
)

# create a word count variable of opensanswer
evdata <- evdata %>%
  mutate(wc = str_count(openanswer, '[\\w]+'))

# Some simple cleaning
evdata$openanswer <- lapply(evdata$openanswer, str_replace_all, pattern = "[Mm]iljøet", replacement = "miljø")
sum(str_count(evdata$openanswer, pattern = 'miljøet')) # checking
# changing to lower case
evdata$openanswer <- lapply(evdata$openanswer, str_to_lower)
# sum(str_count(evdata$openanswer, pattern = '[A-Z]'))

## Are there nynorsk spellings that should/can be standardized? 
# Maybe not... only 12 incidents of ikkje
ikkje <- str_count(evdata$openanswer, '[Ii]kkje')
sum(ikkje)
noreg <- str_count(evdata$openanswer, '[Nn]oreg')
sum(noreg)
# I'm changing 'ikkje' anyway because there might be n-grams including ikke we might want to look at
evdata <- mutate(evdata, openanswer = str_replace(openanswer, "[Ii]kkje", replacement = "ikke"))
## THERE ARE ALSO NYNORSK -EG spellings of adjectives -- need to correct for this??
sum(str_count(evdata$openanswer, '[Mm]iljøvennleg'))
sum(str_count(evdata$openanswer, '[Mm]iljøvennlig'))
## OTHER STEMMING ISSUES: CONVERTING ADJECTIVES to common stems
sum(str_count(evdata$openanswer, '[Mm]iljøvennlige'))
sum(str_count(evdata$openanswer, '[Mm]iljøvennlig'))
# AND...
sum(str_count(evdata$openanswer, '[Bb]illige'))
sum(str_count(evdata$openanswer, '[Bb]illig'))
# and so on...

# Nouns
sum(str_count(evdata$openanswer, '[Bb]il'))
sum(str_count(evdata$openanswer, '[Bb]iler'))

# Stemming -- below

## Making the variables more obvious, and turning numerical values to the their text values
# Gender variable
gen_match <- function(x) {
  if (x == 1)
  {out <- 'Male'}
  else if (x == 2)
  {out <- 'Female'}
  else {out <- NA}
  return(out)
}

evdata$Gender <- sapply(evdata$r10P1, gen_match)
table(evdata$Gender)

# Region variable
region_match <- function(x) {
  if (x == 1)
  {out <- 'Oslo/Akershus'}
  else if (x == 2)
  {out <- 'Østlandet'}
  else if (x == 3)
  {out <- 'Sørlandet'}
  else if (x == 4)
  {out <- 'Vestlandet'}
  else if (x == 5)
  {out <- 'Trøndelag'}
  else if (x == 6)
  {out <- 'Nord-Norge'}
  else {out <- NA}
  return(out)
}

evdata$Region <- sapply(evdata$r10P2, region_match)
table(evdata$Region)

### Age
age_match <- function(x) {
  if (x == 1)
  {out <- '18-25'}
  else if (x == 2)
  {out <- '26-35'}
  else if (x == 3)
  {out <- '36-45'}
  else if (x == 4)
  {out <- '46-55'}
  else if (x == 5)
  {out <- '56-65'}
  else if (x == 6)
  {out <- '66-75'}
  else if (x == 7)
  {out <- '75 år og oppover'}
  else {out <- NA}
  return(out)
}

evdata$Age <- sapply(evdata$r10P5_1, age_match)
table(evdata$Age)

######### Party
party_match <- function(x) {
  if (x == 1)
  {out <- 'KrF'}
  else if (x == 2)
  {out <- 'Høyre'}
  else if (x == 3)
  {out <- 'FrP'}
  else if (x == 4)
  {out <- 'Venstre'}
  else if (x == 5)
  {out <- 'SV'}
  else if (x == 6)
  {out <- 'S'}
  else if (x == 7)
  {out <- 'MDG'}
  else if (x == 8)
  {out <- 'AP'}
  else if (x == 9)
  {out <- 'Rødt'}
  else if (x == 10)
  {out <- 'Stemte ikke'}
  else if (x == 11)
  {out <- 'Ingen stemmerett'}
  else {out <- NA}
  return(out)
}

evdata$Party <- sapply(evdata$r10k3, party_match)
table(evdata$Party)

# Rename r10kmelbil2 -- opinion of elbiler, to something more obvious
evdata$Evopinion <- evdata$r10kmelbil2

# Stemming -- Using tidy to do this so we can 're-nest' texts
evdata_stemmed <- evdata %>%
  unnest_tokens(word, openanswer) %>%
  mutate(stem = wordStem(word, language='no')) %>%
  ungroup()
# re-nesting
evdata_stemmed <- evdata_stemmed %>% 
  group_by(responseid) %>% 
  summarise(openanswer = str_c(stem, collapse = " "))
which(colnames(evdata) == 'openanswer')
evdata_merge <- evdata[,-294]
evdata_stemmed <- merge(evdata_merge, evdata_stemmed,
              by.x="responseid",
              by.y="responseid",
              all.x=TRUE,
              all.y=TRUE
)

# Create feather file and read from here when starting in the future
write_feather(evdata, './evdata_df.feather')
write_feather(evdata_stemmed, './evdata_df_stemmed.feather')

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
  geom_smooth(method=lm)
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
  summarize(total = sum(wc)) 

# other things that might influence/covary with length of answers
evdata %>%
  group_by(r10browsertype) %>%
  summarize(total = sum(wc))
###I had no idea and am slightly disappointed how little used Firefox is in Norway...

# Obviously non-mobile/mobile corellated with how much people write -- 2:1 ration for respondents, 
# but 4:1 in words written.
evdata %>%
  group_by(r10mobil) %>%
  summarize(total = sum(wc))
evdata %>%
  group_by(r10mobil) %>%
  count()

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
  labs(y = 'Word Count', x = 'Positiv/negativ oppfatning om elbiler')
summary(lm(data = pol, wc ~ Evopinion))


########### word frequencies by covariate

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
freq_tesla <- subset(freq_grouped, freq_grouped$feature %in% "tesl")  
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

prop_freq("Evopinion", "snyltere")
prop_freq("Age", "tesla")
prop_freq("Evopinion", 'billig')
prop_freq('Region', 'rekkevidde')

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
corp <- corpus(evdata, text_field = 'openanswer')
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
  labs(x = NULL, y = "Relative frequency") +
  ggtitle("TF-IDF by Opinion of EVs")

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
grid.arrange(plot1net, plot2net, top = 'Female \n Male')

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
l
#############
# Modal verb usage -- nothing very interesting here I don't think.

modals <- function(x) {
  if (x == "måtte")
  {out <- 'må'}
  else if (x == "måttet")
  {out <- 'må'}
  else if (x == "burde")
  {out <- 'bør'}
  else if (x == "burdet")
  {out <- 'bør'}
  else if (x == "kunne")
  {out <- 'kan'}
  else if (x == "kunnet")
  {out <- 'kan'}
  else if (x == "vulle")
  {out <- 'vill'}
  else if (x == "vullet")
  {out <- 'vill'}
  else if (x == "skulle")
  {out <- 'skal'}
  else if (x == "skullet")
  {out <- 'skal'}
  else {out <- ""}
  return(out)
}
# faster and easier
ma <- ' [må]|[måtte]|[måttet] '
boer <-  ' [bør]|[burde]|[burden] '
vil <- ' [vil]|[vulle]|[vullet] '
skal <- ' [skal]|[skulle]|[skullet] '
kan <- ' [kan]|[kunne]|[kunnet]'

dt_words <- evdata %>%
  mutate(ma = str_count(openanswer, ma)) %>%
  mutate(boer = str_count(openanswer, boer)) %>%
  mutate(vil = str_count(openanswer, vil)) %>%
  mutate(skal = str_count(openanswer, skal)) %>%
  mutate(kan = str_count(openanswer, kan))

## modal verbs by opinion of evs
p1 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(ma), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, maP)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using må-')
p2 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(boer), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, maP)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using boer-')
p3 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(vil), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, maP)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using vil-')
p4 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(skal), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, maP)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using skal-')
p5 <- dt_words %>%
  group_by(Evopinion) %>%
  summarize(S = sum(kan), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Evopinion = reorder(Evopinion, maP)) %>%
  ggplot(aes(Evopinion, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using kan-')
grid.arrange(p1, p2, p3, p4, p5)

## modal verbs by age
p1 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(ma), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, maP)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using må-')
p2 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(boer), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, maP)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using boer-')
p3 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(vil), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, maP)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using vil-')
p4 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(skal), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, maP)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using skal-')
p5 <- dt_words %>%
  group_by(Age) %>%
  summarize(S = sum(kan), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Age = reorder(Age, maP)) %>%
  ggplot(aes(Age, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using kan-')
grid.arrange(p1, p2, p3, p4, p5)

## modal verbs by party
p1 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(ma), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, maP)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using må-')
p2 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(boer), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, maP)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using boer-')
p3 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(vil), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, maP)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using vil-')
p4 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(skal), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, maP)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using skal-')
p5 <- dt_words %>%
  group_by(Party) %>%
  summarize(S = sum(kan), words = sum(wc), maP = S/(words/1000)) %>%
  transform(Party = reorder(Party, maP)) %>%
  ggplot(aes(Party, maP)) +
  geom_col()+
  ggtitle('Times per Thousands words using kan-')
grid.arrange(p1, p2, p3, p4, p5)

evdata <- mutate(evdata, modals = modals(openanswer))
evdata$modals <- lapply(evdata$openanswer, modals)
sum(str_count(evdata$modals, 'skal'))
