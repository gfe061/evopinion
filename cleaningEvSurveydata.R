library(readtext)
require(SnowballC)
library(foreign)
library(feather)
library(tidyverse)
rm(list=ls(all=TRUE))


## Personalized -- wd path
datapath <- "/home/ghfc/pCloudDrive/Stellen/UiB - CET/Various/evopinion"

setwd(datapath)


## Script for preparing and cleaning EV df
# saved as two feather dfs

#################################### DF Preparation -- Only need to run once

# read in dataset from SPSS format and merge with elbil open answer
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
#evdata$openanswer <- lapply(evdata$openanswer, str_replace_all, pattern = "[Mm]iljøet", replacement = "miljø")
#sum(str_count(evdata$openanswer, pattern = 'miljøet')) # checking
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
