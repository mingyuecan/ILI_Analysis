## Load required library
library(tm)
library(RColorBrewer)
library(dplyr)
library(textstem)
library(qdap)
library(wordcloud)
library(RWeka)
library(ggplot2)

## Load data
setwd("/Users/Constance/Desktop/ILI_Qualitative")
sk<- read.csv("Self-Knowledge.txt", header = TRUE, stringsAsFactors = FALSE)

## Clean data
sk<-lemmatize_strings(sk[,1])

qdap_clean <- function(x) {
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_number(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  return(x)
}

tm_clean <- function(corpus) {
  corpus<- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords,
                   stopwords("en"))
  return(corpus)
}

sk_c<-qdap_clean(sk)

sk_corpus<-VCorpus(VectorSource(sk_c))

sk_corp<-tm_clean(sk_corpus)


## Unigram word analysis
sk_tdm<-TermDocumentMatrix(sk_corp, control = list(minWordLength = 1))
sk_tdm_m<-as.matrix(sk_tdm)

sk_freq<-sort(rowSums(sk_tdm_m), decreasing = TRUE)

sk_freq_df = data.frame(word=names(sk_freq), freq=sk_freq)

rownames(sk_freq_df) <- NULL 

sk_freq_df[sk_freq_df$freq>=25,]


## Unigram wordcloud
wordcloud(words=names(sk_freq), freq=sk_freq, max.words=50, random.order = FALSE, scale = c(2.5, 0.8),colors=brewer.pal(8, "Set1"))


## Frequent word plot
ggplot(head(sk_freq_df,30), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity", fill="dodgerblue2") + coord_flip() +
  xlab("Unigrams") + ylab("Frequency") +
  ggtitle("Most frequent uigrams")

## Bigram words analysis
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

sk_bi_tdm<-TermDocumentMatrix(sk_corp, control = list(tokenize = BigramTokenizer))

sk_bi_tdm_m<-as.matrix(sk_bi_tdm)

sk_bi_freq<-sort(rowSums(sk_bi_tdm_m), decreasing = TRUE)

sk_bi_freq_df = data.frame(word=names(sk_bi_freq), freq=sk_bi_freq)

rownames(sk_bi_freq_df) <- NULL 

head(sk_bi_freq_df, 15)

## Bigram wordcloud 
wordcloud(words=names(sk_bi_freq), freq=sk_bi_freq,max.words=25, scale = c(2.5, 0.8),colors=brewer.pal(8, "Set1"))

## Frequent word plot
ggplot(head(sk_bi_freq_df,20), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity",fill="dodgerblue2") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")
