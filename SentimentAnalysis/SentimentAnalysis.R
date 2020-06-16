library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
install.packages('wordcloud')
install.packages('RColorBrewer')
library(wordcloud)

install.packages('tidytext')
library(tidytext)
##It comprises of sentiment lexicons that are present in the dataset of 'sentiments'.
##It allows to perform efficient text analysis on data.
sentiments

library(janeaustenr)
##It provides with the textual data in the form of books authored by the novelist Jane Austen.
library(stringr)

#Retrieve lexicons
get_sentiments('bing')
##The bing lexicon model classifies the sentiment into a binary category of negative or positive.
##Used bing lexicons to extract the sentiments out of the data.
##Retrieve these lexicons using the get_sentiments()

#Convert the text
tidy_data=austen_books() %>%
    group_by(book) %>%
    mutate(linenumber=row_number(),
           ##Each row contains a single word.
           chapter=cumsum(str_detect(text, regex('^chapter [\\divxlc]',
                                                 ignore_case = T)))) %>%
    ungroup() %>%
    unnest_tokens(word, text)
##It converts the text of books into a tidy format using unnest_tokens()

positive_senti=get_sentiments('bing') %>%
    ##Make use of the bing lexicon to correspond to joy.
    filter(sentiment=='positive')
    ##Implements filter() over the words that correspond to joy.

#Derive the words
tidy_data %>%
    filter(book=='Emma') %>%
    semi_join(positive_senti) %>%
    count(word, sort=T)
##Observe many positive words like 'good','happy','love' etc.

#Calculate the total sentiment
bing=get_sentiments('bing')
Emma_sentiment=tidy_data %>%
    inner_join(bing) %>%
    count(book = 'Emma' , index = linenumber %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    ##Calculate the totla sentiment with spread()
    ##That is, the difference between positive and negative sentiment.
    mutate(sentiment = positive - negative)

#Visualize the words
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
    geom_bar(stat = 'identity', show.legend = TRUE) +
    facet_wrap(~book, ncol = 2, scales = 'free_x')
##Visualize the words present in the book 'Emma' based on its corrosponding positive and negative scores.

#Count the words
counting_words=tidy_data %>%
    inner_join(bing) %>%
    count(word, sentiment, sort = T)
##Count the most common positive and negative words that are present in the novel.
head(counting_words)

#Visualize the sentiment score
counting_words %>%
    filter(n>150) %>%
    mutate(n=ifelse(sentiment=='negative',-n, n)) %>%
    mutate(word=reorder(word,n)) %>%
    ggplot(aes(word,n,fill=sentiment))+
    geom_col()+
    coord_flip()+
    labs(y='Sentiment Score')

#Wordcloud
tidy_data %>%
    inner_join(bing) %>%
    count(word, sentiment, sort=T) %>%
    acast(word~sentiment, value.var = 'n', fill=0) %>%
    comparison.cloud(colors = c('orange','purple'),
                     max.words = 100)
##Delineates the most recurring positive and negative words.
##Using comparison.cloud() to plot both negative and positive words in a single wordcloud.
