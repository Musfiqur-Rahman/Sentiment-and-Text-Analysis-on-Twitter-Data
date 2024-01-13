# read in the libraries
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(NLP)
library (dplyr)
library (ggplot2)
library(RColorBrewer)
library (wordcloud)


# read in our data
twitter_data <- read_csv(file.choose())

twitter_review <- c(twitter_data$review,twitter_data$rating) #only store review data

view(twitter_review)

head(twitter_review,10) #see the stored first 10 data

typeof(twitter_review)  #verify the data type

user_reviews <- twitter_data %>%
  mutate(length = str_length(review),
         npunct = str_count(review, "[[:punct:]]" ),
         nword = str_count(review, "\\w+" ))
head(user_reviews,20)

# checking the distribution
user_reviews%>%
  ggplot( aes(x=rating)) +
  ggtitle( "Sentiment of Rating distribution" ) +
  geom_histogram(bins = 10 )

user_reviews %>%ggplot( aes(x=length)) +
  ggtitle( "Word length distribution" ) +
  geom_density(fill= "#59c3d2" , color= "#e9ecef" , alpha= 0.8 )
user_reviews %>%
  ggplot( aes(x=nword)) +
  ggtitle( "Number of Words distribution" ) +
  geom_density(fill= "#36d3a2" , color= "#e9ecef" , alpha= 0.8 )


#tokenization
review_Tokens <- user_reviews %>%
  unnest_tokens(word,review)

head(review_Tokens, 10 )

#Stop Words & Top Words

# stop words
print(stop_words)

# before removing stop words
user_reviews %>%
  unnest_tokens(word, review) %>%
  dplyr:: count(word,sort = TRUE ) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head( 20 ) %>%
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat= 'identity' ,colour= "white", fill= "orange") +
  geom_text(aes(x = word, y = 1 , label = paste0( "(" ,n, ")" ,sep= "" )),
            hjust= 0 , vjust= .5 , size = 4 , colour = 'black' ,
            fontface = 'bold' ) +
  labs(x = 'Word' , y = 'Word Count' ,
       title = 'Top 20 most Common Words with Stop word' ) +
  coord_flip() +
  theme_bw()

# after filtering stop words
user_reviews %>%
  unnest_tokens(word, review) %>%
  filter(!word %in% stop_words$word) %>%
  dplyr:: count(word,sort = TRUE ) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head( 20 ) %>%
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat= 'identity' ,colour= "white" , fill ="orange") +
  geom_text(aes(x = word, y = 1 , label = paste0( "(" ,n, ")" ,sep= "" )),
            hjust= 0 , vjust= .5 , size = 4 , colour = 'black' ,
            fontface = 'bold' ) +
  labs(x = 'Word' , y = 'Word Count' ,
       title = 'Top 20 most Common Words' ) +
  coord_flip() +
  theme_bw()


#word cloud for all data

user_reviews %>%
  unnest_tokens(word, review) %>%
  filter(!word %in% stop_words$word) %>%
  dplyr::count(word,sort = TRUE ) %>%
  ungroup() %>%
  head( 50 ) %>%
  with(wordcloud(word, n, max.words = 500 ,colors=brewer.pal( 8 , "Dark2" )))

#word cloud for rating values 5
user_reviews %>%
  filter(rating == 5 ) %>%
  unnest_tokens(word, review) %>%
  filter(!word %in% stop_words$word) %>%
  dplyr::count(word,sort = TRUE ) %>%
  ungroup() %>%
  head( 50 ) %>%
  with(wordcloud(word, n, max.words = 500 ,colors=brewer.pal( 8 , "Dark2" )))

#word cloud for rating values 2
user_reviews %>%
  filter(rating == 2 ) %>%
  unnest_tokens(word, review) %>%
  filter(!word %in% stop_words$word) %>%
  dplyr::count(word,sort = TRUE ) %>%
  ungroup() %>%
  head( 50 ) %>%
  with(wordcloud(word, n, max.words = 500 ,colors=brewer.pal( 8 , "Dark2" )))

