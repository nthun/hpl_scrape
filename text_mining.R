# Scraping and processing and analysing the complete works of H. P. Lovecraft 
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(ggrepel)

hpl_data <- read_csv("hpl_all_works.csv")

# Check the number of stories
hpl_data %>% distinct(title) %>% pull()

# Analyse just one story first
alchemist <- 
        hpl_data %>% 
        filter(title == "The Alchemist") %>% 
        select(-link)

# Get all word frequencies
alhemist_words <-
    alchemist %>% 
    unnest_tokens(word, text, token = "words") %>% 
    anti_join(stop_words, by = "word") %>% 
    count(word, sort = TRUE)

# Create a pretty wordcloud
wordcloud2(alhemist_words)

# Sentiment analysis by sentence

# There are four sentiment libraries in the tidytext package
# 1. afinn is a weighted sentiment value <num>
# 2. bing is for positive/negative sentiments <chr>
# 3. nrc contains all basic emotions + positive/negative <chr> 
# 4. positive/negative + uncertainty/litigious/constraining/superfluous <chr> 

get_sentiments("bing") %>% distinct(sentiment)

# Make an analysis of all sentences
alchemist_sentences <-
    alchemist %>% 
    unnest_tokens(sentence, text, token = "sentences") %>% 
    mutate(sentence_number = row_number()) %>% 
    unnest_tokens(word, sentence, token = "words", drop = FALSE) %>% 
    anti_join(stop_words, by = "word") %>% 
    left_join(get_sentiments("afinn"), by = "word") %>% 
    group_by(sentence_number) %>% 
    summarise(sentence = first(sentence), 
              sentence_sentiment = sum(score, na.rm = TRUE)) %>% 
    mutate(valence = case_when())

horrible_sentences <-
    alchemist_sentences %>% 
    filter(sentence_sentiment <= -10)
    

# Plot the sentence sentiments
ggplot(alchemist_sentences) +
    aes(x = sentence_number, y = sentence_sentiment) +
    geom_line() +
    geom_point(aes(color = sentence_sentiment >= 0)) +
    geom_smooth()




        
