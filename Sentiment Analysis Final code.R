require(dsEssex)
library(dsEssex)
library(tidytext)
library(tidyverse)
library(dplyr)
install.packages("ggrepel")
install.packages("stopwords")
library(stopwords)
install.packages("ggplot2")
library(ggplot2)
data("ted_talks")
head(ted_talks)
library(ted_talks)
MyData <- ted_talks %>%
  filter(speaker %in% c("Eric Whitacre", "Dan Pallotta"))
#converting the text into tokens
tidy_talks <-MyData %>% unnest_tokens(word, text)
tidy_talks


#Identification of top words for certain speakers
Whitacre_words <- ted_talks_nonstop %>% #getting data of ted talks 
  filter(speaker == "Eric Whitacre") %>% #filtering data of Eric Whitacre
 count(speaker, word, sort = TRUE) #Counting the words of the speaker
Whitacre_words #checking the outcome of Eric Whitacre words

Pallotta_words <- ted_talks_nonstop %>% #getting data of ted talks 
  filter(speaker == "Dan Pallotta") %>% #filtering data of Dan Pallotta
  count(speaker, word, sort = TRUE) #Counting the number of words of the respective speaker
Pallotta_words #checking the outcome of Dan Pallotta words

#Visualisation of top words
Pallotta_words %>% #Visualisation of top words of Dan Pallotta
  slice_max(n, n = 25) %>%  # slicing the top n words here it is 25
  mutate(word = reorder(word, n)) %>% #amending or arranging the words using mutate
 ggplot(aes(n, word)) + geom_col()  #plotting the words in the form of bars
#Visualisation of top words for a different speaker
Whitacre_words %>% 
  slice_max(n, n = 25) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) + geom_col()  


library(ggrepel) 
#Comparing speaker words using visualisation
bind_rows(Pallotta_words, Whitacre_words) %>% 
  group_by(word) %>% 
  filter(sum(n) > 10) %>% 
  ungroup() %>%  
  pivot_wider(names_from = "speaker", values_from = "n", values_fill = 0) %>%  #to get output of the data as a wide table form
  ggplot(aes(`Eric Whitacre`, `Dan Pallotta`)) +  
  geom_abline(color = "black", size = 1.2, alpha = 0.75, lty = 3) + #draws a line separating both the speakers words
  geom_text_repel(aes(label = word), max.overlaps = 40) +  # controlling the overlap of words in the plot
  coord_fixed()

#Association between the sentiments of two speakers

sentiments <- bind_rows(Pallotta_words, whitacre_words) %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(speaker, sentiment) %>%
  pivot_wider(names_from = speaker, values_from = n, values_fill = 0)

sentiments

logodds_ratio <- sentiments %>% 
  mutate(OR = dsEssex::compute_OR(`Dan Pallotta`, `Eric Whitacre`, 
    correction = FALSE), log_OR = log(OR),sentiment = reorder(sentiment, log_OR))

logodds_ratio

logodds_ratio %>%
  ggplot(aes(sentiment, log_OR, fill = log_OR < 0)) +
  geom_col(show.legend = FALSE) +
  ylab("Log odds ratio") + ggtitle("The association between sentiments and Log odds ratio") +
  coord_flip() + 
  scale_fill_manual(name = "", values = c("darkgreen", "red"))





