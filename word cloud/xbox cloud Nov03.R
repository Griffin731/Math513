library(wordcloud)

library(RColorBrewer)

library(wordcloud2)
library(tidytext)
library(tm)
library(dplyr)
library(tidyr)

xbox_x_english_1 <- xbox_x_english %>% mutate(created_at = as.Date(created_at)) %>% 
  filter(created_at == '2020-11-03')


xbox_x_english_1$stripped_text <- gsub("https\\S*", "", xbox_x_english_1$text) 
xbox_x_english_1$stripped_text <-gsub("@\\S*", "", xbox_x_english_1$stripped_text) 
xbox_x_english_1$stripped_text <-gsub("amp", "", xbox_x_english_1$stripped_text) 

head(xbox_x_english_1$stripped_text)

xbox_clean <- xbox_x_english_1 %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% 
  unnest_tokens(word, stripped_text)
head(xbox_clean)

data("stop_words")
head(stop_words)

nrow(xbox_clean)

xbox_clean_words <- xbox_clean %>%
  anti_join(stop_words)

nrow(xbox_clean_words)

my_stop_words <- data.frame(word = c("xbox", "ps5", "series","time","microsoft","console","week","launch","gaming",
                                     "playstation5","day","days","xboxseriesx","xboxseriess"))
xbox_clean_words_2 <- xbox_clean_words %>%
  anti_join(my_stop_words) 

nrow(xbox_clean_words_2)

xbox_clean_words_3 <- xbox_clean_words_2 %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))
head(xbox_clean_words_3)


with(xbox_clean_words_3, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 500,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2") ))
