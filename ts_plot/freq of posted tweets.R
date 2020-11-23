library(tidyverse)
library(rtweet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(jsonlite)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tidyr)
library(maps)
library(widgetframe)
library(lubridate)
library(ggplot)


playstation_all_lang <- read_csv("playstation_tweets_alllang.csv", col_names = TRUE)
playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_all_lang <-  read_csv("xboxseriesx_tweets_alllang.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)
xbox_s_english <-  read_csv("xboxseriess_tweets_english.csv", col_names = TRUE)


#timeline

ts_plot(playstation_english, "days") +
  theme_minimal() + # white background 
  theme(plot.title = element_text(face = "bold")) + # boldface title
  labs(
    x = NULL, y = NULL, # no labels on the axes
    title = "Frequency of #playstation Twitter statuses From Oct 28 to Nov 05",
    subtitle = "Twitter status (tweet) counts aggregated using 1-days intervals",
    caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(xbox_x_english, "days") +
  theme_minimal() + # white background 
  theme(plot.title = element_text(face = "bold")) + # boldface title
  labs(
    x = NULL, y = NULL, # no labels on the axes
    title = "Frequency of #xbox Twitter statuses From Oct 28 to Nov 05",
    subtitle = "Twitter status (tweet) counts aggregated using 1-days intervals",
    caption = "Source: Data collected from Twitter's REST API via rtweet"
  )

#------------------------------------------------------------------------------
# Those are shits that I can't make them in the same graph
ps <- cbind(playstation_english, name_1 = "PlayStation")
ps$name_1

xbox <- cbind(xbox_x_english, name_1 = "Xbox")
xbox$name_1

combine <- rbind(playstation_english, xbox_x_english)
class(combine$created_at)
class(combine$screen_name)
class(combine$name_1)

# combine %>% mutate(name = group_by(hashtags))

ts.plot(combine, gpars = list())

ts_plot(combine$name_1, "days") +
  theme_minimal() + # white background 
  theme(plot.title = element_text(face = "bold")) + # boldface title
  labs(
    x = NULL, y = NULL, # no labels on the axes
    title = "Frequency of #xbox Twitter statuses From Oct 28 to Nov 05",
    subtitle = "Twitter status (tweet) counts aggregated using 1-days intervals",
    caption = "Source: Data collected from Twitter's REST API via rtweet"
  )


xbox_x_english$created_at  %>% group_by(xbox_x_english) %>% count()

class(combine$created_at)
as.Date(as.POSIXct(combine$created_at,format='%Y%M%D'))
combine$created_at  %>% group_by()%>% count()

combine %>% group_by(created_at, screen_name) %>%
ggplot(aes(x = created_at, y = screen_name)) + 
  geom_line() 


ggplot(combine,aes(x=created_at)) + 
  geom_line(aes(y = (..count..)))

#-----------------------------------------------------------------
dput(head(xbox_x_english))

i1 <- grepl('XboxSeriesX', combine$text)
i2 <- grepl('PlayStation5', combine$text)

combine$Category <- NA

combine$Category[i1] <- 'Xbox'
combine$Category[i2] <- 'PS'
combine

combine$Category

na.omit(combine$Category)

ggplot(combine,aes(x = combine$created_at, y = count(combine$screen_name))) +
  geom_line(data = combine$Category)

ts.plot(combine, gpars = list(combine$Category))