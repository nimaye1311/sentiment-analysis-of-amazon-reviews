library(twitteR)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotrix)
library(readr)

reviews_data <- read_csv("reviews_Musical_Instruments_5.csv")

reviews_data$`review rating` <- reviews_data$`helpful/0` / reviews_data$`helpful/1`
reviews_data_proper <- reviews_data[reviews_data$`review rating` != "NaN",]

View(reviews_data_proper)