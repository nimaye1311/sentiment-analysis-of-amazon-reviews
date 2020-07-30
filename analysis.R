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
library(textdata)
library(tidyr)
library(viridis)
library(viridisLite)
library(party)
library(randomForest)

reviews_data <- reviews_Musical_Instruments_5 

reviews_data$`review rating` <- reviews_data$`helpful/0` / reviews_data$`helpful/1`
reviews_data_proper <- reviews_data[reviews_data$`review rating` != "NaN",]

reviewText <- reviews_data_proper$reviewText

reviews_data_proper <- mutate(reviews_data_proper, sentiment = NA)
reviews_data_proper <- mutate(reviews_data_proper, length = NA)

reviewTextCorpus <- Corpus(VectorSource(reviewText))

reviewTDM <- TermDocumentMatrix(reviewTextCorpus, control = list(
    removePunctuation = TRUE,
    stopwords = c(stopwords("en"), "one", "get", "bit", "really"),
    removeNumbers = TRUE, tolower = TRUE
))

reviewTDMMatrix <- as.matrix(reviewTDM)

reviewDTMMatrix <- t(reviewTDMMatrix)

reviewDTMMatrix <- data.frame(reviewDTMMatrix)

reviewWordFreq <- sort(rowSums(reviewTDMMatrix), decreasing=TRUE)

reviewWordFreq <- data.frame(word = names(reviewWordFreq), freq = reviewWordFreq)

wordcloud(reviewWordFreq$word, reviewWordFreq$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

sents <- get_sentiments("afinn")

reviewWordFreq <- mutate(reviewWordFreq, sentiment = NA)

for (i in 1:length(reviewWordFreq$freq)) {
    for (j in 1:length(sents$word)) {
        if (reviewWordFreq$word[i] == sents$word[j]) {
            reviewWordFreq$sentiment[i] <- sents$value[j]
        }
    }
}

reviewWordFreq$sentiment <- as.factor(reviewWordFreq$sentiment)

overall_sentiment <- sum(as.numeric(reviewWordFreq$sentiment[!is.na(reviewWordFreq$sentiment)]))

barplot(overall_sentiment, main = "Overall sentiment for amazon musical instruments reviews", xlab = as.character(overall_sentiment))

popular_words <- na.omit(reviewWordFreq[reviewWordFreq$freq >= 200,])

ggplot(popular_words, aes(x = word, y = freq, fill = as.factor(sentiment))) + 
    geom_col() + 
    xlab("Word") +
    ylab("Frequency") + 
    labs("fill" = "Sentiment") 

nas = is.na(reviewWordFreq$sentiment)
sents_lib_words <- as.vector(reviewWordFreq$word[!nas])
sents_lib_sents <- as.vector(reviewWordFreq$sentiment[!nas])
sents_lib <- cbind(word = sents_lib_words, sentiment = sents_lib_sents)
sents_lib <- as.data.frame(sents_lib)
na_sents_lib <- as.vector(reviewWordFreq$word[nas])
reviews_data_proper$length <- sapply(strsplit(reviews_data_proper$reviewText, " "), length)

for (k in 1:nrow(reviews_data_proper)) {
    text <- reviews_data_proper$reviewText[k]
    corpus_text <- Corpus(VectorSource(text))
    TDM <- as.matrix(TermDocumentMatrix(corpus_text, control = list(
        removePunctuation = TRUE,
        stopwords = c(stopwords("en"), "one", "get", "bit", "really", na_sents_lib),
        removeNumbers = TRUE,
        tolower = TRUE
    )))
    word_freq <- sort(rowSums(TDM), decreasing=TRUE)
    word_freq <- mutate(data.frame(word = names(word_freq), freq = word_freq), 
                        sentiment = NA, total_sentiment = NA)
    if (nrow(word_freq) != 0) {
        for (i in 1:nrow(word_freq)) {
            for (j in 1:nrow(sents_lib)) {
                if (word_freq$word[i] == sents_lib$word[j]) {
                    word_freq$sentiment[i] <- sents_lib$sentiment[j]
                    word_freq$total_sentiment[i] <- as.numeric(word_freq$sentiment[i]) * word_freq$freq[i]
                }
            }
        }
        reviews_data_proper$sentiment[k] <- sum(word_freq$total_sentiment)
    } else {
        reviews_data_proper$sentiment[k] <- 0
    }
}

sum(reviews_data_proper$sentiment)

reviews_data_proper$helpful <- ifelse(reviews_data_proper$`review rating` >= 0.5,
                                      "Yes", "No")

reviews_data_proper$helpful <- as.factor(reviews_data_proper$helpful)

ggplot(reviews_data_proper, aes(sentiment, `review rating`, col = `length`)) + 
    geom_jitter() +
    geom_smooth(method = "lm", formula = y ~ x) + 
    scale_color_gradientn(colors = c("yellow", "orange", "red", "maroon", "darkred"))

ggplot(reviews_data_proper, aes(sentiment, `length`, col = helpful)) + 
    geom_jitter() +
    geom_smooth(method = "lm", formula = y ~ x) 

training_set <- reviews_data_proper[sample(nrow(reviews_data_proper), 0.8 * nrow(reviews_data_proper)),]
testing_set <- reviews_data_proper[-sample(nrow(reviews_data_proper), 0.8 * nrow(reviews_data_proper)),]

model.ctree <- ctree(`helpful` ~ overall + sentiment + 
                         length, data = training_set)

testing_set$ctree.predictions <- predict(model.ctree, testing_set)

# All values were reported to be yes by the predict() function

# Confusion matrix
conf.mat.ctree <- table(testing_set$ctree.predictions, testing_set$helpful, 
                        dnn = c("Predicted", "Actual") )

# Error Rate = 102/693*100 = 14.72%

model.randomForest <- randomForest(`helpful` ~ overall + sentiment + 
                                        length, data = training_set, 
                                    ntree = 2000)

testing_set$randomForest.predictions <- predict(model.randomForest, testing_set)

conf.mat.forest <- table(testing_set$randomForest.predictions, testing_set$helpful,
                         dnn = c("Predicted", "Actual"))
plot(model.randomForest)

# Error Rate = 91/693*100 = 13.13% -> No difference with 500 trees 
