#http://www.modsimworld.org/papers/2015/Natural_Language_Processing.pdf

library(NLP)
library(dplyr)
library(readtext)
library(tm)
library(stringi)
library(stringr)
library(lattice)
library(ggplot2)
library(ddalpha)
library(caret)
library(LaF)
library(tidytext)
library(tidyr)
library(ff)
library(knitr)
library(cowplot)
library(kableExtra)
library(filehash)
library(readr)
library(data.table)
library(xgboost)
library(class)
library(e1071)


#read in data sets
setwd("C:/Users/hlevy/Documents/R/SwiftKey/final/en_US")
blogLines <- determine_nlines("C:/Users/hlevy/Documents/R/SwiftKey/final/en_US/en_US.blogs.txt")
blogs2Text <- read.table("en_US.blogs.txt", fill = TRUE, nrows = 899288, sep = "")
blogs2Text <- saveRDS(blogs2Text, "blogs2Text.RDS")
blogs2Text <- read_rds("blogs2Text.RDS")
newsLines <- determine_nlines("C:/Users/hlevy/Documents/R/SwiftKey/final/en_US/en_US.news.txt")
news2Text <- read.table("en_US.news.txt", fill = TRUE, nrows = 1010242, sep = "", header=F,encoding="latin1", quote="", comment='')
news2Text <- saveRDS(news2Text, "news2Text.RDS")
news2Text <- read_rds("news2Text.RDS")
twitterLines <- determine_nlines("C:/Users/hlevy/Documents/R/SwiftKey/final/en_US/en_US.twitter.txt")
twitter2Text <- read.table("en_US.twitter.Rdata", fill = TRUE, nrows = 2360148, sep = "", header=F,encoding="latin1", quote="", comment='')
twitter2Text <- saveRDS(twitter2Text, "twitter2Text.RDS")
twitter2Text <- read_rds("twitter2Text.RDS")

#create exploratory, tokenized data sets
set.seed(123)
exploreBlogs2Text <- sample_frac(blogs2Text, size = .2, replace = FALSE)
exploreBlogs2Text <- as_tibble(gather(exploreBlogs2Text, key = "rowNumber", value = "text"))
set.seed(234)
exploreNews2Text <- sample_frac(news2Text, size = .2, replace = FALSE)
exploreNews2Text <- as_tibble(gather(exploreNews2Text, key = "rowNumber", value = "text"))
set.seed(345)
exploreTwitter2Text <- sample_frac(twitter2Text, size = .2, replace = FALSE)
exploreTwitter2Text <- as_tibble(gather(exploreTwitter2Text, key = "rowNumber", value = "text"))

#clean up data, all lower case, eliminate numbers, punctuation inside sentence, but not end of sentence
exploreBlogs2Text <- exploreBlogs2Text %>% dplyr::mutate(text = tolower(text))
exploreBlogs2Text <- exploreBlogs2Text %>% dplyr::filter(!is.na(text))
exploreBlogs2Text <- exploreBlogs2Text %>% dplyr::mutate(str_extract(text, "[a-z']+"))
#exploreBlogs2Text <- gsub(",", "", exploreBlogs2Text)
#exploreBlogs2Text <- gsub(";", "", exploreBlogs2Text)
#exploreBlogs2Text <- gsub("-", "", exploreBlogs2Text)
#exploreBlogs2Text <- gsub(":", "", exploreBlogs2Text)
#exploreBlogs2Text <- gsub("()", "", exploreBlogs2Text)
#exploreBlogs2Text <- gsub("$", "", exploreBlogs2Text)

head(exploreBlogs2Text)

exploreNews2Text <- exploreNews2Text %>% dplyr::mutate(text = tolower(text))
exploreNews2Text <- exploreNews2Text %>% dplyr::filter(!is.na(text))
exploreNews2Text <- exploreNews2Text %>% dplyr::mutate(str_extract(text, "[a-z']+"))

head(exploreNews2Text)

exploreTwitter2Text <- exploreTwitter2Text %>% dplyr::mutate(text = tolower(text))
exploreTwitter2Text <- exploreTwitter2Text %>% dplyr::filter(!is.na(text))
exploreTwitter2Text <- exploreTwitter2Text %>% dplyr::mutate(str_extract(text, "[a-z']+"))
head(exploreTwitter2Text)


#rbind the three data sets together
explore3 <- rbind(exploreBlogs2Text, exploreNews2Text, exploreTwitter2Text)

explore3a <- saveRDS(explore3, "explore3.RDS")
explore3a <- readRDS("explore3.RDS")
explore3a
        
#build two word n-grams
explore3bigrams <- explore3a %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
explore3bigrams %>% count(bigram, sort = TRUE)

#build three word n-grams
explore3trigrams <- explore3a %>% unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2, word3, sort =TRUE)
explore3trigrams

#build five word n-grams
explore3pentagrams <- explore3a %>% unnest_tokens(pentagram, text, token = "ngrams", n = 5) %>%
        separate(pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>%
        count(word1, word2, word3, word4, word5, sort = TRUE)
tail(explore3pentagrams)

#creating a filehash database
dbCreate("explore3trigrams.db")
summary("explore3trigrams.db")
trigrams3db <- dbInit("explore3trigrams.db")
trigrams3adb <- dbInsert(trigrams3db, "explore3trigrams")
dbFetch(trigrams3db)

#get rid of 2 word n-grams where the words repeat - (fail)
bigrams_separated <- explore3bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>% filter(word1 == word2)
bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
bigram_counts

        
ques1 <- "and a case of"

#Prediction model 1 - xgboost (fail)
numberOfClasses <- length(unique(explore3pentagrams))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

wordPredict1 <- xgb.cv(params = explore3pentagrams, data = explore3bigrams, 
                       nrounds = nround, 
                       label = c("word1", "word2", "word3", "word4", "word5"),
                       nfold = cv.nfold,
                       verbose = FALSE,
                       prediction = TRUE)
wordPredict1(wordPredict1, newdata = ques1)

#Prediction model 2 - knn, nearest neighbor (fail)
# Column bind category (known classification)
explore3bigrams <- DocumentTermMatrix(explore3bigrams)
explore3bigrams <- as.data.frame(data.matrix(explore3bigrams), stringsAsfactors = FALSE)
explore3bigrams <- explore3bigrams %>% dplyr::mutate(explore3bigrams$category)
colnames(explore3bigrams)[ncol(explore3bigrams)] <- "category"
# Isolate classifier
cl <-explore3bigrams[, "category"]
train <- sample(nrow(explore3bigrams), ceiling(nrow(explore3bigrams) * .50))
test <- (1:nrow(explore3bigrams))[- train]
head(train)
head(explore3bigrams)
# Create model: training set, test set, training set classifier
knn.pred <- knn(explore3bigrams[train, ], explore3bigrams[test, ], cl[train])

#Prediction model 3 - naive Bayes
nB_model <- naiveBayes(explore3pentagrams, laplace = 0, subset, na.action = na.pass)

table(predict(ques1))

