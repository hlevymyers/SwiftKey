
library(ngram)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)

#read in data
setwd("C:/Users/hlevy/Documents/R/SwiftKey/final/en_US")
blogLines5 <- readLines("en_US.blogs.txt")
newsLines5 <- readLines("en_US.news.txt")
twitterLines5 <- readLines("en_US.twitter.txt")

blogLines5 <- as.data.frame.character(blogLines5)
newsLines5 <- as.data.frame.character(newsLines5)
twitterLines5 <- as.data.frame.character(twitterLines5)

#creating smaller sample to work with
blogsample <- sample_n(blogLines5, size = 10000, replace = FALSE)
newssample <- sample_n(newsLines5, size = 10000, replace = FALSE)
twittersample <- sample_n(twitterLines5, size = 10000, replace = FALSE)

#combining three data sets to one
allText5 <- cbind(blogsample, newssample, twittersample)
allText5 <- concatenate(allText5)
head(allText5)

#concatenating multiple strings ###at this point, drops words and just row numbers. 
blogLines5 <- ngram::concatenate(blogsample)
newsLines5 <- ngram::concatenate(newssample)
twitterLines5 <- ngram::concatenate(twittersample)

#cleaning up data
blogLines5 <- preprocess(blogLines5)
newsLines5 <- preprocess(newsLines5)
twitterLines5 <- preprocess(twitterLines5)

#combining three datasets into one
alltexts5 <- concatenate(blogLines5, newsLines5, twitterLines5)
alltexts5 <- preprocess(alltexts5)
string.summary(alltexts5)

#saving in RDS format
alltexts5ng <- saveRDS(alltexts5, "alltexts5.RDS")
alltexts5ng <- readRDS("alltexts5.RDS")
string.summary(alltexts5ng)

alltexts5ng <- ngram(alltexts5, n=2)
alltexts5ng

alltexts5ng3 <- ngram(alltexts5, n=3)
alltexts5ng3

ques1 <- "and a case of"
get.string(ngram("case of", n = 2))
get.ngrams("case of")
get.phrasetable(ngram(ques1, n = 2))
get.nextwords(ques1)
get.phrasetable(alltexts5ng["case of"])

print(alltexts5ng, output = "truncated")
