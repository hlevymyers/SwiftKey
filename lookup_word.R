
#This code looks up ngrams for next word and returns top 10

lookup_word <- function(token_tbl){
        # Words are in the column named "tokens"
        num_words <- nrow(token_tbl)
        if(nrow(token_tbl) < 2){
                stop("This app requires two or three words to predict the next word. Please resubmit.")
        }
        
        if(num_words > 2) {
                words <- token_tbl$tokens[(num_words-2):num_words]
                top10 <- quadgrams.RDS %>% filter(word1 == words[1] & word2 ==  words[2] & word3 ==  words[3])
                total_ngram <-sum(top10$n)
                prob_ngram <- round(top10$n/total_ngram, 3)
                top10$probability <- prob_ngram
                top10 <- dplyr::select(top10, -n)
        }
        
        if(num_words == 2) {
                words <- token_tbl$tokens[(num_words-1):num_words]
                top10 <- filter(threegrams.RDS, word1 == words[1] & word2 ==  words[2])
                total_ngram <-sum(top10$n)
                prob_ngram <- round(top10$n/total_ngram, 3)
                top10$probability <- prob_ngram
                top10 <- dplyr::select(top10, -n)
        }
        if(nrow(top10) > 10) {
                return(top10[1:10, ])
        }
        if(nrow(top10) < 1) {
                top10 <- data.frame(message = "These must be usual words. There are no results for this phrase.")
        }
                return(top10) 
}

