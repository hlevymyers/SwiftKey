
tokenize_text <- function(input) {
        
        text_tbl <-  tibble(key = 1:length(input), text = input)
        tokens_tbl <- unnest_tokens(text_tbl, output="tokens", input="text", token="words") 
        return(tokens_tbl)
}

