Tidytext
================
Jesse Cambon
14 March, 2020

Sources: \* <https://www.tidytextmining.com/sentiment.html>

Using Tidytext for sentiment analysis

``` r
library(janeaustenr)
library(tidyverse)
library(tidytext)
library(knitr)
# import original dataset - one row per line of each jane austen book
austen_df <- austen_books()

# tokenize each jane austen book
tidy_books <- austen_df %>%
  group_by(book) %>%
  # add some variables
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
```

Sentiment Analysis (grouping by book)

``` r
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("afinn"))
```

    ## Joining, by = "word"

``` r
kable(head(jane_austen_sentiment,5))
```

| book                | linenumber | chapter | word     | value |
| :------------------ | ---------: | ------: | :------- | ----: |
| Sense & Sensibility |         16 |       1 | engage   |     1 |
| Sense & Sensibility |         16 |       1 | good     |     3 |
| Sense & Sensibility |         18 |       1 | advanced |     1 |
| Sense & Sensibility |         20 |       1 | death    |   \-2 |
| Sense & Sensibility |         20 |       1 | great    |     3 |

``` r
# Summarize sentiment by book
sentiment_summ <- jane_austen_sentiment %>%
  group_by(book) %>%
  summarize(mean_sentiment = mean(value),
            num_sentiment_words= n() ) %>%
  ungroup() %>%
  arrange(desc(mean_sentiment))
  
kable(sentiment_summ)
```

| book                | mean\_sentiment | num\_sentiment\_words |
| :------------------ | --------------: | --------------------: |
| Persuasion          |       0.5502924 |                  5130 |
| Emma                |       0.5354555 |                 10901 |
| Mansfield Park      |       0.5211837 |                 10645 |
| Pride & Prejudice   |       0.5081588 |                  7783 |
| Northanger Abbey    |       0.4318989 |                  5066 |
| Sense & Sensibility |       0.4264365 |                  7762 |

Make a function for quickly tokenizing a string and returning the mean
sentiment

``` r
mean_sentiment <- function(.tbl,text) {
# Returns mean sentiment 
# Args:
#   .tbl : tibble dataframe
#   text (STRING) : quoted column name in .tbl of text content
# Returns:
#   Dataframe with mean sentiment and counts of both total words
#   and words that had a sentiment value

#text <- enquo(text)

# number each row
# use this to join text column back on later
text_num <- .tbl %>%
  mutate(row_num = row_number())
  
# tokenize the dataset (one row per token)
tokens <- text_num %>%
  unnest_tokens(word, {{text}}) %>%
  left_join(get_sentiments("afinn"), by='word') %>%
  mutate(non_missing=case_when(!is.na(value) ~ 1, TRUE ~0)) # record if missing sentiment value

# summarize the sentiment (value column contains sentiment of each token)
summ <- tokens %>%
  group_by(row_num) %>%
  summarize(mean_sentiment = mean(value,na.rm=TRUE),
            num_words = n(),
            num_sentiment_words = sum(non_missing)) %>%
  ungroup() %>%
  left_join(text_num,by='row_num') %>%
  select(row_num,{{text}},everything())

return(summ)
}
```

``` r
test_df <- tribble( ~review,
"This is the worst restaurant I have ever eaten at. It's service is abysmal.",
"Wow, amazing food, great atmosphere. Will definitely be coming back.",
"The restaurant was okay. Not good or bad",
"The restaurant was okay. Not good or bad", # duplicate row
"The stock market crashed and it was a disaster",
"Sunshine and rainbows. Everything is fantastical."
)

test_sentiment <- test_df %>%
  mean_sentiment(review) %>%
  arrange(desc(mean_sentiment))

# test <- test_df %>%
#   unnest_tokens(word,review) %>%
#   left_join(get_sentiments("afinn"), by='word')

kable(test_sentiment)
```

| row\_num | review                                                                      | mean\_sentiment | num\_words | num\_sentiment\_words |
| -------: | :-------------------------------------------------------------------------- | --------------: | ---------: | --------------------: |
|        2 | Wow, amazing food, great atmosphere. Will definitely be coming back.        |        3.666667 |         10 |                     3 |
|        6 | Sunshine and rainbows. Everything is fantastical.                           |        2.000000 |          6 |                     1 |
|        3 | The restaurant was okay. Not good or bad                                    |        0.000000 |          8 |                     2 |
|        4 | The restaurant was okay. Not good or bad                                    |        0.000000 |          8 |                     2 |
|        5 | The stock market crashed and it was a disaster                              |      \-2.000000 |          9 |                     1 |
|        1 | This is the worst restaurant I have ever eaten at. It’s service is abysmal. |      \-3.000000 |         14 |                     1 |