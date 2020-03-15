Sentiment Analysis with R
================
Jesse Cambon
15 March, 2020

## Tidytext

Using Tidytext for sentiment analysis

  - <https://www.tidytextmining.com/sentiment.html>

<!-- end list -->

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

## Sentiment Aggregation with TidyText

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

Note that the tidytext approach doesn’t handle negation in sentiment

``` r
test_df <- tribble( ~review,
"This is the worst restaurant I have ever eaten at. It's service is abysmal.",
"Wow, amazing food, great atmosphere. Will definitely be coming back.",
"The restaurant was okay. Not good or bad",
"The restaurant was okay. Not good or bad", # duplicate row
"The stock market crashed and it was a disaster",
"This place was not terrible.", # test negation
"Really wasn't the best meal.", # test negation
"Sunshine and rainbows. Everything is fantastic, couldn't be better."
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
|        7 | Really wasn’t the best meal.                                                |        3.000000 |          5 |                     1 |
|        8 | Sunshine and rainbows. Everything is fantastic, couldn’t be better.         |        2.666667 |          9 |                     3 |
|        3 | The restaurant was okay. Not good or bad                                    |        0.000000 |          8 |                     2 |
|        4 | The restaurant was okay. Not good or bad                                    |        0.000000 |          8 |                     2 |
|        5 | The stock market crashed and it was a disaster                              |      \-2.000000 |          9 |                     1 |
|        1 | This is the worst restaurant I have ever eaten at. It’s service is abysmal. |      \-3.000000 |         14 |                     1 |
|        6 | This place was not terrible.                                                |      \-3.000000 |          5 |                     1 |

## Sentimentr

<https://github.com/trinker/sentimentr>

An example of using the `sentimentr` package for sentiment analysis.
This approach does handle negation.

``` r
library(sentimentr)

# Split entities into sentences, use 'element_id' column to aggregate back
# to the original entitites
sentences_df <- get_sentences(test_df) 

kable(sentences_df)
```

| review                                             | element\_id | sentence\_id |
| :------------------------------------------------- | ----------: | -----------: |
| This is the worst restaurant I have ever eaten at. |           1 |            1 |
| It’s service is abysmal.                           |           1 |            2 |
| Wow, amazing food, great atmosphere.               |           2 |            1 |
| Will definitely be coming back.                    |           2 |            2 |
| The restaurant was okay.                           |           3 |            1 |
| Not good or bad                                    |           3 |            2 |
| The restaurant was okay.                           |           4 |            1 |
| Not good or bad                                    |           4 |            2 |
| The stock market crashed and it was a disaster     |           5 |            1 |
| This place was not terrible.                       |           6 |            1 |
| Really wasn’t the best meal.                       |           7 |            1 |
| Sunshine and rainbows.                             |           8 |            1 |
| Everything is fantastic, couldn’t be better.       |           8 |            2 |

``` r
# Sentiment by sentence
sentiment_df <- sentiment(sentences_df) 

# Aggregate sentiment to original entities
sentiment_summ <- sentiment_by(sentences_df,by='element_id') %>%
  bind_cols(test_df %>% select(review)) %>%
  select(element_id,review,everything()) %>%
  arrange(desc(ave_sentiment))

kable(sentiment_summ)
```

| element\_id | review                                                                      | word\_count |        sd | ave\_sentiment |
| ----------: | :-------------------------------------------------------------------------- | ----------: | --------: | -------------: |
|           2 | Wow, amazing food, great atmosphere. Will definitely be coming back.        |          10 | 0.6008328 |      0.4636729 |
|           6 | This place was not terrible.                                                |           5 |        NA |      0.3354102 |
|           8 | Sunshine and rainbows. Everything is fantastic, couldn’t be better.         |           9 | 0.2185579 |      0.1341314 |
|           3 | The restaurant was okay. Not good or bad                                    |           8 | 0.0000000 |      0.0000000 |
|           4 | The restaurant was okay. Not good or bad                                    |           8 | 0.0000000 |      0.0000000 |
|           7 | Really wasn’t the best meal.                                                |           5 |        NA |    \-0.0447214 |
|           1 | This is the worst restaurant I have ever eaten at. It’s service is abysmal. |          14 | 0.0649733 |    \-0.2040569 |
|           5 | The stock market crashed and it was a disaster                              |           9 |        NA |    \-0.5333333 |
