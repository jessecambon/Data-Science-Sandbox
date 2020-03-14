Tidytext
================
Jesse Cambon
14 March, 2020

Sources: \* <https://www.tidytextmining.com/sentiment.html>

Using Tidytext for sentiment analysis

``` r
library(janeaustenr)
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ forcats 0.4.0
    ## ✓ readr   1.3.1

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
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

#text <- enquo(text)
  
df <- .tbl %>%
  unnest_tokens(word, {{text}}) %>%
  left_join(get_sentiments("afinn"), by='word') %>%
  group_by({{text}}) %>%
  summarize(mean_sentiment = mean(value,na.rm=TRUE),
            num_words = n()) %>%
  ungroup()

return(df)
  
}
```

``` r
test_df <- tribble( ~review,
"This is the worst restaurant I have ever eaten at. It's service is abysmal.",
"Wow, amazing food, great atmosphere. Wil. definitely be coming back.",
"The restaurant was okay. Not good or bad"
)

test_sentiment <- test_df %>%
  mean_sentiment(review)
```
