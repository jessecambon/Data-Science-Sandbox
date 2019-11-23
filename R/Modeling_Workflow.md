Modeling Workflow
================
Jesse Cambon
23 November, 2019

Demonstrate model workflows with tidyverse, modelr, and broom. This
notebook includes both a group\_by and a nested approach which offer
similar results. However, the nested model workflow embeds the data into
the dataframe along with objects such as models.

## References

  - <http://r4ds.had.co.nz/many-models.html>

## Setup

``` r
library(tidyverse)
library(gapminder)
library(broom)
#library(modelr)
library(knitr)
library(kableExtra)
```

## Exploration

These graphs show why log transforming GDP per Capita makes it correlate
more linearly to our response variable, life expectancy. Log
transformations are often useful for highly skewed variables in
regression.

``` r
ggplot(data=gapminder,
          aes(x = gdpPercap, y = lifeExp, color = continent,group=1)) +
geom_point(alpha=0.7) +
theme_bw() +
geom_smooth() +
theme(legend.position='top',
  plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) + 
guides(color=guide_legend(override.aes = list(size=2.5))) 
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Modeling_Workflow/explore-1.png)<!-- -->

``` r
ggplot(data=gapminder,
          aes(x = log10(gdpPercap), y = lifeExp, color = continent,group=1)) +
geom_point(alpha=0.7) +
theme_bw() +
geom_smooth() +
theme(legend.position='top',
  plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) + 
guides(color=guide_legend(override.aes = list(size=2.5))) 
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Modeling_Workflow/explore-2.png)<!-- -->

``` r
ggplot(data=gapminder,
          aes(x = log10(pop), y = lifeExp, color = continent,group=1)) +
geom_point(alpha=0.7) +
#facet_grid(~continent) +
theme_bw() +
geom_smooth() +
theme(legend.position='top',
  plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) + 
guides(color=guide_legend(override.aes = list(size=2.5))) 
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Modeling_Workflow/explore-3.png)<!-- -->

## Grouped Models

``` r
# One model per continent
models <- gapminder %>%
  group_by(continent) %>%
  do(fit=lm(lifeExp ~ log10(gdpPercap)+log10(pop) + year, data=.)) 

stats <- glance(models,fit) %>%
  arrange(desc(r.squared))

coefficients <- tidy(models,fit) %>%
  filter(term != '(Intercept)') %>%
  arrange(continent,p.value)

model_fit <- augment(models,fit)
```

``` r
ggplot(data=model_fit,
          aes(x = .fitted, y = .resid, color = continent,group=1)) +
geom_point(alpha=0.8) +
facet_grid(~continent) +
ggtitle('Fitted vs. Residual Check') +
theme_bw() +
geom_hline(yintercept=0,color='blue') + # horizontal line at 0 residual
theme(legend.position='none',
  plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) + 
guides(color=guide_legend(override.aes = list(size=2.5))) +
xlab('Fitted') +
ylab('Residual')
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Modeling_Workflow/plot-1.png)<!-- -->

``` r
ggplot(data=model_fit,
          aes(.resid)) +
geom_histogram(aes(fill=continent)) +
facet_grid(~continent) +
ggtitle('Residual Distribution') +
theme_bw() +
scale_y_continuous(expand = c(0,0,0.05,0)) + 
theme(legend.position='none',
  plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) + 
guides(color=guide_legend(override.aes = list(size=2.5))) +
xlab('Residual') +
ylab('Count')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Modeling_Workflow/plot-2.png)<!-- -->

``` r
kable(stats,format='markdown',digits=2) %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| continent | r.squared | adj.r.squared | sigma | statistic | p.value | df |    logLik |     AIC |     BIC | deviance | df.residual |
| :-------- | --------: | ------------: | ----: | --------: | ------: | -: | --------: | ------: | ------: | -------: | ----------: |
| Oceania   |      0.96 |          0.96 |  0.78 |    172.97 |       0 |  4 |   \-26.03 |   62.06 |   67.95 |    12.30 |          20 |
| Europe    |      0.80 |          0.80 |  2.41 |    487.82 |       0 |  4 |  \-825.98 | 1661.96 | 1681.39 |  2073.54 |         356 |
| Americas  |      0.72 |          0.72 |  4.96 |    255.52 |       0 |  4 |  \-903.93 | 1817.85 | 1836.37 |  7274.08 |         296 |
| Asia      |      0.70 |          0.70 |  6.50 |    308.12 |       0 |  4 | \-1301.08 | 2612.15 | 2632.06 | 16558.14 |         392 |
| Africa    |      0.50 |          0.50 |  6.48 |    207.77 |       0 |  4 | \-2049.22 | 4108.45 | 4130.63 | 26011.51 |         620 |

``` r
kable(coefficients,format='markdown',digits=4) %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| continent | term             | estimate | std.error | statistic | p.value |
| :-------- | :--------------- | -------: | --------: | --------: | ------: |
| Africa    | year             |   0.2551 |    0.0160 |   15.8991 |  0.0000 |
| Africa    | log10(gdpPercap) |  11.0142 |    0.7141 |   15.4237 |  0.0000 |
| Africa    | log10(pop)       | \-0.5390 |    0.4192 |  \-1.2857 |  0.1990 |
| Americas  | log10(gdpPercap) |  18.5492 |    1.1513 |   16.1118 |  0.0000 |
| Americas  | year             |   0.2690 |    0.0179 |   15.0519 |  0.0000 |
| Americas  | log10(pop)       | \-1.9190 |    0.5545 |  \-3.4607 |  0.0006 |
| Asia      | log10(gdpPercap) |  12.6233 |    0.7074 |   17.8454 |  0.0000 |
| Asia      | year             |   0.2974 |    0.0219 |   13.5703 |  0.0000 |
| Asia      | log10(pop)       |   2.0425 |    0.4854 |    4.2077 |  0.0000 |
| Europe    | log10(gdpPercap) |  11.5695 |    0.4930 |   23.4667 |  0.0000 |
| Europe    | year             |   0.1005 |    0.0091 |   11.0939 |  0.0000 |
| Europe    | log10(pop)       | \-1.0054 |    0.2244 |  \-4.4804 |  0.0000 |
| Oceania   | year             |   0.1737 |    0.0384 |    4.5299 |  0.0002 |
| Oceania   | log10(pop)       |   0.6644 |    0.5984 |    1.1102 |  0.2801 |
| Oceania   | log10(gdpPercap) |   4.1229 |    4.9721 |    0.8292 |  0.4168 |

## Nested Models

Now we create a similar model with nesting

``` r
my_model <- function(df) {
  lm(lifeExp ~ log10(gdpPercap)+log10(pop) + year, data= df)
}

# Nest models by continent 
nested_models <- gapminder %>% 
  group_by(continent,country) %>% 
  nest() %>%
  # fit models
  mutate(fit = map(data, my_model)) %>%
  # calculate residuals
  mutate(augment = map(fit, augment),
    stats = map(fit,glance),
    terms = map(fit,tidy)) %>%
  ungroup()

# Dataset with predictions and residuals
nest_fit <- nested_models %>% unnest(augment)

nest_stats <- nested_models %>%
  unnest(stats,.drop=TRUE) %>%
  arrange(desc(r.squared)) 
```

    ## Warning: The `.drop` argument of `unnest()` is deprecated as of tidyr 1.0.0.
    ## All list-columns are now preserved.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
nest_coefficients <- nested_models %>%
  unnest(terms,.drop=TRUE) %>%
  filter(term != '(Intercept)') %>%
  arrange(continent,country,desc(p.value))

most_important_vars <- nest_coefficients %>%
  group_by(country) %>% 
  slice(1)

summ_imp_vars <- most_important_vars %>%
  group_by(continent) %>%
  count(term) %>%
  arrange(continent,desc(n))
```
