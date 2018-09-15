Modeling Workflow
================
Jesse Cambon
14 September, 2018

-   [References](#references)
-   [Setup](#setup)
-   [Grouped Models](#grouped-models)
-   [Nested Models](#nested-models)

Demonstrate model workflows with tidyverse, modelr, and broom.

References
----------

-   <http://r4ds.had.co.nz/many-models.html>

Setup
-----

``` r
library(tidyverse)
library(gapminder)
library(broom)
library(modelr)
library(knitr)
library(kableExtra)
```

Grouped Models
--------------

``` r
# One model per continent
models <- gapminder %>%
  group_by(continent) %>%
  do(fit=lm(lifeExp ~ (gdpPercap*log(pop)) + year, data=.))

stats <- glance(models,fit) %>%
  arrange(desc(r.squared))

coefficients <- tidy(models,fit) %>%
  filter(term != '(Intercept)') %>%
  arrange(continent,desc(p.value))

fit_check <- augment(models,fit)
```

``` r
kable(stats,format='markdown',digits=2) %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

<table style="width:100%;">
<colgroup>
<col width="9%" />
<col width="9%" />
<col width="12%" />
<col width="5%" />
<col width="9%" />
<col width="7%" />
<col width="3%" />
<col width="8%" />
<col width="7%" />
<col width="7%" />
<col width="8%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">continent</th>
<th align="right">r.squared</th>
<th align="right">adj.r.squared</th>
<th align="right">sigma</th>
<th align="right">statistic</th>
<th align="right">p.value</th>
<th align="right">df</th>
<th align="right">logLik</th>
<th align="right">AIC</th>
<th align="right">BIC</th>
<th align="right">deviance</th>
<th align="right">df.residual</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Oceania</td>
<td align="right">0.97</td>
<td align="right">0.97</td>
<td align="right">0.67</td>
<td align="right">180.74</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-21.58</td>
<td align="right">55.16</td>
<td align="right">62.22</td>
<td align="right">8.49</td>
<td align="right">19</td>
</tr>
<tr class="even">
<td align="left">Europe</td>
<td align="right">0.71</td>
<td align="right">0.70</td>
<td align="right">2.96</td>
<td align="right">213.61</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-898.98</td>
<td align="right">1809.96</td>
<td align="right">1833.28</td>
<td align="right">3110.58</td>
<td align="right">355</td>
</tr>
<tr class="odd">
<td align="left">Americas</td>
<td align="right">0.64</td>
<td align="right">0.63</td>
<td align="right">5.67</td>
<td align="right">129.49</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-943.58</td>
<td align="right">1899.17</td>
<td align="right">1921.39</td>
<td align="right">9475.45</td>
<td align="right">295</td>
</tr>
<tr class="even">
<td align="left">Asia</td>
<td align="right">0.58</td>
<td align="right">0.58</td>
<td align="right">7.71</td>
<td align="right">135.85</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-1368.43</td>
<td align="right">2748.86</td>
<td align="right">2772.74</td>
<td align="right">23266.91</td>
<td align="right">391</td>
</tr>
<tr class="odd">
<td align="left">Africa</td>
<td align="right">0.44</td>
<td align="right">0.44</td>
<td align="right">6.86</td>
<td align="right">122.28</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-2084.63</td>
<td align="right">4181.26</td>
<td align="right">4207.87</td>
<td align="right">29137.24</td>
<td align="right">619</td>
</tr>
</tbody>
</table>

``` r
kable(coefficients,format='markdown',digits=4) %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| continent | term               |  estimate|  std.error|  statistic|  p.value|
|:----------|:-------------------|---------:|----------:|----------:|--------:|
| Africa    | gdpPercap          |   -0.0049|     0.0012|    -4.0501|   0.0001|
| Africa    | log(pop)           |   -1.2766|     0.2681|    -4.7615|   0.0000|
| Africa    | gdpPercap:log(pop) |    0.0004|     0.0001|     4.9788|   0.0000|
| Africa    | year               |    0.2748|     0.0170|    16.1750|   0.0000|
| Americas  | log(pop)           |    0.9745|     0.3889|     2.5060|   0.0127|
| Americas  | gdpPercap:log(pop) |   -0.0002|     0.0000|    -5.3917|   0.0000|
| Americas  | gdpPercap          |    0.0038|     0.0006|     6.3883|   0.0000|
| Americas  | year               |    0.2771|     0.0208|    13.2944|   0.0000|
| Asia      | log(pop)           |   -0.7482|     0.2407|    -3.1080|   0.0020|
| Asia      | gdpPercap          |   -0.0014|     0.0002|    -6.1772|   0.0000|
| Asia      | gdpPercap:log(pop) |    0.0001|     0.0000|     7.3138|   0.0000|
| Asia      | year               |    0.3759|     0.0251|    14.9969|   0.0000|
| Europe    | gdpPercap          |   -0.0003|     0.0002|    -1.3337|   0.1832|
| Europe    | gdpPercap:log(pop) |    0.0000|     0.0000|     2.9321|   0.0036|
| Europe    | log(pop)           |   -0.8667|     0.2276|    -3.8082|   0.0002|
| Europe    | year               |    0.1147|     0.0114|    10.0449|   0.0000|
| Oceania   | gdpPercap          |   -0.0010|     0.0013|    -0.7456|   0.4651|
| Oceania   | log(pop)           |   -0.9298|     1.0872|    -0.8552|   0.4031|
| Oceania   | gdpPercap:log(pop) |    0.0001|     0.0001|     0.9078|   0.3753|
| Oceania   | year               |    0.1867|     0.0573|     3.2575|   0.0041|

Nested Models
-------------

``` r
my_model <- function(df) {
  lm(lifeExp ~ (gdpPercap*log(pop)) + year, data= df)
}

# Nest models by continent 

by_continent <- gapminder %>% 
  group_by(continent) %>% 
  nest() %>%
  # fit models
  mutate(fit = map(data, my_model)) %>%
  # calculate residuals
  mutate(augment = map(fit, augment),
    stats = map(fit,glance),
    terms = map(fit,tidy)) %>%
  ungroup()

# Dataset with predictions and residuals
model_fit <- by_continent %>% unnest(augment)

nest_stats <- by_continent %>%
  unnest(stats,.drop=TRUE) %>%
  arrange(desc(r.squared)) 

nest_coefficients <- by_continent %>%
  unnest(terms,.drop=TRUE) %>%
  filter(term != '(Intercept)') %>%
  arrange(continent,desc(p.value))
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

![](Modeling_Workflow_files/figure-markdown_github/plot-1.png)

``` r
kable(nest_stats,format='markdown',digits=2) %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

<table style="width:100%;">
<colgroup>
<col width="9%" />
<col width="9%" />
<col width="12%" />
<col width="5%" />
<col width="9%" />
<col width="7%" />
<col width="3%" />
<col width="8%" />
<col width="7%" />
<col width="7%" />
<col width="8%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">continent</th>
<th align="right">r.squared</th>
<th align="right">adj.r.squared</th>
<th align="right">sigma</th>
<th align="right">statistic</th>
<th align="right">p.value</th>
<th align="right">df</th>
<th align="right">logLik</th>
<th align="right">AIC</th>
<th align="right">BIC</th>
<th align="right">deviance</th>
<th align="right">df.residual</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Oceania</td>
<td align="right">0.97</td>
<td align="right">0.97</td>
<td align="right">0.67</td>
<td align="right">180.74</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-21.58</td>
<td align="right">55.16</td>
<td align="right">62.22</td>
<td align="right">8.49</td>
<td align="right">19</td>
</tr>
<tr class="even">
<td align="left">Europe</td>
<td align="right">0.71</td>
<td align="right">0.70</td>
<td align="right">2.96</td>
<td align="right">213.61</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-898.98</td>
<td align="right">1809.96</td>
<td align="right">1833.28</td>
<td align="right">3110.58</td>
<td align="right">355</td>
</tr>
<tr class="odd">
<td align="left">Americas</td>
<td align="right">0.64</td>
<td align="right">0.63</td>
<td align="right">5.67</td>
<td align="right">129.49</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-943.58</td>
<td align="right">1899.17</td>
<td align="right">1921.39</td>
<td align="right">9475.45</td>
<td align="right">295</td>
</tr>
<tr class="even">
<td align="left">Asia</td>
<td align="right">0.58</td>
<td align="right">0.58</td>
<td align="right">7.71</td>
<td align="right">135.85</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-1368.43</td>
<td align="right">2748.86</td>
<td align="right">2772.74</td>
<td align="right">23266.91</td>
<td align="right">391</td>
</tr>
<tr class="odd">
<td align="left">Africa</td>
<td align="right">0.44</td>
<td align="right">0.44</td>
<td align="right">6.86</td>
<td align="right">122.28</td>
<td align="right">0</td>
<td align="right">5</td>
<td align="right">-2084.63</td>
<td align="right">4181.26</td>
<td align="right">4207.87</td>
<td align="right">29137.24</td>
<td align="right">619</td>
</tr>
</tbody>
</table>

``` r
kable(nest_coefficients,format='markdown',digits=4) %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| continent | term               |  estimate|  std.error|  statistic|  p.value|
|:----------|:-------------------|---------:|----------:|----------:|--------:|
| Africa    | gdpPercap          |   -0.0049|     0.0012|    -4.0501|   0.0001|
| Africa    | log(pop)           |   -1.2766|     0.2681|    -4.7615|   0.0000|
| Africa    | gdpPercap:log(pop) |    0.0004|     0.0001|     4.9788|   0.0000|
| Africa    | year               |    0.2748|     0.0170|    16.1750|   0.0000|
| Americas  | log(pop)           |    0.9745|     0.3889|     2.5060|   0.0127|
| Americas  | gdpPercap:log(pop) |   -0.0002|     0.0000|    -5.3917|   0.0000|
| Americas  | gdpPercap          |    0.0038|     0.0006|     6.3883|   0.0000|
| Americas  | year               |    0.2771|     0.0208|    13.2944|   0.0000|
| Asia      | log(pop)           |   -0.7482|     0.2407|    -3.1080|   0.0020|
| Asia      | gdpPercap          |   -0.0014|     0.0002|    -6.1772|   0.0000|
| Asia      | gdpPercap:log(pop) |    0.0001|     0.0000|     7.3138|   0.0000|
| Asia      | year               |    0.3759|     0.0251|    14.9969|   0.0000|
| Europe    | gdpPercap          |   -0.0003|     0.0002|    -1.3337|   0.1832|
| Europe    | gdpPercap:log(pop) |    0.0000|     0.0000|     2.9321|   0.0036|
| Europe    | log(pop)           |   -0.8667|     0.2276|    -3.8082|   0.0002|
| Europe    | year               |    0.1147|     0.0114|    10.0449|   0.0000|
| Oceania   | gdpPercap          |   -0.0010|     0.0013|    -0.7456|   0.4651|
| Oceania   | log(pop)           |   -0.9298|     1.0872|    -0.8552|   0.4031|
| Oceania   | gdpPercap:log(pop) |    0.0001|     0.0001|     0.9078|   0.3753|
| Oceania   | year               |    0.1867|     0.0573|     3.2575|   0.0041|
