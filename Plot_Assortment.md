Plot Assortment
================

This notebook will demonstrate an assortment of basic ggplots such as bar charts, line charts, and scatter plots.

We will use the inbuilt starwars dataset in tidyverse just for fun.

Libraries and Global Variables
------------------------------

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggrepel) # loads ggplot2 as well
library(DT)

# Color blind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
```

Data Prep
---------

``` r
# Average height and weight by species
species_summ <- starwars %>% group_by(species) %>%
  drop_na(c(height,mass)) %>%
  summarise(average_height=mean(height),
            average_mass=mean(mass),
            count=sum(n())) %>% ungroup() %>%
  mutate(height_to_mass_ratio = average_height / average_mass) %>%
  filter(count!=1) # don't look at species that only have one member

## Drop missing height and weight values for scatter plot
# Also drop Jabba and Yoda because they are outliers
starwars_ht_wt <- starwars %>% drop_na(c(height,mass,gender)) %>%
  filter(!str_detect(name,'Jabba|Yoda')) %>% 
  slice(1:15) # grab only top 15 characters listed
```

Create plots
------------

To save any plot as an SVG use this command: ggsave('filename.svg',plot=plotname, device = "svg")

``` r
# A simple bar chart - average heights of the species
bar1 <- ggplot(data=species_summ,
          aes(x = species, y=average_height, fill = species)) +
geom_bar(stat='identity',position='dodge') +
theme_bw() +
#scale_y_continuous(limits=c(0,XXXX),labels = scales::comma) +
#scale_x_continuous(breaks=min(f2$year):max(f2$year)) + 
scale_fill_manual(values=rep(cbPalette,5)) +
theme(legend.position="none") +
labs(title='Average Height of Selected Star Wars Species') +
theme(plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) +
xlab('Species') +
ylab('')
bar1
```

![](Plot_Assortment_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# Scatter plot of heights and weights 
ht_wt <- ggplot(data=starwars_ht_wt,
          aes(x = mass, y = height, color = gender)) +
geom_point() +
#coord_cartesian(xlim = c(min(combi_filt$fiscal_year), max(combi_filt$fiscal_year) + 2)) + # gives us room for labels?
geom_text_repel(
    data = starwars_ht_wt,
    aes(label = name),
    size = 3,
    nudge_x = 0,
    nudge_y = 0,
    segment.color = NA
  ) +
   theme_bw() +
  theme(legend.position = "top")  +
#scale_y_continuous(labels = scales::percent) +
#scale_x_continuous(breaks=min(combi_filt$fiscal_year):max(combi_filt$fiscal_year)) + 
scale_color_manual(values=rep(cbPalette,1)) +
labs(title='Heights and Weights of Selected Star Wars Characters') +
theme(plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) +
xlab('Mass') +
ylab('Height')
ht_wt
```

![](Plot_Assortment_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
# Create interactive data table of raw data
# This only works in HTML format so comment it out if knitting to github format 
#datatable(starwars %>% select(-hair_color,skin_color,-birth_year), options = list(pageLength = 10))
```
