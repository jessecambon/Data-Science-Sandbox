R Quickstart
================
Jesse Cambon
21 November, 2019

## Todo

  - Simplify dplyr workflows (diamonds dataset?)
      - summarize
      - arrange
      - filter
      - etc.
  - tidyr pivot wide, pivot long
  - basic minimal ggplot (histogram,point, bar, line)

## Setup

``` r
library(tidyverse)
library(ggplot2)

# Set default ggplot theme
theme_set(theme_bw()+
  theme(legend.position = "top",
            plot.subtitle= element_text(face="bold",hjust=0.5),
            plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)))
```

## Data Preparation

``` r
# Average height and weight by species
starwars_jac <- starwars %>% group_by(name) %>%
  mutate(num_films=length(unlist(films)),
         height_to_mass_ratio = height / mass,
           # bucket species variable
  species_collapsed=case_when(!(species %in% c('Human','Droid')) ~ 'Other',
    TRUE ~ species)) %>%
  ungroup() %>%
  mutate(gender=str_to_title(gender)) # capitalize gender

## Drop missing height and weight values for scatter plot
# Also drop Jabba and Yoda because they are outliers
starwars_ht_wt <- starwars_jac %>% drop_na(c(height,mass,gender)) %>%
  filter(!str_detect(name,'Jabba|Yoda')) %>% 
  filter(num_films >= 3) 

### Crime Data

murder_rates <- USArrests %>% 
  rownames_to_column('State') %>%
  as_tibble() %>%
  arrange(desc(UrbanPop)) %>%
  head(15) %>%
  arrange(desc(Murder)) %>% 
  mutate(State=factor(State,levels=rev(State)))

### Stock Data
eu_stock <- EuStockMarkets %>% 
  as_tibble() %>%
  gather(Index,Price) %>%
  mutate(Year=rep(time(EuStockMarkets),4)) 
```

``` r
# Histogram with autobinning based on gender
ggplot(starwars_jac %>% replace_na(list(gender='None')), aes(height)) + #scale_fill_manual(values = wes_palette('Moonrise2')) +
  geom_histogram(aes(fill=gender), 
                   binwidth = 10, 
                   col="black") +
            #       size=.1) +  # change binwidth
  # remove bottom inner margin with expand
scale_y_continuous(expand = c(0,0,0.08,0)) + 
  labs(title="Height Distribution of Star Wars Characters", 
       caption="Han Shot First") +
xlab('Height (cm)') +
ylab('Count') +
guides(fill = guide_legend(title='Gender'))
```

    ## Warning: Removed 6 rows containing non-finite values (stat_bin).

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/R-Quickstart/histogram-1.png)<!-- -->

## Lollipop

``` r
  ggplot(data=murder_rates, aes(x=State, y=Murder) ) +
    geom_segment( aes(x=State ,xend=State, y=0, yend=Murder), color="grey") +
    geom_point(size=3, color="navy") +
   theme_minimal() +
  theme(    plot.subtitle= element_text(face="bold",hjust=0.5),
            plot.title = element_text(lineheight=1, face="bold",hjust = 0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position="none"
    ) +
  coord_flip() +
    # expand gets rid of space between labels stem of lollipops
    scale_y_continuous(expand = c(0, .15)) + 
    labs(title='Murder Rates of Selected States - 1975',
        caption='Data: World Almanac and Book of facts 1975. (Crime rates)') +
    xlab("") +
    ylab('Murders Per 100,000 Residents')
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/R-Quickstart/lollipop-1.png)<!-- -->

## Line

``` r
# Start and end for the breaks on the horizontal axis
eu_plot_lims <- c(ceiling(min(eu_stock$Year)),floor(max(eu_stock$Year)))

# Performance of EU Stock Indexes
ggplot(eu_stock,
          aes(x=Year,y=Price,color = fct_rev(Index))) +
geom_line() +
scale_x_continuous(breaks=eu_plot_lims[1]:eu_plot_lims[2],
                   expand=c(0,0,0.02,0)) +
scale_y_continuous(labels=scales::comma) + 
#scale_color_manual(values=wes_palette('GrandBudapest2')) +
labs(title='EU Stock Indexes',
     caption='Data provided by Erste Bank AG, Vienna, Austria') +
theme(legend.title = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.text=element_text(size=10),
      legend.position='right') +
xlab('Year') +
ylab('Value') +
# make legend lines bigger
guides(colour = guide_legend(override.aes = list(size=2.5))) 
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/R-Quickstart/line-1.png)<!-- -->
