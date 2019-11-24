Visualization Cookbook
================
Jesse Cambon
23 November, 2019

The purpose of this document is to provide code that can be easily
copied and adapted for use in data science projects to generate a wide
variety of data visualizations. Because of this, I have opted to use
inbuilt datasets in R so that you should not have to download any
datasets external to R packages.

## Getting Started

  - Install all the listed packages in the setup block using
    install.packages(‘package’) or
    install.packages(‘package1’,‘package2’,‘package3’,…) for
    multiple packages.
  - To recreate any graph, make sure to first run the theme\_set()
    command from the setup code block and also the relevant code from
    the data preparation code block to create the relevant dataset.
    After you have successfully recreated a graph, you can switch out
    the data and variable names for your own uses.
  - Use ‘fill’ commands for areas and ‘color’ for lines.
  - To save any plot to a file, use the ggsave() command

## References

  - ggplot: <https://ggplot2.tidyverse.org/reference/index.html>
  - ggplot cheat sheet:
    <https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf>
  - dplyr: <https://dplyr.tidyverse.org/>
  - [“Top 50
    GGplots”](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html)

## Tips for Effective Visual Communication

  - Simplicity is key. Remove duplicative labels and other elements,
    hide gridlines that aren’t visually helpful, and remove everything
    from the graph that isn’t part of the story you are looking to tell.
  - Make use of ordering as a visual cue. For example, a bar chart that
    has the bars ordered by size is much easier to read.
  - Experiment with variations of a graph. For example, changing the bin
    width for histograms can yield different results.

# Setup

``` r
# Load libraries - you will need to install these with install.packages('packagename')
# if you do not have them installed

# Need to load this package first to prevent it
# from masking the 'select' command in dplyr (tidyverse)
library(PASWR) #titanic3 dataset (for age-sex population pyramid)

## General:
library(tidyverse) # dplyr, data manipulation
library(formattable) # percent format
library(wesanderson) # Color Palettes from Wes Anderson Movies

## For specific plots:
library(ggrepel) # text labels
library(viridis) # colors
library(gapminder) # gdp life expectancy data
library(gridExtra) 

# Set default ggplot theme
theme_set(theme_bw()+
  theme(legend.position = "top",
            plot.subtitle= element_text(face="bold",hjust=0.5),
            plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)))

# Color blind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")
```

## Data Preparation

``` r
### Titanic Data
data(Titanic)
titanic <- Titanic %>% as_tibble()  %>%
  mutate(Sex=str_to_title(Sex)) # capitalize

titanic_bar <- titanic %>%
  # add a percent for Class 
  group_by(Sex,Survived,Class) %>%
  summarize(n=sum(n)) %>%
  group_by(Sex,Survived) %>%
  mutate(percent_num=n/sum(n),percent_char=as.character(percent(n/sum(n),0)))

# Titanic passenger composition (for waffle chart)
titanic_class <- titanic %>%
  group_by(Class) %>%
  summarize(n=sum(n)) %>%
  ungroup()

### Starwars Data

# The chronological order of the star wars films
film_chron_order = c("The Phantom Menace", "Attack of the Clones", "Revenge of the Sith",
  "A New Hope","The Empire Strikes Back", "Return of the Jedi","The Force Awakens")

# Average height and weight by species
starwars_jac <- starwars %>% group_by(name) %>%
  mutate(num_films=length(unlist(films)),
         height_to_mass_ratio = height / mass,
           # bucket species variable
  species_collapsed=case_when(!(species %in% c('Human','Droid')) ~ 'Other',
    TRUE ~ species)) %>%
  ungroup() %>%
  mutate(gender=str_to_title(gender)) # capitalize gender

# put each film on a different row
# ie. character-film level dataset
starwars_unnest <- starwars_jac %>%
  unnest(films) %>%
  mutate(films= factor(films,levels=film_chron_order)) %>%
  mutate(episode=as.integer(films)) %>%
  rename(film=films)

# Number of characters of each species by film
starwars_species_film <- starwars_unnest %>% 
  count(episode,species_collapsed) %>% drop_na() 

# % of characters in each film by gender
starwars_gender_film <- starwars_unnest %>%
  count(episode,gender) %>% drop_na() %>% 
  group_by(episode) %>% 
  mutate(prop=n/sum(n)) %>%
  ungroup() 

species_summ <- starwars_jac %>% group_by(species) %>%
  drop_na(c(height,mass)) %>%
  summarise(average_height=mean(height),
            average_mass=mean(mass),
            count=sum(n())) %>% ungroup() %>%
  mutate(height_to_mass_ratio = average_height / average_mass) %>%
  filter(count!=1) # don't look at species that only have one member

homeworld_summ <- starwars_jac %>%
  count(homeworld,species_collapsed) %>%
  group_by(homeworld) %>%
  mutate(homeworld_pop=sum(n)) %>%
  ungroup() %>%
  # If homeworld is missing, label it unknown
  replace_na(list(homeworld='Unknown')) %>%
  arrange(desc(n)) %>%
  filter(homeworld_pop > 3)

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

# Chart Types

## Distribution

### Histogram

``` r
# Histogram with autobinning based on gender
ggplot(starwars_jac %>% replace_na(list(gender='None')), aes(height)) + scale_fill_manual(values = wes_palette('Moonrise2')) +
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

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/histogram-1.png)<!-- -->

### Ridgeplot

<https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html>

``` r
library(ggridges) # text labels
```

    ## 
    ## Attaching package: 'ggridges'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     scale_discrete_manual

``` r
ggplot(lincoln_weather , aes(x = `Mean Wind Speed[MPH]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale =2, rel_min_height = 0.01) +
  scale_y_discrete(expand=c(0,0,0.2,0)) + # add top margin
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position='none') +
  scale_fill_viridis(option='C',direction=-1) +
  labs(title = 'Daily Mean Wind Speeds in Lincoln, NE in 2016') +
  xlab('Mean Wind Speed (MPH)')
```

    ## Picking joint bandwidth of 1.32

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/ridge-1.png)<!-- -->

### Population Pyramid

``` r
## Transform titanic data with age into format for population pyramid
titanic_age_sex <- titanic3 %>% as_tibble() %>%
  filter(!is.na(age)) %>%
  select(sex,age) %>%
  mutate(age_cat=cut(age, breaks=c(0,10,20,30,40,50,60,80),
    labels=c('0-10','11-20','21-30','31-40','41-50','51-60','61-80')
  )) %>%
  count(sex,age_cat) %>%
  mutate(sex=str_to_title(sex)) %>% # capitalize
  # invert counts for pyramid
  # need as.numeric() since introducing negative values means we need
  # to use the double format (numeric) instead of integer
  mutate(n=case_when(sex=='Female' ~ -1*n, TRUE ~ as.numeric(n)))


ggplot(data=titanic_age_sex %>% mutate(sex=str_to_title(sex)), aes(x = age_cat, y = n, fill = sex)) + 
geom_bar(stat='identity',color='black',size=0.25) +  
#  facet_grid(~sex,scales='free',space='free') +
  scale_y_continuous(limits=c(-240,240),
    breaks = seq(-240, 240, 80), 
                     labels = as.character(c(seq(240,0,-80), seq(80,240,80)))) + 
  coord_flip() + 
  scale_fill_manual(values = wes_palette('Darjeeling2')[c(2,3)]) +
  theme(legend.position='top',panel.grid.minor.x=element_blank(),
        strip.placement = "outside") +
  labs(title='Passengers on the Titanic') +
  xlab('Age') +
  ylab('Passengers') +
  guides(fill = guide_legend(title='')) # remove legend title
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/pyramid-1.png)<!-- -->

### Boxplot

A tukey-style boxplot.

“The upper whisker extends from the \[ upper hinge (ie. 75 percentile)
\] to the largest value no further than 1.5 \* IQR from the \[upper
hinge\] (where IQR is the inter-quartile range, or distance between the
first and third quartiles). The lower whisker extends from the \[lower
hinge (ie. 25 percentile)\] to the smallest value at most 1.5 \* IQR of
the \[lower hinge\]. Data beyond the end of the whiskers are
called”outlying" points and are plotted individually."
<https://ggplot2.tidyverse.org/reference/geom_boxplot.html>

``` r
 ggplot(eu_stock, aes(x=Index, y=Price,fill=Index)) + 
  geom_boxplot(outlier.shape = NA) + # outliers hidden
  theme(legend.position='none',panel.grid.major.x=element_blank()) +
  ylab('Value') +
  labs(title='Boxplot of EU Stock Indexes') +
   # imperfect solution but the negative third argument in expand 
   # shrinks the ylimit. (the hidden outliers otherwise expand the graph)
  scale_y_continuous(labels=scales::comma,expand=c(0.1,0,-.20,0)) +
 #  geom_jitter(width = 0.1) +
  scale_fill_manual(values = wes_palette('Zissou1'))
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/boxplot-1.png)<!-- -->

### Dotplot

``` r
ggplot(starwars_jac %>% filter(gender %in% c('Male','Female')), aes(x = factor(gender), y = height,fill=gender)) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  xlab('Gender') +
  ylab('Height (cm)') +
  theme(legend.position='none') +
  labs(title='Height Distribution of Starwars Characters')
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 5 rows containing non-finite values (stat_bindot).

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/dotplot-1.png)<!-- -->

### Violin

``` r
ggplot(starwars_jac %>% filter(gender %in% c('Male','Female')), aes(x = factor(gender), y = mass,fill=gender)) +
  geom_violin() +
  xlab('Gender') +
  ylab('Weight (kg)') +
  theme(legend.position='none') +
  labs(title='Weight Distribution of Starwars Characters')
```

    ## Warning: Removed 27 rows containing non-finite values (stat_ydensity).

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/violin-1.png)<!-- -->

## Ranking

### Dotplot

``` r
gapminder_continent_life <- gapminder %>% filter(year==2007) %>% group_by(continent) %>%
         summarize(mean_life=mean(lifeExp),min_life=min(lifeExp),max_life=max(lifeExp))

ggplot(gapminder_continent_life, aes(x=continent, y=mean_life,color=continent)) + 
#  geom_point(col="tomato2", size=3) +   # Draw points
geom_pointrange(mapping=aes(ymin=min_life, ymax=max_life)) + 
  theme(legend.position='none') +
  labs(title="Life Expectancy Range by Continent",
       caption="Data: 2007. Minimum, Mean, and Maximum Life Expectancies Shown") +  
  coord_flip() +
  xlab('Continent') +
  ylab('Life Expectancy at Birth') 
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/dotplot-rank-1.png)<!-- -->

### Lollipop

``` r
  ggplot(data=murder_rates, aes(x=State, y=Murder) ) +
    geom_segment( aes(x=State ,xend=State, y=0, yend=Murder), color="grey") +
    geom_point(size=3, color=cbPalette[6]) +
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

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/lollipop-1.png)<!-- -->

### Bar

``` r
# A simple bar chart - average heights of the species
# the reorder command orders our bars in order of descending height
ggplot(data=species_summ,
          aes(x = reorder(species,-average_height), y=average_height, fill = species)) +
geom_bar(stat='identity',position='dodge',color='black') +
# remove bottom inner margin with expand
scale_y_continuous(expand = c(0,0,0.08,0)) + 
scale_fill_manual(values=wes_palette('Moonrise3')) +
geom_text(aes(label=round(average_height)), vjust=-0.5) +
theme(legend.position="none",
      panel.grid = element_blank()) + # turn off grid
labs(title='Average Height of Selected Star Wars Species (cm)') +
xlab('Species') +
ylab('')
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/bar-1.png)<!-- -->

``` r
## Side-by-Side Barchart

p1 <- ggplot(mtcars, aes(x = reorder(row.names(mtcars),cyl), y=cyl)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle("Cylinders") +
        xlab('Car') +
        ylab('')

# order bars
p2 <- ggplot(mtcars, aes(x = reorder(row.names(mtcars), mpg), y = mpg)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle("MPG") + 
        xlab('') +
        ylab('')

grid.arrange(p1, p2, ncol = 2)
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/bar-2.png)<!-- -->

``` r
##  Take a look at number of each species from each Starwars homeworld
ggplot(data=homeworld_summ,
          aes(x = species_collapsed, y=n,fill = species_collapsed)) +
# The scales argument suppress the presense of an empty "Other" species
# slot on Tatooine
facet_grid(~homeworld,scales = 'free',space='free') +
geom_bar(stat='identity',color='black') +
# remove bottom inner margin with expand
scale_y_continuous(expand = c(0,0,0.08,0)) + 
scale_fill_manual(values=wes_palette('Moonrise2')) +
theme(legend.position="none",legend.title=element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank()) +
labs(title='Number of Star Wars Characters of Each Species from Selected Homeworlds') +
xlab('') +
ylab('')
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/bar-3.png)<!-- -->

``` r
## Stacked bar of Titanic dataset

ggplot(data=titanic_bar,
          aes(x = Sex, y=percent_num,fill = fct_rev(Class))) +
facet_grid(~Survived) +
geom_bar(stat='identity',color='black') +
coord_flip() +
  geom_text(data=titanic_bar,aes(label = ifelse(percent_num > 0.07 ,percent_char,NA)),
    size = 3,position = position_stack(vjust = 0.5)) +
scale_fill_manual(values=wes_palette('Royal2')) +
theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
      panel.grid = element_blank())+
labs(title='Titanic Passengers by Survival Status') +
xlab('') +
ylab('') +
guides(fill = guide_legend(title='Class',reverse=T))
```

    ## Warning: Removed 4 rows containing missing values (geom_text).

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/bar-4.png)<!-- -->

## Correlation

### Scatterplot

``` r
# Linear model
fit <- lm(height ~ mass, data=starwars_ht_wt)
coeff <- as.numeric(fit$coefficients[2])
r_square <- summary(fit)$r.squared

# Scatter plot of heights and weights 
# Note - group=1 is set in ggplot aes to force geom_smooth to fit
# both groups
ggplot(data=starwars_ht_wt,
          aes(x = mass, y = height, color = gender,group=1)) +
geom_point() +
  # remove legend margins
theme( legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0)) +
geom_label_repel( # Labels
    data = starwars_ht_wt,
    aes(label = name),
    size = 3,
    force = 5,
    box.padding = 0.2, # use this to control label spacing
    segment.color = 'grey',
    show.legend = F # need this to fix legend
  ) +

geom_smooth(method="lm",show.legend=F,size=0.5,alpha=0.25) + # Regression line
scale_color_manual(values=c(cbPalette[2:5])) +
labs(title='Heights and Weights of Selected Star Wars Characters',
     subtitle = bquote(Slope == .(round(coeff,2)) ~ ' | ' ~ R^2  ==  .(round(r_square,2)) ),
     caption='95% confidence interval is shaded') +
xlab('Weight (kg)') +
ylab('Height (cm)') +
guides(color=guide_legend(title='Gender',override.aes = list(size=2.5)))
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/scatter-1.png)<!-- -->

### Bubbleplot

``` r
ggplot(data=gapminder %>% filter(year==2007),
          aes(x = gdpPercap, y = lifeExp, color = continent,size=pop,group=1)) +
geom_point() +
  # remove legend margins
theme( legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0),
      legend.pos='right') +
scale_x_continuous(labels=scales::dollar) +
geom_smooth(method="loess",show.legend=F,size=0.5,alpha=0.25) + # Regression line
#scale_color_manual(values=wes_palette('Royal2')) +
labs(title='The Wealth of Nations - GDP v. Life Expectancy',
     caption='Data is for 2007. 95% confidence interval is shaded.') +
xlab('GDP Per Capita (USD, inflation-adjusted)') +
ylab('Life Expectancy (at birth)') +
guides(color=guide_legend(title='Continent',override.aes = list(size=2.5)),
       size=guide_legend(title='Population'))
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/bubbleplot-1.png)<!-- -->

## Evolution

### Line

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
scale_color_manual(values=wes_palette('GrandBudapest2')) +
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

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/line-1.png)<!-- -->

``` r
# Number of characters from each species 
ggplot(data=starwars_gender_film,
          aes(x = episode, y=prop,color = gender)) +
# show.legend=F prevents the geom (in this case a line) from being in the legend
geom_line(show.legend=F) + geom_point() +
geom_text_repel( # Labels
    data = starwars_gender_film %>% mutate(gender=str_to_title(gender)) %>%
      group_by(gender) %>% filter(episode==max(episode)), # only display label on last episode
    aes(label =gender),
    size = 3,
    nudge_x=0.3,
    point.padding=.4, # give the data points room
    force = 1,
    box.padding = 0.2, # use this to control label spacing
    segment.color = NA,
    show.legend = F # need this to fix legend
  ) +
scale_x_continuous(breaks=c(1:7),expand=c(0.02,0,0.07,0)) +
scale_y_continuous(labels=scales::percent) + 
scale_color_manual(values=cbPalette) +
labs(title='Percentage of Star Wars Characters in Each Film by Gender') +
theme(legend.title = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position='none') +
xlab('Episode') +
ylab('') +
  guides(color=guide_legend(title='Gender',override.aes = list(size=2.5)))
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/line-2.png)<!-- -->

### Stacked Area

``` r
# Number of characters from each species 
ggplot(data=starwars_species_film,
          aes(x = episode, y=n,fill = species_collapsed)) +
geom_area(aes(group=species_collapsed),color='black') +
# remove bottom inner margin with expand
scale_y_continuous(expand = c(0,0,0.05,0)) + 
scale_x_continuous(breaks=c(1:7),expand=c(0,0)) +
scale_fill_manual(values=wes_palette('Moonrise2')) +
labs(title='Number of Star Wars Characters Appearing from Each Species by Film') +
theme(legend.title = element_blank(),
      panel.grid.minor.x = element_blank()) +
xlab('Episode') +
ylab('')
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/stackedarea-1.png)<!-- -->

## Composition

### Heatmap

``` r
titanic_survival_rates <- titanic %>%
  group_by(Class,Sex,Survived) %>%
  summarize(n=sum(n)) %>%
  group_by(Class,Sex) %>% 
  mutate(survival_rate = n/sum(n)) %>%
  ungroup() %>%
  filter(Survived == 'Yes') %>%
  select(-Survived)

ggplot(titanic_survival_rates, aes(Sex, Class)) + 
     geom_tile(aes(fill = survival_rate), colour = "grey") + 
     scale_fill_gradient(limits=c(0,1),labels=scales::percent, low = "white",high = "steelblue") +
     geom_text(aes(label=formattable::percent(survival_rate,1))) +
     ggtitle('Survival Rates on the Titanic') +
     theme_minimal() +
     theme(panel.grid = element_blank(),
           plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)) +
     scale_x_discrete(expand=c(0,0,0,0)) +
     scale_y_discrete(expand=c(0,0,0,0)) +  
     # quo_name() access the character name of a variable
     guides(fill=guide_legend(title='Survival Rate')) #+
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/heatmap-1.png)<!-- -->

``` r
  #   xlab(quo_name(axis1)) + ylab(quo_name(axis2))
```

### Treemap

``` r
library(treemap)
# Treemap of titanic
treemap(titanic, #Your data frame object
        index=c("Sex","Class"),  #A list of your categorical variables
        vSize = "n",  #This is your quantitative variable
        vColor='Class',
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = wes_palette('Moonrise3'),  #Select your color palette from the RColorBrewer presets or make your own.
        title="Titanic Passengers by Class and Gender", #Customize your title
        fontsize.title = 15 #Change the font size of the title
        )
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/treemap-1.png)<!-- -->

``` r
library(treemapify)
# Treemap of star wars character mass
ggplot(data=starwars %>% drop_na(mass) %>% replace_na(list(gender='none')) %>%
         mutate(gender=str_to_title(gender)),
                aes(area=mass,fill=gender,label=name)) + 
  labs(title='Relative Weights of Star Wars Characters') +
  scale_fill_manual(values=wes_palette('Moonrise3')) +
  geom_treemap(color='black') +
  geom_treemap_text(colour = "white", place = "centre", grow = F) +
  guides(fill=guide_legend(title="Gender"))
```

![](/home/cambonator/Programming/Data-Science-Codex/rmd_images/Visualization_Cookbook/treemap-2.png)<!-- -->

### Waffle

The code currently doesn’t run. (eval=FALSE)

``` r
library(waffle) # waffle charts, make sure to install the github version with devtools::install_github("hrbrmstr/waffle")

waffle_palette <- wes_palette('Darjeeling2')
waffle_palette[5] <- 'white' # 

waffle( titanic_class %>% 
    rename(names=Class,vals=n),  # rename data to match waffle chart syntax
  rows = 33, size = 0.5, 
  colors = waffle_palette) +
  # remove margin around graph with expand
  scale_x_continuous(expand=c(0,0,0,0)) +
  scale_y_continuous(expand=c(0,0,0,0)) +
  labs(title='Titanic Passengers by Class') +
  theme(plot.title = element_text(lineheight=1, face="bold",hjust = 0.5,size=14),
        legend.position='bottom') +
  guides(fill = guide_legend(title='Class'))
```
