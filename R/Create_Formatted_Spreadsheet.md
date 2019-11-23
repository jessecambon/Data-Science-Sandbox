Formatted Spreadsheet Generator
================
Jesse Cambon
23 November, 2019

We will use the classic gapminder dataset to create a formatted
spreadsheet report using the openxlsx package. This code allows us to
create the spreadsheet report directly from code instead of using manual
copy and pasting and formatting. This is especially conveniant for
recurring reports.

To view the resulting spreadsheet report, [click
here](gapminder_summary_report.xlsx).

## Setup

``` r
output_file <- 'gapminder_summary_report.xlsx'

library(gapminder)
library(tidyverse)
library(openxlsx)
library(knitr)
library(kableExtra)

gap_continent_summ <- gapminder %>% 
  filter(year==2007) %>% select(-year) %>%
  group_by(continent) %>%
  summarize(lifeExp=weighted.mean(lifeExp,pop),
            gdpPercap=weighted.mean(gdpPercap,pop),
            pop=sum(as.numeric(pop))) %>%
  ungroup() %>%
  mutate(header_level=1)

# Grab the top 5 most populuous countries from each continent
gap_top_countries <- gapminder %>%
  filter(year==2007) %>% select(-year) %>%
  group_by(continent) %>%
  arrange(desc(pop)) %>% slice(1:5) %>%
  ungroup() %>% 
  mutate(indent_level=1)

gap_combi <- bind_rows(gap_continent_summ,gap_top_countries) %>%
  arrange(continent,desc(pop)) %>%
  # combine country and continent fields for stacking
  mutate(category=coalesce(as.character(country),
                           as.character(continent)),
         pop=pop/10^6) %>%   # Convert to counting millions for easy display
  select(category,lifeExp,gdpPercap,pop,everything()) %>%
  select(-country,-continent) %>%
  rename(`Life Expectancy` = lifeExp, `GDP Per Capita`=gdpPercap, `Population (Millions)`=pop)
```

``` r
kable(gap_combi %>% select(-header_level,-indent_level) %>% rename(`Continent/Country`=category),format='markdown',
      format.args=list(big.mark=','),digits=c(0,1,0,0)) %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

    ## Warning in kable_styling(., bootstrap_options = c("striped", "border")): Please
    ## specify format in kable. kableExtra can customize either HTML or LaTeX outputs.
    ## See https://haozhu233.github.io/kableExtra/ for details.

| Continent/Country | Life Expectancy | GDP Per Capita | Population (Millions) |
| :---------------- | --------------: | -------------: | --------------------: |
| Africa            |            54.6 |          2,561 |                   930 |
| Nigeria           |            46.9 |          2,014 |                   135 |
| Egypt             |            71.3 |          5,581 |                    80 |
| Ethiopia          |            52.9 |            691 |                    77 |
| Congo, Dem. Rep.  |            46.5 |            278 |                    65 |
| South Africa      |            49.3 |          9,270 |                    44 |
| Americas          |            75.4 |         21,603 |                   899 |
| United States     |            78.2 |         42,952 |                   301 |
| Brazil            |            72.4 |          9,066 |                   190 |
| Mexico            |            76.2 |         11,978 |                   109 |
| Colombia          |            72.9 |          7,007 |                    44 |
| Argentina         |            75.3 |         12,779 |                    40 |
| Asia              |            69.4 |          5,432 |                 3,812 |
| China             |            73.0 |          4,959 |                 1,319 |
| India             |            64.7 |          2,452 |                 1,110 |
| Indonesia         |            70.7 |          3,541 |                   224 |
| Pakistan          |            65.5 |          2,606 |                   169 |
| Bangladesh        |            64.1 |          1,391 |                   150 |
| Europe            |            77.9 |         25,244 |                   586 |
| Germany           |            79.4 |         32,170 |                    82 |
| Turkey            |            71.8 |          8,458 |                    71 |
| France            |            80.7 |         30,470 |                    61 |
| United Kingdom    |            79.4 |         33,203 |                    61 |
| Italy             |            80.5 |         28,570 |                    58 |
| Oceania           |            81.1 |         32,885 |                    25 |
| Australia         |            81.2 |         34,435 |                    20 |
| New Zealand       |            80.2 |         25,185 |                     4 |

## Create Spreadsheet

Now we will create the formatted spreadsheet

Comma, rounded, and percent columns can be entered either as column
position or as a column name. Alternatively, you can pass a list of
specific format strings to ‘format\_cols’.

You can pass ‘indent\_level’ and ‘header\_level’ columns to the
create\_worksheet function for formatting purposes. These columns will
not be shown in the resulting spreadsheet.

``` r
## Define the function for creating a worksheet
create_worksheet <- function(wb,sheet_name,data,
         title1_text="", # Sheet title
         comma_cols=NULL, # Column #s or names to be in comma format
         percent_cols=NULL, # Column #s or names to be in percent format
         round_cols=NULL, # Column #s or names to be rounded
         format_cols=NULL, # Pass a list of formats for each column to this parameter
         caption=NULL, # A description to be put below the main table in the spreadsheet
         col_widths=NULL # Pass a list of column widths (1 per column)
         ) {
  
  # Extract the indent values if they are provided 
  # and then remove from dataframe
  if (("indent_level") %in% colnames(data)) {

    indent_1_rows <- which(1 == data$indent_level)
    #print(indent_1_rows)
    indent_2_rows <- which(2 == data$indent_level)
    data <- data %>% select(-indent_level)
  }
  
  # Extract the header level values if they are provided 
  # and then remove from dataframe
  if (("header_level") %in% colnames(data)) {

    header_1_rows <- which(1 == data$header_level)
    header_2_rows <- which(2 == data$header_level)
    data <- data %>% select(-header_level)
  }
  
  # If percent and column columns were provided in character
  # (variable name) format then extract the column numbers
  if (class(percent_cols)=='character') {
    percent_cols = which(colnames(data) %in% percent_cols)
  }
  if (class(comma_cols)=='character') {
    comma_cols = which(colnames(data) %in% comma_cols)
  }
  
  if (class(round_cols)=='character') {
    round_cols = which(colnames(data) %in% round_cols)
  }

  # delete column header
  colnames(data) <- str_replace_all(colnames(data),
            c('category' = ''))

  
  # a thick vertical border will be drawn to the right of these columns
  sep_borders <- c()
    
  ## row on which column headers are (data table begins on this row)
  table_header_row = 3
  sheet_header_row = table_header_row - 1 # Overall sheet title
  # grouping_header_row = table_header_row - 1 # titles that group columns
  
  table_width = ncol(data) # number of columns
  table_last_row = table_header_row + nrow(data) # last row of data in the table

  ##############
  
  # Create worksheet
  addWorksheet(wb, sheet_name)
  
  # Style for sheet title
  table_header_style <- createStyle(halign='center',valign='center',textDecoration="Bold")
  
  # Style for column headers
  column_header_style <- createStyle(halign='center',valign='center',textDecoration="Bold",
                                     fgFill='#BFBFBF')
  
  ### Titles 
  # Merge cells for titles
  mergeCells(wb, sheet_name, cols = 2:(table_width+1), rows = sheet_header_row) # Overall Header
  
  # Write titles to Worksheet
  writeData(wb,sheet_name,title1_text,startCol=2,startRow=sheet_header_row)

  ## Style titles
  addStyle(wb,sheet_name,style=table_header_style,cols=1:table_width+1,rows=sheet_header_row,gridExpand = T,stack=T)

  # Write main data table to worksheet
  writeData(wb,sheet_name,data,startRow = table_header_row,startCol=2)
  
  if (!is.null(caption)) {
  ## Write caption text below table if it is included
  writeData(wb,sheet_name,caption,startCol=2,startRow=table_last_row+2)
  }
  
  ### Table styling
  # Draw borders for all cells in table
  addStyle(wb,sheet_name, style=createStyle(border='TopBottomLeftRight'), cols=1:table_width+1,
           rows=table_header_row:table_last_row,gridExpand = T)
  
  ## Format specified columns as percent, comma, or rounded (decimal) format
  if (!is.null(comma_cols)) {
  ## Apply Comma format
  addStyle(wb,sheet_name, style=createStyle(numFmt='COMMA'), cols=comma_cols+1,
           rows=table_header_row+1:table_last_row-table_header_row,gridExpand = T,stack=T)
  }
  if (!is.null(percent_cols)) {
  ## Apply Percent format
  addStyle(wb,sheet_name, style=createStyle(numFmt='PERCENTAGE'), cols=percent_cols+1,
           rows=table_header_row+1:table_last_row-table_header_row,gridExpand = T,stack=T)
  }
  
  if (!is.null(round_cols)) {
  ## Apply Rounded format
  addStyle(wb,sheet_name, style=createStyle(numFmt="0."), cols=round_cols+1,
           rows=table_header_row+1:table_last_row-table_header_row,gridExpand = T,stack=T)
  }
  
  if (!is.null(format_cols)) {
  ## Apply formats in list to the columns in the table
  
  format_col_num <- 2
  for(format in format_cols) {
    addStyle(wb,sheet_name, style=createStyle(numFmt=format), cols=format_col_num,
           rows=table_header_row+1:table_last_row-table_header_row,gridExpand = T,stack=T)
    format_col_num <- format_col_num + 1
    }
  }

  ## Apply header formats to specifed rows
  if (exists("header_1_rows")) {
    # Bold and Center
  addStyle(wb,sheet_name,style=createStyle(textDecoration = "Bold"),
           cols=c(2),rows=table_header_row+header_1_rows,stack=T)
  # Highlight
  addStyle(wb,sheet_name,style=createStyle(fgFill='#D9D9D9'),
           cols=1:table_width+1,rows=table_header_row+header_1_rows,stack=T,gridExpand = T)
  }
  if (exists("header_2_rows")) {
  addStyle(wb,sheet_name,style=createStyle(textDecoration="bold"),cols=c(2),
         rows=table_header_row+header_2_rows,stack=T)
  }
  
  ## Indent specifed rows (just the first column) 
  if (exists("indent_1_rows")) {
  addStyle(wb,sheet_name, style=createStyle(indent=1), cols=c(2),
           rows=table_header_row+indent_1_rows,gridExpand = T,stack=T)
  }
  if (exists("indent_2_rows")) {
  addStyle(wb,sheet_name, style=createStyle(indent=2), cols=c(2),
           rows=table_header_row+indent_2_rows,gridExpand = T,stack=T)
  }

  # Column headers formatting
  addStyle(wb,sheet_name,style=column_header_style,
    cols=1:table_width+1, rows=table_header_row,
         gridExpand = T,stack=T)
  
  ## Draw thick borders surrounding table and also around
  # select columns within the table
  addStyle(wb,sheet_name,style=createStyle(border='Top',borderStyle='medium'),
           cols=2:(table_width+1),rows=c(table_header_row,table_last_row+1),stack=T,gridExpand = T )
  addStyle(wb,sheet_name,style=createStyle(border='Left',borderStyle='medium'),
           cols=c(2,table_width+2,sep_borders+1),rows=table_header_row:table_last_row,stack=T,gridExpand = T )
  
  
  # Set column widths
  if (!is.null(col_widths)) {
  setColWidths(wb,sheet= sheet_name,
     cols=2:(table_width+1),
     widths = col_widths)
  } else {
    setColWidths(wb,sheet= sheet_name,
       cols=2:(table_width+1),
      widths='auto')
  }
}

## Create the spreadsheet

wb <- createWorkbook()
create_worksheet(wb,"Summary",gap_combi,title1_text="Gapminder Data by Continent",
                 format_cols=c('GENERAL','0',"$#,##0",'#,##0'),
                 caption='The 5 most populous countries for each continent are shown.',
                 col_widths=c(18,15,15,20)
              #   comma_cols=c('Population'),
              #   round_cols=c('Life Expectancy','GDP Per Capita')
                 )
create_worksheet(wb,"All Countries",gapminder %>% filter(year==2007) %>% select(-year),title1_text="All Gapminder Countries",comma_cols=c('pop'),
                 format_cols=c('GENERAL','GENERAL','0',"#,##0",'$#,##0'),
                col_widths=c(22,15,15,15,15)
                # round_cols=c('gdpPercap','lifeExp'))
                )
saveWorkbook(wb, file = output_file, overwrite = T)
```

    ## Note: zip::zip() is deprecated, please use zip::zipr() instead
