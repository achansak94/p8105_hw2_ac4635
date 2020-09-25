Homework 2
================
JR Chansakul
2020-09-21

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

## Problem 1

Read the Mr. Trashwheel dataset.

``` r
trashwheel_df =
  read_xlsx("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "Mr. Trash Wheel",
    range = cell_cols("A:N")) %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>% 
  mutate(
    sports_ball = round(sports_balls),
    sports_ball = as.integer(sports_balls)
  )
```

This dataset contains information for the Mr. Trashwell trash collector
in Baltimore, Maryland. As trash enters the inner harbor, the trashwheel
collects that trash

Read precipitation data\!

``` r
precip_2018 =
  read_xlsx("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "2018 Precipitation",
    skip = 1
  ) %>%  
  janitor::clean_names() %>% 
  drop_na(month) %>% 
    mutate(year = 2018) %>% 
    relocate(year)
  
precip_2017 =
  read_xlsx("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "2018 Precipitation",
    skip = 1
  ) %>%  
  janitor::clean_names() %>% 
  drop_na(month) %>% 
    mutate(year = 2017) %>% 
    relocate(year)
```

Now combine annual precipitation

``` r
month_df =
    tibble(
      month_num = 1:12
    )

precip_df = 
  bind_rows(precip_2018, precip_2017)
```

This dataset contains information from the Mr. Trashwheel trash
collector in Baltimore, Maryland. As trash enters the inner harbor, the
trashwheel collects that trash, and stores it in a dumpster. The dataset
contains information on year, month, and trash collected, include some
specific kinds of trash. There are a total of 344 rows in our final
dataset. Additional data sheets include month precipitation data. In
this dataset:

  - The median number of sports balls found in a dumpster in 2017 was 8
  - The total precipitation in 2018 was 70.33 inches.

## Problem 2

## Problem 2: Part 1

Import and clean NYC transit data.

``` r
NYC_transit_df =
  read_csv("./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
      col_types = cols(
      Route8 = col_character(),
      Route9 = col_character(),
      Route10 = col_character(),
      Route11 = col_character()
      )) %>% 
    janitor::clean_names() %>% 
    select(line:ada, -staffing, -staff_hours, -exit_only) %>% 
    mutate(entry = recode(entry, 'YES' = TRUE, 'NO' = FALSE)
    )
    
```

This dataset contains entry and exit information for the NYC subway
station with the following variables of interest filtered from the
imported dataset: line, station\_name, station\_latitude,
station\_longitude, route1, route2, route3, route4, route5, route6,
route7, route8, route9, route10, route11, entrance\_type, entry,
vending, ada. I kept variables with key information such as station
name, entrance type and ADA compliance and removed variables such as
exit and entrance location. Additionally, the entry variable was
converted from a character to a logical variable. The dataset has a
total of 1868 rows and 19 columns with each row representing an entrance
or exit. Each row contains the line, name, location (latitude,
longitude) and route(s) associated with the station. The dataset is not
tidy because the route 1 through route 11 columns contains variable
information (e.g. subways on that line) and has many NAs.

## Problem 2: Part 2

``` r
count(distinct(NYC_transit_df, line, station_name, na.rm=TRUE))
## # A tibble: 1 x 1
##       n
##   <int>
## 1   465

count(filter(NYC_transit_df, ada == TRUE, na.rm=TRUE) %>%
        distinct(line, station_name))
## # A tibble: 1 x 1
##       n
##   <int>
## 1    84

count(filter(NYC_transit_df, vending == "NO", na.rm=TRUE) %>%
  filter (entry == "TRUE"))
## # A tibble: 1 x 1
##       n
##   <int>
## 1    69

count(filter (NYC_transit_df, vending == "NO", na.rm=TRUE))
## # A tibble: 1 x 1
##       n
##   <int>
## 1   183
```

There are 465 distinct stations.

There are 84 stations that are ADA compliant.

Among the 183 subway entrances/exits without vending, 69 allowed entry.

## Problem 2: Part 3

``` r
NYC_transit_tidy =
    NYC_transit_df
```