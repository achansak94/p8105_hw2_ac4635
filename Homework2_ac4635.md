Homework 2
================
JR Chansakul
2020-09-28

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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
      )) %>%  #change route to character names 
    janitor::clean_names() %>% # clean names
    select(line:ada, -staffing, -staff_hours, -exit_only) %>%  # Select variables I want to remove and keep 
    mutate(entry = recode(entry, 'YES' = TRUE, 'NO' = FALSE) # Mutate entry code to logical variable  
    )
    
```

This dataset contains entry and exit information for the NYC subway
station with the following variables of interest selected from the
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
count(distinct(NYC_transit_df, line, station_name)) # distinct stations 
## # A tibble: 1 x 1
##       n
##   <int>
## 1   465

count(filter(NYC_transit_df, ada == TRUE) %>% # distinct stations that are ADA compliant
        distinct(line, station_name))
## # A tibble: 1 x 1
##       n
##   <int>
## 1    84

count(filter (NYC_transit_df, vending == "NO")) # subway stations without vending 
## # A tibble: 1 x 1
##       n
##   <int>
## 1   183

count(filter(NYC_transit_df, vending == "NO") %>% # subway stations without vending that allowed entry
  filter (entry == "FALSE"))
## # A tibble: 1 x 1
##       n
##   <int>
## 1   114

(count(filter(NYC_transit_df, vending == "NO") %>%
  filter (entry == "TRUE")))/(count(filter (NYC_transit_df, vending == "NO"))) # Proportion of subway stations without vending that allowed entry
##           n
## 1 0.3770492
```

There are 465 distinct stations.

There are 84 stations that are ADA compliant.

Among the 183 subway entrances/exits without vending, 69 allowed entry.
The proportion of station entrances/exits without vending that allow
entry is 0.3770492.

## Problem 2: Part 3

Reformated the data so that route number and route name are distinct
variables.

``` r
NYC_transit_tidy =
  pivot_longer(
    NYC_transit_df, 
    route1:route11,
    names_to = "route_number", 
    values_to = "route_names") %>% 
    drop_na(route_names) # Pivot longer to tidy dataset 

view (NYC_transit_tidy) # view dataset 
```

After cleaning the dataset, there are a total of 4270 rows and 10
columns. Dataset is now tidy because the NA responses have been removed
from the route\_names variable.

The code chunk below creates a dataset that counts distinct serve the A
Train.

``` r
# Dataset that counts distinct stations that serve A train
  Distinct_A_stations= 
    NYC_transit_tidy %>%
    filter(route_names == 'A') %>% 
    distinct(station_name, line) %>%
    count()
    
# Dataste that counts stations that serve A train that are ADA compliant
ADA_compliant_A_trains = 
  NYC_transit_tidy %>% 
  filter(route_names == 'A', ada == TRUE) %>% 
  distinct(station_name, line) %>% 
  count()  
```

There are 60 distinct stations that serve the A train and 17 are ADA
compliant.

## Problem 3

Clean and tidy the dataset in pols-month.csv.

``` r
pols_month_df = 
  read_csv("./data/pols-month.csv") %>% #read in csv file
  janitor::clean_names() %>% #clean names
  separate (mon, into = c("Year", "Month", "Day")) %>% #Separate date into 3 columns: month, day, year 
  mutate(Month = as.numeric(Month)) %>% #Convert month to numeric variable
  mutate(Month = month.abb[Month]) %>%  #Abbreviate month variable from number
  pivot_longer(
    cols = starts_with("prez"),
    names_to = "president_party",
    values_to = "value") %>% #Take the month column variables and make it into rows 
  filter(value %in% c(1,2)) %>% 
  mutate(president_party = recode(
    president_party, `prez_gop` = 'gop', `prez_dem` = 'dem')) %>% #recode president party observations to gop and dem
  select(-Day, -value) %>% #Remove day and value columns 
  arrange (Year, Month) %>% #Arrange year and month to front of dataset 
  view ()
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_double(),
    ##   gov_gop = col_double(),
    ##   sen_gop = col_double(),
    ##   rep_gop = col_double(),
    ##   prez_dem = col_double(),
    ##   gov_dem = col_double(),
    ##   sen_dem = col_double(),
    ##   rep_dem = col_double()
    ## )

``` r
view(pols_month_df) # View dataset 
```

Clean and tidy the dataset in the snp dataset.

``` r
snp_df = 
  read_csv("./data/snp.csv") %>% #read in csv file
  janitor::clean_names() %>% #clean names
  separate(date, into = c("Month", "Day", "Year")) %>% #Separate date into 3 columns: month, day, year 
  relocate(Year, Month) %>% #Relocate year and month variable to front
  mutate(Month = as.numeric(Month)) %>%  #Convert month to numeric variable 
  mutate(Month = month.abb[Month]) %>% #Abbreviate month variable from number
  arrange (Year, Month) %>% #Arrange year and month
  select (-Day) %>% #Remove day variable 
  view()
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

Clean and tidy the dataset in the umeployment dataset.

``` r
unemployment_tidy_df =
  read_csv("./data/unemployment.csv") %>% #read in csv file
  pivot_longer(
    cols = Jan:Dec,
    names_to = "Month",
    values_to = "Unemployment_rate") %>% #Take the month column variables and make it into rows 
  mutate(Year = as.character(Year)) %>% #Change Year to character variable to merge in final dataset  
  view()
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Jan = col_double(),
    ##   Feb = col_double(),
    ##   Mar = col_double(),
    ##   Apr = col_double(),
    ##   May = col_double(),
    ##   Jun = col_double(),
    ##   Jul = col_double(),
    ##   Aug = col_double(),
    ##   Sep = col_double(),
    ##   Oct = col_double(),
    ##   Nov = col_double(),
    ##   Dec = col_double()
    ## )

Last step, join the datasets by merging snp into pols, and then merging
unemployment into the result.

``` r
Combine_data = 
  left_join(pols_month_df, snp_df, by = c("Year", "Month")) #left join pols_month_df dataset by snp_df

Final_df = 
  left_join(Combine_data, unemployment_tidy_df, by = c("Year", "Month")) #left join combine_data by unemployment_tidy_df

#View combined datasets 
view(Combine_data)
view(Final_df)
```

Write a short paragraph about these datasets. Explain briefly what each
dataset contained, and describe the resulting dataset (e.g. give the
dimension, range of years, and names of key variables):

The pols-month\_df dataset contains the number of democratic and
republican governors, US Senators, and US House of Representatives as
well as the president’s political affiliation at a moment in time. The
dataset was restricted to the democractic and republican parties. After
tidying the dataset, I have political information between the years 1947
and 2015 with 822 observations and 9 variables in the dataset. These are
the following variables in this dataset: gov\_dem, gov\_gop, Month,
president\_party, rep\_dem, rep\_gop, sen\_dem, sen\_gop, Year. I
created a “prez\_party” variable indicated the incumbent’s president
political party. I kept prez\_gop that was coded as 2 because that was
during Nixon’s resignation and the vice president, Ford, was affiliated
with the gop party when he was acting president.

After the “snp\_df” dataset, it contained 787 observations and 9
variables in the dataset, which were close, Month, Year. This datset
provides information on the Standard & Poor’s stock market index (S\&P)
from 1950 to 2015. I separated the dates into month, day and year like
the pols-month\_df dataset and removed day from the “snp\_df” dataset.
The variable “close” represents the closing values of the S\&P stock
index on that specific date.

After tidying the unemployment\_tidy\_df dataset, it contained 816
observations and 3 variables in the dataset, which were Month,
Unemployment\_rate, Year. I created two variables, month and
unemployment rate in our tidy dataset using pivot\_longer. This datset
provides unemployment rate by month from 1948 to 2015.

The final dataset was combined using left\_join and contains 822 rows by
11 columns that includes NA for empty observations for each variable ..
The dataset contains on politicians’ political affiliation, unemploymnet
rate and the S\&P stock market from 1947 to 2015. The important
variables are described above in the separate datasets and the final
dataset includes these variables:close, gov\_dem, gov\_gop, Month,
president\_party, rep\_dem, rep\_gop, sen\_dem, sen\_gop,
Unemployment\_rate, Year.
