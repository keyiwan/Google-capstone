Google Data Analytics Certificate - Bike Share Case Study - Part1
================
K.Wan
2022/3/28

## Case Background

This analysis is for case study 1 from the Google Data Analytics
Certificate (Cyclistic). It’s originally based on the case study
*[Sophisticated, Clear, and Polished: Divvy and Data
Visualization](https://artscience.blog/home/divvy-dataviz-case-study)*
by Kevin Hartman.

I will be using the [Divvy
dataset](https://divvy-tripdata.s3.amazonaws.com/index.html) for this
capstone project. The purpose of this script is to consolidate
downloaded Divvy data into a single dataframe and then conduct simple
analysis to help answer the key question:“**In what ways do members and
casual riders use Divvy bikes differently?**”

In the Google Data Analytics Certificate programme, there are mainly six
phases: **Ask**, **Prepare**, **Process**, **Analyse**, **Share**, and
**Act**. Since this capstone challenge has outlined my tasks as a junior
data analyst in this fictitious bike sharing company and provided real
first-party data, my job will jump right into the **Process** of data.

## Process

### Get data ready

For convenience, I will first load three useful packages: `tidyverse`
for data import and wrangling, `lubridate` for date functions, and
`ggplot2` for visualisation.

``` r
# install.packages("tidyverse")
# install.packages("lubridate")

library(tidyverse) # data import and tidy
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate) # functions set for date, time
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2) # helps visualise data
```

**1. Upload datasets**

In this analysis, I will investigate data in the past twelve months,
spanning from December 2021 to February 2022.

Function `readr::read_csv` is preferred to `read.csv` of package `base`
for a number of reasons:
<https://r4ds.had.co.nz/data-import.html#compared-to-base-r>.

``` r
dt_2202 <- read_csv("202202-divvy-tripdata.csv", na = "")
# na = "" is to assign blank cell with NA
dt_2201 <- read_csv("202201-divvy-tripdata.csv", na = "")
dt_2103 <- read_csv("202103-divvy-tripdata.csv", na = "")
dt_2104 <- read_csv("202104-divvy-tripdata.csv", na = "")
dt_2105 <- read_csv("202105-divvy-tripdata.csv", na = "")
dt_2106 <- read_csv("202106-divvy-tripdata.csv", na = "")
dt_2107 <- read_csv("202107-divvy-tripdata.csv", na = "")
dt_2108 <- read_csv("202108-divvy-tripdata.csv", na = "")
dt_2109 <- read_csv("202109-divvy-tripdata.csv", na = "")
dt_2110 <- read_csv("202110-divvy-tripdata.csv", na = "")
dt_2111 <- read_csv("202111-divvy-tripdata.csv", na = "")
dt_2112 <- read_csv("202112-divvy-tripdata.csv", na = "")
```

**2. Merging multiple tables**

Before combining tables into a single data frame, I need to observe
components of each table, specifically column names of each file, in
order to make merge compatible.

Following code chunk displays some useful techniques when dealing with
this issue.

``` r
# check column names, for they need to match perfectly before we can combine files by rows
colnames(dt_2103)
colnames(dt_2104)
# if there are mismatched column names, we can rename columns to make them consisten, e.g.:
dt_2103 <- rename(dt_2103, new_name = old_name)
# inspect the dataframes and look for inconguencies
str(dt_2103)
# change column types if needed, e.g. convert ride_id to character so that they can stack correctly
dt_2103 <- mutate(dt_2103, ride_id = as.character(ride_id))
```

``` r
dt_all <- rbind(dt_2103, dt_2104, dt_2105, dt_2106,
                dt_2107, dt_2108, dt_2109, dt_2110,
                dt_2111, dt_2112, dt_2201, dt_2202)
# dplyr::bind_rows() is a good alternative 
glimpse(dt_all)  # view the structure of the dataframe
```

    ## Rows: 5,667,986
    ## Columns: 13
    ## $ ride_id            <chr> "CFA86D4455AA1030", "30D9DC61227D1AF3", "846D87A156~
    ## $ rideable_type      <chr> "classic_bike", "classic_bike", "classic_bike", "cl~
    ## $ started_at         <dttm> 2021-03-16 08:32:30, 2021-03-28 01:26:28, 2021-03-~
    ## $ ended_at           <dttm> 2021-03-16 08:36:34, 2021-03-28 01:36:55, 2021-03-~
    ## $ start_station_name <chr> "Humboldt Blvd & Armitage Ave", "Humboldt Blvd & Ar~
    ## $ start_station_id   <chr> "15651", "15651", "15443", "TA1308000021", "525", "~
    ## $ end_station_name   <chr> "Stave St & Armitage Ave", "Central Park Ave & Bloo~
    ## $ end_station_id     <chr> "13266", "18017", "TA1308000043", "13323", "E008", ~
    ## $ start_lat          <dbl> 41.91751, 41.91751, 41.84273, 41.96881, 42.01270, 4~
    ## $ start_lng          <dbl> -87.70181, -87.70181, -87.63549, -87.65766, -87.666~
    ## $ end_lat            <dbl> 41.91774, 41.91417, 41.83066, 41.95283, 42.05049, 4~
    ## $ end_lng            <dbl> -87.69139, -87.71676, -87.64717, -87.64999, -87.677~
    ## $ member_casual      <chr> "casual", "casual", "casual", "casual", "casual", "~

``` r
old_records <- nrow(dt_all)
```

The resulting tibble consists of 5,667,986 rows with 13 columns of
character and numeric data such as ride id, start and end time of ride,
start location, geographic coordinates, etc.

### Data cleaning

Data tidying usually involves checking for missing values, trailing
white spaces, duplicate records, and other data anomalies.

**1. Duplicates**

To check duplicates, `distinct()` is used to check duplicates based on
all of columns while it is also an option to screen on `ride_id` alone.
The result shows no duplicates removed.

``` r
dt_all <- dt_all %>% 
  distinct()
# distinct(ride_id, .keep_all = TRUE), based on ride_id alone
## no duplicates removed
nrow_old <- nrow(dt_all)
```

**2. Missing values**

``` r
apply(dt_all, 2, function (x) {mean(is.na(x))})
```

    ##            ride_id      rideable_type         started_at           ended_at 
    ##       0.0000000000       0.0000000000       0.0000000000       0.0000000000 
    ## start_station_name   start_station_id   end_station_name     end_station_id 
    ##       0.1257903601       0.1257898308       0.1344070010       0.1344070010 
    ##          start_lat          start_lng            end_lat            end_lng 
    ##       0.0000000000       0.0000000000       0.0008145751       0.0008145751 
    ##      member_casual 
    ##       0.0000000000

Since there is no missing value on identifier column `ride_id` and the
proportion of missing data on each variable is no more than 14%, it is
practically acceptable to drop rows containing `NA`.

``` r
dt_all <- dt_all %>% drop_na(start_station_name, start_station_id, end_station_name, end_station_id,
                       end_lat, end_lng)
nrow_new <- nrow(dt_all)
```

Then 1,036,883 rows have been deleted, accounting for merely 18% of
original data size.

**3. Strings**

When it comes to characters, typos or miscellaneous errors are not
uncommon. Error-prone strings can be due to careless data entry or even
on purpose, they are not easy to spot and even more time-consuming or
painstaking to fix.

Note that there are a few station ids representing more than one station
name due to either typo or unknown minor changes. The following steps
show how to identify mistyped station names and then conduct error
checking.

However, manually fixing the problem of station names is not essential
to answering questions of our interest and using station id alone is
sufficient.

``` r
dup_id <- dt_all %>% 
  count(start_station_id, start_station_name) %>% 
  filter(duplicated(start_station_id)) %>% 
  select(start_station_id) # return ids appearing more than once

dup_id <- unlist(as.list(dup_id)) # transform tibble into list first then to vector as `as.vector()` not applicable 

dt_all %>% 
  count(start_station_id, start_station_name) %>% 
  filter(start_station_id %in% dup_id)
```

**4. Dates**

Addressing `date` columns is necessary for analysing the dynamics of
rides on different time frames. With each ride record contains the exact
time points for start and end, this analysis can come up with a more
meaningful metric - ride length - a calculated field using the
difference between columns `ended_at` and `started_at`.

``` r
dt_all <- dt_all %>% 
  mutate(started_at = ymd_hms(started_at),
         ended_at = ymd_hms(ended_at),
         ride_length = ended_at - started_at) # figures in seconds, difftime is an alternative function
```

Convert the unit of ride length from `seconds` to `minutes`.

``` r
dt_all <- dt_all %>% 
  mutate(ride_length_m = round((as.numeric(ride_length)/60), digits = 2))
```

Dissect `date` by year, month, day and weekday.

The reason is that data can only be aggregated at the ride-level, which
is too granular based on original date formats. So I will add some
additional columns of data – such as day, month, year, weekday – that
provide additional opportunities to aggregate the data.

``` r
dt_all <- dt_all %>% 
  mutate(start_year = year(started_at), 
         end_year = year(ended_at), 
         start_month = month(started_at), # month as number
         end_month = month(ended_at),
         start_Month = month(started_at, label = TRUE), # months as Jan, Feb 
         end_Month = month(ended_at, label = TRUE), 
         start_day = mday(started_at), # day of month
         end_day = mday(ended_at), 
         start_wday = wday(started_at, label = TRUE), # day of week
         end_wday = wday(ended_at, label = TRUE))
```

**5. Remove “bad” data**

Despite no clear definition of “bad” data, here I need to spot data that
takes only a small number while having values that can
disproportionately influence our findings ,and then sometimes delete
them.

In this case, I will remove extremely large and negative values in
`ride_length` since they make no practical sense for the majority of
users and can distort data distribution, but how to address outliers is
a case-by-case study.

Specifically, records for trips less than 60 seconds (false starts) or
longer than 24 hours were removed. Bikes out longer than 24 hours are
considered stolen and the rider is charged for a replacement. Idea
inspired from:
<https://vibrantoutlook.wordpress.com/2021/09/19/google-data-analytics-certificate-capstone-project/>

``` r
dt_all02 <- dt_all %>% 
  filter(end_day - start_day < 2 & 
           between(ride_length_m, 1, 1440))
# filter rides spanning across within 2 days 
# while not lasting above 24 hours/ 1440 minutes, 
# above 1 for filtering out negative results and false starts
```

61,649 records removed.

Meanwhile, rides that started or ended at DIVVY CASSETTE *REPAIR* MOBILE
STATION or HUBBARD ST BIKE *CHECKING* (LBS-WH-TEST) or WATSON *TESTING*
DIVVY could be removed as these are administrative stations.  
(Link for
reference:<https://vibrantoutlook.wordpress.com/2021/09/19/google-data-analytics-certificate-capstone-project/>)

For brevity, I use `str_detect()` to find matches containing any
characters of `REPAIR`, `TESTING` or `CHECKING`.

``` r
# find matches
dt_all02 %>% 
  filter(str_detect(start_station_name, "REPAIR|TESTING|CHECKING") | str_detect(end_station_name, "REPAIR|TESTING|CHECKING")) %>% 
  select(ride_id, start_station_id, end_station_id)
```

    ## # A tibble: 8 x 3
    ##   ride_id          start_station_id                     end_station_id          
    ##   <chr>            <chr>                                <chr>                   
    ## 1 04B18873381AACA9 DIVVY CASSETTE REPAIR MOBILE STATION 13029                   
    ## 2 A4354906AD557A6C DIVVY CASSETTE REPAIR MOBILE STATION 13300                   
    ## 3 7FC9FCDB10ED2658 13042                                DIVVY CASSETTE REPAIR M~
    ## 4 DC4828A1C0B5E51F 13042                                DIVVY CASSETTE REPAIR M~
    ## 5 81EAE44D67406AFE 13042                                DIVVY CASSETTE REPAIR M~
    ## 6 886DF564595B8928 13042                                DIVVY CASSETTE REPAIR M~
    ## 7 74B6C966F82A9FD5 DIVVY CASSETTE REPAIR MOBILE STATION 13300                   
    ## 8 C0D3A64896642496 DIVVY CASSETTE REPAIR MOBILE STATION 13300

8 records detected to be removed.

``` r
# remove matches
dt_all02 <- dt_all02 %>% 
  filter(!str_detect(start_station_name, "REPAIR|TESTING|CHECKING") & !str_detect(end_station_name, "REPAIR|TESTING|CHECKING")) # !(A | B) == !A & !B
```

At the end of data cleaning, 1,098,540 rows have been deleted,
accounting for 19% of original data size.

Alternatively, I decide to export the cleaned data as a new csv file and
document the following process of analysing and visualisation directly
on the new file.

``` r
write_csv(dt_all02, file = "cleaned_data.csv")
```
