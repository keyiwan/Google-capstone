Google Data Analytics Certificate - Bike Share Case Study - Part2
================
K.Wan
2022/4/4

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

## Analyse

``` r
dt_all02 <- read_csv("cleaned_data.csv")
```

Here comes the first question: How many observations fall under each
user type?

``` r
(dt_all_freq <- dt_all02 %>% 
  count(member_casual))
```

    ## # A tibble: 2 x 2
    ##   member_casual       n
    ##   <chr>           <int>
    ## 1 casual        2031039
    ## 2 member        2538407

As shown above, more than 507,368 rides were by members than casual
riders in the whole time period.

Then, I conduct descriptive analysis on **ride length**, answering
questions like:

> Is there a difference on total/average ride lengths among the two user
> groups?  
> How is that difference varied on monthly/daily level?

**1. Total ride lengths**

Let’s have a look at the total, average, median, minimum and maximum.

``` r
 dt_all02 %>%  
   group_by(member_casual) %>%  
   summarise(number_of_rides = n(), 
             total_length = sum(ride_length_m),  
             mean_length = mean(ride_length_m), 
             median_length = median(ride_length_m),  
             max_length = max(ride_length_m),  
             min_length = min(ride_length_m)) 
```

    ## # A tibble: 2 x 7
    ##   member_casual number_of_rides total_length mean_length median_length
    ##   <chr>                   <int>        <dbl>       <dbl>         <dbl>
    ## 1 casual                2031039    57695524.        28.4         16.8 
    ## 2 member                2538407    33592312.        13.2          9.75
    ## # ... with 2 more variables: max_length <dbl>, min_length <dbl>

Overall, more rides were taken by members while casual riders enjoyed
longer trips, contributing nearly double the total ride length of
members’ during the same time period.

The average duration of each ride by casual riders is around 28.4
minutes, two times more of the 13.2 minutes for annual members.

**2. Ride length by month**

``` r
 dt_all02 %>%  
   group_by(member_casual, start_Month) %>%  
   summarise(sum_length_m = sum(ride_length_m),  
             mean_length_m = mean(ride_length_m)) %>% 
   arrange(member_casual, start_Month) %>%  
   head(12) 
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 12 x 4
    ## # Groups:   member_casual [1]
    ##    member_casual start_Month sum_length_m mean_length_m
    ##    <chr>         <chr>              <dbl>         <dbl>
    ##  1 casual        Apr             3860655.          32.4
    ##  2 casual        Aug             9246271.          27.4
    ##  3 casual        Dec              906566.          20.3
    ##  4 casual        Feb              322847.          21.6
    ##  5 casual        Jan              227496.          18.3
    ##  6 casual        Jul            10502955.          28.8
    ##  7 casual        Jun             9341985.          31.1
    ##  8 casual        Mar             2433254.          32.5
    ##  9 casual        May             7173380.          33.5
    ## 10 casual        Nov             1410656.          20.4
    ## 11 casual        Oct             4600798.          24.6
    ## 12 casual        Sep             7668660.          26.4

It seems like the second quarter (April, May & June) to be a period that
has experienced relatively longer duration of rides by casual riders.

**3. Which day of a week induced most traffic**

See the number of trips and average ride duration on each day of a week
for all users.

``` r
 dt_all02 %>%  
   group_by(start_wday) %>%  
   summarise(number_of_rides = n(),  
             total_length = sum(ride_length_m),  
             mean_length = mean(ride_length_m)) 
```

    ## # A tibble: 7 x 4
    ##   start_wday number_of_rides total_length mean_length
    ##   <chr>                <int>        <dbl>       <dbl>
    ## 1 Fri                 648908    12232708.        18.9
    ## 2 Mon                 582014    11138952.        19.1
    ## 3 Sat                 812241    19363918.        23.8
    ## 4 Sun                 710796    17826587.        25.1
    ## 5 Thu                 594966    10018268.        16.8
    ## 6 Tue                 605821    10420861.        17.2
    ## 7 Wed                 614700    10286544.        16.7

Compare ridership, average ride length by weekday for members vs casual
riders.

``` r
 dt_all02 %>%  
   group_by(member_casual, start_wday) %>%  
   summarise(number_of_rides = n(),  
             total_length = sum(ride_length_m),  
             mean_length = mean(ride_length_m)) %>%  
   arrange(member_casual, start_wday)  
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 14 x 5
    ## # Groups:   member_casual [2]
    ##    member_casual start_wday number_of_rides total_length mean_length
    ##    <chr>         <chr>                <int>        <dbl>       <dbl>
    ##  1 casual        Fri                 287122     7568258.        26.4
    ##  2 casual        Mon                 229094     6631544.        28.9
    ##  3 casual        Sat                 461605    14143649.        30.6
    ##  4 casual        Sun                 400541    13082608.        32.7
    ##  5 casual        Thu                 222197     5395626.        24.3
    ##  6 casual        Tue                 213847     5556801.        26.0
    ##  7 casual        Wed                 216633     5317038.        24.5
    ##  8 member        Fri                 361786     4664450.        12.9
    ##  9 member        Mon                 352920     4507408.        12.8
    ## 10 member        Sat                 350636     5220269.        14.9
    ## 11 member        Sun                 310255     4743979.        15.3
    ## 12 member        Thu                 372769     4622642.        12.4
    ## 13 member        Tue                 391974     4864059.        12.4
    ## 14 member        Wed                 398067     4969505.        12.5

Regardless of the user type, weekends undoubtedly enjoyed peak usage,
which can possibly guide us in deciding marketing times.

**4. Top 10 busiest stations**

To better market membership of Divvy, hence attracting more casual
riders to subscribe as members, stations mostly visited by casual users
may be a great choice as locations for the upcoming campaign.

``` r
dt_all02 %>%  
   filter(member_casual == "casual") %>%  
   group_by(start_station_id) %>%  
   summarise(n = n()) %>%  
   arrange(desc(n)) %>%  
   select(start_station_id, n) %>%  
   head(10) 
```

    ## # A tibble: 10 x 2
    ##    start_station_id     n
    ##    <chr>            <int>
    ##  1 13022            63837
    ##  2 13300            34325
    ##  3 13008            31788
    ##  4 13042            28327
    ##  5 LF-005           28322
    ##  6 15544            22353
    ##  7 TA1308000001     20329
    ##  8 TA1308000050     18741
    ##  9 13179            16093
    ## 10 KA1504000135     15708

Is there any difference on station preference between casual riders and
annual members?

``` r
dt_all02 %>% 
  filter(member_casual == "member") %>% 
  group_by(start_station_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(start_station_id, n) %>% 
  head(10)
```

    ## # A tibble: 10 x 2
    ##    start_station_id     n
    ##    <chr>            <int>
    ##  1 TA1307000039     23636
    ##  2 KA1503000043     23310
    ##  3 TA1308000050     22768
    ##  4 KA1504000135     20123
    ##  5 13045            18247
    ##  6 13016            18065
    ##  7 TA1306000012     18018
    ##  8 LF-005           17425
    ##  9 13137            16842
    ## 10 TA1305000032     16680

Overall, which stations can reach out to most users regardless of user
type.

``` r
dt_all02 %>% 
  group_by(start_station_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(start_station_id, n) %>% 
  head(10)
```

    ## # A tibble: 10 x 2
    ##    start_station_id     n
    ##    <chr>            <int>
    ##  1 13022            79396
    ##  2 LF-005           45747
    ##  3 13300            44216
    ##  4 13042            42232
    ##  5 TA1308000050     41509
    ##  6 13008            40051
    ##  7 TA1307000039     39125
    ##  8 KA1504000135     35831
    ##  9 TA1308000001     35270
    ## 10 KA1503000043     32742

As shown above, there is undeniably a distinct difference on stations
choices made by members and casual riders.

Even though I think sticking to station id is easier, but for map viz,
names of locations are better for presentation. Since I will focus on
mainly the top 10 stations most visited by casual riders for past 12
months, only two ids in the top 10 list that also appears in the
duplicated list before: `13300` and `LF-005`. So I will fix the names of
stations belonging to those two ids.

It turns out, recently [Chicago’s iconic Lake Shore Drive has been
renamed in honor of Jean Baptiste Point
DuSable](https://blockclubchicago.org/2021/10/21/lake-shore-drive-signs-now-have-its-new-name-dusable-lake-shore-drive-honoring-citys-black-founder/),
so the `Lake Shore Dr` in the data set has been renamed
`DuSable Lake Shore Dr` shown in more recent records. And luckily, both
cases of duplicates are due to the same reason. Then I will just update
the station names accordingly.

``` r
dt_all02 %>% 
  count(start_station_id, start_station_name) %>% 
  filter(start_station_id %in% c("13300", "LF-005"))
```

    ## # A tibble: 4 x 3
    ##   start_station_id start_station_name                     n
    ##   <chr>            <chr>                              <int>
    ## 1 13300            DuSable Lake Shore Dr & Monroe St  20582
    ## 2 13300            Lake Shore Dr & Monroe St          23634
    ## 3 LF-005           DuSable Lake Shore Dr & North Blvd 23433
    ## 4 LF-005           Lake Shore Dr & North Blvd         22314

``` r
dt_all02 <- dt_all02 %>% 
  mutate(start_station_name = case_when(
    start_station_id == "13300" ~ "DuSable Lake Shore Dr & Monroe St",
    start_station_id == "LF-005" ~ "DuSable Lake Shore Dr & North Blvd",
    TRUE ~ start_station_name
  ))
```

Now, test with updated station names.

``` r
dt_all02 %>% 
  filter(member_casual == "casual") %>% 
  group_by(start_station_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(start_station_id, n) %>% 
  head(10)
```

    ## # A tibble: 10 x 2
    ##    start_station_id     n
    ##    <chr>            <int>
    ##  1 13022            63837
    ##  2 13300            34325
    ##  3 13008            31788
    ##  4 13042            28327
    ##  5 LF-005           28322
    ##  6 15544            22353
    ##  7 TA1308000001     20329
    ##  8 TA1308000050     18741
    ##  9 13179            16093
    ## 10 KA1504000135     15708

``` r
dt_all02 %>% 
  filter(member_casual == "casual") %>% 
  group_by(start_station_name) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(start_station_name, n) %>% 
  head(10)
```

    ## # A tibble: 10 x 2
    ##    start_station_name                     n
    ##    <chr>                              <int>
    ##  1 Streeter Dr & Grand Ave            63837
    ##  2 DuSable Lake Shore Dr & Monroe St  34325
    ##  3 Millennium Park                    31788
    ##  4 Michigan Ave & Oak St              28327
    ##  5 DuSable Lake Shore Dr & North Blvd 28322
    ##  6 Shedd Aquarium                     22353
    ##  7 Theater on the Lake                20329
    ##  8 Wells St & Concord Ln              18741
    ##  9 Clark St & Lincoln Ave             16093
    ## 10 Wells St & Elm St                  15708

**5. The proportion of casual riders and annual members for each
station**

I want to produce a table with each row representing a unique station
id, columns involving number of rides by members, by casual riders, and
their individual proportion of total number of rides.

``` r
(prop_dt <- dt_all02 %>% 
  group_by(start_station_id, member_casual) %>% 
  summarise(n = n()))
```

    ## `summarise()` has grouped output by 'start_station_id'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 1,632 x 3
    ## # Groups:   start_station_id [835]
    ##    start_station_id member_casual     n
    ##    <chr>            <chr>         <int>
    ##  1 13001            casual        13488
    ##  2 13001            member        10852
    ##  3 13006            casual         3212
    ##  4 13006            member         6692
    ##  5 13008            casual        31788
    ##  6 13008            member         8263
    ##  7 13011            casual         5204
    ##  8 13011            member        11765
    ##  9 13016            casual        10233
    ## 10 13016            member        18065
    ## # ... with 1,622 more rows

This tibble having multiple values stacked in a small number of columns
needs the function `pivot_wider` to make it wider.

``` r
prop_dt %>% 
  pivot_wider(names_from = member_casual, values_from = n) %>% 
  mutate(total = casual + member, casual_freq = casual/total, member_freq = member/total) %>% 
  arrange(desc(casual))
```

    ## # A tibble: 835 x 6
    ## # Groups:   start_station_id [835]
    ##    start_station_id casual member total casual_freq member_freq
    ##    <chr>             <int>  <int> <int>       <dbl>       <dbl>
    ##  1 13022             63837  15559 79396       0.804       0.196
    ##  2 13300             34325   9891 44216       0.776       0.224
    ##  3 13008             31788   8263 40051       0.794       0.206
    ##  4 13042             28327  13905 42232       0.671       0.329
    ##  5 LF-005            28322  17425 45747       0.619       0.381
    ##  6 15544             22353   4747 27100       0.825       0.175
    ##  7 TA1308000001      20329  14941 35270       0.576       0.424
    ##  8 TA1308000050      18741  22768 41509       0.451       0.549
    ##  9 13179             16093  15496 31589       0.509       0.491
    ## 10 KA1504000135      15708  20123 35831       0.438       0.562
    ## # ... with 825 more rows

In result, the list of 10 stations boasting far more visits from casual
riders than members is the same as before.

## Share

It is time to visualise our findings to vividly demonstrate differences
on riding patterns between different rider types.

**1. Members ride more often than non-members**

Look at the pie chart below, showing the discrepancy on numbers of rides
by rider type.

``` r
library(scales)
```

    ## Warning: package 'scales' was built under R version 4.0.5

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
dt_all02 %>% 
  group_by(member_casual) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(x = "", y = n, fill = member_casual))+
  geom_col()+
  geom_text(aes(label = member_casual), position = position_stack(vjust = 0.5)) +
  labs(title = "Number of Trips by Rider Type")+
  guides(fill = guide_legend(title = "Rider Type"))+
  coord_polar("y")
```

![](bike-sharing02_files/figure-gfm/pie_trips1-1.png)<!-- -->

``` r
dt_all02 %>% 
  group_by(member_casual) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = n/sum(n)) %>% 
  mutate(labels = scales::percent(perc)) %>% 
  ggplot(aes(x = "", y = perc, fill = member_casual))+
  geom_col()+
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  labs(title = "Number of Trips by Rider Type")+
  guides(fill = guide_legend(title = "Rider Type"))+
  coord_polar("y")
```

![](bike-sharing02_files/figure-gfm/pie_trips2-1.png)<!-- -->

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](bike-sharing02_files/figure-gfm/rides_bar-1.png)<!-- -->

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](bike-sharing02_files/figure-gfm/length_bar-1.png)<!-- -->
