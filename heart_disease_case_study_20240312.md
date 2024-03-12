US Heart Disease Mortality Rates - 10 Year Analysis
================
Clint Barnard-El
March 12, 2024

### Objective

- Determine the groups and regions of the US that has been most/least
  impacted by heart disease mortality rates during the period between
  2010-2019

### Data Sources

- Centers for Disease Control & Prevention (CDC)
- [National Vital Statistics System
  (NVSS)](https://data.cdc.gov/Heart-Disease-Stroke-Prevention/National-Vital-Statistics-System-NVSS-National-Car/kztq-p2jf/about_data)
- Period: 2010-2019

### R Markdown

#### Load Libraries

``` r
library(gridExtra)
library(maps)
library(geosphere)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::combine() masks gridExtra::combine()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ✖ purrr::map()     masks maps::map()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dtplyr)
library(readxl)
library(tidyselect)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(skimr)
library(tools)
```

#### Import, clean dataset

- Import table from NVSS, clean columns

``` r
df_raw <- read_csv("C:/Users/Clint/OneDrive/Documents/Data/cdc_heart_disease/nvss_hd_2010_2020_raw.csv")
```

    ## Rows: 91520 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (12): RowId, LocationAbbr, Topic, Data_Value_Type, Data_Value_Unit, Brea...
    ## dbl  (4): YearStart, Data_Value, Data_Value_Alt, LocationId
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
clean_df <- clean_names(df_raw) #lower case all col, no spaces
print(clean_df)
```

    ## # A tibble: 91,520 × 16
    ##    row_id         year_start location_abbr topic data_value_type data_value_unit
    ##    <chr>               <dbl> <chr>         <chr> <chr>           <chr>          
    ##  1 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  2 NVSS~2010~1~N…       2010 AL            Majo… Crude           Rate per 100,0…
    ##  3 NVSS~2010~1~N…       2010 AL            Majo… Crude           Rate per 100,0…
    ##  4 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  5 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  6 NVSS~2010~1~N…       2010 AL            Majo… Crude           Rate per 100,0…
    ##  7 NVSS~2010~1~N…       2010 AL            Majo… Crude           Rate per 100,0…
    ##  8 NVSS~2010~1~N…       2010 AL            Majo… Crude           Rate per 100,0…
    ##  9 NVSS~2010~1~N…       2010 AL            Majo… Crude           Rate per 100,0…
    ## 10 NVSS~2010~1~N…       2010 AL            Majo… Crude           Rate per 100,0…
    ## # ℹ 91,510 more rows
    ## # ℹ 10 more variables: data_value <dbl>, data_value_alt <dbl>,
    ## #   break_out_category <chr>, break_out <chr>, class_id <chr>, topic_id <chr>,
    ## #   data_value_type_id <chr>, break_out_category_id <chr>, break_out_id <chr>,
    ## #   location_id <dbl>

``` r
str(clean_df) #review col structure
```

    ## spc_tbl_ [91,520 × 16] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ row_id               : chr [1:91520] "NVSS~2010~1~NV001~OVR01~Age-Standardized" "NVSS~2010~1~NV001~OVR01~Crude" "NVSS~2010~1~NV001~GEN01~Crude" "NVSS~2010~1~NV001~GEN01~Age-Standardized" ...
    ##  $ year_start           : num [1:91520] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
    ##  $ location_abbr        : chr [1:91520] "AL" "AL" "AL" "AL" ...
    ##  $ topic                : chr [1:91520] "Major Cardiovascular Disease" "Major Cardiovascular Disease" "Major Cardiovascular Disease" "Major Cardiovascular Disease" ...
    ##  $ data_value_type      : chr [1:91520] "Age-Standardized" "Crude" "Crude" "Age-Standardized" ...
    ##  $ data_value_unit      : chr [1:91520] "Rate per 100,000" "Rate per 100,000" "Rate per 100,000" "Rate per 100,000" ...
    ##  $ data_value           : num [1:91520] 382 430 438 416 352 ...
    ##  $ data_value_alt       : num [1:91520] 382 430 438 416 352 ...
    ##  $ break_out_category   : chr [1:91520] "Overall" "Overall" "Gender" "Gender" ...
    ##  $ break_out            : chr [1:91520] "Overall" "Overall" "Male" "Male" ...
    ##  $ class_id             : chr [1:91520] "C1" "C1" "C1" "C1" ...
    ##  $ topic_id             : chr [1:91520] "T1" "T1" "T1" "T1" ...
    ##  $ data_value_type_id   : chr [1:91520] "AgeStdz" "Crude" "Crude" "AgeStdz" ...
    ##  $ break_out_category_id: chr [1:91520] "BOC01" "BOC01" "BOC02" "BOC02" ...
    ##  $ break_out_id         : chr [1:91520] "OVR01" "OVR01" "GEN01" "GEN01" ...
    ##  $ location_id          : num [1:91520] 1 1 1 1 1 1 1 1 1 1 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   RowId = col_character(),
    ##   ..   YearStart = col_double(),
    ##   ..   LocationAbbr = col_character(),
    ##   ..   Topic = col_character(),
    ##   ..   Data_Value_Type = col_character(),
    ##   ..   Data_Value_Unit = col_character(),
    ##   ..   Data_Value = col_double(),
    ##   ..   Data_Value_Alt = col_double(),
    ##   ..   Break_Out_Category = col_character(),
    ##   ..   Break_Out = col_character(),
    ##   ..   ClassId = col_character(),
    ##   ..   TopicId = col_character(),
    ##   ..   Data_Value_TypeID = col_character(),
    ##   ..   BreakOutCategoryId = col_character(),
    ##   ..   BreakOutId = col_character(),
    ##   ..   LocationId = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
#remove NA from table
clean_df1 <- clean_df[!is.na(clean_df$data_value), ]
```

#### Filter Relevant Data

- Include data during 2010-2019 (10 years)
- Target specific (break_out_category) data by omitting nationwide
  “overall” data
- Include “age-standardized” data since the objective is to compare
  groups, exclude “crude” type
- Include relevant data only (i.e. year, HD type, state, mortality rate,
  etc.)

``` r
df_age1.1 <- clean_df1 %>% #filter 2020, omit 'overall" from break_out, exclude negative #'s, type id "age" = break out category is gender/race only
  filter(data_value_type_id == "AgeStdz") %>%
  filter(year_start != "2020") %>%
  filter(break_out_category != "Overall") %>%
  filter(data_value_alt > 0)
print(df_age1.1)
```

    ## # A tibble: 20,468 × 16
    ##    row_id         year_start location_abbr topic data_value_type data_value_unit
    ##    <chr>               <dbl> <chr>         <chr> <chr>           <chr>          
    ##  1 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  2 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  3 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  4 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  5 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  6 NVSS~2010~1~N…       2010 AL            Majo… Age-Standardiz… Rate per 100,0…
    ##  7 NVSS~2010~2~N…       2010 AK            Majo… Age-Standardiz… Rate per 100,0…
    ##  8 NVSS~2010~2~N…       2010 AK            Majo… Age-Standardiz… Rate per 100,0…
    ##  9 NVSS~2010~2~N…       2010 AK            Majo… Age-Standardiz… Rate per 100,0…
    ## 10 NVSS~2010~2~N…       2010 AK            Majo… Age-Standardiz… Rate per 100,0…
    ## # ℹ 20,458 more rows
    ## # ℹ 10 more variables: data_value <dbl>, data_value_alt <dbl>,
    ## #   break_out_category <chr>, break_out <chr>, class_id <chr>, topic_id <chr>,
    ## #   data_value_type_id <chr>, break_out_category_id <chr>, break_out_id <chr>,
    ## #   location_id <dbl>

``` r
#remove columns outside objective
df_age1 <- df_age1.1[, c(1:4, 7, 9:10)]
print(df_age1)
```

    ## # A tibble: 20,468 × 7
    ##    row_id year_start location_abbr topic data_value break_out_category break_out
    ##    <chr>       <dbl> <chr>         <chr>      <dbl> <chr>              <chr>    
    ##  1 NVSS~…       2010 AL            Majo…       416. Gender             Male     
    ##  2 NVSS~…       2010 AL            Majo…       352. Gender             Female   
    ##  3 NVSS~…       2010 AL            Majo…       370. Race               Non-Hisp…
    ##  4 NVSS~…       2010 AL            Majo…       453. Race               Non-Hisp…
    ##  5 NVSS~…       2010 AL            Majo…       102. Race               Hispanic 
    ##  6 NVSS~…       2010 AL            Majo…       133. Race               Other    
    ##  7 NVSS~…       2010 AK            Majo…       236. Gender             Male     
    ##  8 NVSS~…       2010 AK            Majo…       177  Gender             Female   
    ##  9 NVSS~…       2010 AK            Majo…       196. Race               Non-Hisp…
    ## 10 NVSS~…       2010 AK            Majo…       268. Race               Other    
    ## # ℹ 20,458 more rows

#### Clean Filtered Data

- Rename columns, race groups to ease understanding of values (simple
  terms)
- Group HD types that did not include a specification (i.e. “Stroke”)

``` r
#rename columns
new_names <- c("year", "state", "type", "rate", "group", "sub_group")
colnames(df_age1)[c(2:7)] <- new_names
print(df_age1)
```

    ## # A tibble: 20,468 × 7
    ##    row_id                                 year state type   rate group sub_group
    ##    <chr>                                 <dbl> <chr> <chr> <dbl> <chr> <chr>    
    ##  1 NVSS~2010~1~NV001~GEN01~Age-Standard…  2010 AL    Majo…  416. Gend… Male     
    ##  2 NVSS~2010~1~NV001~GEN02~Age-Standard…  2010 AL    Majo…  352. Gend… Female   
    ##  3 NVSS~2010~1~NV001~RAC01~Age-Standard…  2010 AL    Majo…  370. Race  Non-Hisp…
    ##  4 NVSS~2010~1~NV001~RAC02~Age-Standard…  2010 AL    Majo…  453. Race  Non-Hisp…
    ##  5 NVSS~2010~1~NV001~RAC04~Age-Standard…  2010 AL    Majo…  102. Race  Hispanic 
    ##  6 NVSS~2010~1~NV001~RAC07~Age-Standard…  2010 AL    Majo…  133. Race  Other    
    ##  7 NVSS~2010~2~NV001~GEN01~Age-Standard…  2010 AK    Majo…  236. Gend… Male     
    ##  8 NVSS~2010~2~NV001~GEN02~Age-Standard…  2010 AK    Majo…  177  Gend… Female   
    ##  9 NVSS~2010~2~NV001~RAC01~Age-Standard…  2010 AK    Majo…  196. Race  Non-Hisp…
    ## 10 NVSS~2010~2~NV001~RAC07~Age-Standard…  2010 AK    Majo…  268. Race  Other    
    ## # ℹ 20,458 more rows

``` r
#update type values, verify
df_age1$type[grepl("^Major |^Diseases of", df_age1$type)] <- "Type Unspecified"
df_age1$type[grepl("^Acute", df_age1$type)] <- "Heart Attack"
unique(df_age1$type)
```

    ## [1] "Type Unspecified"       "Heart Attack"           "Coronary Heart Disease"
    ## [4] "Heart Failure"          "Stroke"

``` r
#rename sub groups, verify
df_age1$sub_group[grepl("^Non-Hispanic Wh", df_age1$sub_group)] <- "White"
df_age1$sub_group[grepl("^Non-Hispanic Bl", df_age1$sub_group)] <- "Black"
df_age1$sub_group[grepl("^Oth", df_age1$sub_group)] <- "Unknown/Other"
unique(df_age1$sub_group)
```

    ## [1] "Male"          "Female"        "White"         "Black"        
    ## [5] "Hispanic"      "Unknown/Other"

### Summarize Data into 5 distinct tables

##### Summary stats of mortality rates grouped by (1) Gender, (2) Race, (3) Type/Gender, (4) Type/All Groups, (5) Top/Bottom 5 states

- **Plot 1 - Gender**
- Includes mean/min/max rates of both genders

``` r
#summarize gender sub_group
plot1.1 <- df_age1 %>% #------------------------------------- By Gender
  filter(group == 'Gender') %>%
  group_by(year, sub_group) %>%
  summarize(rate_sum = sum(rate),
            avg_rate = mean(rate),
            min_rate = min(rate),
            max_rate = max(rate))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
plot1 <- plot1.1 %>%
  mutate(avg_rate = round(avg_rate, 1))
print(plot1)
```

    ## # A tibble: 20 × 6
    ## # Groups:   year [10]
    ##     year sub_group rate_sum avg_rate min_rate max_rate
    ##    <dbl> <chr>        <dbl>    <dbl>    <dbl>    <dbl>
    ##  1  2010 Female      39430.     94.8      8.5     375.
    ##  2  2010 Male        48248.    116.       6.3     444.
    ##  3  2011 Female      37971.     91.5      7.5     358.
    ##  4  2011 Male        47104.    114.       6.5     425.
    ##  5  2012 Female      37085.     89.1      8.1     345.
    ##  6  2012 Male        46328     112.       7.1     421.
    ##  7  2013 Female      36189.     87        7.3     343.
    ##  8  2013 Male        46144.    111.       5.7     441.
    ##  9  2014 Female      35441.     85.2      6.1     332.
    ## 10  2014 Male        45709.    110.       7.7     433.
    ## 11  2015 Female      35491.     85.3      6.5     347.
    ## 12  2015 Male        45637.    110        5.2     448 
    ## 13  2016 Female      34480.     82.9      7.9     332.
    ## 14  2016 Male        45285.    109.       7.2     442 
    ## 15  2017 Female      33973.     81.7      6.2     338.
    ## 16  2017 Male        45039.    109.       6.3     444.
    ## 17  2018 Female      33188.     79.8      5.9     321.
    ## 18  2018 Male        44740.    108.       6.7     433.
    ## 19  2019 Female      32510.     78.1      6.6     318.
    ## 20  2019 Male        44228.    106.       5.5     444.

- **Plot 2 - Race**
- Includes mean/min/max rates of all 4 race groups

``` r
#summarize race sub_group
plot2.1 <- df_age1 %>% #------------------------------------- By Race
  filter(group == 'Race') %>%
  group_by(year, sub_group) %>%
  summarize(rate_sum = sum(rate),
            avg_rate = mean(rate),
            min_rate = min(rate),
            max_rate = max(rate))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
  plot2 <- plot2.1 %>%
    mutate(avg_rate = round(avg_rate, 1))
  print(plot2)
```

    ## # A tibble: 40 × 6
    ## # Groups:   year [10]
    ##     year sub_group     rate_sum avg_rate min_rate max_rate
    ##    <dbl> <chr>            <dbl>    <dbl>    <dbl>    <dbl>
    ##  1  2010 Black           38890     131.       8.9     491.
    ##  2  2010 Hispanic        15554.     72        4.8     274.
    ##  3  2010 Unknown/Other   18408.     77.7      2.7     386.
    ##  4  2010 White           43930.    106.       8       391.
    ##  5  2011 Black           37619.    128        8.3     443.
    ##  6  2011 Hispanic        15636.     71.1      4.9     270.
    ##  7  2011 Unknown/Other   18693      76.6      2.7     309.
    ##  8  2011 White           42830.    104.       5.6     384.
    ##  9  2012 Black           37086.    127.       9.5     468.
    ## 10  2012 Hispanic        15820.     68.2      5.2     284.
    ## # ℹ 30 more rows

- **Plot 3 - Type (All Genders)**
- Includes mean rates of all genders

``` r
plot3.1 <- df_age1 %>% #------------------------------------- By Type, All Genders
  filter(group == 'Gender', type != 'Type Unspecified') %>%  #excludes generic types
  group_by(year, type, sub_group) %>%
  summarize(avg_rate = mean(rate))
```

    ## `summarise()` has grouped output by 'year', 'type'. You can override using the
    ## `.groups` argument.

``` r
  plot3 <- plot3.1 %>%
    mutate(avg_rate = round(avg_rate, 1))
  print(plot3)
```

    ## # A tibble: 80 × 4
    ## # Groups:   year, type [40]
    ##     year type                   sub_group avg_rate
    ##    <dbl> <chr>                  <chr>        <dbl>
    ##  1  2010 Coronary Heart Disease Female       115. 
    ##  2  2010 Coronary Heart Disease Male         174. 
    ##  3  2010 Heart Attack           Female        37.8
    ##  4  2010 Heart Attack           Male          59.1
    ##  5  2010 Heart Failure          Female        24.6
    ##  6  2010 Heart Failure          Male          22.3
    ##  7  2010 Stroke                 Female        34.1
    ##  8  2010 Stroke                 Male          28.1
    ##  9  2011 Coronary Heart Disease Female       109. 
    ## 10  2011 Coronary Heart Disease Male         170. 
    ## # ℹ 70 more rows

- **Plot 4 - Type (All Groups/Sub-Groups)**
- Includes mean rates of population whole

``` r
plot4 <- df_age1 %>% #------------------------------------- By Type, Overall
  filter(type != 'Type Unspecified') %>%  #excludes missing type
  group_by(year, type) %>%
  summarize(avg_rate = mean(rate))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
print(plot4)
```

    ## # A tibble: 40 × 3
    ## # Groups:   year [10]
    ##     year type                   avg_rate
    ##    <dbl> <chr>                     <dbl>
    ##  1  2010 Coronary Heart Disease    127. 
    ##  2  2010 Heart Attack               44.7
    ##  3  2010 Heart Failure              22.6
    ##  4  2010 Stroke                     31.2
    ##  5  2011 Coronary Heart Disease    121. 
    ##  6  2011 Heart Attack               42.9
    ##  7  2011 Heart Failure              22.0
    ##  8  2011 Stroke                     30.1
    ##  9  2012 Coronary Heart Disease    119. 
    ## 10  2012 Heart Attack               41.3
    ## # ℹ 30 more rows

- **Plot 5 - Top/Bottom 5 States)**
- Includes mean rates of 5 states with lowest and highest avg. mortality
  rates during total period - Sort table based on avg_rate in ascending
  order - create table that includes only the top/bottom 5 states
  (exclude)

``` r
plot5.1 <- df_age1 %>% #------------------------------------- By Top/Bottom 5 States
  filter(state != "DC") %>%
  group_by(state) %>%
  summarize(avg_rate = mean(rate))
print(plot5.1)
```

    ## # A tibble: 51 × 2
    ##    state avg_rate
    ##    <chr>    <dbl>
    ##  1 AK        79.7
    ##  2 AL       118. 
    ##  3 AR       130. 
    ##  4 AZ        79.0
    ##  5 CA        88.5
    ##  6 CO        71.8
    ##  7 CT        79.8
    ##  8 DE        97.0
    ##  9 FL        83.6
    ## 10 GA        79.1
    ## # ℹ 41 more rows

``` r
plot5.2 <- plot5.1[order(-plot5.1$avg_rate), ] #sort table

top_bottom <- plot5.2 %>% #create new table, top/bottom 5
  arrange(avg_rate) %>%
  filter(row_number() <= 5 | row_number() > n() - 5)
     
plot5 <- top_bottom 
print(plot5)
```

    ## # A tibble: 10 × 2
    ##    state avg_rate
    ##    <chr>    <dbl>
    ##  1 MA        68.1
    ##  2 CO        71.8
    ##  3 MN        73.7
    ##  4 WA        74.3
    ##  5 VA        75.7
    ##  6 SD       119. 
    ##  7 OK       121. 
    ##  8 WV       124. 
    ##  9 AR       130. 
    ## 10 MS       135.

## Create Plots 1-5

### **Plot 1**: Heart Disease Mortality Rate - Gender

- Visualize avg. mortality rate change during period (all genders)

``` r
ggplot(plot1, aes(x = year, y = avg_rate, color = sub_group)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(title = "Heart Disease Mortality Rate - Gender",
       subtitle = "(2010-2019)",
       x = "Year",
       y = "Avg Rate per 100K",
       color = "Gender") +
  theme_bw()
```

![](heart_disease_case_study_20240312_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### **Plot 2**: Heart Disease Mortality Rate - Race

- Visualize avg. mortality rate change during period (all race groups)

``` r
ggplot(plot2, aes(x = year, y = avg_rate, color = sub_group)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(title = "Heart Disease Mortality Rate - Race",
       subtitle = "(2010-2019)",
       x = "Year",
       y = "Avg Rate per 100K",
       color = "Race") +
  theme_bw()
```

![](heart_disease_case_study_20240312_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### **Plot 3**: Heart Disease Mortality Rate - Gender+Type

- Visualize which types of heart disease had the most impact, divide by
  genders  

``` r
ggplot(plot3, aes(x=type, y=avg_rate, fill=type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sub_group) +
  labs(title = "Heart Disease Mortality Rate - Gender+Type",
       subtitle = "(2010-2019)",
       x = "Type",
       y = "Avg Rate per 100K",
       color = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](heart_disease_case_study_20240312_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### **Plot 4**: Heart Disease Mortality Rate - Type (Overall)

- Visualize mortality rate changes during this period for each heart
  disease type  

``` r
ggplot(plot4, aes(x = year, y = avg_rate, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(title = "Heart Disease Mortality Rate - Type",
       subtitle = "(2010-2019)",
       x = "Year",
       y = "Avg Rate per 100K",
       color = "Type") +
  theme_bw()
```

![](heart_disease_case_study_20240312_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### **Plot 5**: Heart Disease Mortality Rate - Top/Bottom 5 States

- Visualize the top and bottom 5 states that had the highest/lowest
  mortality rates during this period  

``` r
ggplot(plot5, aes(x = reorder(state, -avg_rate), y = avg_rate, fill=avg_rate)) + 
  geom_bar(stat = "identity") +
  #facet_wrap(~ sub_group) +
  labs(title = "Heart Disease Mortality Rates - Top/Bottom 5 States",
       subtitle = "(2010-2019)",
       x = "State",
       y = "Avg Rate per 100K",
       color = "Rate")
```

![](heart_disease_case_study_20240312_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
  theme_minimal()
```

    ## List of 136
    ##  $ line                            :List of 6
    ##   ..$ colour       : chr "black"
    ##   ..$ linewidth    : num 0.5
    ##   ..$ linetype     : num 1
    ##   ..$ lineend      : chr "butt"
    ##   ..$ arrow        : logi FALSE
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
    ##  $ rect                            :List of 5
    ##   ..$ fill         : chr "white"
    ##   ..$ colour       : chr "black"
    ##   ..$ linewidth    : num 0.5
    ##   ..$ linetype     : num 1
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
    ##  $ text                            :List of 11
    ##   ..$ family       : chr ""
    ##   ..$ face         : chr "plain"
    ##   ..$ colour       : chr "black"
    ##   ..$ size         : num 11
    ##   ..$ hjust        : num 0.5
    ##   ..$ vjust        : num 0.5
    ##   ..$ angle        : num 0
    ##   ..$ lineheight   : num 0.9
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : logi FALSE
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ title                           : NULL
    ##  $ aspect.ratio                    : NULL
    ##  $ axis.title                      : NULL
    ##  $ axis.title.x                    :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title.x.top                :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 0
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title.x.bottom             : NULL
    ##  $ axis.title.y                    :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : num 90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title.y.left               : NULL
    ##  $ axis.title.y.right              :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : num -90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text                       :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : chr "grey30"
    ##   ..$ size         : 'rel' num 0.8
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.x                     :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.x.top                 :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 0
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.x.bottom              : NULL
    ##  $ axis.text.y                     :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 1
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.y.left                : NULL
    ##  $ axis.text.y.right               :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 0
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.theta                 : NULL
    ##  $ axis.text.r                     :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 0.5
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 2.2points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.ticks                      : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ axis.ticks.x                    : NULL
    ##  $ axis.ticks.x.top                : NULL
    ##  $ axis.ticks.x.bottom             : NULL
    ##  $ axis.ticks.y                    : NULL
    ##  $ axis.ticks.y.left               : NULL
    ##  $ axis.ticks.y.right              : NULL
    ##  $ axis.ticks.theta                : NULL
    ##  $ axis.ticks.r                    : NULL
    ##  $ axis.minor.ticks.x.top          : NULL
    ##  $ axis.minor.ticks.x.bottom       : NULL
    ##  $ axis.minor.ticks.y.left         : NULL
    ##  $ axis.minor.ticks.y.right        : NULL
    ##  $ axis.minor.ticks.theta          : NULL
    ##  $ axis.minor.ticks.r              : NULL
    ##  $ axis.ticks.length               : 'simpleUnit' num 2.75points
    ##   ..- attr(*, "unit")= int 8
    ##  $ axis.ticks.length.x             : NULL
    ##  $ axis.ticks.length.x.top         : NULL
    ##  $ axis.ticks.length.x.bottom      : NULL
    ##  $ axis.ticks.length.y             : NULL
    ##  $ axis.ticks.length.y.left        : NULL
    ##  $ axis.ticks.length.y.right       : NULL
    ##  $ axis.ticks.length.theta         : NULL
    ##  $ axis.ticks.length.r             : NULL
    ##  $ axis.minor.ticks.length         : 'rel' num 0.75
    ##  $ axis.minor.ticks.length.x       : NULL
    ##  $ axis.minor.ticks.length.x.top   : NULL
    ##  $ axis.minor.ticks.length.x.bottom: NULL
    ##  $ axis.minor.ticks.length.y       : NULL
    ##  $ axis.minor.ticks.length.y.left  : NULL
    ##  $ axis.minor.ticks.length.y.right : NULL
    ##  $ axis.minor.ticks.length.theta   : NULL
    ##  $ axis.minor.ticks.length.r       : NULL
    ##  $ axis.line                       : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ axis.line.x                     : NULL
    ##  $ axis.line.x.top                 : NULL
    ##  $ axis.line.x.bottom              : NULL
    ##  $ axis.line.y                     : NULL
    ##  $ axis.line.y.left                : NULL
    ##  $ axis.line.y.right               : NULL
    ##  $ axis.line.theta                 : NULL
    ##  $ axis.line.r                     : NULL
    ##  $ legend.background               : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ legend.margin                   : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
    ##   ..- attr(*, "unit")= int 8
    ##  $ legend.spacing                  : 'simpleUnit' num 11points
    ##   ..- attr(*, "unit")= int 8
    ##  $ legend.spacing.x                : NULL
    ##  $ legend.spacing.y                : NULL
    ##  $ legend.key                      : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ legend.key.size                 : 'simpleUnit' num 1.2lines
    ##   ..- attr(*, "unit")= int 3
    ##  $ legend.key.height               : NULL
    ##  $ legend.key.width                : NULL
    ##  $ legend.key.spacing              : 'simpleUnit' num 5.5points
    ##   ..- attr(*, "unit")= int 8
    ##  $ legend.key.spacing.x            : NULL
    ##  $ legend.key.spacing.y            : NULL
    ##  $ legend.frame                    : NULL
    ##  $ legend.ticks                    : NULL
    ##  $ legend.ticks.length             : 'rel' num 0.2
    ##  $ legend.axis.line                : NULL
    ##  $ legend.text                     :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : 'rel' num 0.8
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ legend.text.position            : NULL
    ##  $ legend.title                    :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 0
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ legend.title.position           : NULL
    ##  $ legend.position                 : chr "right"
    ##  $ legend.position.inside          : NULL
    ##  $ legend.direction                : NULL
    ##  $ legend.byrow                    : NULL
    ##  $ legend.justification            : chr "center"
    ##  $ legend.justification.top        : NULL
    ##  $ legend.justification.bottom     : NULL
    ##  $ legend.justification.left       : NULL
    ##  $ legend.justification.right      : NULL
    ##  $ legend.justification.inside     : NULL
    ##  $ legend.location                 : NULL
    ##  $ legend.box                      : NULL
    ##  $ legend.box.just                 : NULL
    ##  $ legend.box.margin               : 'margin' num [1:4] 0cm 0cm 0cm 0cm
    ##   ..- attr(*, "unit")= int 1
    ##  $ legend.box.background           : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ legend.box.spacing              : 'simpleUnit' num 11points
    ##   ..- attr(*, "unit")= int 8
    ##   [list output truncated]
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi TRUE
    ##  - attr(*, "validate")= logi TRUE
