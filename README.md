Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
```

``` r
# Some EDA codes from last week
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10

# Relative Humidity
met$rh <- 100*((112-0.1*met$temp+met$dew.point)/(112+0.9*met$temp))^8

# Filter out implausible longitude and latitude values
met <- met[abs(met$lon) <= 180 | abs(met$lat) <= 90,]

# Filter out implausible humidity values
met <- met[met$rh >= 0 & met$rh <= 100,]
met <- met[complete.cases(met$temp, met$rh, met$wind.sp, met$elev, met$atm.press), ]
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
merged_data <- merge(met, stations, by.x = "USAFID", by.y = "USAF", all.x = TRUE)
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
library(dplyr)

# Calculate median values for temperature, wind speed, and atmospheric pressure
median_temp <- quantile(merged_data$temp, 0.5, na.rm = TRUE)
median_wind_sp <- quantile(merged_data$wind.sp, 0.5, na.rm = TRUE)
median_atm_press <- quantile(merged_data$atm.press, 0.5, na.rm = TRUE)

# Filter stations with median values
stations_median_temp <- merged_data %>% 
  filter(round(temp, 1) == round(median_temp, 1)) %>%
  distinct(USAFID, .keep_all = TRUE)

stations_median_wind_sp <- merged_data %>% 
  filter(round(wind.sp, 1) == round(median_wind_sp, 1)) %>%
  distinct(USAFID, .keep_all = TRUE)

stations_median_atm_press <- merged_data %>% 
  filter(round(atm.press, 1) == round(median_atm_press, 1)) %>%
  distinct(USAFID, .keep_all = TRUE)

# Check if stations coincide
coincide <- intersect(intersect(stations_median_temp$USAFID, stations_median_wind_sp$USAFID), stations_median_atm_press$USAFID)

# Output the stations
stations_median_temp
```

    ##       USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ##    1: 690150 93121 2023     6   1    5  56 34.294 -116.147  696       20
    ##    2: 720175 53919 2023     6   6   12  53 33.636  -91.756   82      180
    ##    3: 720198 54813 2023     6   1   12  56 46.412  -86.650  187      180
    ##    4: 720269 12982 2023     6   2   10  55 27.207  -98.121   34       60
    ##    5: 720306 53879 2023     6   1    3  53 38.958  -94.371  306      130
    ##   ---                                                                   
    ## 1050: 747930 12843 2023     6   3    4  53 27.655  -80.414    9      280
    ## 1051: 747931 12876 2023     6   1    6  53 28.061  -81.757   45       40
    ## 1052: 747940 12868 2023     6   3    7  55 28.483  -80.567    3       30
    ## 1053: 747946 12886 2023     6   8   NA  55 28.617  -80.683    3      150
    ## 1054: 747950 12867 2023     6  20    2  55 28.233  -80.600    3      170
    ##       wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##    1:           5              N     3.1          5      22000             5
    ##    2:           5              N     2.6          5      22000             5
    ##    3:           5              N     2.6          5      99999             9
    ##    4:           5              N     1.5          5      22000             5
    ##    5:           5              N     2.6          5      22000             5
    ##   ---                                                                       
    ## 1050:           5              N     2.6          5      22000             5
    ## 1051:           5              N     2.6          5        610             5
    ## 1052:           5              N     2.6          5      22000             5
    ## 1053:           5              N     1.5          5       3353             5
    ## 1054:           5              N     6.2          5      22000             5
    ##       ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ##    1:                 9        N    16093           5       N          5 22.2
    ##    2:                 9        N    16093           5       N          5 22.2
    ##    3:                 9        N       NA           7       N          5 22.2
    ##    4:                 9        N    16093           5       N          5 22.2
    ##    5:                 9        N    16093           5       N          5 22.2
    ##   ---                                                                        
    ## 1050:                 9        N    16093           5       N          5 22.2
    ## 1051:                 M        N    16093           5       N          5 22.2
    ## 1052:                 9        N    16093           5       N          5 22.2
    ## 1053:                 M        N    16093           5       N          5 22.2
    ## 1054:                 9        N    16093           5       N          5 22.2
    ##       temp.qc dew.point dew.point.qc atm.press atm.press.qc        rh CTRY
    ##    1:       5       6.7            5    1006.5            5  36.80831   US
    ##    2:       5      20.0            5    1014.5            5  87.41726   US
    ##    3:       5      16.1            5    1018.7            5  68.48396   US
    ##    4:       5      22.2            5    1006.7            5 100.00000   US
    ##    5:       5      18.3            5    1013.1            5  78.66573   US
    ##   ---                                                                     
    ## 1050:       5      20.0            5    1006.8            5  87.41726   US
    ## 1051:       5      21.7            5    1011.4            5  97.00912   US
    ## 1052:       5      21.0            5    1004.7            5  92.95348   US
    ## 1053:       5      20.8            5    1008.5            5  91.82233   US
    ## 1054:       5      19.9            5    1014.3            5  86.87984   US
    ##       STATE    LAT      LON
    ##    1:    CA 34.294 -116.147
    ##    2:    AR 33.636  -91.756
    ##    3:    MI 46.412  -86.650
    ##    4:    TX 27.207  -98.121
    ##    5:    MO 38.958  -94.371
    ##   ---                      
    ## 1050:    FL 27.655  -80.414
    ## 1051:    FL 28.061  -81.757
    ## 1052:    FL 28.483  -80.567
    ## 1053:    FL 28.617  -80.683
    ## 1054:    FL 28.233  -80.600

``` r
stations_median_wind_sp
```

    ##       USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ##    1: 690150 93121 2023     6   1    5  56 34.294 -116.147  696       20
    ##    2: 720175 53919 2023     6   1   21  53 33.636  -91.756   82      100
    ##    3: 720198 54813 2023     6   1   NA  56 46.412  -86.650  187      170
    ##    4: 720269 12982 2023     6   1    1  55 27.207  -98.121   34      100
    ##    5: 720306 53879 2023     6   2    4  53 38.958  -94.371  306      100
    ##   ---                                                                   
    ## 1082: 747930 12843 2023     6   1    4  53 27.655  -80.414    9       70
    ## 1083: 747931 12876 2023     6   1    9  53 28.061  -81.757   45       40
    ## 1084: 747940 12868 2023     6   1    4  38 28.483  -80.567    3       90
    ## 1085: 747946 12886 2023     6   1   NA  55 28.617  -80.683    3      100
    ## 1086: 747950 12867 2023     6   2   NA  40 28.233  -80.600    3       50
    ##       wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##    1:           5              N     3.1          5      22000             5
    ##    2:           5              N     3.1          5      22000             5
    ##    3:           5              N     3.1          5      99999             9
    ##    4:           5              N     3.1          5      22000             5
    ##    5:           5              N     3.1          5      22000             5
    ##   ---                                                                       
    ## 1082:           5              N     3.1          5      22000             5
    ## 1083:           5              N     3.1          5      22000             5
    ## 1084:           5              N     3.1          5      22000             5
    ## 1085:           5              N     3.1          5      22000             5
    ## 1086:           5              N     3.1          5        701             5
    ##       ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ##    1:                 9        N    16093           5       N          5 22.2
    ##    2:                 9        N    16093           5       N          5 31.7
    ##    3:                 9        N       NA           7       N          5 26.7
    ##    4:                 9        N     9656           5       N          5 26.5
    ##    5:                 9        N    16093           5       N          5 22.2
    ##   ---                                                                        
    ## 1082:                 9        N    16093           5       N          5 24.4
    ## 1083:                 9        N    16093           5       N          5 21.7
    ## 1084:                 9        N    16093           5       N          5 25.0
    ## 1085:                 9        N    16093           5       N          5 24.3
    ## 1086:                 M        N    16093           5       N          5 26.0
    ##       temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh CTRY STATE
    ##    1:       5       6.7            5    1006.5            5 36.80831   US    CA
    ##    2:       5      17.2            5    1012.8            5 41.84457   US    AR
    ##    3:       5      15.0            5    1014.8            5 48.70010   US    MI
    ##    4:       5      22.6            5    1005.5            5 79.21328   US    TX
    ##    5:       5      17.8            5    1015.3            5 76.24227   US    MO
    ##   ---                                                                          
    ## 1082:       5      22.2            5    1011.6            5 87.59263   US    FL
    ## 1083:       5      20.6            5    1011.2            5 93.50211   US    FL
    ## 1084:       5      22.0            5    1011.5            5 83.48871   US    FL
    ## 1085:       5      18.7            5    1013.3            5 71.04519   US    FL
    ## 1086:       5      23.0            5    1010.6            5 83.59005   US    FL
    ##          LAT      LON
    ##    1: 34.294 -116.147
    ##    2: 33.636  -91.756
    ##    3: 46.412  -86.650
    ##    4: 27.207  -98.121
    ##    5: 38.958  -94.371
    ##   ---                
    ## 1082: 27.655  -80.414
    ## 1083: 28.061  -81.757
    ## 1084: 28.483  -80.567
    ## 1085: 28.617  -80.683
    ## 1086: 28.233  -80.600

``` r
stations_median_atm_press
```

    ##       USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ##    1: 690150 93121 2023     6   2   13  56 34.294 -116.147  696      290
    ##    2: 720175 53919 2023     6   5   19  53 33.636  -91.756   82      220
    ##    3: 720198 54813 2023     6  10   19  56 46.412  -86.650  187      320
    ##    4: 720306 53879 2023     6   6   23  53 38.958  -94.371  306       70
    ##    5: 720333 53175 2023     6   4    2  56 33.898 -117.602  162      270
    ##   ---                                                                   
    ## 1007: 747910 13717 2023     6  10    6  56 33.680  -78.928    8      340
    ## 1008: 747915 93718 2023     6   4   10  53 33.816  -78.720   10       20
    ## 1009: 747930 12843 2023     6   1   17  53 27.655  -80.414    9       50
    ## 1010: 747931 12876 2023     6   7   15  53 28.061  -81.757   45      210
    ## 1011: 747940 12868 2023     6   1    4  38 28.483  -80.567    3       90
    ##       wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##    1:           5              N     3.1          5      22000             5
    ##    2:           5              N     3.1          5      22000             5
    ##    3:           5              N     5.7          5      99999             9
    ##    4:           5              N     2.1          5      22000             5
    ##    5:           5              N     5.1          5      22000             5
    ##   ---                                                                       
    ## 1007:           1              N     2.1          1      22000             1
    ## 1008:           5              N     5.1          5        549             5
    ## 1009:           5              N     5.1          5       2134             5
    ## 1010:           5              N     3.1          5      22000             5
    ## 1011:           5              N     3.1          5      22000             5
    ##       ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ##    1:                 9        N    16093           5       N          5 18.9
    ##    2:                 9        N    16093           5       N          5 33.9
    ##    3:                 9        N       NA           7       N          5  9.4
    ##    4:                 9        N    16093           5       N          5 29.4
    ##    5:                 9        N    16093           5       N          5 18.3
    ##   ---                                                                        
    ## 1007:                 9        N    16093           1       9          9 17.2
    ## 1008:                 M        N    16093           5       N          5 17.2
    ## 1009:                 M        N    16093           5       N          5 28.3
    ## 1010:                 9        N    16093           5       N          5 28.9
    ## 1011:                 9        N    16093           5       N          5 25.0
    ##       temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh CTRY STATE
    ##    1:       5       6.7            5    1011.5            5 45.17032   US    CA
    ##    2:       5      16.7            5    1011.5            5 35.73741   US    AR
    ##    3:       5       7.8            5    1011.5            5 89.85514   US    MI
    ##    4:       5      16.1            5    1011.5            5 44.57895   US    MO
    ##    5:       5      13.9            5    1011.5            5 75.66943   US    CA
    ##   ---                                                                          
    ## 1007:       1      15.6            1    1011.5            1 90.38939   US    SC
    ## 1008:       5      14.4            5    1011.5            5 83.72168   US    SC
    ## 1009:       5      22.2            5    1011.5            5 69.55144   US    FL
    ## 1010:       5      20.6            5    1011.5            5 60.88401   US    FL
    ## 1011:       5      22.0            5    1011.5            5 83.48871   US    FL
    ##          LAT      LON
    ##    1: 34.294 -116.147
    ##    2: 33.636  -91.756
    ##    3: 46.412  -86.650
    ##    4: 38.958  -94.371
    ##    5: 33.898 -117.602
    ##   ---                
    ## 1007: 33.683  -78.933
    ## 1008: 33.816  -78.720
    ## 1009: 27.655  -80.414
    ## 1010: 28.061  -81.757
    ## 1011: 28.483  -80.567

``` r
coincide
```

    ##   [1] 690150 720175 720198 720306 720333 720334 720361 720362 720363 720365
    ##  [11] 720369 720377 720379 720383 720394 720407 720413 720652 720735 720964
    ##  [21] 722011 722014 722020 722024 722026 722029 722030 722037 722038 722039
    ##  [31] 722040 722049 722050 722053 722054 722055 722057 722060 722065 722066
    ##  [41] 722069 722070 722080 722085 722093 722095 722096 722103 722106 722108
    ##  [51] 722110 722115 722116 722137 722140 722142 722151 722160 722166 722170
    ##  [61] 722175 722180 722181 722185 722188 722190 722195 722196 722197 722200
    ##  [71] 722212 722213 722215 722223 722225 722226 722230 722235 722238 722247
    ##  [81] 722250 722255 722260 722267 722268 722269 722270 722275 722276 722279
    ##  [91] 722280 722285 722286 722287 722300 722310 722314 722316 722320 722329
    ## [101] 722330 722340 722345 722348 722350 722354 722357 722358 722390 722400
    ## [111] 722403 722405 722410 722416 722420 722427 722429 722430 722440 722444
    ## [121] 722446 722447 722448 722469 722470 722479 722480 722484 722485 722486
    ## [131] 722487 722488 722489 722499 722505 722506 722508 722510 722516 722517
    ## [141] 722518 722520 722523 722524 722526 722527 722530 722533 722535 722536
    ## [151] 722539 722540 722541 722542 722543 722544 722547 722550 722555 722560
    ## [161] 722563 722570 722575 722576 722577 722580 722587 722589 722590 722594
    ## [171] 722595 722597 722599 722610 722616 722620 722630 722640 722648 722650
    ## [181] 722660 722670 722677 722678 722680 722686 722687 722689 722710 722720
    ## [191] 722725 722728 722730 722740 722745 722748 722764 722780 722783 722784
    ## [201] 722789 722810 722820 722823 722860 722868 722869 722874 722880 722886
    ## [211] 722897 722899 722900 722903 722904 722909 722920 722926 722927 722931
    ## [221] 722934 722956 722970 722975 722976 722977 723010 723020 723034 723035
    ## [231] 723037 723060 723066 723068 723069 723070 723075 723080 723084 723085
    ## [241] 723086 723087 723090 723095 723096 723097 723100 723104 723106 723108
    ## [251] 723109 723110 723115 723116 723117 723118 723119 723120 723124 723139
    ## [261] 723140 723143 723147 723150 723160 723170 723171 723174 723190 723193
    ## [271] 723194 723200 723230 723231 723235 723240 723250 723260 723270 723273
    ## [281] 723280 723290 723300 723307 723320 723340 723346 723347 723350 723403
    ## [291] 723405 723406 723407 723409 723415 723416 723417 723418 723419 723429
    ## [301] 723435 723436 723439 723440 723443 723444 723445 723447 723448 723449
    ## [311] 723450 723484 723495 723510 723520 723525 723526 723528 723530 723535
    ## [321] 723537 723540 723544 723545 723550 723556 723560 723564 723565 723566
    ## [331] 723575 723600 723627 723630 723635 723650 723656 723658 723660 723663
    ## [341] 723676 723677 723710 723723 723740 723750 723783 723810 723815 723816
    ## [351] 723820 723830 723840 723860 723870 723890 723895 723896 723898 723925
    ## [361] 723926 723940 723965 723980 723990 724010 724016 724019 724020 724030
    ## [371] 724035 724036 724040 724050 724060 724066 724070 724074 724075 724077
    ## [381] 724080 724084 724085 724088 724090 724093 724094 724095 724100 724106
    ## [391] 724110 724120 724125 724140 724170 724175 724176 724177 724180 724190
    ## [401] 724200 724210 724220 724230 724233 724235 724237 724238 724243 724250
    ## [411] 724270 724273 724275 724276 724280 724286 724287 724288 724290 724294
    ## [421] 724295 724296 724297 724298 724303 724320 724335 724336 724338 724340
    ## [431] 724345 724347 724350 724356 724365 724373 724375 724380 724384 724386
    ## [441] 724387 724388 724390 724397 724400 724420 724430 724450 724453 724454
    ## [451] 724455 724457 724458 724460 724463 724464 724467 724468 724475 724490
    ## [461] 724500 724502 724504 724505 724506 724507 724508 724509 724510 724515
    ## [471] 724516 724517 724518 724519 724520 724530 724550 724555 724556 724560
    ## [481] 724565 724580 724585 724586 724620 724625 724635 724636 724640 724645
    ## [491] 724646 724650 724655 724660 724665 724666 724673 724674 724675 724676
    ## [501] 724677 724680 724689 724695 724698 724700 724750 724754 724755 724756
    ## [511] 724760 724765 724767 724768 724769 724770 724776 724796 724797 724800
    ## [521] 724815 724828 724830 724838 724839 724846 724855 724860 724880 724885
    ## [531] 724920 724926 724927 724940 724945 724950 724955 724957 724988 725014
    ## [541] 725015 725016 725020 725025 725027 725029 725030 725036 725040 725046
    ## [551] 725053 725054 725059 725060 725064 725066 725068 725069 725070 725073
    ## [561] 725075 725079 725080 725086 725087 725088 725090 725098 725100 725103
    ## [571] 725104 725105 725107 725109 725114 725116 725117 725118 725124 725125
    ## [581] 725126 725127 725130 725140 725144 725150 725155 725157 725165 725170
    ## [591] 725172 725180 725190 725194 725196 725200 725205 725208 725210 725214
    ## [601] 725216 725217 725220 725224 725229 725235 725240 725245 725250 725254
    ## [611] 725256 725260 725267 725287 725290 725300 725305 725314 725315 725316
    ## [621] 725317 725320 725327 725330 725335 725336 725340 725342 725347 725350
    ## [631] 725360 725366 725370 725374 725375 725376 725377 725386 725387 725390
    ## [641] 725394 725395 725396 725404 725407 725420 725434 725440 725450 725460
    ## [651] 725461 725462 725465 725470 725472 725473 725480 725485 725490 725499
    ## [661] 725500 725510 725514 725520 725524 725525 725526 725527 725533 725540
    ## [671] 725555 725560 725565 725570 725610 725620 725625 725626 725635 725636
    ## [681] 725640 725645 725650 725660 725686 725690 725700 725705 725710 725717
    ## [691] 725720 725724 725744 725750 725755 725760 725763 725775 725776 725780
    ## [701] 725784 725785 725805 725810 725825 725830 725845 725847 725850 725864
    ## [711] 725866 725867 725895 725905 725910 725920 725955 725957 725958 725970
    ## [721] 725975 725976 725985 726050 726056 726064 726070 726073 726077 726079
    ## [731] 726083 726114 726116 726145 726155 726160 726163 726164 726165 726166
    ## [741] 726183 726184 726185 726190 726196 726223 726225 726227 726228 726350
    ## [751] 726355 726357 726360 726370 726375 726379 726380 726387 726390 726400
    ## [761] 726410 726416 726419 726424 726425 726430 726435 726436 726438 726440
    ## [771] 726452 726455 726456 726458 726463 726480 726487 726499 726500 726505
    ## [781] 726506 726508 726510 726514 726515 726516 726517 726518 726519 726525
    ## [791] 726539 726540 726545 726546 726550 726555 726556 726557 726559 726560
    ## [801] 726574 726575 726579 726580 726584 726585 726586 726587 726590 726620
    ## [811] 726627 726650 726654 726660 726665 726667 726676 726685 726690 726700
    ## [821] 726710 726720 726770 726776 726777 726797 726798 726810 726813 726815
    ## [831] 726816 726818 726824 726830 726836 726837 726875 726880 726881 726883
    ## [841] 726884 726885 726886 726904 726920 726930 726940 726985 726988 727033
    ## [851] 727119 727120 727130 727135 727340 727344 727347 727415 727437 727440
    ## [861] 727445 727450 727453 727455 727458 727469 727470 727476 727530 727535
    ## [871] 727555 727570 727573 727575 727584 727630 727640 727675 727676 727677
    ## [881] 727680 727684 727686 727687 727690 727720 727730 727740 727750 727770
    ## [891] 727790 727810 727825 727827 727830 727834 727845 727846 727850 727855
    ## [901] 727856 727857 727870 727883 727885 727890 727900 727910 727920 727924
    ## [911] 727925 727928 727930 727934 727935 727937 727938 727945 727976 727985
    ## [921] 740001 740030 740035 742060 742300 743312 743700 743945 743946 744550
    ## [931] 744655 744665 744860 744864 744865 744904 744915 744989 745046 745048
    ## [941] 745056 745058 745310 745431 745946 745966 746110 746120 746140 746410
    ## [951] 746710 746716 746930 747020 747040 747185 747187 747188 747320 747360
    ## [961] 747390 747400 747460 747540 747560 747570 747590 747680 747688 747750
    ## [971] 747760 747804 747808 747810 747812 747820 747830 747870 747900 747910
    ## [981] 747915 747930 747931 747940

Next identify the stations have these median values.

``` r
# Filter merged data for stations with median values
stations_median_values <- merged_data %>%
  filter(round(temp, 1) == round(median_temp, 1) &
         round(wind.sp, 1) == round(median_wind_sp, 1) &
         round(atm.press, 1) == round(median_atm_press, 1)) %>%
  distinct(USAFID)

stations_median_values
```

    ##     USAFID
    ##  1: 720394
    ##  2: 722200
    ##  3: 722267
    ##  4: 722544
    ##  5: 722678
    ##  6: 722740
    ##  7: 723085
    ##  8: 723416
    ##  9: 723419
    ## 10: 723895
    ## 11: 724010
    ## 12: 724030
    ## 13: 724276
    ## 14: 724500
    ## 15: 724580
    ## 16: 725080
    ## 17: 725336
    ## 18: 725366
    ## 19: 725460
    ## 20: 726777

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
