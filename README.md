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

medians <- merged_data[, .(
  temp_50= quantile(temp, probs=0.5, na.rm=TRUE),
  wind.sp_50 = quantile(wind.sp, probs=0.5, na.rm=TRUE),
  atm.press_50 = quantile(atm.press, probs=0.5, na.rm=TRUE)
)]
medians
```

    ##    temp_50 wind.sp_50 atm.press_50
    ## 1:    21.7        3.1       1011.7

Next identify the stations have these median values.

``` r
station_med <- merged_data[, .(
  temp_50= quantile(temp, probs=0.5, na.rm=TRUE),
  wind.sp_50 = quantile(wind.sp, probs=0.5, na.rm=TRUE),
  atm.press_50 = quantile(atm.press, probs=0.5, na.rm=TRUE)
), by=.(USAFID, STATE)]

station_med[, temp_dist:=abs(temp_50 - medians$temp_50)]
median_temp_stations <- station_med[temp_dist == 0]
median_temp_stations
```

    ##     USAFID STATE temp_50 wind.sp_50 atm.press_50 temp_dist
    ##  1: 720263    GA    21.7       2.60           NA         0
    ##  2: 720312    IA    21.7       2.60           NA         0
    ##  3: 720327    WI    21.7       4.60           NA         0
    ##  4: 720344    IA    21.7       3.10           NA         0
    ##  5: 720498    VA    21.7       3.10      1012.70         0
    ##  6: 722076    IL    21.7       3.60           NA         0
    ##  7: 722180    GA    21.7       3.10      1010.70         0
    ##  8: 722196    GA    21.7       2.85      1010.60         0
    ##  9: 722197    GA    21.7       2.60      1011.60         0
    ## 10: 723075    VA    21.7       4.10      1011.40         0
    ## 11: 723086    VA    21.7       3.60      1011.50         0
    ## 12: 723110    GA    21.7       2.60      1011.20         0
    ## 13: 723119    SC    21.7       3.10      1011.70         0
    ## 14: 723190    SC    21.7       3.10      1011.10         0
    ## 15: 723194    NC    21.7       3.10      1011.95         0
    ## 16: 723200    GA    21.7       2.10      1010.60         0
    ## 17: 723658    NM    21.7       3.60      1011.70         0
    ## 18: 723895    CA    21.7       3.10      1011.80         0
    ## 19: 724010    VA    21.7       3.60      1011.70         0
    ## 20: 724345    MO    21.7       3.10      1011.30         0
    ## 21: 724356    IN    21.7       3.60      1011.90         0
    ## 22: 724365    IN    21.7       3.10      1012.00         0
    ## 23: 724373    IN    21.7       3.10      1012.10         0
    ## 24: 724380    IN    21.7       4.10      1012.20         0
    ## 25: 724397    IL    21.7       4.10      1012.70         0
    ## 26: 724454    MO    21.7       3.10      1013.20         0
    ## 27: 724457    MO    21.7       2.60      1011.20         0
    ## 28: 724517    KS    21.7       4.10      1011.40         0
    ## 29: 724585    KS    21.7       4.10      1010.80         0
    ## 30: 724815    CA    21.7       3.10      1011.40         0
    ## 31: 724838    CA    21.7       3.60      1011.90         0
    ## 32: 725116    PA    21.7       3.10      1010.30         0
    ## 33: 725317    IL    21.7       3.60      1012.00         0
    ## 34: 725326    IL    21.7       3.10       999.70         0
    ## 35: 725340    IL    21.7       4.10      1012.20         0
    ## 36: 725450    IA    21.7       3.60      1012.35         0
    ## 37: 725472    IA    21.7       3.60      1012.00         0
    ## 38: 725473    IA    21.7       3.60      1013.30         0
    ## 39: 725480    IA    21.7       3.10      1012.20         0
    ## 40: 725499    IA    21.7       2.60      1012.00         0
    ## 41: 725513    NE    21.7       3.10           NA         0
    ## 42: 725720    UT    21.7       3.60      1009.60         0
    ## 43: 726515    SD    21.7       4.10      1013.10         0
    ## 44: 726525    SD    21.7       3.60      1012.80         0
    ## 45: 726530    SD    21.7       3.10           NA         0
    ## 46: 726546    SD    21.7       3.60      1011.35         0
    ## 47: 726556    MN    21.7       3.60      1012.15         0
    ## 48: 726560    SD    21.7       4.10      1010.35         0
    ## 49: 727555    MN    21.7       4.10      1013.30         0
    ## 50: 727570    ND    21.7       3.60      1011.45         0
    ## 51: 727845    WA    21.7       3.10      1012.40         0
    ## 52: 727900    WA    21.7       3.60      1012.00         0
    ## 53: 745046    CA    21.7       3.60      1011.80         0
    ## 54: 746410    OK    21.7       4.10      1010.10         0
    ## 55: 747808    GA    21.7       2.60      1012.60         0
    ##     USAFID STATE temp_50 wind.sp_50 atm.press_50 temp_dist

``` r
station_med[, wind.sp_dist:=abs(wind.sp_50 - medians$wind.sp_50)]
median_wind.sp_stations <- station_med[wind.sp_dist == 0]
median_wind.sp_stations
```

    ##      USAFID STATE temp_50 wind.sp_50 atm.press_50 temp_dist wind.sp_dist
    ##   1: 720110    TX    28.0        3.1           NA       6.3            0
    ##   2: 720113    MI    20.0        3.1           NA       1.7            0
    ##   3: 720258    MN    18.0        3.1           NA       3.7            0
    ##   4: 720261    TX    27.7        3.1           NA       6.0            0
    ##   5: 720266    IN    18.5        3.1           NA       3.2            0
    ##  ---                                                                    
    ## 580: 747804    GA    24.0        3.1       1010.8       2.3            0
    ## 581: 747809    GA    22.0        3.1           NA       0.3            0
    ## 582: 747900    SC    22.8        3.1       1011.1       1.1            0
    ## 583: 747918    SC    23.0        3.1           NA       1.3            0
    ## 584: 749483    GA    23.0        3.1           NA       1.3            0

``` r
station_med[, atm.press_dist:=abs(atm.press_50 - medians$atm.press_50)]
median_atm.press_stations <- station_med[atm.press_dist == 0]
median_atm.press_stations
```

    ##     USAFID STATE temp_50 wind.sp_50 atm.press_50 temp_dist wind.sp_dist
    ##  1: 720394    AR    23.9        2.1       1011.7       2.2          1.0
    ##  2: 722085    SC    24.4        3.1       1011.7       2.7          0.0
    ##  3: 722348    MS    25.0        2.1       1011.7       3.3          1.0
    ##  4: 723020    NC    24.4        3.6       1011.7       2.7          0.5
    ##  5: 723090    NC    23.9        4.1       1011.7       2.2          1.0
    ##  6: 723119    SC    21.7        3.1       1011.7       0.0          0.0
    ##  7: 723124    SC    21.1        2.6       1011.7       0.6          0.5
    ##  8: 723270    TN    23.9        3.1       1011.7       2.2          0.0
    ##  9: 723658    NM    21.7        3.6       1011.7       0.0          0.5
    ## 10: 724010    VA    21.7        3.6       1011.7       0.0          0.5
    ## 11: 724100    VA    19.4        3.1       1011.7       2.3          0.0
    ## 12: 724235    KY    21.1        3.1       1011.7       0.6          0.0
    ## 13: 724280    OH    20.6        3.6       1011.7       1.1          0.5
    ## 14: 724336    IL    23.3        3.1       1011.7       1.6          0.0
    ## 15: 724926    CA    20.6        4.1       1011.7       1.1          1.0
    ## 16: 725126    PA    18.3        3.1       1011.7       3.4          0.0
    ## 17: 725266    PA    16.1        3.1       1011.7       5.6          0.0
    ## 18: 725510    NE    23.9        3.6       1011.7       2.2          0.5
    ## 19: 725570    IA    22.2        3.1       1011.7       0.5          0.0
    ## 20: 725620    NE    20.6        3.6       1011.7       1.1          0.5
    ## 21: 725845    CA    14.4        2.6       1011.7       7.3          0.5
    ## 22: 726690    WY    13.3        4.6       1011.7       8.4          1.5
    ## 23: 726810    ID    20.0        3.1       1011.7       1.7          0.0
    ##     USAFID STATE temp_50 wind.sp_50 atm.press_50 temp_dist wind.sp_dist
    ##     atm.press_dist
    ##  1:              0
    ##  2:              0
    ##  3:              0
    ##  4:              0
    ##  5:              0
    ##  6:              0
    ##  7:              0
    ##  8:              0
    ##  9:              0
    ## 10:              0
    ## 11:              0
    ## 12:              0
    ## 13:              0
    ## 14:              0
    ## 15:              0
    ## 16:              0
    ## 17:              0
    ## 18:              0
    ## 19:              0
    ## 20:              0
    ## 21:              0
    ## 22:              0
    ## 23:              0
    ##     atm.press_dist

``` r
coincide <- station_med[temp_dist == 0 & wind.sp_dist == 0 & atm.press_dist == 0]
coincide
```

    ##    USAFID STATE temp_50 wind.sp_50 atm.press_50 temp_dist wind.sp_dist
    ## 1: 723119    SC    21.7        3.1       1011.7         0            0
    ##    atm.press_dist
    ## 1:              0

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
# Calculate Euclidean distance for temperature and wind speed
station_med[, temp_wind_dist := sqrt((temp_50 - medians$temp_50)^2 + (wind.sp_50 - medians$wind.sp_50)^2)]

# Find the station with the minimum distance per state
unique_representative_stations <- station_med[, .SD[which.min(temp_wind_dist)], by = STATE]

# Display representative stations per state
unique_representative_stations[, .(STATE, USAFID, temp_50, wind.sp_50, temp_wind_dist)]
```

    ##     STATE USAFID temp_50 wind.sp_50 temp_wind_dist
    ##  1:    CA 723895    21.7        3.1      0.0000000
    ##  2:    TX 722637    22.2        3.6      0.7071068
    ##  3:    MI 725375    20.6        3.1      1.1000000
    ##  4:    SC 723119    21.7        3.1      0.0000000
    ##  5:    IL 725326    21.7        3.1      0.0000000
    ##  6:    MO 724345    21.7        3.1      0.0000000
    ##  7:    AR 723445    22.2        2.6      0.7071068
    ##  8:    OR 726883    21.1        3.1      0.6000000
    ##  9:    WA 727845    21.7        3.1      0.0000000
    ## 10:    GA 722180    21.7        3.1      0.0000000
    ## 11:    MN 722003    21.5        3.1      0.2000000
    ## 12:    AL 720376    22.0        2.6      0.5830952
    ## 13:    IN 724365    21.7        3.1      0.0000000
    ## 14:    NC 723194    21.7        3.1      0.0000000
    ## 15:    VA 720498    21.7        3.1      0.0000000
    ## 16:    IA 720344    21.7        3.1      0.0000000
    ## 17:    PA 725116    21.7        3.1      0.0000000
    ## 18:    NE 725513    21.7        3.1      0.0000000
    ## 19:    ID 727830    21.1        2.6      0.7810250
    ## 20:    WI 720586    21.8        3.1      0.1000000
    ## 21:    WV 724250    20.6        2.6      1.2083046
    ## 22:    MD 722244    21.8        3.1      0.1000000
    ## 23:    AZ 723745    22.0        3.6      0.5830952
    ## 24:    OK 720358    21.4        3.1      0.3000000
    ## 25:    WY 726667    17.0        3.1      4.7000000
    ## 26:    LA 722251    23.1        2.6      1.4866069
    ## 27:    KY 724230    22.2        3.1      0.5000000
    ## 28:    FL 722067    23.0        3.1      1.3000000
    ## 29:    OH 724287    21.1        3.6      0.7810250
    ## 30:    NJ 725020    21.1        3.6      0.7810250
    ## 31:    NM 723658    21.7        3.6      0.5000000
    ## 32:    KS 724655    22.2        3.6      0.7071068
    ## 33:    ND 727570    21.7        3.6      0.5000000
    ## 34:    VT 726170    18.3        3.1      3.4000000
    ## 35:    CO 720531    20.8        3.1      0.9000000
    ## 36:    MS 722165    23.0        3.1      1.3000000
    ## 37:    CT 725045    20.6        3.1      1.1000000
    ## 38:    NV 724885    19.4        3.1      2.3000000
    ## 39:    UT 725720    21.7        3.6      0.5000000
    ## 40:    SD 726530    21.7        3.1      0.0000000
    ## 41:    TN 723240    22.2        3.1      0.5000000
    ## 42:    NY 744994    21.0        3.1      0.7000000
    ## 43:    RI 722151    18.9        3.1      2.8000000
    ## 44:    MA 725069    19.4        3.1      2.3000000
    ## 45:    DE 724180    22.0        3.6      0.5830952
    ## 46:    NH 743945    18.3        2.6      3.4365681
    ## 47:    ME 727119    16.9        2.1      4.9030603
    ## 48:    MT 727686    20.6        3.1      1.1000000
    ##     STATE USAFID temp_50 wind.sp_50 temp_wind_dist

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
library(leaflet)
```

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
library(knitr)

# Calculate elevation categories
merged_data[, elevation_category := cut(elev, breaks = c(-Inf, 93, 401, Inf), labels = c("Low", "Mid", "High"))]

# Calculate average temperature for each elevation category within each state
summary_table <- merged_data[, .(
  avg_temp = mean(temp, na.rm = TRUE)
), by = .(STATE, elevation_category)]

# Reshape summary table for better presentation
summary_table_wide <- reshape(summary_table, idvar = "STATE", timevar = "elevation_category", direction = "wide")

# Rename columns for clarity
colnames(summary_table_wide) <- c("STATE", "Low Elevation", "Mid Elevation", "High Elevation")

# Print summary table with kable
summary_table_md <- kable(summary_table_wide, format = "markdown")
writeLines(summary_table_md, "summary_table.md")
summary_table_md
```

| STATE | Low Elevation | Mid Elevation | High Elevation |
|:------|--------------:|--------------:|---------------:|
| CA    |     18.168493 |      18.77291 |       18.28147 |
| TX    |     26.510134 |      28.10119 |       28.75371 |
| MI    |     18.086806 |      18.55130 |             NA |
| SC    |            NA |      22.39744 |       23.68426 |
| IL    |     20.843173 |      22.12500 |             NA |
| MO    |            NA |      23.75204 |       25.79654 |
| AR    |     23.723926 |      24.42746 |       25.59598 |
| OR    |     16.714169 |      16.39100 |       15.20318 |
| WA    |     16.809634 |      17.81729 |       15.25193 |
| GA    |            NA |      23.25156 |       24.75861 |
| MN    |     19.936902 |      21.16681 |       22.66275 |
| AL    |            NA |      23.79844 |       25.07164 |
| IN    |            NA |      20.14294 |             NA |
| NC    |     18.074166 |      21.22338 |       22.85846 |
| VA    |     17.964222 |      20.50870 |       21.37290 |
| IA    |     22.067188 |      22.26465 |             NA |
| PA    |     17.286934 |      19.41418 |       20.38417 |
| NE    |     21.053404 |      23.48527 |             NA |
| ID    |     16.424158 |            NA |             NA |
| WI    |     18.032934 |      19.53827 |             NA |
| WV    |     17.490616 |      19.33798 |             NA |
| MD    |     20.552997 |      20.62025 |       21.29812 |
| AZ    |     23.876864 |      30.37686 |       29.28486 |
| OK    |     24.014658 |      25.08148 |             NA |
| WY    |     13.748400 |            NA |             NA |
| LA    |            NA |      26.13198 |       27.62075 |
| KY    |     20.178196 |      21.39425 |             NA |
| FL    |            NA |            NA |       26.66320 |
| OH    |            NA |      19.48407 |             NA |
| NJ    |            NA |      19.31963 |       19.97914 |
| NM    |     22.451541 |            NA |             NA |
| KS    |     22.099784 |      24.16366 |             NA |
| ND    |     20.451593 |      21.80981 |             NA |
| VT    |            NA |      16.91005 |             NA |
| CO    |     15.198150 |            NA |             NA |
| MS    |            NA |      24.64754 |       26.36904 |
| CT    |            NA |      18.78433 |       19.37249 |
| NV    |     20.852050 |            NA |             NA |
| UT    |     19.754587 |            NA |             NA |
| SD    |     20.645196 |      22.79495 |             NA |
| TN    |     19.457179 |      22.93625 |       26.09351 |
| NY    |     15.917107 |      18.32753 |       18.76420 |
| RI    |            NA |      17.46589 |       17.87880 |
| MA    |            NA |      17.59058 |       17.44824 |
| DE    |            NA |            NA |       21.42545 |
| NH    |      7.243417 |      16.79792 |       17.78300 |
| ME    |     15.329681 |      15.52840 |       15.23070 |
| MT    |     16.330794 |            NA |             NA |

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

``` r
library(ggplot2)
library(mgcv)

# Create a lazy table with median values per station
lazy_table <- station_med

# Filter out values of atmospheric pressure outside of the range 1000 to 1020
lazy_table <- lazy_table[atm.press_50 >= 1000 & atm.press_50 <= 1020, ]

# Scatterplot of temperature and atmospheric pressure with linear regression line and smooth line
ggplot(lazy_table, aes(x = atm.press_50, y = temp_50)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red") +
  labs(x = "Atmospheric Pressure", y = "Temperature") +
  ggtitle("Scatterplot of Temperature and Atmospheric Pressure") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Fit linear model
linear_model <- lm(temp_50 ~ atm.press_50, data = lazy_table)

# Fit spline model
spline_model <- gam(temp_50 ~ s(wind.sp_50, bs = "cr"), data = lazy_table)

# Summary of models
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = temp_50 ~ atm.press_50, data = lazy_table)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.385  -2.613   0.198   2.304  11.624 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1175.95049   58.39858   20.14   <2e-16 ***
    ## atm.press_50   -1.14162    0.05772  -19.78   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.755 on 1083 degrees of freedom
    ## Multiple R-squared:  0.2653, Adjusted R-squared:  0.2647 
    ## F-statistic: 391.2 on 1 and 1083 DF,  p-value: < 2.2e-16

``` r
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp_50 ~ s(wind.sp_50, bs = "cr")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  20.9545     0.1297   161.6   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                 edf Ref.df     F p-value    
    ## s(wind.sp_50) 2.817  3.606 16.31  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0514   Deviance explained = 5.38%
    ## GCV = 18.269  Scale est. = 18.205    n = 1083

``` r
# Plot results from models
plot(linear_model)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
plot(spline_model)
```

![](README_files/figure-gfm/unnamed-chunk-12-5.png)<!-- --> Answer:

First, compare the adjusted R-square value of each model. The adjusted
R-squared of linear model is 0.2647 which is higher than 0.0514, the
adjusted R-squared value of the spline model.

Second, compare the residual plot of each model. It seems that both plot
are performing well in terms of distribution around zero and the fitted
line. Comparatively, the QQ-plot of the linear model performs better
where the scatterplot does not deviate from the fitted line at all.

Overall, the linear model performs better such that it is a better fit.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
