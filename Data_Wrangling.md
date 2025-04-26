Data_Wrangling
================
Mike
2025-04-22

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(fuzzyjoin)
```

## Data Processing

### Converting the data to long form which may be easier lookups and transformations

``` r
filenames <- list.files("Datasets/NTS Data") # get file names

# read data
combined_tables <- lapply(filenames, function(file) {
  read.csv(paste0("Datasets/NTS Data/",file), fileEncoding = "ISO-8859-1")  
})

#replcace names
combined_tables <- setNames(combined_tables, gsub("\\.csv$", "", filenames))
```

### Current state of the data

``` r
head(combined_tables[[1]])
```

    ##          Player Team Position GP       TOI Goals Total.Assists First.Assists
    ## 1    Ryan Suter  STL        D 82 1396.8667     1            12             7
    ## 2   Brent Burns  CAR        D 82 1293.4000     6            16             8
    ## 3   Corey Perry  EDM        R 80  828.5167    13             6             3
    ## 4 Alex Ovechkin  WSH        L 65  822.2000    21            17            12
    ## 5 Evgeni Malkin  PIT        C 68  981.5500    10            17            12
    ## 6 Sidney Crosby  PIT        C 80 1272.8333    16            40            23
    ##   Second.Assists Total.Points   IPP Shots   SH.   ixG iCF iFF iSCF iHDCF
    ## 1              5           13 22.41    58  1.72  2.83 151  90   52     7
    ## 2              8           22 36.07   135  4.44  4.20 384 225   84     6
    ## 3              3           19 54.29    82 15.85 10.90 166 123  105    54
    ## 4              5           38 77.55   140 15.00 13.08 302 215  139    57
    ## 5              5           27 71.05    86 11.63 11.67 179 121  109    53
    ## 6             17           56 76.71   151 10.60 17.93 288 236  155    69
    ##   Rush.Attempts Rebounds.Created PIM Total.Penalties Minor Major Misconduct
    ## 1            13               11  24              12    12     0          0
    ## 2            20               35  20              10    10     0          0
    ## 3             5               13  59              18    12     5          1
    ## 4             9               20  12               6     6     0          0
    ## 5             1               16  34              16    16     0          0
    ## 6            10               24  29              13    12     1          0
    ##   Penalties.Drawn Giveaways Takeaways Hits Hits.Taken Shots.Blocked
    ## 1               2        60        14   33         70            90
    ## 2               5        85        29    7         41            68
    ## 3              26        37        12   34         68            25
    ## 4               2        36         5  102         24            11
    ## 5              23        68        28   29         59            42
    ## 6              18        68        24   57         35            35
    ##   Faceoffs.Won Faceoffs.Lost Faceoffs..   season rate stdoi sit
    ## 1            0             0         NA 20242025    n   std 5v5
    ## 2            0             0         NA 20242025    n   std 5v5
    ## 3           13            14      48.15 20242025    n   std 5v5
    ## 4            0             1       0.00 20242025    n   std 5v5
    ## 5          271           323      45.62 20242025    n   std 5v5
    ## 6          785           611      56.23 20242025    n   std 5v5

### Vs what we want to get to

``` r
## Vs what we want to get to
combined_tables[[1]] %>%
  pivot_longer(cols = !c("Player", "Team", "Position", "season", "rate", "stdoi", "sit"),
               names_to = "stat",
               values_to = "value")
```

    ## # A tibble: 284,487 × 9
    ##    Player     Team  Position   season rate  stdoi sit   stat             value
    ##    <chr>      <chr> <chr>       <int> <chr> <chr> <chr> <chr>            <dbl>
    ##  1 Ryan Suter STL   D        20242025 n     std   5v5   GP               82   
    ##  2 Ryan Suter STL   D        20242025 n     std   5v5   TOI            1397.  
    ##  3 Ryan Suter STL   D        20242025 n     std   5v5   Goals             1   
    ##  4 Ryan Suter STL   D        20242025 n     std   5v5   Total.Assists    12   
    ##  5 Ryan Suter STL   D        20242025 n     std   5v5   First.Assists     7   
    ##  6 Ryan Suter STL   D        20242025 n     std   5v5   Second.Assists    5   
    ##  7 Ryan Suter STL   D        20242025 n     std   5v5   Total.Points     13   
    ##  8 Ryan Suter STL   D        20242025 n     std   5v5   IPP              22.4 
    ##  9 Ryan Suter STL   D        20242025 n     std   5v5   Shots            58   
    ## 10 Ryan Suter STL   D        20242025 n     std   5v5   SH.               1.72
    ## # ℹ 284,477 more rows

### Lets get there

``` r
combined_tables_long <- list()
for (name in names(combined_tables)) {
  long_df <- combined_tables[[name]] %>%
  pivot_longer(cols = !c("Player", "Team", "Position", "season", "rate", "stdoi", "sit"),
               names_to = "stat",
               values_to = "value")
  
   combined_tables_long[[name]] <- long_df
}

rm(long_df)

all_data_long <- bind_rows(combined_tables_long)

rm(combined_tables_long)

#write.csv(all_data_long, file = "Datasets/all_data_long.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
```

### Getting player IDs onto the natural stat trick data

``` r
skaters_by_season <- read.csv("Datasets/skaterdata.csv", fileEncoding = "ISO-8859-1")

## List of skaters from natural stat trick data
skaters <- all_data_long %>%
  select(Player, Team, Position, season) %>%
  distinct(Player, Position, season, .keep_all = TRUE)
```

``` r
skaters_duplicates <- skaters %>%
  group_by(Player, season) %>%
  summarise(n = n()) %>%
  filter(n > 1)
```

    ## `summarise()` has grouped output by 'Player'. You can override using the
    ## `.groups` argument.

``` r
print(skaters_duplicates)
```

    ## # A tibble: 6 × 3
    ## # Groups:   Player [2]
    ##   Player             season     n
    ##   <chr>               <int> <int>
    ## 1 Elias Pettersson 20242025     2
    ## 2 Sebastian Aho    20172018     2
    ## 3 Sebastian Aho    20202021     2
    ## 4 Sebastian Aho    20212022     2
    ## 5 Sebastian Aho    20222023     2
    ## 6 Sebastian Aho    20232024     2

``` r
## Need to worry about Sebastian Ahos and Elias Pettersons
```

``` r
## List of skaters from NHL API
nhl_skater_names <- skaters_by_season %>%
  select("Player" = "skaterFullName", playerId, "Position" = "positionCode") %>%
  group_by(Player, playerId) %>%
  distinct() %>%
  ungroup()

## Fuzzy matching because the names are not 1:1. this will save some time 
fuzzyjoin_player <- stringdist_left_join(
  skaters, nhl_skater_names,
  by = "Player", 
  method = "jw",  
  max_dist = 0.33,  
  distance_col = "dist") 
```

``` r
## running the antti join until it returns zero. Trying to get the fuzzy match to try for everyone
fuzzyanti_join <- fuzzyjoin_player %>%
  filter(is.na(playerId))

head(fuzzyanti_join)
```

    ## # A tibble: 0 × 8
    ## # ℹ 8 variables: Player.x <chr>, Team <chr>, Position.x <chr>, season <int>,
    ## #   Player.y <chr>, playerId <int>, Position.y <chr>, dist <dbl>

``` r
## only showing the best match for each player
fuzzyjoin_df <- fuzzyjoin_player %>% 
  group_by(Player.x, Position.x) %>%
  slice_min(order_by = dist, with_ties = FALSE) %>%
  ungroup() 
  
head(fuzzyjoin_df %>%
       arrange(desc(dist)))
```

    ## # A tibble: 6 × 8
    ##   Player.x            Team  Position.x season Player.y playerId Position.y  dist
    ##   <chr>               <chr> <chr>       <int> <chr>       <int> <chr>      <dbl>
    ## 1 Pierre-Olivier Jos… PIT,… D          2.02e7 Pierre-…  8479400 L          0.322
    ## 2 Benoit-Olivier Gro… ANA   C          2.02e7 Benoit …  8471678 L          0.306
    ## 3 Cristoval Nieves    NYR   C          2.02e7 Chris N…  8467493 R          0.253
    ## 4 Janis Moser         T.B   D          2.02e7 Jason C…  8466251 L          0.237
    ## 5 Tommy Novak         NSH,… C          2.02e7 Thomas …  8478438 C          0.237
    ## 6 Zachary Hayes       VGK   D          2.02e7 Zack Ha…  8481849 D          0.210

``` r
#write.csv(fuzzyjoin_df, file = "fuzzyjoin.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
```

## Make edits directly in the file to quality check the fuzzy matches

``` r
#rm(fuzzyjoin_df)
#rm(fuzzyjoin_player)
#rm(fuzzyanti_join)

combo_player_wids <- read.csv(file = "fuzzyjoin.csv", fileEncoding = "ISO-8859-1")

nts_player_wids <- combo_player_wids %>%
  select("Player" = "Player.x", Team, "Position" = "Position.x", playerId)

nhl_player_wids <- combo_player_wids %>%
  select("Player" = "Player.y", Team, "Position" = "Position.y", playerId)

combo_player_wids <- rbind(nts_player_wids, nhl_player_wids) %>%
  distinct()

#write.csv(combo_player_wids, "playeridindex.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")

#rm(nts_player_wids)
#rm(nhl_player_wids)
#rm(combo_player_wids)
```
