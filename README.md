ST558 Project 1
================
David Arthur
6/14/2021

-   [Function to get Team ID when name is
    input](#function-to-get-team-id-when-name-is-input)
    -   [Records API functions](#records-api-functions)
    -   [Wrapper function](#wrapper-function)

# Function to get Team ID when name is input

``` r
franchiseInfo <- GET("https://records.nhl.com/site/api/franchise") %>% 
  content("text") %>% 
  fromJSON(flatten = TRUE)
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
teamNameIdMatch <- franchiseInfo$data %>% select(id, mostRecentTeamId, fullName)

getTeamId <- function(team){
  Id <- teamNameIdMatch %>% filter(str_detect(fullName, team)) %>%
    select(id)
  if (nrow(Id) == 1){
    return(as.numeric(Id))
  } else {
    stop("try again entering team name or ID")
  }
}
```

## Records API functions

``` r
recordsBaseURL <- "https://records.nhl.com/site/api"

getFranchiseInfo <- function(team = NULL){
  if(is.null(team)){
    fullURL <- paste0(recordsBaseURL, "/franchise")
    franchiseInfo <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  } else{
    if(!is.numeric(team)){team <- getTeamId(team)}
    fullURL <- paste0(recordsBaseURL, "/franchise?cayenneExp=id=", team)
    franchiseInfo <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  }
  return(franchiseInfo$data)
}

getTeamTotals <- function(team = NULL){
  if(is.null(team)){
    fullURL <- paste0(recordsBaseURL, "/franchise-team-totals")
    teamTotals <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  } else{
    if(!is.numeric(team)){team <- getTeamId(team)}
    fullURL <- paste0(recordsBaseURL, "/franchise-team-totals?cayenneExp=franchiseId=", team)
    teamTotals <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  }
  return(teamTotals$data)
}

getSeasonRecords <- function(team = NULL){
  if(is.null(team)){
    fullURL <- paste0(recordsBaseURL, "/franchise-season-records")
    seasonRecords <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  } else{
  if(!is.numeric(team)){team <- getTeamId(team)}
  fullURL <- paste0(recordsBaseURL, "/franchise-season-records?cayenneExp=franchiseId=", team)
  seasonRecords <- GET(fullURL) %>% content("text") %>% fromJSON(flatten = TRUE)
  }
  return(seasonRecords$data)
}

getGoalieRecords <- function(team = NULL){
  if(is.null(team)){
    fullURL <- paste0(recordsBaseURL, "/franchise-goalie-records")
    goalieRecords <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  } else{
  if(!is.numeric(team)){team <- getTeamId(team)}
  fullURL <- paste0(recordsBaseURL, "/franchise-goalie-records?cayenneExp=franchiseId=", team)
  goalieRecords <- GET(fullURL) %>% content("text") %>% fromJSON(flatten = TRUE)
  }
  return(goalieRecords$data)
}

getSkaterRecords<- function(team = NULL){
  if(is.null(team)){
    fullURL <- paste0(recordsBaseURL, "/franchise-skater-records")
    skaterRecords <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  } else{
  if(!is.numeric(team)){team <- getTeamId(team)}
  fullURL <- paste0(recordsBaseURL, "/franchise-skater-records?cayenneExp=franchiseId=", team)
  skaterRecords <- GET(fullURL) %>% content("text") %>% fromJSON(flatten = TRUE)
  }
  return(skaterRecords$data)
}

getFranchiseDetail <- function(team = NULL){
  if(is.null(team)){
    fullURL <- paste0(recordsBaseURL, "/franchise-detail")
    franchiseDetail <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE)
  } else{
  if(!is.numeric(team)){team <- getTeamId(team)}
  fullURL <- paste0(recordsBaseURL, "/franchise-detail?cayenneExp=id=", team)
  franchiseDetail <- GET(fullURL) %>% content("text") %>% fromJSON(flatten = TRUE)
  }
  return(franchiseDetail$data)
}

franchiseInfo <- getFranchiseInfo()
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
franchiseInfo %>% filter(is.na(lastSeasonId))
```

    ##    id firstSeasonId              fullName lastSeasonId
    ## 1   1      19171918    Montréal Canadiens           NA
    ## 2   5      19171918   Toronto Maple Leafs           NA
    ## 3   6      19241925         Boston Bruins           NA
    ## 4  10      19261927      New York Rangers           NA
    ## 5  11      19261927    Chicago Blackhawks           NA
    ## 6  12      19261927     Detroit Red Wings           NA
    ## 7  14      19671968     Los Angeles Kings           NA
    ## 8  15      19671968          Dallas Stars           NA
    ## 9  16      19671968   Philadelphia Flyers           NA
    ## 10 17      19671968   Pittsburgh Penguins           NA
    ## 11 18      19671968       St. Louis Blues           NA
    ## 12 19      19701971        Buffalo Sabres           NA
    ## 13 20      19701971     Vancouver Canucks           NA
    ## 14 21      19721973        Calgary Flames           NA
    ## 15 22      19721973    New York Islanders           NA
    ## 16 23      19741975     New Jersey Devils           NA
    ## 17 24      19741975   Washington Capitals           NA
    ## 18 25      19791980       Edmonton Oilers           NA
    ## 19 26      19791980   Carolina Hurricanes           NA
    ## 20 27      19791980    Colorado Avalanche           NA
    ## 21 28      19791980       Arizona Coyotes           NA
    ## 22 29      19911992       San Jose Sharks           NA
    ## 23 30      19921993       Ottawa Senators           NA
    ## 24 31      19921993   Tampa Bay Lightning           NA
    ## 25 32      19931994         Anaheim Ducks           NA
    ## 26 33      19931994      Florida Panthers           NA
    ## 27 34      19981999   Nashville Predators           NA
    ## 28 35      19992000         Winnipeg Jets           NA
    ## 29 36      20002001 Columbus Blue Jackets           NA
    ## 30 37      20002001        Minnesota Wild           NA
    ## 31 38      20172018  Vegas Golden Knights           NA
    ## 32 39      20212022        Seattle Kraken           NA
    ##    mostRecentTeamId teamAbbrev teamCommonName teamPlaceName
    ## 1                 8        MTL      Canadiens      Montréal
    ## 2                10        TOR    Maple Leafs       Toronto
    ## 3                 6        BOS         Bruins        Boston
    ## 4                 3        NYR        Rangers      New York
    ## 5                16        CHI     Blackhawks       Chicago
    ## 6                17        DET      Red Wings       Detroit
    ## 7                26        LAK          Kings   Los Angeles
    ## 8                25        DAL          Stars        Dallas
    ## 9                 4        PHI         Flyers  Philadelphia
    ## 10                5        PIT       Penguins    Pittsburgh
    ## 11               19        STL          Blues     St. Louis
    ## 12                7        BUF         Sabres       Buffalo
    ## 13               23        VAN        Canucks     Vancouver
    ## 14               20        CGY         Flames       Calgary
    ## 15                2        NYI      Islanders      New York
    ## 16                1        NJD         Devils    New Jersey
    ## 17               15        WSH       Capitals    Washington
    ## 18               22        EDM         Oilers      Edmonton
    ## 19               12        CAR     Hurricanes      Carolina
    ## 20               21        COL      Avalanche      Colorado
    ## 21               53        ARI        Coyotes       Arizona
    ## 22               28        SJS         Sharks      San Jose
    ## 23                9        OTT       Senators        Ottawa
    ## 24               14        TBL      Lightning     Tampa Bay
    ## 25               24        ANA          Ducks       Anaheim
    ## 26               13        FLA       Panthers       Florida
    ## 27               18        NSH      Predators     Nashville
    ## 28               52        WPG           Jets      Winnipeg
    ## 29               29        CBJ   Blue Jackets      Columbus
    ## 30               30        MIN           Wild     Minnesota
    ## 31               54        VGK Golden Knights         Vegas
    ## 32               55        SEA         Kraken       Seattle

``` r
getFranchiseInfo("Hurricanes")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##   id firstSeasonId            fullName lastSeasonId
    ## 1 26      19791980 Carolina Hurricanes           NA
    ##   mostRecentTeamId teamAbbrev teamCommonName teamPlaceName
    ## 1               12        CAR     Hurricanes      Carolina

``` r
teamTotals <- getTeamTotals()
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
activeTeams <- teamTotals %>% filter(activeFranchise == 1) %>% select(teamName) %>% distinct()
getTeamTotals("Hurricanes")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##   id activeFranchise firstSeasonId franchiseId gameTypeId
    ## 1 24               1      19971998          26          2
    ## 2 67               1      19791980          26          2
    ## 3 23               1      19971998          26          3
    ## 4 68               1      19791980          26          3
    ##   gamesPlayed goalsAgainst goalsFor homeLosses
    ## 1        1812         5140     4914        323
    ## 2        1420         5345     4704        297
    ## 3         112          282      272         24
    ## 4          49          177      143         10
    ##   homeOvertimeLosses homeTies homeWins lastSeasonId losses
    ## 1                 77       52      453           NA    725
    ## 2                 NA       95      318     19961997    709
    ## 3                  0       NA       32           NA     54
    ## 4                 NA       NA       12     19961997     31
    ##   overtimeLosses penaltyMinutes pointPctg points roadLosses
    ## 1            174          19429    0.5281   1914        402
    ## 2             NA          29656    0.4384   1245        412
    ## 3              0           1310    0.0714     16         30
    ## 4             NA           1273    0.0000      0         21
    ##   roadOvertimeLosses roadTies roadWins shootoutLosses
    ## 1                 97       34      374             61
    ## 2                 NA       82      216              0
    ## 3                  2       NA       26              0
    ## 4                 NA       NA        6              0
    ##   shootoutWins shutouts teamId            teamName ties
    ## 1           50       99     12 Carolina Hurricanes   86
    ## 2            0       50     34    Hartford Whalers  177
    ## 3            0       11     12 Carolina Hurricanes   NA
    ## 4            0        1     34    Hartford Whalers   NA
    ##   triCode wins
    ## 1     CAR  827
    ## 2     HFD  534
    ## 3     CAR   58
    ## 4     HFD   18

``` r
getSeasonRecords()
```

    ## No encoding supplied: defaulting to UTF-8.

    ##    id fewestGoals fewestGoalsAgainst
    ## 1   1         174                164
    ## 2   2         170                190
    ## 3   3         150                177
    ## 4   4         173                164
    ## 5   5         182                188
    ## 6   6         147                172
    ## 7   7         157                175
    ## 8   8         155                131
    ## 9   9         191                179
    ## 10 10         147                131
    ## 11 11         170                203
    ## 12 12         171                202
    ## 13 13         176                201
    ## 14 14         151                192
    ## 15 15         181                182
    ## 16 16         133                164
    ## 17 17         145                132
    ##    fewestGoalsAgainstSeasons         fewestGoalsSeasons
    ## 1               2003-04 (82)               2010-11 (82)
    ## 2               1975-76 (80)               1972-73 (78)
    ## 3               1970-71 (78)               1954-55 (70)
    ## 4               1973-74 (78)               1967-68 (74)
    ## 5               1997-98 (82)               1969-70 (76)
    ## 6               1952-53 (70)               1955-56 (70)
    ## 7               1998-99 (82)               2013-14 (82)
    ## 8               1955-56 (70)               1952-53 (70)
    ## 9               1998-99 (82) 1995-96 (82), 2019-20 (82)
    ## 10              1953-54 (70)               1954-55 (70)
    ## 11              2019-20 (82)               1999-00 (82)
    ## 12              1998-99 (82)               2002-03 (82)
    ## 13              1996-97 (82)               2002-03 (82)
    ## 14              2003-04 (82)               1997-98 (82)
    ## 15              2016-17 (82)               1974-75 (80)
    ## 16              1973-74 (78)               1953-54 (70)
    ## 17              1953-54 (70)               2019-20 (82)
    ##    fewestLosses                      fewestLossesSeasons
    ## 1            19                             2000-01 (82)
    ## 2            15                             1978-79 (80)
    ## 3            17                             1971-72 (78)
    ## 4            12                             1979-80 (80)
    ## 5            21               1992-93 (84), 2016-17 (82)
    ## 6            13                             1971-72 (78)
    ## 7            16                             1974-75 (80)
    ## 8             8                             1976-77 (80)
    ## 9            21 2000-01 (82), 2002-03 (82), 2005-06 (82)
    ## 10           16                             1950-51 (70)
    ## 11           20                             2017-18 (82)
    ## 12           22                             2005-06 (82)
    ## 13           26 2011-12 (82), 2015-16 (82), 2019-20 (82)
    ## 14           16                             2018-19 (82)
    ## 15           15                             2009-10 (82)
    ## 16           14                             1973-74 (78)
    ## 17           13               1950-51 (70), 1995-96 (82)
    ##    fewestPoints fewestPointsSeasons fewestTies
    ## 1            36        1975-76 (80)          3
    ## 2            30        1972-73 (78)          4
    ## 3            47        1965-66 (70)          4
    ## 4            56        2006-07 (82)          4
    ## 5            38        1983-84 (80)          4
    ## 6            38        1961-62 (70)          5
    ## 7            51        1971-72 (78)          5
    ## 8            65        1950-51 (70)          5
    ## 9            24        1992-93 (84)          4
    ## 10           48        1984-85 (80)          4
    ## 11           39        1999-00 (82)          7
    ## 12           45        1982-83 (80)          4
    ## 13           60        2001-02 (82)          6
    ## 14           44        1997-98 (82)          6
    ## 15           21        1974-75 (80)          5
    ## 16           31        1953-54 (70)          6
    ## 17           39        2019-20 (82)          4
    ##             fewestTiesSeasons fewestWins
    ## 1                1985-86 (80)         12
    ## 2                1983-84 (80)         12
    ## 3                2001-02 (82)         17
    ## 4                1985-86 (80)         17
    ## 5                1995-96 (82)         16
    ## 6                1972-73 (78)         14
    ## 7                2000-01 (82)         16
    ## 8                1983-84 (80)         25
    ## 9                1992-93 (84)         10
    ## 10               1989-90 (80)         20
    ## 11 1999-00 (82), 2002-03 (82)         14
    ## 12               1985-86 (80)         19
    ## 13               1999-00 (82)         22
    ## 14               2000-01 (82)         17
    ## 15 1974-75 (80), 1983-84 (80)          8
    ## 16               1989-90 (80)         12
    ## 17               1966-67 (70)         16
    ##                           fewestWinsSeasons franchiseId
    ## 1                              1975-76 (80)          23
    ## 2                              1972-73 (78)          22
    ## 3  1952-53 (70), 1954-55 (70), 1959-60 (70)          10
    ## 4                              1969-70 (76)          16
    ## 5                              1983-84 (80)          17
    ## 6                              1962-63 (70)           6
    ## 7                              1971-72 (78)          19
    ## 8                              1950-51 (70)           1
    ## 9                              1992-93 (84)          30
    ## 10               1981-82 (80), 1984-85 (80)           5
    ## 11                             1999-00 (82)          35
    ## 12                             1982-83 (80)          26
    ## 13               2000-01 (82), 2001-02 (82)          33
    ## 14                             1997-98 (82)          31
    ## 15                             1974-75 (80)          24
    ## 16                             1953-54 (70)          11
    ## 17                             1976-77 (80)          12
    ##          franchiseName homeLossStreak
    ## 1    New Jersey Devils             10
    ## 2   New York Islanders              7
    ## 3     New York Rangers              7
    ## 4  Philadelphia Flyers              8
    ## 5  Pittsburgh Penguins             14
    ## 6        Boston Bruins             11
    ## 7       Buffalo Sabres              7
    ## 8   Montréal Canadiens              7
    ## 9      Ottawa Senators             11
    ## 10 Toronto Maple Leafs              7
    ## 11       Winnipeg Jets              9
    ## 12 Carolina Hurricanes              8
    ## 13    Florida Panthers              7
    ## 14 Tampa Bay Lightning             10
    ## 15 Washington Capitals             11
    ## 16  Chicago Blackhawks              9
    ## 17   Detroit Red Wings              7
    ##                                     homeLossStreakDates
    ## 1                             Jan 26 2021 - Mar 13 2021
    ## 2  Nov 13 1999 - Dec 14 1999, Feb 27 2001 - Mar 29 2001
    ## 3  Oct 20 1976 - Nov 14 1976, Mar 24 1993 - Apr 14 1993
    ## 4                             Dec 09 2006 - Jan 27 2007
    ## 5                             Dec 31 2003 - Feb 22 2004
    ## 6                             Dec 08 1924 - Feb 17 1925
    ## 7                             Oct 10 2013 - Nov 02 2013
    ## 8  Dec 16 1939 - Jan 18 1940, Oct 28 2000 - Nov 25 2000
    ## 9                             Oct 27 1993 - Dec 08 1993
    ## 10                            Nov 11 1984 - Dec 05 1984
    ## 11                            Feb 03 2000 - Mar 16 2000
    ## 12                            Mar 14 2013 - Apr 09 2013
    ## 13                            Feb 26 2003 - Mar 24 2003
    ## 14                            Jan 03 1998 - Feb 26 1998
    ## 15                            Feb 18 1975 - Mar 30 1975
    ## 16                            Feb 08 1928 - Mar 21 1928
    ## 17                            Feb 20 1982 - Mar 25 1982
    ##    homePointStreak
    ## 1               15
    ## 2               23
    ## 3               24
    ## 4               26
    ## 5               20
    ## 6               27
    ## 7               21
    ## 8               34
    ## 9               23
    ## 10              18
    ## 11              11
    ## 12              15
    ## 13              10
    ## 14              13
    ## 15              17
    ## 16              18
    ## 17              24
    ##                                    homePointStreakDates
    ## 1  Jan 08 1997 - Mar 15 1997, Dec 14 1999 - Feb 17 2000
    ## 2  Oct 17 1978 - Jan 20 1979, Jan 02 1982 - Apr 03 1982
    ## 3  Oct 14 1970 - Jan 31 1971, Oct 24 1995 - Feb 15 1996
    ## 4                             Oct 11 1979 - Feb 03 1980
    ## 5                             Nov 30 1974 - Feb 22 1975
    ## 6                             Nov 22 1970 - Mar 20 1971
    ## 7                             Oct 08 1972 - Jan 07 1973
    ## 8                             Nov 01 1976 - Apr 02 1977
    ## 9                             Dec 18 2003 - Mar 20 2004
    ## 10 Nov 28 1933 - Mar 10 1934, Oct 31 1953 - Jan 23 1954
    ## 11 Dec 09 2005 - Jan 13 2006, Oct 20 2017 - Dec 11 2017
    ## 12                            Dec 13 2005 - Jan 28 2006
    ## 13 Oct 02 1999 - Nov 20 1999, Dec 13 2005 - Jan 27 2006
    ## 14 Jan 03 2004 - Mar 12 2004, Dec 17 2019 - Feb 15 2020
    ## 15                            Dec 23 2016 - Mar 04 2017
    ## 16                            Oct 11 1970 - Dec 20 1970
    ## 17                            Nov 05 2011 - Feb 23 2012
    ##    homeWinStreak
    ## 1             11
    ## 2             14
    ## 3             14
    ## 4             20
    ## 5             13
    ## 6             20
    ## 7             12
    ## 8             13
    ## 9              9
    ## 10            13
    ## 11             9
    ## 12            12
    ## 13             8
    ## 14            11
    ## 15            15
    ## 16            13
    ## 17            23
    ##                                      homeWinStreakDates
    ## 1                             Feb 09 2009 - Mar 20 2009
    ## 2                             Jan 02 1982 - Feb 25 1982
    ## 3                             Dec 19 1939 - Feb 25 1940
    ## 4                             Jan 04 1976 - Apr 03 1976
    ## 5                             Nov 15 2013 - Jan 15 2014
    ## 6                             Dec 03 1929 - Mar 18 1930
    ## 7  Nov 12 1972 - Jan 07 1973, Oct 13 1989 - Dec 10 1989
    ## 8  Nov 02 1943 - Jan 08 1944, Jan 30 1977 - Mar 26 1977
    ## 9                             Mar 05 2009 - Apr 07 2009
    ## 10                            Jan 31 2018 - Mar 24 2018
    ## 11                            Mar 02 2018 - Apr 07 2018
    ## 12                            Feb 20 2009 - Apr 07 2009
    ## 13                            Feb 22 2018 - Mar 10 2018
    ## 14                            Dec 23 2019 - Feb 15 2020
    ## 15                            Jan 01 2017 - Mar 04 2017
    ## 16                            Nov 11 1970 - Dec 20 1970
    ## 17                            Nov 05 2011 - Feb 19 2012
    ##    homeWinlessStreak
    ## 1                 14
    ## 2                  9
    ## 3                 10
    ## 4                 13
    ## 5                 16
    ## 6                 11
    ## 7                 12
    ## 8                 15
    ## 9                 17
    ## 10                11
    ## 11                17
    ## 12                13
    ## 13                13
    ## 14                11
    ## 15                14
    ## 16                12
    ## 17                10
    ##                                  homeWinlessStreakDates
    ## 1  Feb 12 1976 - Mar 30 1976, Feb 04 1979 - Mar 31 1979
    ## 2                             Mar 02 1999 - Apr 06 1999
    ## 3                             Jan 30 1944 - Mar 19 1944
    ## 4                             Nov 29 2006 - Feb 08 2007
    ## 5                             Dec 31 2003 - Mar 04 2004
    ## 6                             Dec 08 1924 - Feb 17 1925
    ## 7                             Jan 27 1991 - Mar 10 1991
    ## 8                             Dec 16 1939 - Mar 07 1940
    ## 9                             Oct 28 1995 - Jan 27 1996
    ## 10 Dec 19 1987 - Jan 25 1988, Feb 11 2012 - Mar 29 2012
    ## 11                            Jan 19 2000 - Mar 29 2000
    ## 12                            Jan 15 1985 - Mar 10 1985
    ## 13                            Feb 05 2003 - Mar 24 2003
    ## 14                            Jan 02 1998 - Feb 26 1998
    ## 15                            Dec 03 1975 - Jan 21 1976
    ## 16                            Dec 03 1950 - Jan 28 1951
    ## 17                            Dec 11 1985 - Jan 18 1986
    ##    lossStreak
    ## 1          14
    ## 2          12
    ## 3          11
    ## 4           9
    ## 5          13
    ## 6          11
    ## 7          14
    ## 8          12
    ## 9          14
    ## 10         10
    ## 11         10
    ## 12          9
    ## 13         13
    ## 14         13
    ## 15         17
    ## 16         12
    ## 17         14
    ##                                         lossStreakDates
    ## 1                             Dec 30 1975 - Jan 29 1976
    ## 2  Dec 27 1972 - Jan 16 1973, Nov 22 1988 - Dec 15 1988
    ## 3                             Oct 30 1943 - Nov 27 1943
    ## 4                             Dec 08 2006 - Dec 27 2006
    ## 5                             Jan 13 2004 - Feb 12 2004
    ## 6                             Dec 03 1924 - Jan 05 1925
    ## 7                             Dec 29 2014 - Jan 30 2015
    ## 8                             Feb 13 1926 - Mar 13 1926
    ## 9                             Mar 02 1993 - Apr 07 1993
    ## 10                            Jan 15 1967 - Feb 08 1967
    ## 11                            Feb 25 2000 - Mar 16 2000
    ## 12                            Feb 19 1983 - Mar 08 1983
    ## 13                            Feb 07 1998 - Mar 23 1998
    ## 14                            Jan 03 1998 - Feb 02 1998
    ## 15                            Feb 18 1975 - Mar 26 1975
    ## 16                            Feb 25 1951 - Mar 25 1951
    ## 17                            Feb 24 1982 - Mar 25 1982
    ##    mostGameGoals
    ## 1              9
    ## 2             11
    ## 3             12
    ## 4             13
    ## 5             12
    ## 6             14
    ## 7             14
    ## 8             16
    ## 9             11
    ## 10            14
    ## 11             9
    ## 12            11
    ## 13            10
    ## 14             9
    ## 15            12
    ## 16            12
    ## 17            15
    ##                                                                                                                                                                                                                                                     mostGameGoalsDates
    ## 1  Apr 01 1979 - STL 5 @ CLR 9, Feb 12 1982 - QUE 2 @ CLR 9, Apr 06 1986 - NYI 7 @ NJD 9, Mar 10 1990 - QUE 3 @ NJD 9, Dec 05 1990 - VAN 4 @ NJD 9, Oct 26 1991 - SJS 0 @ NJD 9, Mar 23 1993 - TBL 3 @ NJD 9, Mar 10 2000 - NJD 9 @ ATL 0, Oct 28 2000 - NJD 9 @ PIT 0
    ## 2                                                                                                                                                                                                           Dec 20 1983 - PIT 3 @ NYI 11, Mar 03 1984 - NYI 11 @ TOR 6
    ## 3                                                                                                                                                                                                                                         Nov 21 1971 - CGS 1 @ NYR 12
    ## 4                                                                                                                                                                                                           Mar 22 1984 - PIT 4 @ PHI 13, Oct 18 1984 - VAN 2 @ PHI 13
    ## 5                                                                                                                                                                                                           Mar 15 1975 - WSH 1 @ PIT 12, Dec 26 1991 - TOR 1 @ PIT 12
    ## 6                                                                                                                                                                                                                                         Jan 21 1945 - NYR 3 @ BOS 14
    ## 7                                                                                                                                                                                                           Dec 21 1975 - WSH 2 @ BUF 14, Mar 19 1981 - TOR 4 @ BUF 14
    ## 8                                                                                                                                                                                                                                         Mar 03 1920 - MTL 16 @ QBD 3
    ## 9                                                                                                                                                                                                                                         Nov 13 2001 - OTT 11 @ WSH 5
    ## 10                                                                                                                                                                                                                                        Mar 16 1957 - NYR 1 @ TOR 14
    ## 11                                                                                                                                                                               Nov 12 2005 - ATL 9 @ CAR 0, Oct 27 2011 - WPG 9 @ PHI 8, Feb 02 2019 - ANA 3 @ WPG 9
    ## 12                                                                                                                                              Feb 12 1984 - EDM 0 @ HFD 11, Oct 19 1985 - MTL 6 @ HFD 11, Jan 17 1986 - QUE 6 @ HFD 11, Mar 15 1986 - CHI 4 @ HFD 11
    ## 13                                                                                                                                                                                                                                        Nov 26 1997 - BOS 5 @ FLA 10
    ## 14                                                                                                                                                                               Nov 08 2003 - PIT 0 @ TBL 9, Nov 14 2019 - NYR 3 @ TBL 9, Jan 07 2020 - VAN 2 @ TBL 9
    ## 15                                                                                                                                                                                                          Feb 06 1990 - QUE 2 @ WSH 12, Jan 11 2003 - FLA 2 @ WSH 12
    ## 16                                                                                                                                                                                                                                        Jan 30 1969 - CHI 12 @ PHI 0
    ## 17                                                                                                                                                                                                                                        Jan 23 1944 - NYR 0 @ DET 15
    ##    mostGoals mostGoalsAgainst mostGoalsAgainstSeasons
    ## 1        308              374            1985-86 (80)
    ## 2        385              347            1972-73 (78)
    ## 3        321              345            1984-85 (80)
    ## 4        350              319            1992-93 (84)
    ## 5        367              394            1982-83 (80)
    ## 6        399              306            1961-62 (70)
    ## 7        354              308            1986-87 (80)
    ## 8        387              295            1983-84 (80)
    ## 9        314              397            1993-94 (84)
    ## 10       337              387            1983-84 (80)
    ## 11       281              313            1999-00 (82)
    ## 12       332              403            1982-83 (80)
    ## 13       267              280            2018-19 (82)
    ## 14       325              332            1992-93 (84)
    ## 15       330              446            1974-75 (80)
    ## 16       351              363            1981-82 (80)
    ## 17       369              415            1985-86 (80)
    ##    mostGoalsSeasons mostLosses
    ## 1      1992-93 (84)         56
    ## 2      1981-82 (80)         60
    ## 3      1991-92 (80)         44
    ## 4      1983-84 (80)         48
    ## 5      1992-93 (84)         58
    ## 6      1970-71 (78)         47
    ## 7      1974-75 (80)         51
    ## 8      1976-77 (80)         40
    ## 9      2005-06 (82)         70
    ## 10     1989-90 (80)         52
    ## 11     2005-06 (82)         57
    ## 12     1985-86 (80)         54
    ## 13     2018-19 (82)         45
    ## 14     2018-19 (82)         55
    ## 15     1991-92 (80)         67
    ## 16     1985-86 (80)         51
    ## 17     1992-93 (84)         57
    ##                           mostLossesSeasons
    ## 1                1975-76 (80), 1983-84 (80)
    ## 2                              1972-73 (78)
    ## 3                              1984-85 (80)
    ## 4                              2006-07 (82)
    ## 5                              1983-84 (80)
    ## 6                1961-62 (70), 1996-97 (82)
    ## 7                2013-14 (82), 2014-15 (82)
    ## 8  1983-84 (80), 2000-01 (82), 2017-18 (82)
    ## 9                              1992-93 (84)
    ## 10                             1984-85 (80)
    ## 11                             1999-00 (82)
    ## 12                             1982-83 (80)
    ## 13                             2013-14 (82)
    ## 14                             1997-98 (82)
    ## 15                             1974-75 (80)
    ## 16                             1953-54 (70)
    ## 17                             1985-86 (80)
    ##    mostPenaltyMinutes mostPenaltyMinutesSeasons mostPoints
    ## 1                2494              1988-89 (80)        111
    ## 2                1857              1986-87 (80)        118
    ## 3                2021              1989-90 (80)        113
    ## 4                2621              1980-81 (80)        118
    ## 5                2674              1988-89 (80)        119
    ## 6                2443              1987-88 (80)        121
    ## 7                2713              1991-92 (80)        113
    ## 8                1847              1995-96 (82)        132
    ## 9                1716              1992-93 (84)        113
    ## 10               2419              1989-90 (80)        105
    ## 11               1505              2003-04 (82)        114
    ## 12               2354              1992-93 (84)        112
    ## 13               1994              2001-02 (82)        103
    ## 14               1823              1997-98 (82)        128
    ## 15               2204              1989-90 (80)        121
    ## 16               2663              1991-92 (80)        112
    ## 17               2391              1987-88 (80)        131
    ##             mostPointsSeasons mostShutouts
    ## 1                2000-01 (82)           14
    ## 2                1981-82 (80)           11
    ## 3                2014-15 (82)           13
    ## 4                1975-76 (80)           13
    ## 5                1992-93 (84)           10
    ## 6                1970-71 (78)           15
    ## 7  1974-75 (80), 2006-07 (82)           13
    ## 8                1976-77 (80)           22
    ## 9  2002-03 (82), 2005-06 (82)           10
    ## 10               2017-18 (82)           13
    ## 11               2017-18 (82)            7
    ## 12               2005-06 (82)            8
    ## 13               2015-16 (82)            9
    ## 14               2018-19 (82)            9
    ## 15               2009-10 (82)           12
    ## 16               2009-10 (82)           15
    ## 17               1995-96 (82)           13
    ##           mostShutoutsSeasons mostTies
    ## 1                2003-04 (82)       21
    ## 2                2018-19 (82)       22
    ## 3                1928-29 (44)       21
    ## 4                1974-75 (80)       24
    ## 5                2014-15 (82)       20
    ## 6                1927-28 (44)       21
    ## 7  1997-98 (82), 2000-01 (82)       21
    ## 8                1928-29 (44)       23
    ## 9  2001-02 (82), 2016-17 (82)       15
    ## 10               1953-54 (70)       22
    ## 11 2014-15 (82), 2017-18 (82)       12
    ## 12               1998-99 (82)       19
    ## 13               2008-09 (82)       19
    ## 14               2001-02 (82)       16
    ## 15               2016-17 (82)       18
    ## 16               1969-70 (76)       23
    ## 17               1953-54 (70)       18
    ##                             mostTiesSeasons mostWins
    ## 1                              1977-78 (80)       51
    ## 2                              1974-75 (80)       54
    ## 3                              1950-51 (70)       53
    ## 4                              1969-70 (76)       53
    ## 5                              1970-71 (78)       56
    ## 6                              1954-55 (70)       57
    ## 7                              1980-81 (80)       53
    ## 8                              1962-63 (70)       60
    ## 9  1996-97 (82), 1997-98 (82), 1998-99 (82)       52
    ## 10                             1954-55 (70)       49
    ## 11                             2000-01 (82)       52
    ## 12                             1979-80 (80)       52
    ## 13                             1996-97 (82)       47
    ## 14                             2002-03 (82)       62
    ## 15                             1980-81 (80)       56
    ## 16                             1973-74 (78)       52
    ## 17 1952-53 (70), 1980-81 (80), 1996-97 (82)       62
    ##               mostWinsSeasons pointStreak
    ## 1                2008-09 (82)          16
    ## 2                1981-82 (80)          17
    ## 3                2014-15 (82)          19
    ## 4  1984-85 (80), 1985-86 (80)          35
    ## 5                1992-93 (84)          18
    ## 6                1970-71 (78)          23
    ## 7                2006-07 (82)          14
    ## 8                1976-77 (80)          28
    ## 9  2002-03 (82), 2005-06 (82)          11
    ## 10               2017-18 (82)          16
    ## 11               2017-18 (82)           9
    ## 12               2005-06 (82)          13
    ## 13               2015-16 (82)          13
    ## 14               2018-19 (82)          18
    ## 15               2015-16 (82)          15
    ## 16               2009-10 (82)          24
    ## 17               1995-96 (82)          20
    ##                                                                                                                         pointStreakDates
    ## 1                                                                                                              Dec 26 1999 - Jan 28 2000
    ## 2                                                                                                              Oct 12 2019 - Nov 23 2019
    ## 3                                                                                                              Nov 23 1939 - Jan 13 1940
    ## 4                                                                                                              Oct 14 1979 - Jan 06 1980
    ## 5                                                                                                              Mar 09 1993 - Apr 14 1993
    ## 6                                                                                                              Dec 22 1940 - Feb 23 1941
    ## 7                                                                                                              Mar 06 1980 - Apr 06 1980
    ## 8                                                                                                              Dec 18 1977 - Feb 23 1978
    ## 9  Dec 28 1998 - Jan 16 1999, Oct 25 2001 - Nov 22 2001, Dec 18 2003 - Jan 08 2004, Mar 04 2007 - Mar 24 2007, Jan 14 2010 - Feb 04 2010
    ## 10                                                                                                             Nov 22 2003 - Dec 26 2003
    ## 11                                                                                                             Dec 09 2005 - Dec 28 2005
    ## 12                                                                                  Mar 09 2017 - Mar 30 2017, Apr 15 2021 - May 06 2021
    ## 13                                                                                                             Dec 15 2015 - Jan 11 2016
    ## 14                                                                                                             Feb 05 2004 - Mar 12 2004
    ## 15                                                                                                             Jan 13 2010 - Feb 10 2010
    ## 16                                                                                                             Jan 19 2013 - Mar 06 2013
    ## 17                                                                                                             Mar 09 2006 - Apr 17 2006
    ##    roadLossStreak
    ## 1              12
    ## 2              15
    ## 3              10
    ## 4               8
    ## 5              18
    ## 6              14
    ## 7              12
    ## 8              10
    ## 9              38
    ## 10             11
    ## 11              8
    ## 12             13
    ## 13              7
    ## 14             11
    ## 15             37
    ## 16             16
    ## 17             14
    ##                                     roadLossStreakDates
    ## 1                             Oct 19 1983 - Dec 01 1983
    ## 2                             Jan 20 1973 - Mar 31 1973
    ## 3  Oct 30 1943 - Dec 23 1943, Feb 08 1961 - Mar 15 1961
    ## 4  Oct 25 1972 - Nov 26 1972, Mar 03 1988 - Mar 29 1988
    ## 5                             Dec 23 1982 - Mar 04 1983
    ## 6                             Dec 27 1964 - Feb 21 1965
    ## 7                             Dec 17 2011 - Jan 21 2012
    ## 8                             Jan 16 1926 - Mar 13 1926
    ## 9                             Oct 10 1992 - Apr 03 1993
    ## 10                            Feb 20 1988 - Apr 01 1988
    ## 11 Oct 17 1999 - Nov 27 1999, Nov 01 2001 - Nov 18 2001
    ## 12                            Dec 18 1982 - Feb 05 1983
    ## 13                            Feb 07 1998 - Mar 21 1998
    ## 14                            Oct 24 1997 - Dec 10 1997
    ## 15                            Oct 09 1974 - Mar 26 1975
    ## 16 Jan 02 1954 - Mar 21 1954, Nov 22 2003 - Jan 29 2004
    ## 17                            Oct 19 1966 - Dec 21 1966
    ##    roadPointStreak
    ## 1               10
    ## 2                8
    ## 3               11
    ## 4               16
    ## 5                8
    ## 6               16
    ## 7               10
    ## 8               23
    ## 9               14
    ## 10              11
    ## 11               8
    ## 12              12
    ## 13              14
    ## 14              10
    ## 15              10
    ## 16              12
    ## 17              15
    ##                                                                                          roadPointStreakDates
    ## 1                                                        Feb 27 2001 - Apr 07 2001, Jan 30 2007 - Mar 15 2007
    ## 2  Feb 12 1976 - Mar 14 1976, Nov 12 1978 - Dec 09 1978, Feb 27 1981 - Mar 29 1981, Oct 07 1981 - Nov 11 1981
    ## 3                                                                                   Nov 05 1939 - Jan 13 1940
    ## 4                                                                                   Oct 20 1979 - Jan 06 1980
    ## 5                                                        Jan 13 2007 - Feb 16 2007, Mar 11 2016 - Apr 07 2016
    ## 6                                                                                   Jan 11 2014 - Mar 30 2014
    ## 7                                                        Dec 10 1983 - Jan 23 1984, Oct 04 2006 - Nov 13 2006
    ## 8                                                                                   Nov 27 1974 - Mar 12 1975
    ## 9                                                                                   Feb 10 2015 - Apr 11 2015
    ## 10                                                                                  Dec 03 2016 - Jan 25 2017
    ## 11                                                       Nov 16 2014 - Dec 27 2014, Jan 20 2018 - Mar 08 2018
    ## 12                                                                                  Feb 23 2004 - Mar 27 2004
    ## 13                                                                                  Oct 30 2002 - Jan 02 2003
    ## 14                                                       Feb 05 2004 - Mar 10 2004, Jan 05 2008 - Feb 20 2008
    ## 15                                                                                  Nov 24 1982 - Jan 08 1983
    ## 16                                                       Nov 02 1967 - Dec 16 1967, Jan 19 2013 - Mar 03 2013
    ## 17                                                                                  Oct 18 1951 - Dec 20 1951
    ##    roadWinStreak
    ## 1             10
    ## 2              8
    ## 3              9
    ## 4              8
    ## 5              8
    ## 6              9
    ## 7             10
    ## 8              8
    ## 9              6
    ## 10             7
    ## 11             5
    ## 12             6
    ## 13             6
    ## 14             7
    ## 15             6
    ## 16             8
    ## 17            12
    ##                                                                 roadWinStreakDates
    ## 1                                                        Feb 27 2001 - Apr 07 2001
    ## 2                                                        Feb 27 1981 - Mar 29 1981
    ## 3                                                        Jan 16 2020 - Feb 27 2020
    ## 4                                                        Dec 22 1982 - Jan 16 1983
    ## 5                                                        Mar 11 2016 - Apr 07 2016
    ## 6                                                        Mar 02 2014 - Mar 30 2014
    ## 7                             Dec 10 1983 - Jan 23 1984, Oct 04 2006 - Nov 13 2006
    ## 8                             Dec 18 1977 - Jan 18 1978, Jan 21 1982 - Feb 21 1982
    ## 9                             Mar 18 2003 - Apr 05 2003, Jan 14 2010 - Feb 03 2010
    ## 10 Nov 14 1940 - Dec 15 1940, Dec 04 1960 - Jan 05 1961, Jan 29 2003 - Feb 22 2003
    ## 11 Feb 23 2018 - Mar 08 2018, Nov 24 2018 - Dec 14 2018, Nov 01 2019 - Nov 19 2019
    ## 12                            Nov 10 1990 - Dec 07 1990, May 09 2002 - Jun 04 2002
    ## 13                                                       Dec 15 2015 - Jan 10 2016
    ## 14                                                       Jan 07 2007 - Feb 01 2007
    ## 15                            Feb 26 1984 - Apr 01 1984, Feb 20 2011 - Mar 15 2011
    ## 16                                                       Feb 02 2017 - Mar 04 2017
    ## 17                                                       Mar 01 2006 - Apr 15 2006
    ##    roadWinlessStreak
    ## 1                 32
    ## 2                 20
    ## 3                 16
    ## 4                 19
    ## 5                 18
    ## 6                 14
    ## 7                 23
    ## 8                 12
    ## 9                 38
    ## 10                18
    ## 11                10
    ## 12                15
    ## 13                16
    ## 14                17
    ## 15                37
    ## 16                22
    ## 17                26
    ##                                                             roadWinlessStreakDates
    ## 1                                                        Nov 12 1977 - Mar 15 1978
    ## 2                                                        Nov 03 1972 - Jan 13 1973
    ## 3                                                        Oct 09 1952 - Dec 20 1952
    ## 4                                                        Oct 23 1971 - Jan 27 1972
    ## 5                             Oct 25 1970 - Jan 14 1971, Dec 23 1982 - Mar 04 1983
    ## 6  Oct 12 1963 - Dec 14 1963, Dec 27 1964 - Feb 21 1965, Nov 09 1966 - Jan 07 1967
    ## 7                                                        Oct 30 1971 - Feb 19 1972
    ## 8                             Nov 26 1933 - Jan 28 1934, Oct 20 1951 - Dec 13 1951
    ## 9                                                        Oct 10 1992 - Apr 03 1993
    ## 10                                                       Oct 06 1982 - Jan 05 1983
    ## 11                            Oct 06 2001 - Nov 18 2001, Feb 16 2008 - Mar 18 2008
    ## 12                            Nov 11 1979 - Jan 09 1980, Jan 07 2003 - Mar 02 2003
    ## 13                                                       Jan 02 1998 - Mar 21 1998
    ## 14                                                       Dec 02 1999 - Feb 19 2000
    ## 15                                                       Oct 09 1974 - Mar 26 1975
    ## 16                                                       Dec 19 1950 - Mar 25 1951
    ## 17                                                       Dec 15 1976 - Apr 03 1977
    ##    winStreak
    ## 1         13
    ## 2         15
    ## 3         10
    ## 4         13
    ## 5         17
    ## 6         14
    ## 7         10
    ## 8         12
    ## 9         11
    ## 10        10
    ## 11         7
    ## 12         9
    ## 13        12
    ## 14        11
    ## 15        14
    ## 16        12
    ## 17         9
    ##                                                                                                                                                                                                            winStreakDates
    ## 1                                                                                                                                                                                               Feb 26 2001 - Mar 23 2001
    ## 2                                                                                                                                                                                               Jan 21 1982 - Feb 20 1982
    ## 3                                                                                                                                                                    Dec 19 1939 - Jan 13 1940, Jan 19 1973 - Feb 10 1973
    ## 4                                                                                                                                                                                               Oct 19 1985 - Nov 17 1985
    ## 5                                                                                                                                                                                               Mar 09 1993 - Apr 10 1993
    ## 6                                                                                                                                                                                               Dec 03 1929 - Jan 09 1930
    ## 7                                                                                                                                         Jan 04 1984 - Jan 23 1984, Oct 04 2006 - Oct 26 2006, Nov 08 2018 - Nov 27 2018
    ## 8                                                                                                                                                                                               Jan 06 1968 - Feb 03 1968
    ## 9                                                                                                                                                                                               Jan 14 2010 - Feb 04 2010
    ## 10                                                                                                                                                                                              Oct 07 1993 - Oct 28 1993
    ## 11                                                                                                                                                                                              Mar 26 2017 - Apr 08 2017
    ## 12                                                                                                                                        Oct 22 2005 - Nov 11 2005, Dec 31 2005 - Jan 19 2006, Mar 18 2009 - Apr 07 2009
    ## 13                                                                                                                                                                                              Dec 15 2015 - Jan 10 2016
    ## 14                                                                                                                                                                                              Jan 29 2020 - Feb 17 2020
    ## 15                                                                                                                                                                                              Jan 13 2010 - Feb 07 2010
    ## 16                                                                                                                                                                                              Dec 29 2015 - Jan 19 2016
    ## 17 Mar 03 1951 - Mar 21 1951, Feb 27 1955 - Mar 20 1955, Dec 12 1995 - Dec 31 1995, Mar 03 1996 - Mar 22 1996, Oct 13 2005 - Nov 01 2005, Oct 25 2006 - Nov 14 2006, Oct 18 2007 - Nov 09 2007, Apr 18 2008 - May 12 2008
    ##    winlessStreak
    ## 1             27
    ## 2             15
    ## 3             21
    ## 4             12
    ## 5             18
    ## 6             20
    ## 7             18
    ## 8             12
    ## 9             21
    ## 10            15
    ## 11            16
    ## 12            14
    ## 13            15
    ## 14            16
    ## 15            25
    ## 16            21
    ## 17            19
    ##                                      winlessStreakDates
    ## 1                             Feb 12 1976 - Apr 04 1976
    ## 2                             Nov 22 1972 - Dec 21 1972
    ## 3                             Jan 23 1944 - Mar 19 1944
    ## 4                             Feb 24 1999 - Mar 16 1999
    ## 5  Jan 02 1983 - Feb 10 1983, Jan 13 2004 - Feb 22 2004
    ## 6                             Jan 28 1962 - Mar 11 1962
    ## 7                             Feb 25 2021 - Mar 29 2021
    ## 8  Feb 13 1926 - Mar 13 1926, Nov 28 1935 - Dec 29 1935
    ## 9                             Oct 10 1992 - Nov 23 1992
    ## 10                            Dec 26 1987 - Jan 25 1988
    ## 11                            Jan 16 2000 - Feb 20 2000
    ## 12 Jan 04 1992 - Feb 09 1992, Oct 10 2009 - Nov 13 2009
    ## 13                            Feb 01 1998 - Mar 23 1998
    ## 14 Oct 10 1997 - Nov 17 1997, Jan 02 1998 - Feb 05 1998
    ## 15                            Nov 29 1975 - Jan 21 1976
    ## 16                            Dec 17 1950 - Jan 28 1951
    ## 17                            Feb 26 1977 - Apr 03 1977
    ##  [ reached 'max' / getOption("max.print") -- omitted 22 rows ]

``` r
getSeasonRecords("Carolina")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##   id fewestGoals fewestGoalsAgainst
    ## 1 12         171                202
    ##   fewestGoalsAgainstSeasons fewestGoalsSeasons fewestLosses
    ## 1              1998-99 (82)       2002-03 (82)           22
    ##   fewestLossesSeasons fewestPoints fewestPointsSeasons
    ## 1        2005-06 (82)           45        1982-83 (80)
    ##   fewestTies fewestTiesSeasons fewestWins fewestWinsSeasons
    ## 1          4      1985-86 (80)         19      1982-83 (80)
    ##   franchiseId       franchiseName homeLossStreak
    ## 1          26 Carolina Hurricanes              8
    ##         homeLossStreakDates homePointStreak
    ## 1 Mar 14 2013 - Apr 09 2013              15
    ##        homePointStreakDates homeWinStreak
    ## 1 Dec 13 2005 - Jan 28 2006            12
    ##          homeWinStreakDates homeWinlessStreak
    ## 1 Feb 20 2009 - Apr 07 2009                13
    ##      homeWinlessStreakDates lossStreak
    ## 1 Jan 15 1985 - Mar 10 1985          9
    ##             lossStreakDates mostGameGoals
    ## 1 Feb 19 1983 - Mar 08 1983            11
    ##                                                                                                       mostGameGoalsDates
    ## 1 Feb 12 1984 - EDM 0 @ HFD 11, Oct 19 1985 - MTL 6 @ HFD 11, Jan 17 1986 - QUE 6 @ HFD 11, Mar 15 1986 - CHI 4 @ HFD 11
    ##   mostGoals mostGoalsAgainst mostGoalsAgainstSeasons
    ## 1       332              403            1982-83 (80)
    ##   mostGoalsSeasons mostLosses mostLossesSeasons
    ## 1     1985-86 (80)         54      1982-83 (80)
    ##   mostPenaltyMinutes mostPenaltyMinutesSeasons mostPoints
    ## 1               2354              1992-93 (84)        112
    ##   mostPointsSeasons mostShutouts mostShutoutsSeasons
    ## 1      2005-06 (82)            8        1998-99 (82)
    ##   mostTies mostTiesSeasons mostWins mostWinsSeasons
    ## 1       19    1979-80 (80)       52    2005-06 (82)
    ##   pointStreak
    ## 1          13
    ##                                       pointStreakDates
    ## 1 Mar 09 2017 - Mar 30 2017, Apr 15 2021 - May 06 2021
    ##   roadLossStreak       roadLossStreakDates roadPointStreak
    ## 1             13 Dec 18 1982 - Feb 05 1983              12
    ##        roadPointStreakDates roadWinStreak
    ## 1 Feb 23 2004 - Mar 27 2004             6
    ##                                     roadWinStreakDates
    ## 1 Nov 10 1990 - Dec 07 1990, May 09 2002 - Jun 04 2002
    ##   roadWinlessStreak
    ## 1                15
    ##                                 roadWinlessStreakDates
    ## 1 Nov 11 1979 - Jan 09 1980, Jan 07 2003 - Mar 02 2003
    ##   winStreak
    ## 1         9
    ##                                                                    winStreakDates
    ## 1 Oct 22 2005 - Nov 11 2005, Dec 31 2005 - Jan 19 2006, Mar 18 2009 - Apr 07 2009
    ##   winlessStreak
    ## 1            14
    ##                                     winlessStreakDates
    ## 1 Jan 04 1992 - Feb 09 1992, Oct 10 2009 - Nov 13 2009

``` r
getGoalieRecords()
```

    ## No encoding supplied: defaulting to UTF-8.

    ##     id activePlayer firstName franchiseId
    ## 1  235        FALSE       Don          15
    ## 2  236        FALSE       Bob          28
    ## 3  237        FALSE      Tony          11
    ## 4  238        FALSE     Grant          25
    ## 5  239        FALSE       Ron          16
    ## 6  240        FALSE    Curtis          18
    ## 7  241        FALSE      Olie          24
    ## 8  242        FALSE      Mike          18
    ## 9  243        FALSE      Kirk          20
    ## 10 244        FALSE    Gilles          13
    ## 11 245        FALSE      Greg          18
    ## 12 246        FALSE      Turk           5
    ## 13 247        FALSE     Gerry           6
    ## 14 248        FALSE      Alec           3
    ## 15 249        FALSE      Jake           4
    ## 16 250        FALSE      Bert           2
    ## 17 251        FALSE    Howard           4
    ## 18 252        FALSE    Cesare          15
    ## 19 253        FALSE       Joe           9
    ## 20 254        FALSE     Terry          12
    ## 21 255        FALSE      Flat           7
    ## 22 256        FALSE       Roy           8
    ## 23 257        FALSE       Roy           9
    ## 24 258        FALSE     Daren          31
    ## 25 259        FALSE      Bill          25
    ## 26 260        FALSE      Mike          10
    ## 27 261        FALSE   Patrick           1
    ## 28 262        FALSE   Patrick          27
    ## 29 263        FALSE     Billy          22
    ## 30 264        FALSE        Ed          18
    ## 31 265        FALSE      Mike          21
    ## 32 266        FALSE    Martin          23
    ## 33 267        FALSE     Chris          12
    ## 34 268        FALSE     Marty          15
    ##           franchiseName gameTypeId gamesPlayed   lastName
    ## 1          Dallas Stars          2         315    Beaupre
    ## 2       Arizona Coyotes          2         281    Essensa
    ## 3    Chicago Blackhawks          2         873   Esposito
    ## 4       Edmonton Oilers          2         423       Fuhr
    ## 5   Philadelphia Flyers          2         489    Hextall
    ## 6       St. Louis Blues          2         280     Joseph
    ## 7   Washington Capitals          2         711     Kolzig
    ## 8       St. Louis Blues          2         347       Liut
    ## 9     Vancouver Canucks          2         516     McLean
    ## 10     Cleveland Barons          2         250    Meloche
    ## 11      St. Louis Blues          2         209     Millen
    ## 12  Toronto Maple Leafs          2         629      Broda
    ## 13        Boston Bruins          2         416   Cheevers
    ## 14     St. Louis Eagles          2         294    Connell
    ## 15      Hamilton Tigers          2          78     Forbes
    ## 16   Montreal Wanderers          2           4    Lindsay
    ## 17      Hamilton Tigers          2           1   Lockhart
    ## 18         Dallas Stars          2         420    Maniago
    ## 19 Philadelphia Quakers          2          87     Miller
    ## 20    Detroit Red Wings          2         734    Sawchuk
    ## 21     Montreal Maroons          2         107      Walsh
    ## 22   Brooklyn Americans          2         360    Worters
    ## 23 Philadelphia Quakers          2         123    Worters
    ## 24  Tampa Bay Lightning          2         206      Puppa
    ## 25      Edmonton Oilers          2         449    Ranford
    ## 26     New York Rangers          2         666    Richter
    ## 27   Montréal Canadiens          2         551        Roy
    ## 28   Colorado Avalanche          2         478        Roy
    ## 29   New York Islanders          2         674      Smith
    ## 30      St. Louis Blues          2         137 Staniowski
    ## 31       Calgary Flames          2         527     Vernon
    ## 32    New Jersey Devils          2        1259    Brodeur
    ## 33    Detroit Red Wings          2         565     Osgood
    ## 34         Dallas Stars          2         509      Turco
    ##    losses
    ## 1     125
    ## 2     114
    ## 3     302
    ## 4     117
    ## 5     172
    ## 6      96
    ## 7     293
    ## 8     133
    ## 9     228
    ## 10    140
    ## 11     87
    ## 12    222
    ## 13    103
    ## 14    106
    ## 15     43
    ## 16      3
    ## 17      1
    ## 18    190
    ## 19     62
    ## 20    245
    ## 21     44
    ## 22    171
    ## 23     59
    ## 24     91
    ## 25    193
    ## 26    258
    ## 27    175
    ## 28    140
    ## 29    230
    ## 30     68
    ## 31    188
    ## 32    394
    ## 33    149
    ## 34    154
    ##                                         mostGoalsAgainstDates
    ## 1                                                  1983-10-07
    ## 2                                      1992-12-11, 1992-10-12
    ## 3                                      1983-10-15, 1980-11-26
    ## 4                                      1984-02-05, 1982-10-12
    ## 5                                                  1987-04-05
    ## 6                                      1992-11-25, 1990-02-20
    ## 7                          2006-01-25, 2005-10-08, 1989-10-21
    ## 8                                                  1982-02-25
    ## 9                                                  1996-10-19
    ## 10                                                 1973-10-21
    ## 11                                                 1986-01-06
    ## 12                                                 1938-01-22
    ## 13 1977-02-12, 1970-01-24, 1966-12-04, 1965-12-15, 1965-12-11
    ## 14                                                 1925-02-11
    ## 15                                                 1922-12-23
    ## 16                                                 1917-12-22
    ## 17                                                 1921-02-26
    ## 18                                                 1974-11-03
    ## 19                                                 1929-11-26
    ## 20                                                 1959-03-07
    ## 21 1933-01-17, 1932-03-19, 1932-02-13, 1932-02-04, 1932-01-14
    ## 22                                                 1932-03-17
    ## 23                                                 1927-03-10
    ## 24                                     1995-10-15, 1993-12-23
    ## 25 1999-12-30, 1995-12-30, 1995-12-01, 1993-01-23, 1992-11-27
    ## 26                                                 1993-03-06
    ## 27                                                 1995-12-02
    ## 28                                                 1999-11-26
    ## 29                         1986-04-06, 1972-11-04, 1972-10-29
    ## 30                                     1980-10-22, 1980-03-09
    ## 31                                     1993-01-26, 1991-02-23
    ## 32 2014-01-26, 2012-01-04, 2011-11-30, 2009-04-01, 2009-03-07
    ## 33                         2009-03-07, 2008-11-11, 1997-02-06
    ## 34             2008-10-25, 2007-11-10, 2006-02-12, 2003-11-12
    ##    mostGoalsAgainstOneGame         mostSavesDates
    ## 1                       10             1987-03-15
    ## 2                        8             1989-12-29
    ## 3                       10             1977-02-26
    ## 4                        9             1986-03-12
    ## 5                        9             1990-12-23
    ## 6                        8             1992-03-02
    ## 7                        8             2000-03-03
    ## 8                        9 1982-03-18, 1980-11-02
    ## 9                        9 1997-04-05, 1987-12-17
    ## 10                      11             1973-01-31
    ## 11                       9             1988-03-22
    ## 12                       9                   <NA>
    ## 13                       8             1969-02-06
    ## 14                      10                   <NA>
    ## 15                       9                   <NA>
    ## 16                      11                   <NA>
    ## 17                      13                   <NA>
    ## 18                      10             1970-11-01
    ## 19                       9                   <NA>
    ## 20                      10             1959-11-14
    ## 21                       6                   <NA>
    ## 22                      10                   <NA>
    ## 23                       7                   <NA>
    ## 24                       7             1995-11-27
    ## 25                       8             1993-03-17
    ## 26                      10             1991-01-31
    ## 27                       9             1991-03-06
    ## 28                       7             1997-12-10
    ## 29                       9             1972-11-22
    ## 30                       9             1980-10-22
    ## 31                       8 1989-01-14, 1986-12-20
    ## 32                       6             2010-01-12
    ## 33                       7 2010-12-27, 2000-10-22
    ## 34                       6             2010-03-08
    ##    mostSavesOneGame  mostShotsAgainstDates
    ## 1                52             1986-03-21
    ## 2                49             1989-12-29
    ## 3                50             1976-12-12
    ## 4                49             1986-03-12
    ## 5                45             1988-10-13
    ## 6                51             1992-03-02
    ## 7                52             2000-03-03
    ## 8                44             1982-03-18
    ## 9                48             1988-11-17
    ## 10               55             1973-01-31
    ## 11               43 1988-11-03, 1988-03-22
    ## 12               NA                   <NA>
    ## 13               45             1969-02-06
    ## 14               NA                   <NA>
    ## 15               NA                   <NA>
    ## 16               NA                   <NA>
    ## 17               NA                   <NA>
    ## 18               54             1970-11-01
    ## 19               NA                   <NA>
    ## 20               50             1957-11-07
    ## 21               NA                   <NA>
    ## 22               NA                   <NA>
    ## 23               NA                   <NA>
    ## 24               45             1995-10-26
    ## 25               56             1993-03-17
    ## 26               59             1991-01-31
    ## 27               49             1995-02-27
    ## 28               51             1997-12-10
    ## 29               55             1972-11-22
    ## 30               45             1980-10-22
    ## 31               41 1987-11-13, 1986-12-20
    ## 32               51             2010-01-12
    ## 33               46 2010-12-27, 2009-01-12
    ## 34               49             2010-03-08
    ##    mostShotsAgainstOneGame mostShutoutsOneSeason
    ## 1                       55                     1
    ## 2                       50                     5
    ## 3                       53                    15
    ## 4                       54                     4
    ## 5                       50                     5
    ## 6                       54                     2
    ## 7                       54                     6
    ## 8                       48                     3
    ## 9                       52                     5
    ## 10                      58                     4
    ## 11                      46                     6
    ## 12                      NA                     9
    ## 13                      48                     4
    ## 14                      NA                    15
    ## 15                      NA                     6
    ## 16                      NA                     0
    ## 17                      NA                     1
    ## 18                      59                     6
    ## 19                      NA                    11
    ## 20                      53                    12
    ## 21                      NA                     2
    ## 22                      NA                    13
    ## 23                      NA                    11
    ## 24                      46                     5
    ## 25                      59                     2
    ## 26                      62                     5
    ## 27                      53                     7
    ## 28                      53                     9
    ## 29                      60                     3
    ## 30                      54                     0
    ## 31                      44                     3
    ## 32                      51                    12
    ## 33                      49                     6
    ## 34                      52                     9
    ##                                         mostShutoutsSeasonIds
    ## 1                                19841985, 19851986, 19861987
    ## 2                                                    19911992
    ## 3                                                    19691970
    ## 4                                                    19871988
    ## 5                                                    19961997
    ## 6                                                    19911992
    ## 7                                                    20012002
    ## 8                                                    19831984
    ## 9                                                    19911992
    ## 10                                                   19711972
    ## 11                                                   19881989
    ## 12                                                   19491950
    ## 13                                         19691970, 19791980
    ## 14                                         19251926, 19271928
    ## 15                                                   19241925
    ## 16                                                   19171918
    ## 17                                                   19201921
    ## 18                                                   19671968
    ## 19                                                   19281929
    ## 20                               19511952, 19531954, 19541955
    ## 21                     19291930, 19301931, 19311932, 19321933
    ## 22                                                   19281929
    ## 23                                                   19271928
    ## 24                                                   19951996
    ## 25                                                   19941995
    ## 26                                                   19931994
    ## 27                                                   19931994
    ## 28                                                   20012002
    ## 29                                         19741975, 19751976
    ## 30 19751976, 19761977, 19771978, 19781979, 19791980, 19801981
    ## 31                                         19931994, 20002001
    ## 32                                                   20062007
    ## 33                               19961997, 19971998, 19992000
    ## 34                                                   20032004
    ##    mostWinsOneSeason  mostWinsSeasonIds overtimeLosses
    ## 1                 25           19851986             NA
    ## 2                 33           19921993             NA
    ## 3                 38           19691970             NA
    ## 4                 40           19871988             NA
    ## 5                 37           19861987             NA
    ## 6                 36           19931994             NA
    ## 7                 41           19992000             23
    ## 8                 33           19801981             NA
    ## 9                 38           19911992             NA
    ## 10                19           19761977             NA
    ## 11                22           19881989             NA
    ## 12                32           19471948             NA
    ## 13                30           19761977             NA
    ## 14                30           19261927             NA
    ## 15                19           19241925             NA
    ## 16                 1           19171918             NA
    ## 17                 6 19201921, 19211922             NA
    ## 18                22           19671968             NA
    ## 19                 9           19281929             NA
    ## 20                44 19501951, 19511952             NA
    ## 21                17           19291930             NA
    ## 22                18           19301931             NA
    ## 23                19           19271928             NA
    ## 24                29           19951996             NA
    ## 25                27 19901991, 19911992             NA
    ## 26                42           19931994             NA
    ## 27                36           19911992             NA
    ## 28                40           20002001             NA
    ## 29                32           19811982             NA
    ## 30                10 19761977, 19801981             NA
    ## 31                39           19871988             NA
    ## 32                48           20062007             49
    ## 33                39           19951996             29
    ## 34                41           20052006             37
    ##    playerId positionCode rookieGamesPlayed rookieShutouts
    ## 1   8445381            G                44              0
    ## 2   8446719            G                36              1
    ## 3   8446720            G                63             15
    ## 4   8446991            G                48              0
    ## 5   8447775            G                66              1
    ## 6   8448382            G                30              0
    ## 7   8448535            G                14              0
    ## 8   8448865            G                NA             NA
    ## 9   8449474            G                41              1
    ## 10  8449550            G                56              4
    ## 11  8449627            G                NA             NA
    ## 12  8449837            G                45              3
    ## 13  8449853            G                22              1
    ## 14  8449856            G                30              7
    ## 15  8449918            G                NA             NA
    ## 16  8450014            G                 4              0
    ## 17  8450017            G                 1              0
    ## 18  8450020            G                NA             NA
    ## 19  8450043            G                NA             NA
    ## 20  8450111            G                70             11
    ## 21  8450144            G                 1              0
    ## 22  8450153            G                NA             NA
    ## 23  8450153            G                35              7
    ## 24  8450627            G                NA             NA
    ## 25  8450651            G                NA             NA
    ## 26  8450833            G                45              0
    ## 27  8451033            G                47              1
    ## 28  8451033            G                NA             NA
    ## 29  8451525            G                37              0
    ## 30  8451655            G                29              0
    ## 31  8452217            G                55              1
    ## 32  8455710            G                47              3
    ## 33  8458568            G                41              2
    ## 34  8460612            G                26              3
    ##    rookieWins seasons shutouts ties wins
    ## 1          18       9        3   45  126
    ## 2          18       7       14   32  116
    ## 3          38      15       74  148  418
    ## 4          28      10        9   54  226
    ## 5          37      11       18   58  240
    ## 6          16       6        5   34  137
    ## 7           2      16       35   63  301
    ## 8          NA       6       10   52  151
    ## 9          11      11       20   62  211
    ## 10         16       7        8   48   58
    ## 11         NA       6        9   33   85
    ## 12         22      14       61  102  304
    ## 13          5      12       26   76  226
    ## 14         17       8       64   47  140
    ## 15         NA       3        7    1   34
    ## 16          1       1        0    0    1
    ## 17          0       3        0    0    0
    ## 18         NA       9       26   70  145
    ## 19         NA       3       11   11   14
    ## 20         44      14       85  132  350
    ## 21          0       7        9   13   46
    ## 22         NA       9       45   70  118
    ## 23         18       3       22   12   52
    ## 24         NA       7       12   26   77
    ## 25         NA      10        8   54  167
    ## 26         21      14       24   73  301
    ## 27         23      12       29   66  289
    ## 28         NA       8       37   65  262
    ## 29          7      17       22  104  304
    ## 30         10       6        0   14   37
    ## 31         30      13       13   57  262
    ## 32         27      21      124  105  688
    ## 33         23      14       39   46  317
    ## 34         13       9       40   26  262
    ##  [ reached 'max' / getOption("max.print") -- omitted 1044 rows ]

``` r
getGoalieRecords("Hurricanes")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##      id activePlayer firstName franchiseId
    ## 1   336        FALSE       Tom          26
    ## 2   363        FALSE   Richard          26
    ## 3   369        FALSE      Sean          26
    ## 4   411        FALSE      Mark          26
    ## 5   425        FALSE      John          26
    ## 6   430        FALSE     Mario          26
    ## 7   470        FALSE       Pat          26
    ## 8   490        FALSE      Mike          26
    ## 9   508        FALSE      Kirk          26
    ## 10  525        FALSE      Greg          26
    ## 11  662        FALSE     Frank          26
    ## 12  676        FALSE      Jeff          26
    ## 13  698        FALSE     Peter          26
    ## 14  704        FALSE        Al          26
    ## 15  713        FALSE        Ed          26
    ## 16  743        FALSE      Mike          26
    ## 17  758        FALSE     Steve          26
    ## 18  310        FALSE    Arturs          26
    ## 19  789        FALSE    Trevor          26
    ## 20  868        FALSE     Tyler          26
    ## 21  872        FALSE     Kevin          26
    ## 22  879        FALSE     Jamie          26
    ## 23  881        FALSE      Eric          26
    ## 24  907        FALSE      John          26
    ## 25  920        FALSE     Brian          26
    ## 26  993        FALSE   Michael          26
    ## 27 1001        FALSE       Dan          26
    ## 28 1034        FALSE    Martin          26
    ## 29 1262         TRUE    Curtis          26
    ## 30  277        FALSE       Cam          26
    ## 31 1083         TRUE     Anton          26
    ## 32 1287         TRUE     James          26
    ## 33 1121        FALSE     Scott          26
    ## 34 1158        FALSE     Eddie          26
    ##          franchiseName gameTypeId gamesPlayed     lastName
    ## 1  Carolina Hurricanes          2          34     Barrasso
    ## 2  Carolina Hurricanes          2           6      Brodeur
    ## 3  Carolina Hurricanes          2         256        Burke
    ## 4  Carolina Hurricanes          2           3  Fitzpatrick
    ## 5  Carolina Hurricanes          2         122      Garrett
    ## 6  Carolina Hurricanes          2          23     Gosselin
    ## 7  Carolina Hurricanes          2           5    Jablonski
    ## 8  Carolina Hurricanes          2         252         Liut
    ## 9  Carolina Hurricanes          2           8       McLean
    ## 10 Carolina Hurricanes          2         219       Millen
    ## 11 Carolina Hurricanes          2          54  Pietrangelo
    ## 12 Carolina Hurricanes          2          37        Reese
    ## 13 Carolina Hurricanes          2         178 Sidorkiewicz
    ## 14 Carolina Hurricanes          2          30        Smith
    ## 15 Carolina Hurricanes          2          19   Staniowski
    ## 16 Carolina Hurricanes          2          69       Veisor
    ## 17 Carolina Hurricanes          2          94        Weeks
    ## 18 Carolina Hurricanes          2         309         Irbe
    ## 19 Carolina Hurricanes          2          72         Kidd
    ## 20 Carolina Hurricanes          2          12         Moss
    ## 21 Carolina Hurricanes          2         119       Weekes
    ## 22 Carolina Hurricanes          2          14        Storr
    ## 23 Carolina Hurricanes          2           9      Fichaud
    ## 24 Carolina Hurricanes          2          45      Grahame
    ## 25 Carolina Hurricanes          2          10      Boucher
    ## 26 Carolina Hurricanes          2          33     Leighton
    ## 27 Carolina Hurricanes          2          19        Ellis
    ## 28 Carolina Hurricanes          2          60       Gerber
    ## 29 Carolina Hurricanes          2          33   McElhinney
    ## 30 Carolina Hurricanes          2         668         Ward
    ## 31 Carolina Hurricanes          2          70     Khudobin
    ## 32 Carolina Hurricanes          2          47       Reimer
    ## 33 Carolina Hurricanes          2          51      Darling
    ## 34 Carolina Hurricanes          2          54         Lack
    ##    losses                          mostGoalsAgainstDates
    ## 1      12             2001-12-30, 2001-12-18, 2001-11-29
    ## 2       2                                     1988-03-09
    ## 3     120                                     1992-12-11
    ## 4       2                                     2000-02-15
    ## 5      57 1982-01-02, 1980-10-11, 1980-03-08, 1980-02-26
    ## 6      13                         1993-04-10, 1993-03-24
    ## 7       4                                     1998-01-03
    ## 8     111                                     1985-10-23
    ## 9       2                         1998-03-06, 1998-01-06
    ## 10    120                         1983-02-23, 1982-12-06
    ## 11     27                                     1992-10-31
    ## 12     17                                     1994-02-02
    ## 13     79             1991-01-29, 1990-12-29, 1989-02-25
    ## 14     10                                     1980-03-26
    ## 15      9                         1984-01-03, 1983-12-27
    ## 16     37                                     1980-11-23
    ## 17     40                                     1984-12-03
    ## 18    122                                     2002-01-12
    ## 19     31                                     1999-02-13
    ## 20      6                                     2000-10-31
    ## 21     54                                     2003-02-07
    ## 22      8                                     2003-11-21
    ## 23      5                                     1999-11-03
    ## 24     20                                     2007-12-01
    ## 25      6             2012-03-31, 2011-10-29, 2011-10-22
    ## 26     14                                     2009-10-31
    ## 27      8                         2013-04-04, 2013-02-23
    ## 28     14 2006-04-08, 2006-04-03, 2005-12-28, 2005-12-26
    ## 29     11                                     2019-03-08
    ## 30    244             2017-01-20, 2007-01-27, 2005-11-12
    ## 31     31                                     2015-04-02
    ## 32     11 2021-02-07, 2021-02-04, 2020-02-08, 2019-12-27
    ## 33     25                                     2017-12-19
    ## 34     21                                     2016-10-22
    ##    mostGoalsAgainstOneGame         mostSavesDates
    ## 1                        5             2001-12-10
    ## 2                        4             1988-04-03
    ## 3                        9             1994-01-01
    ## 4                        5             2000-02-15
    ## 5                        9             1981-02-14
    ## 6                        6             1993-03-22
    ## 7                        6             1997-12-27
    ## 8                        9             1985-12-19
    ## 9                        4             1998-03-06
    ## 10                      11             1982-01-30
    ## 11                       7             1992-03-29
    ## 12                       9 1995-01-22, 1994-04-10
    ## 13                       8             1992-01-16
    ## 14                       7             1980-04-01
    ## 15                       7             1984-02-05
    ## 16                      11             1982-11-02
    ## 17                       9             1987-04-05
    ## 18                       7             1999-01-30
    ## 19                       6             1998-03-02
    ## 20                       5             2000-10-14
    ## 21                       7             2003-02-18
    ## 22                       5             2003-10-25
    ## 23                       6             1999-12-23
    ## 24                       8             2006-11-22
    ## 25                       5             2011-12-03
    ## 26                       6             2008-12-07
    ## 27                       5 2013-03-19, 2013-01-25
    ## 28                       5             2005-10-24
    ## 29                       8             2018-11-27
    ## 30                       7             2008-10-25
    ## 31                       6             2014-03-18
    ## 32                       5             2019-10-08
    ## 33                       8             2018-03-27
    ## 34                       6             2016-03-17
    ##    mostSavesOneGame  mostShotsAgainstDates
    ## 1                40             2001-12-10
    ## 2                31             1988-04-03
    ## 3                51 1996-01-27, 1994-01-01
    ## 4                40             2000-02-15
    ## 5                50             1981-02-14
    ## 6                45             1993-03-22
    ## 7                26             1997-12-27
    ## 8                45             1987-03-10
    ## 9                46             1998-03-06
    ## 10               52             1982-01-30
    ## 11               48             1992-03-29
    ## 12               38             1995-01-22
    ## 13               39             1992-01-16
    ## 14               36             1980-04-01
    ## 15               38             1984-02-05
    ## 16               52             1982-11-02
    ## 17               43             1987-04-05
    ## 18               44             1999-01-30
    ## 19               43             1998-03-02
    ## 20               32             2000-10-14
    ## 21               43             2003-02-18
    ## 22               28             2003-10-25
    ## 23               31             1999-12-23
    ## 24               44             2006-11-22
    ## 25               37             2011-12-03
    ## 26               38             2009-02-05
    ## 27               40             2013-03-19
    ## 28               45             2005-10-24
    ## 29               48             2018-11-27
    ## 30               57             2008-10-25
    ## 31               46             2014-03-18
    ## 32               47             2019-10-08
    ## 33               41             2018-03-27
    ## 34               44             2016-03-17
    ##    mostShotsAgainstOneGame mostShutoutsOneSeason
    ## 1                       43                     2
    ## 2                       34                     0
    ## 3                       54                     4
    ## 4                       45                     0
    ## 5                       57                     0
    ## 6                       50                     0
    ## 7                       27                     0
    ## 8                       48                     4
    ## 9                       50                     0
    ## 10                      54                     2
    ## 11                      50                     0
    ## 12                      40                     1
    ## 13                      43                     4
    ## 14                      41                     2
    ## 15                      41                     0
    ## 16                      59                     1
    ## 17                      49                     2
    ## 18                      45                     6
    ## 19                      44                     3
    ## 20                      34                     0
    ## 21                      47                     6
    ## 22                      32                     0
    ## 23                      35                     1
    ## 24                      47                     0
    ## 25                      40                     0
    ## 26                      40                     0
    ## 27                      43                     1
    ## 28                      47                     3
    ## 29                      49                     2
    ## 30                      60                     6
    ## 31                      47                     1
    ## 32                      50                     3
    ## 33                      45                     0
    ## 34                      48                     2
    ##                     mostShutoutsSeasonIds mostWinsOneSeason
    ## 1                                20012002                13
    ## 2                                19871988                 4
    ## 3                      19951996, 19961997                28
    ## 4                                19992000                 0
    ## 5            19791980, 19801981, 19811982                16
    ## 6                      19921993, 19931994                 5
    ## 7                                19971998                 1
    ## 8                                19861987                31
    ## 9                                19971998                 4
    ## 10                               19831984                21
    ## 11           19911992, 19921993, 19931994                 5
    ## 12                     19931994, 19951996                 5
    ## 13                               19881989                22
    ## 14                               19791980                11
    ## 15                     19831984, 19841985                 6
    ## 16                     19801981, 19821983                 6
    ## 17                               19841985                13
    ## 18                     19981999, 20002001                37
    ## 19                               19971998                21
    ## 20                               20002001                 1
    ## 21                               20032004                23
    ## 22                               20032004                 0
    ## 23                               19992000                 3
    ## 24                     20062007, 20072008                10
    ## 25                               20112012                 1
    ## 26 20072008, 20082009, 20092010, 20162017                 6
    ## 27                               20122013                 6
    ## 28                               20052006                38
    ## 29                               20182019                20
    ## 30                               20082009                39
    ## 31                     20132014, 20142015                19
    ## 32                               20192020                15
    ## 33                     20172018, 20182019                13
    ## 34                               20152016                12
    ##    mostWinsSeasonIds overtimeLosses playerId positionCode
    ## 1           20012002             NA  8445275            G
    ## 2           19871988             NA  8445694            G
    ## 3           19951996             NA  8445769            G
    ## 4           19992000             NA  8446829            G
    ## 5           19791980             NA  8447066            G
    ## 6           19921993             NA  8447303            G
    ## 7           19971998             NA  8448207            G
    ## 8           19861987             NA  8448865            G
    ## 9           19971998             NA  8449474            G
    ## 10          19831984             NA  8449627            G
    ## 11          19931994             NA  8450390            G
    ## 12          19931994             NA  8450743            G
    ## 13          19881989             NA  8451369            G
    ## 14          19791980             NA  8451474            G
    ## 15          19831984             NA  8451655            G
    ## 16          19801981             NA  8452205            G
    ## 17          19851986             NA  8452355            G
    ## 18          20002001             NA  8456692            G
    ## 19          19971998             NA  8456830            G
    ## 20          20002001             NA  8459451            G
    ## 21          20032004             NA  8459463            G
    ## 22          20032004             NA  8460497            G
    ## 23          19992000             NA  8460506            G
    ## 24          20062007              3  8460715            G
    ## 25          20112012              1  8462052            G
    ## 26          20082009              2  8468038            G
    ## 27          20122013              2  8468540            G
    ## 28          20052006              6  8469675            G
    ## 29          20182019              2  8470147            G
    ## 30          20082009             84  8470320            G
    ## 31          20132014              7  8471418            G
    ## 32          20202021              4  8473503            G
    ## 33          20172018              9  8474152            G
    ## 34          20152016              9  8475663            G
    ##    rookieGamesPlayed rookieShutouts rookieWins seasons
    ## 1                 NA             NA         NA       1
    ## 2                 NA             NA         NA       1
    ## 3                 NA             NA         NA       6
    ## 4                 NA             NA         NA       1
    ## 5                 NA             NA         NA       3
    ## 6                 NA             NA         NA       2
    ## 7                 NA             NA         NA       1
    ## 8                 NA             NA         NA       6
    ## 9                 NA             NA         NA       1
    ## 10                NA             NA         NA       4
    ## 11                NA             NA         NA       3
    ## 12                NA             NA         NA       3
    ## 13                44              4         22       5
    ## 14                NA             NA         NA       1
    ## 15                NA             NA         NA       2
    ## 16                NA             NA         NA       4
    ## 17                NA             NA         NA       4
    ## 18                NA             NA         NA       6
    ## 19                NA             NA         NA       2
    ## 20                NA             NA         NA       1
    ## 21                NA             NA         NA       3
    ## 22                NA             NA         NA       1
    ## 23                NA             NA         NA       1
    ## 24                NA             NA         NA       2
    ## 25                NA             NA         NA       1
    ## 26                NA             NA         NA       4
    ## 27                NA             NA         NA       1
    ## 28                NA             NA         NA       1
    ## 29                NA             NA         NA       1
    ## 30                28              0         14      13
    ## 31                NA             NA         NA       2
    ## 32                NA             NA         NA       2
    ## 33                NA             NA         NA       2
    ## 34                NA             NA         NA       2
    ##    shutouts ties wins
    ## 1         2    5   13
    ## 2         0    0    4
    ## 3        10   24  100
    ## 4         0    0    0
    ## 5         0   27   36
    ## 6         0    1    5
    ## 7         0    0    1
    ## 8        13   17  115
    ## 9         0    0    4
    ## 10        4   33   62
    ## 11        0    3   12
    ## 12        2    4    9
    ## 13        8   24   71
    ## 14        2    8   11
    ## 15        0    1    6
    ## 16        2    9   17
    ## 17        4    6   41
    ## 18       20   44  130
    ## 19        5    9   28
    ## 20        0    0    1
    ## 21       11   20   39
    ## 22        0    2    0
    ## 23        1    1    3
    ## 24        0   NA   15
    ## 25        0   NA    1
    ## 26        0   NA   10
    ## 27        1   NA    6
    ## 28        3   NA   38
    ## 29        2    0   20
    ## 30       27    0  318
    ## 31        2   NA   27
    ## 32        3    0   29
    ## 33        0    0   15
    ## 34        3   NA   20
    ##  [ reached 'max' / getOption("max.print") -- omitted 4 rows ]

``` r
getSkaterRecords()
```

    ## No encoding supplied: defaulting to UTF-8.

    ##       id activePlayer assists firstName franchiseId
    ## 1  16888        FALSE     417    George           5
    ## 2  16889        FALSE       0     Billy           2
    ## 3  16890        FALSE     794    Johnny           6
    ## 4  16891        FALSE     712      Jean           1
    ## 5  16892        FALSE    1111       Ray           6
    ## 6  16893        FALSE      33    Harold           9
    ## 7  16894        FALSE      13      Herb           9
    ## 8  16895        FALSE     852     Bobby          16
    ## 9  16896        FALSE     142       Ken          23
    ## 10 16897        FALSE       0     Gerry           2
    ## 11 16898        FALSE     721    Bernie          18
    ## 12 16899        FALSE     112    Normie           8
    ## 13 16900        FALSE     793       Ron          26
    ## 14 16901        FALSE       1     Harry           2
    ## 15 16902        FALSE     615       Rod          10
    ## 16 16903        FALSE       0      Jack           2
    ## 17 16904        FALSE       1      Jack           2
    ## 18 16905        FALSE      34       Hib           9
    ## 19 16906        FALSE    1023    Gordie          12
    ## 20 16907        FALSE     100     Frank           3
    ## 21 16908        FALSE       0    George           2
    ## 22 16909        FALSE      27    Goldie           4
    ## 23 16910        FALSE     361     Calle          24
    ## 24 16911        FALSE     688     Henri           1
    ## 25 16912        FALSE       2      Dave           2
    ## 26 16913        FALSE      33    Mickey           4
    ## 27 16914        FALSE       0       Art           2
    ## 28 16915        FALSE    1033     Mario          17
    ## 29 16916        FALSE     309     Kevin          25
    ## 30 16917        FALSE       0    Raymie           2
    ## 31 16918        FALSE       4    Rodger           9
    ## 32 16919        FALSE       0      Phil           2
    ##           franchiseName gameTypeId gamesPlayed goals
    ## 1   Toronto Maple Leafs          2        1188   296
    ## 2    Montreal Wanderers          2           2     1
    ## 3         Boston Bruins          2        1436   545
    ## 4    Montréal Canadiens          2        1125   507
    ## 5         Boston Bruins          2        1518   395
    ## 6  Philadelphia Quakers          2         216    60
    ## 7  Philadelphia Quakers          2         216    24
    ## 8   Philadelphia Flyers          2        1144   358
    ## 9     New Jersey Devils          2        1283    36
    ## 10   Montreal Wanderers          2           4     0
    ## 11      St. Louis Blues          2         927   352
    ## 12   Brooklyn Americans          2         402   107
    ## 13  Carolina Hurricanes          2        1186   382
    ## 14   Montreal Wanderers          2           4     6
    ## 15     New York Rangers          2        1065   406
    ## 16   Montreal Wanderers          2           1     0
    ## 17   Montreal Wanderers          2           4     3
    ## 18 Philadelphia Quakers          2         253    88
    ## 19    Detroit Red Wings          2        1687   786
    ## 20     St. Louis Eagles          2         326   137
    ## 21   Montreal Wanderers          2           4     0
    ## 22      Hamilton Tigers          2          95    55
    ## 23  Washington Capitals          2         983   113
    ## 24   Montréal Canadiens          2        1258   358
    ## 25   Montreal Wanderers          2           4     5
    ## 26      Hamilton Tigers          2         112    51
    ## 27   Montreal Wanderers          2           3     1
    ## 28  Pittsburgh Penguins          2         915   690
    ## 29      Edmonton Oilers          2        1037    74
    ## 30   Montreal Wanderers          2           1     0
    ## 31 Philadelphia Quakers          2         210    20
    ## 32   Montreal Wanderers          2           4     1
    ##     lastName
    ## 1  Armstrong
    ## 2       Bell
    ## 3      Bucyk
    ## 4   Beliveau
    ## 5    Bourque
    ## 6    Darragh
    ## 7      Drury
    ## 8     Clarke
    ## 9    Daneyko
    ## 10     Geran
    ## 11   Federko
    ## 12     Himes
    ## 13   Francis
    ## 14    Hyland
    ## 15   Gilbert
    ## 16     Marks
    ## 17  McDonald
    ## 18     Milks
    ## 19      Howe
    ## 20   Nighbor
    ## 21   O'Grady
    ## 22  Prodgers
    ## 23 Johansson
    ## 24   Richard
    ## 25   Ritchie
    ## 26     Roach
    ## 27      Ross
    ## 28   Lemieux
    ## 29      Lowe
    ## 30   Skilton
    ## 31     Smith
    ## 32   Stevens
    ##                                                                                                                                          mostAssistsGameDates
    ## 1                                                  1956-01-07, 1957-03-16, 1957-11-24, 1961-01-15, 1961-12-02, 1962-02-25, 1964-02-23, 1965-12-18, 1969-01-31
    ## 2                                                                                                                                      1917-12-19, 1917-12-29
    ## 3                                                                                                                                                  1971-01-01
    ## 4                                                                                                  1955-02-19, 1956-12-01, 1962-11-24, 1965-11-20, 1967-12-28
    ## 5                                                                                                                                      1990-02-18, 1994-01-02
    ## 6                                                                                                  1926-01-19, 1929-11-19, 1929-11-23, 1929-12-10, 1930-01-18
    ## 7  1926-02-06, 1926-03-04, 1926-11-20, 1927-03-08, 1927-12-31, 1928-03-17, 1929-02-14, 1929-02-21, 1929-03-05, 1929-03-17, 1930-12-09, 1931-02-05, 1931-02-14
    ## 8                                                                                                                                                  1976-04-01
    ## 9                                                                                                                                                  1999-02-13
    ## 10                                                                                                             1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 11                                                                                                                                                 1988-02-27
    ## 12                                                                                                                         1932-01-19, 1933-03-23, 1935-02-10
    ## 13                                                                                                                                                 1987-03-05
    ## 14                                                                                                                                                 1917-12-29
    ## 15                                                                                                                         1975-03-02, 1975-03-30, 1976-10-08
    ## 16                                                                                                                                                 1917-12-29
    ## 17                                                                                                                                                 1917-12-29
    ## 18                                                                                                             1927-01-18, 1928-11-20, 1929-12-05, 1931-03-21
    ## 19                                                                                                                                                 1950-12-28
    ## 20                                                                                                                                     1918-02-25, 1920-01-31
    ## 21                                                                                                             1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 22                                                                                                 1921-02-02, 1921-02-12, 1921-02-28, 1922-01-21, 1922-12-16
    ## 23             1990-02-04, 1990-10-19, 1991-03-14, 1991-03-16, 1991-11-13, 1992-03-15, 1993-03-16, 1993-12-17, 1995-04-04, 1999-03-02, 1999-11-27, 2000-10-07
    ## 24                                                                                                                                     1963-01-12, 1964-02-01
    ## 25                                                                                                                                                 1917-12-19
    ## 26                                                                                                                                                 1921-02-23
    ## 27                                                                                                             1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 28                                                                                                                         1988-10-15, 1992-12-05, 1995-11-01
    ## 29                                                                                                                                                 1983-02-19
    ## 30                                                                                                             1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 31                                                                                                             1925-12-09, 1929-01-03, 1929-03-05, 1930-01-02
    ## 32                                                                                                             1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ##    mostAssistsOneGame mostAssistsOneSeason
    ## 1                   3                   35
    ## 2                   0                    0
    ## 3                   5                   65
    ## 4                   4                   58
    ## 5                   5                   73
    ## 6                   2                   17
    ## 7                   1                    4
    ## 8                   5                   89
    ## 9                   3                   16
    ## 10                  0                    0
    ## 11                  5                   73
    ## 12                  3                   25
    ## 13                  6                   69
    ## 14                  1                    1
    ## 15                  5                   61
    ## 16                  0                    0
    ## 17                  1                    1
    ## 18                  2                   11
    ## 19                  5                   59
    ## 20                  4                   16
    ## 21                  0                    0
    ## 22                  2                    9
    ## 23                  3                   42
    ## 24                  5                   52
    ## 25                  2                    2
    ## 26                  4                    9
    ## 27                  0                    0
    ## 28                  6                  114
    ## 29                  5                   42
    ## 30                  0                    0
    ## 31                  1                    2
    ## 32                  0                    0
    ##    mostAssistsSeasonIds
    ## 1              19651966
    ## 2              19171918
    ## 3              19701971
    ## 4              19601961
    ## 5              19901991
    ## 6              19291930
    ## 7              19281929
    ## 8    19741975, 19751976
    ## 9              19901991
    ## 10             19171918
    ## 11   19801981, 19841985
    ## 12             19321933
    ## 13             19891990
    ## 14             19171918
    ## 15             19741975
    ## 16             19171918
    ## 17             19171918
    ## 18             19291930
    ## 19             19681969
    ## 20             19191920
    ## 21             19171918
    ## 22             19201921
    ## 23             19911992
    ## 24             19571958
    ## 25             19171918
    ## 26             19221923
    ## 27             19171918
    ## 28             19881989
    ## 29             19831984
    ## 30             19171918
    ## 31             19281929
    ## 32             19171918
    ##                                                                                                                                                                                                                    mostGoalsGameDates
    ## 1                                                                                                                                                                                                              1959-03-15, 1961-12-16
    ## 2                                                                                                                                                                                                                          1917-12-19
    ## 3                                                                                                                                                                                                              1973-01-18, 1974-01-05
    ## 4                                                                                                                                                                                                  1955-11-05, 1959-03-07, 1969-02-11
    ## 5                                                                                                                                                                                                                          1983-03-08
    ## 6                                                                                                                                                  1927-03-20, 1928-03-12, 1928-03-17, 1929-11-16, 1930-01-18, 1930-02-01, 1930-02-22
    ## 7                                                                                                                                                                                                  1925-12-09, 1928-02-14, 1930-03-18
    ## 8                                                                                                                                                                          1973-02-17, 1974-02-02, 1974-03-28, 1980-12-13, 1982-12-27
    ## 9                                                                                                                                                                                                              1989-11-08, 1990-01-16
    ## 10                                                                                                                                                                                     1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 11                                                                                                 1977-02-05, 1977-02-19, 1977-04-02, 1980-02-09, 1982-02-27, 1982-03-26, 1983-11-29, 1984-02-29, 1984-03-24, 1988-03-13, 1989-02-27
    ## 12                                                                                                                                                                                                 1930-02-18, 1930-03-15, 1931-02-01
    ## 13                                                                                                                                                                                                                         1984-02-12
    ## 14                                                                                                                                                                                                                         1917-12-19
    ## 15                                                                                                                                                                                                 1968-02-24, 1975-02-02, 1976-02-07
    ## 16                                                                                                                                                                                                                         1917-12-29
    ## 17                                                                                                                                                                                                 1917-12-19, 1917-12-22, 1917-12-26
    ## 18                                                                                                                                                                                                                         1926-03-04
    ## 19 1950-02-11, 1950-03-19, 1951-01-17, 1951-01-23, 1951-03-17, 1951-12-31, 1952-03-23, 1953-01-11, 1953-01-29, 1955-03-03, 1956-01-19, 1956-12-25, 1961-12-31, 1965-03-21, 1965-12-12, 1968-03-16, 1969-02-06, 1969-02-16, 1969-11-02
    ## 20                                                                                                                                                                                                                         1918-03-06
    ## 21                                                                                                                                                                                     1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 22                                                                                                                                                                                                                         1921-01-22
    ## 23                                                                                                                                                                                     1990-10-20, 1996-02-10, 1998-01-06, 1998-12-04
    ## 24                                                                                                                                                                         1957-10-17, 1959-03-14, 1961-03-11, 1965-02-24, 1967-03-19
    ## 25                                                                                                                                                                                                             1917-12-19, 1917-12-29
    ## 26                                                                                                                                                                                     1922-02-08, 1922-03-08, 1922-12-27, 1923-02-28
    ## 27                                                                                                                                                                                                                         1917-12-19
    ## 28                                                                                                                                                                                                 1988-12-31, 1993-04-09, 1996-03-26
    ## 29                                                                                                                                                                                                                         1983-01-01
    ## 30                                                                                                                                                                                     1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 31                                                                                                                                                                                                                         1926-01-15
    ## 32                                                                                                                                                                                                                         1917-12-22
    ##    mostGoalsOneGame mostGoalsOneSeason mostGoalsSeasonIds
    ## 1                 3                 23           19591960
    ## 2                 1                  1           19171918
    ## 3                 4                 51           19701971
    ## 4                 4                 47           19551956
    ## 5                 3                 31           19831984
    ## 6                 2                 15           19291930
    ## 7                 2                  6 19251926, 19271928
    ## 8                 3                 37           19721973
    ## 9                 2                  6           19891990
    ## 10                0                  0           19171918
    ## 11                3                 41           19831984
    ## 12                3                 28           19291930
    ## 13                4                 32           19891990
    ## 14                5                  6           19171918
    ## 15                4                 43           19711972
    ## 16                0                  0           19171918
    ## 17                1                  3           19171918
    ## 18                4                 18 19271928, 19301931
    ## 19                3                 49           19521953
    ## 20                4                 26           19191920
    ## 21                0                  0           19171918
    ## 22                3                 18           19201921
    ## 23                2                 15           19971998
    ## 24                3                 30           19591960
    ## 25                2                  5           19171918
    ## 26                3                 17           19221923
    ## 27                1                  1           19171918
    ## 28                5                 85           19881989
    ## 29                2                 10           19801981
    ## 30                0                  0           19171918
    ## 31                3                  9           19251926
    ## 32                1                  1           19171918
    ##    mostPenaltyMinutesOneSeason mostPenaltyMinutesSeasonIds
    ## 1                           97                    19551956
    ## 2                            0                    19171918
    ## 3                           57                    19571958
    ## 4                          143                    19551956
    ## 5                           96                    19801981
    ## 6                            8          19271928, 19291930
    ## 7                           54                    19261927
    ## 8                          154                    19811982
    ## 9                          283                    19881989
    ## 10                           0                    19171918
    ## 11                          70                    19811982
    ## 12                          25                    19281929
    ## 13                          87                    19871988
    ## 14                           6                    19171918
    ## 15                          65                    19701971
    ## 16                           0                    19171918
    ## 17                           3                    19171918
    ## 18                          42                    19301931
    ## 19                         109                    19531954
    ## 20                          48                    19271928
    ## 21                           0                    19171918
    ## 22                          15                    19221923
    ## 23                          59                    19931994
    ## 24                          91                    19601961
    ## 25                           3                    19171918
    ## 26                           9                    19211922
    ## 27                          12                    19171918
    ## 28                         100                    19881989
    ## 29                         140                    19891990
    ## 30                           0                    19171918
    ## 31                          69                    19291930
    ## 32                           3                    19171918
    ##                                                       mostPointsGameDates
    ## 1              1957-03-16, 1962-02-25, 1964-12-12, 1965-03-21, 1967-11-02
    ## 2                                                              1917-12-19
    ## 3                                                  1970-12-10, 1971-02-25
    ## 4                                                              1959-03-07
    ## 5                                                              1990-02-18
    ## 6                                                              1930-01-18
    ## 7                          1925-12-09, 1926-02-06, 1928-02-14, 1930-03-18
    ## 8                                                  1974-02-02, 1976-10-31
    ## 9                                                              1999-02-13
    ## 10                         1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 11             1978-10-25, 1981-03-28, 1984-12-01, 1986-01-15, 1988-02-27
    ## 12                         1930-01-02, 1930-02-04, 1930-02-18, 1933-03-23
    ## 13                                                 1987-03-05, 1989-10-08
    ## 14                                                             1917-12-19
    ## 15 1968-02-24, 1975-03-02, 1975-03-30, 1976-02-18, 1976-10-08, 1976-12-04
    ## 16                                                             1917-12-29
    ## 17                         1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 18                                                             1926-03-04
    ## 19                                                             1956-12-25
    ## 20                                                             1920-01-31
    ## 21                         1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 22                                     1921-01-22, 1922-01-21, 1922-12-16
    ## 23                                                             1991-03-14
    ## 24                                                             1957-10-17
    ## 25                                                             1917-12-19
    ## 26                                                 1921-02-23, 1922-03-08
    ## 27                                                             1917-12-19
    ## 28                                                 1988-10-15, 1988-12-31
    ## 29                                                             1983-02-19
    ## 30                         1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 31                                                             1926-01-15
    ## 32                                                             1917-12-22
    ##    mostPointsOneGame mostPointsOneSeason
    ## 1                  4                  53
    ## 2                  1                   1
    ## 3                  6                 116
    ## 4                  7                  91
    ## 5                  6                  96
    ## 6                  4                  32
    ## 7                  2                   9
    ## 8                  6                 119
    ## 9                  3                  21
    ## 10                 0                   0
    ## 11                 5                 107
    ## 12                 4                  50
    ## 13                 6                 101
    ## 14                 5                   7
    ## 15                 5                  97
    ## 16                 0                   0
    ## 17                 1                   4
    ## 18                 4                  24
    ## 19                 6                 103
    ## 20                 6                  42
    ## 21                 0                   0
    ## 22                 3                  27
    ## 23                 4                  56
    ## 24                 6                  80
    ## 25                 4                   7
    ## 26                 5                  26
    ## 27                 1                   1
    ## 28                 8                 199
    ## 29                 6                  46
    ## 30                 0                   0
    ## 31                 3                  10
    ## 32                 1                   1
    ##    mostPointsSeasonIds penaltyMinutes playerId points
    ## 1             19611962            726  8444971    713
    ## 2             19171918              0  8445044      1
    ## 3             19701971            436  8445240   1339
    ## 4             19581959           1033  8445408   1219
    ## 5             19831984           1087  8445621   1506
    ## 6             19291930             32  8445843     93
    ## 7             19281929            223  8445955     37
    ## 8             19751976           1453  8446098   1210
    ## 9             19891990           2516  8446309    178
    ## 10            19171918              0  8446580      0
    ## 11            19831984            463  8446803   1073
    ## 12            19291930            125  8446877    219
    ## 13            19891990            682  8446951   1175
    ## 14            19171918              6  8447013      7
    ## 15  19711972, 19741975            510  8447172   1021
    ## 16            19171918              0  8447616      0
    ## 17            19171918              3  8447761      4
    ## 18  19291930, 19301931            167  8447859    122
    ## 19            19681969           1643  8448000   1809
    ## 20            19191920            264  8448011    237
    ## 21            19171918              0  8448052      0
    ## 22            19201921             35  8448212     82
    ## 23            19911992            449  8448287    474
    ## 24            19571958            932  8448320   1046
    ## 25            19171918              3  8448336      7
    ## 26            19221923             32  8448349     84
    ## 27            19171918             12  8448456      1
    ## 28            19881989            834  8448782   1723
    ## 29            19831984           1236  8448884    383
    ## 30            19171918              0  8449048      0
    ## 31            19251926            180  8449082     24
    ## 32            19171918              3  8449181      1
    ##    positionCode rookieGamesPlayed rookiePoints seasons
    ## 1             R                52           25      21
    ## 2             C                 2            1       1
    ## 3             L                NA           NA      21
    ## 4             C                44           34      20
    ## 5             D                80           65      21
    ## 6             L                35           17       6
    ## 7             D                35            8       6
    ## 8             C                76           46      15
    ## 9             D                44           10      20
    ## 10            C                 4            0       1
    ## 11            C                31           23      13
    ## 12            C                42           11       9
    ## 13            C                59           68      16
    ## 14            R                 4            7       1
    ## 15            R                70           31      18
    ## 16            L                 1            0       1
    ## 17            L                 4            4       1
    ## 18            L                36           19       6
    ## 19            R                58           22      25
    ## 20            C                10           19      13
    ## 21            D                 4            0       1
    ## 22            C                NA           NA       5
    ## 23            D                NA           NA      15
    ## 24            C                64           40      20
    ## 25            D                 4            7       1
    ## 26            C                NA           NA       5
    ## 27            D                 3            1       1
    ## 28            C                73          100      17
    ## 29            D                64           21      15
    ## 30            D                 1            0       1
    ## 31            D                36           10       6
    ## 32            C                 4            1       1
    ##  [ reached 'max' / getOption("max.print") -- omitted 17177 rows ]

``` r
getSkaterRecords(26)
```

    ## No encoding supplied: defaulting to UTF-8.

    ##       id activePlayer assists firstName franchiseId
    ## 1  17239        FALSE       0       Jim          26
    ## 2  17418        FALSE       1      Mike          26
    ## 3  17543        FALSE       0      Fred          26
    ## 4  17703        FALSE       2    Jergus          26
    ## 5  17728        FALSE       0      Reid          26
    ## 6  18169        FALSE       0       Bob          26
    ## 7  18233        FALSE       0   Charlie          26
    ## 8  18288        FALSE       0      Greg          26
    ## 9  18328        FALSE       1      Jeff          26
    ## 10 18799        FALSE       1     Shane          26
    ## 11 18953        FALSE       0      Yves          26
    ## 12 19034        FALSE       1       Jim          26
    ## 13 19349        FALSE       0     Steve          26
    ## 14 20327        FALSE       1     Randy          26
    ## 15 21075        FALSE       0       Bob          26
    ## 16 21324        FALSE       0      Rick          26
    ## 17 21556        FALSE       0       Pat          26
    ## 18 21851        FALSE       4     David          26
    ## 19 22073        FALSE       0     Kevin          26
    ## 20 22105        FALSE       6       Tim          26
    ## 21 22302        FALSE       0      Marc          26
    ## 22 22675        FALSE       1   Charles          26
    ## 23 23333        FALSE       3       Rob          26
    ## 24 23416        FALSE       0    Girard          26
    ## 25 23447        FALSE       3       Bob          26
    ## 26 23595        FALSE       0     Glenn          26
    ## 27 23869        FALSE       0      John          26
    ## 28 23926        FALSE       0       Lee          26
    ## 29 24037        FALSE       0      Jeff          26
    ## 30 24078        FALSE       0       Jim          26
    ## 31 24317        FALSE       9      Mark          26
    ## 32 24538        FALSE       0      Jean          26
    ##          franchiseName gameTypeId gamesPlayed goals
    ## 1  Carolina Hurricanes          2          16     0
    ## 2  Carolina Hurricanes          2           5     0
    ## 3  Carolina Hurricanes          2           3     0
    ## 4  Carolina Hurricanes          2          10     0
    ## 5  Carolina Hurricanes          2          12     0
    ## 6  Carolina Hurricanes          2           1     0
    ## 7  Carolina Hurricanes          2           1     0
    ## 8  Carolina Hurricanes          2           1     0
    ## 9  Carolina Hurricanes          2           7     0
    ## 10 Carolina Hurricanes          2          22     0
    ## 11 Carolina Hurricanes          2           4     0
    ## 12 Carolina Hurricanes          2           6     0
    ## 13 Carolina Hurricanes          2           9     0
    ## 14 Carolina Hurricanes          2           2     0
    ## 15 Carolina Hurricanes          2           3     0
    ## 16 Carolina Hurricanes          2           6     0
    ## 17 Carolina Hurricanes          2           2     0
    ## 18 Carolina Hurricanes          2          13     0
    ## 19 Carolina Hurricanes          2           3     0
    ## 20 Carolina Hurricanes          2          22     0
    ## 21 Carolina Hurricanes          2           9     0
    ## 22 Carolina Hurricanes          2           8     0
    ## 23 Carolina Hurricanes          2          17     0
    ## 24 Carolina Hurricanes          2           8     0
    ## 25 Carolina Hurricanes          2          30     0
    ## 26 Carolina Hurricanes          2           7     0
    ## 27 Carolina Hurricanes          2           3     0
    ## 28 Carolina Hurricanes          2           6     0
    ## 29 Carolina Hurricanes          2           4     0
    ## 30 Carolina Hurricanes          2           5     0
    ## 31 Carolina Hurricanes          2          45     0
    ## 32 Carolina Hurricanes          2           1     0
    ##        lastName
    ## 1         Agnew
    ## 2    Antonovich
    ## 3        Arthur
    ## 4          Baca
    ## 5        Bailey
    ## 6         Bodak
    ## 7     Bourgeois
    ## 8         Britz
    ## 9  Brownschidle
    ## 10       Churla
    ## 11     Courteau
    ## 12      Culhane
    ## 13      Dykstra
    ## 14       Gilhen
    ## 15         Hess
    ## 16      Hodgson
    ## 17       Hughes
    ## 18       Jensen
    ## 19         Kemp
    ## 20         Kerr
    ## 21      Laforge
    ## 22        Luksa
    ## 23   McClanahan
    ## 24     McDonald
    ## 25       McGill
    ## 26     Merkosky
    ## 27     Newberry
    ## 28      Norwood
    ## 29       Parker
    ## 30       Pavese
    ## 31        Reeds
    ## 32       Savard
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              mostAssistsGameDates
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1992-10-06, 1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-17, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-28, 1992-10-31, 1992-11-03, 1992-11-06, 1992-11-07, 1992-11-11, 1992-11-13, 1992-11-14, 1992-11-18, 1992-11-19, 1992-11-21, 1992-11-25, 1992-11-27, 1992-11-28, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-09, 1992-12-11, 1992-12-12, 1992-12-16, 1992-12-18, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-03, 1993-01-06, 1993-01-09, 1993-01-10, 1993-01-13, 1993-01-15, 1993-01-16, 1993-01-18, 1993-01-21, 1993-01-23, 1993-01-24, 1993-01-27, 1993-01-28, 1993-01-30, 1993-02-03, 1993-02-08, 1993-02-12, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-21, 1993-02-24, 1993-02-27, 1993-02-28, 1993-03-03, 1993-03-05, 1993-03-06, 1993-03-08, 1993-03-10, 1993-03-13, 1993-03-16, 1993-03-19, 1993-03-22, 1993-03-24, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-05, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14, 1993-04-16
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1979-10-13
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1990-10-27, 1991-02-24
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1988-03-15, 1988-03-19, 1988-03-20, 1988-03-22, 1988-03-24, 1988-03-26, 1988-03-27, 1988-03-31, 1988-04-02, 1988-04-03
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1986-10-11, 1986-10-12, 1986-10-16, 1986-10-18, 1986-10-24, 1986-10-25, 1986-10-28, 1986-10-30, 1986-11-01, 1986-11-02, 1986-11-05, 1986-11-08, 1986-11-12, 1986-11-13, 1986-11-15, 1986-11-19, 1986-11-21, 1986-11-22, 1986-11-26, 1986-11-29, 1986-12-01, 1986-12-03, 1986-12-04, 1986-12-06, 1986-12-10, 1986-12-13, 1986-12-14, 1986-12-17, 1986-12-18, 1986-12-20, 1986-12-21, 1986-12-23, 1986-12-26, 1986-12-27, 1986-12-30, 1986-12-31, 1987-01-03, 1987-01-04, 1987-01-07, 1987-01-09, 1987-01-10, 1987-01-12, 1987-01-14, 1987-01-15, 1987-01-17, 1987-01-19, 1987-01-21, 1987-01-23, 1987-01-24, 1987-01-27, 1987-01-29, 1987-01-31, 1987-02-01, 1987-02-04, 1987-02-06, 1987-02-07, 1987-02-14, 1987-02-17, 1987-02-18, 1987-02-21, 1987-02-22, 1987-02-25, 1987-02-28, 1987-03-01, 1987-03-03, 1987-03-05, 1987-03-07, 1987-03-10, 1987-03-11, 1987-03-13, 1987-03-15, 1987-03-18, 1987-03-21, 1987-03-22, 1987-03-25, 1987-03-28, 1987-03-29, 1987-04-01, 1987-04-04, 1987-04-05
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1982-04-03
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1987-02-01
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1986-10-11, 1986-10-12, 1986-10-16, 1986-10-18, 1986-10-24, 1986-10-25, 1986-10-28, 1986-10-30, 1986-11-01, 1986-11-02, 1986-11-05, 1986-11-08, 1986-11-12, 1986-11-13, 1986-11-15, 1986-11-19, 1986-11-21, 1986-11-22, 1986-11-26, 1986-11-29, 1986-12-01, 1986-12-03, 1986-12-04, 1986-12-06, 1986-12-10, 1986-12-13, 1986-12-14, 1986-12-17, 1986-12-18, 1986-12-20, 1986-12-21, 1986-12-23, 1986-12-26, 1986-12-27, 1986-12-30, 1986-12-31, 1987-01-03, 1987-01-04, 1987-01-07, 1987-01-09, 1987-01-10, 1987-01-12, 1987-01-14, 1987-01-15, 1987-01-17, 1987-01-19, 1987-01-21, 1987-01-23, 1987-01-24, 1987-01-27, 1987-01-29, 1987-01-31, 1987-02-01, 1987-02-04, 1987-02-06, 1987-02-07, 1987-02-14, 1987-02-17, 1987-02-18, 1987-02-21, 1987-02-22, 1987-02-25, 1987-02-28, 1987-03-01, 1987-03-03, 1987-03-05, 1987-03-07, 1987-03-10, 1987-03-11, 1987-03-13, 1987-03-15, 1987-03-18, 1987-03-21, 1987-03-22, 1987-03-25, 1987-03-28, 1987-03-29, 1987-04-01, 1987-04-04, 1987-04-05
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1990-03-03
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-11-08, 1989-11-10, 1989-11-12, 1989-11-14, 1989-11-15, 1989-11-18, 1989-11-22, 1989-11-25, 1989-11-26, 1989-11-28, 1989-11-30, 1989-12-02, 1989-12-06, 1989-12-07, 1989-12-09, 1989-12-13, 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1983-04-03
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1987-04-04, 1987-04-05
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1984-10-11, 1984-10-13, 1984-10-17, 1984-10-28
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1992-10-14, 1992-10-22, 1992-10-24, 1992-10-28, 1992-11-03, 1992-11-06
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1989-10-08, 1989-10-11, 1989-10-13, 1989-10-14, 1989-10-18, 1989-10-19, 1989-10-21, 1989-10-23, 1989-10-25, 1989-10-26, 1989-10-28, 1989-11-01, 1989-11-03, 1989-11-04, 1989-11-08, 1989-11-10, 1989-11-12, 1989-11-14, 1989-11-15, 1989-11-18, 1989-11-22, 1989-11-25, 1989-11-26, 1989-11-28, 1989-11-30, 1989-12-02, 1989-12-06, 1989-12-07, 1989-12-09, 1989-12-13, 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1979-10-19
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1981-12-27
    ## 24 1981-10-06, 1981-10-08, 1981-10-10, 1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-10-29, 1981-10-31, 1981-11-01, 1981-11-04, 1981-11-07, 1981-11-08, 1981-11-11, 1981-11-12, 1981-11-14, 1981-11-18, 1981-11-19, 1981-11-21, 1981-11-25, 1981-11-28, 1981-11-29, 1981-12-02, 1981-12-04, 1981-12-06, 1981-12-09, 1981-12-12, 1981-12-13, 1981-12-16, 1981-12-19, 1981-12-20, 1981-12-22, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-02, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-16, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25, 1982-01-27, 1982-01-30, 1982-01-31, 1982-02-02, 1982-02-05, 1982-02-06, 1982-02-10, 1982-02-13, 1982-02-14, 1982-02-16, 1982-02-19, 1982-02-20, 1982-02-22, 1982-02-24, 1982-02-27, 1982-02-28, 1982-03-03, 1982-03-06, 1982-03-07, 1982-03-10, 1982-03-13, 1982-03-14, 1982-03-16, 1982-03-18, 1982-03-20, 1982-03-21, 1982-03-24, 1982-03-27, 1982-03-28, 1982-03-30, 1982-03-31, 1982-04-03, 1982-04-04, 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1993-12-29, 1994-03-19, 1994-04-14
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1981-10-06, 1981-10-08, 1981-10-10, 1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-10-29, 1981-10-31, 1981-11-01, 1981-11-04, 1981-11-07, 1981-11-08, 1981-11-11, 1981-11-12, 1981-11-14, 1981-11-18, 1981-11-19, 1981-11-21, 1981-11-25, 1981-11-28, 1981-11-29, 1981-12-02, 1981-12-04, 1981-12-06, 1981-12-09, 1981-12-12, 1981-12-13, 1981-12-16, 1981-12-19, 1981-12-20, 1981-12-22, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-02, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-16, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25, 1982-01-27, 1982-01-30, 1982-01-31, 1982-02-02, 1982-02-05, 1982-02-06, 1982-02-10, 1982-02-13, 1982-02-14, 1982-02-16, 1982-02-19, 1982-02-20, 1982-02-22, 1982-02-24, 1982-02-27, 1982-02-28, 1982-03-03, 1982-03-06, 1982-03-07, 1982-03-10, 1982-03-13, 1982-03-14, 1982-03-16, 1982-03-18, 1982-03-20, 1982-03-21, 1982-03-24, 1982-03-27, 1982-03-28, 1982-03-30, 1982-03-31, 1982-04-03, 1982-04-04
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1985-10-10, 1985-10-12, 1985-10-15, 1985-10-17, 1985-10-19, 1985-10-23, 1985-10-24, 1985-10-26, 1985-10-29, 1985-10-30, 1985-11-02, 1985-11-05, 1985-11-07, 1985-11-09, 1985-11-13, 1985-11-16, 1985-11-19, 1985-11-21, 1985-11-23, 1985-11-27, 1985-11-29, 1985-11-30, 1985-12-04, 1985-12-07, 1985-12-11, 1985-12-13, 1985-12-14, 1985-12-16, 1985-12-18, 1985-12-19, 1985-12-21, 1985-12-23, 1985-12-26, 1985-12-28, 1985-12-29, 1985-12-31, 1986-01-02, 1986-01-04, 1986-01-07, 1986-01-10, 1986-01-12, 1986-01-15, 1986-01-17, 1986-01-18, 1986-01-20, 1986-01-23, 1986-01-25, 1986-01-27, 1986-01-29, 1986-02-01, 1986-02-02, 1986-02-06, 1986-02-08, 1986-02-09, 1986-02-11, 1986-02-14, 1986-02-15, 1986-02-18, 1986-02-19, 1986-02-22, 1986-02-23, 1986-02-26, 1986-03-01, 1986-03-02, 1986-03-05, 1986-03-07, 1986-03-08, 1986-03-10, 1986-03-13, 1986-03-15, 1986-03-18, 1986-03-19, 1986-03-22, 1986-03-23, 1986-03-26, 1986-03-29, 1986-04-01, 1986-04-03, 1986-04-05, 1986-04-06
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1991-10-16, 1991-10-19, 1991-10-23, 1991-10-26, 1991-10-27, 1991-10-30, 1991-11-01, 1991-11-02, 1991-11-06, 1991-11-09, 1991-11-10, 1991-11-12
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1991-03-10, 1991-03-12, 1991-03-14, 1991-03-16, 1991-03-17, 1991-03-19, 1991-03-23, 1991-03-25, 1991-03-27, 1991-03-30, 1991-03-31
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-03-19, 1989-03-22, 1989-03-23, 1989-03-25, 1989-03-28, 1989-03-30, 1989-04-01, 1989-04-02
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1987-11-01, 1987-12-19, 1987-12-22, 1988-01-16, 1988-01-19, 1988-01-23, 1988-01-30, 1988-12-23, 1989-01-02
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ##    mostAssistsOneGame mostAssistsOneSeason
    ## 1                   0                    0
    ## 2                   1                    1
    ## 3                   0                    0
    ## 4                   1                    2
    ## 5                   0                    0
    ## 6                   0                    0
    ## 7                   0                    0
    ## 8                   0                    0
    ## 9                   1                    1
    ## 10                  1                    1
    ## 11                  0                    0
    ## 12                  1                    1
    ## 13                  0                    0
    ## 14                  1                    1
    ## 15                  0                    0
    ## 16                  0                    0
    ## 17                  0                    0
    ## 18                  1                    4
    ## 19                  0                    0
    ## 20                  1                    6
    ## 21                  0                    0
    ## 22                  1                    1
    ## 23                  2                    3
    ## 24                  0                    0
    ## 25                  1                    3
    ## 26                  0                    0
    ## 27                  0                    0
    ## 28                  0                    0
    ## 29                  0                    0
    ## 30                  0                    0
    ## 31                  1                    7
    ## 32                  0                    0
    ##    mostAssistsSeasonIds
    ## 1              19921993
    ## 2              19791980
    ## 3              19801981
    ## 4              19901991
    ## 5              19831984
    ## 6              19891990
    ## 7              19871988
    ## 8              19861987
    ## 9              19811982
    ## 10             19861987
    ## 11             19861987
    ## 12             19891990
    ## 13             19891990
    ## 14             19821983
    ## 15             19831984
    ## 16             19791980
    ## 17             19861987
    ## 18             19841985
    ## 19             19801981
    ## 20             19921993
    ## 21             19891990
    ## 22             19791980
    ## 23             19811982
    ## 24   19811982, 19831984
    ## 25             19931994
    ## 26             19811982
    ## 27             19851986
    ## 28             19911992
    ## 29             19901991
    ## 30             19881989
    ## 31             19871988
    ## 32             19791980
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                mostGoalsGameDates
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1992-10-06, 1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-17, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-28, 1992-10-31, 1992-11-03, 1992-11-06, 1992-11-07, 1992-11-11, 1992-11-13, 1992-11-14, 1992-11-18, 1992-11-19, 1992-11-21, 1992-11-25, 1992-11-27, 1992-11-28, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-09, 1992-12-11, 1992-12-12, 1992-12-16, 1992-12-18, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-03, 1993-01-06, 1993-01-09, 1993-01-10, 1993-01-13, 1993-01-15, 1993-01-16, 1993-01-18, 1993-01-21, 1993-01-23, 1993-01-24, 1993-01-27, 1993-01-28, 1993-01-30, 1993-02-03, 1993-02-08, 1993-02-12, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-21, 1993-02-24, 1993-02-27, 1993-02-28, 1993-03-03, 1993-03-05, 1993-03-06, 1993-03-08, 1993-03-10, 1993-03-13, 1993-03-16, 1993-03-19, 1993-03-22, 1993-03-24, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-05, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14, 1993-04-16
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              1990-10-17, 1990-10-19, 1990-10-24, 1990-10-27, 1990-10-28, 1990-10-31, 1990-11-03, 1990-11-06, 1990-11-09, 1990-11-10, 1990-11-14, 1990-11-15, 1990-11-17, 1990-11-21, 1990-11-23, 1990-11-24, 1990-11-28, 1990-11-29, 1990-12-01, 1990-12-03, 1990-12-05, 1990-12-07, 1990-12-08, 1990-12-12, 1990-12-13, 1990-12-15, 1990-12-18, 1990-12-20, 1990-12-22, 1990-12-23, 1990-12-26, 1990-12-29, 1990-12-30, 1991-01-02, 1991-01-05, 1991-01-08, 1991-01-10, 1991-01-12, 1991-01-13, 1991-01-16, 1991-01-23, 1991-01-24, 1991-01-26, 1991-01-29, 1991-01-31, 1991-02-02, 1991-02-03, 1991-02-06, 1991-02-09, 1991-02-10, 1991-02-13, 1991-02-15, 1991-02-16, 1991-02-20, 1991-02-23, 1991-02-24, 1991-02-26, 1991-02-28, 1991-03-02, 1991-03-03, 1991-03-05, 1991-03-09, 1991-03-10, 1991-03-12, 1991-03-14, 1991-03-16, 1991-03-17, 1991-03-19, 1991-03-23, 1991-03-25, 1991-03-27, 1991-03-30, 1991-03-31, 1992-02-25, 1992-02-27, 1992-02-29, 1992-03-01, 1992-03-03, 1992-03-05, 1992-03-07, 1992-03-09, 1992-03-11, 1992-03-13, 1992-03-14, 1992-03-16, 1992-03-18, 1992-03-21, 1992-03-22, 1992-03-24, 1992-03-26, 1992-03-28, 1992-03-29, 1992-04-12, 1992-04-13, 1992-04-15
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1988-03-15, 1988-03-19, 1988-03-20, 1988-03-22, 1988-03-24, 1988-03-26, 1988-03-27, 1988-03-31, 1988-04-02, 1988-04-03
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1986-10-11, 1986-10-12, 1986-10-16, 1986-10-18, 1986-10-24, 1986-10-25, 1986-10-28, 1986-10-30, 1986-11-01, 1986-11-02, 1986-11-05, 1986-11-08, 1986-11-12, 1986-11-13, 1986-11-15, 1986-11-19, 1986-11-21, 1986-11-22, 1986-11-26, 1986-11-29, 1986-12-01, 1986-12-03, 1986-12-04, 1986-12-06, 1986-12-10, 1986-12-13, 1986-12-14, 1986-12-17, 1986-12-18, 1986-12-20, 1986-12-21, 1986-12-23, 1986-12-26, 1986-12-27, 1986-12-30, 1986-12-31, 1987-01-03, 1987-01-04, 1987-01-07, 1987-01-09, 1987-01-10, 1987-01-12, 1987-01-14, 1987-01-15, 1987-01-17, 1987-01-19, 1987-01-21, 1987-01-23, 1987-01-24, 1987-01-27, 1987-01-29, 1987-01-31, 1987-02-01, 1987-02-04, 1987-02-06, 1987-02-07, 1987-02-14, 1987-02-17, 1987-02-18, 1987-02-21, 1987-02-22, 1987-02-25, 1987-02-28, 1987-03-01, 1987-03-03, 1987-03-05, 1987-03-07, 1987-03-10, 1987-03-11, 1987-03-13, 1987-03-15, 1987-03-18, 1987-03-21, 1987-03-22, 1987-03-25, 1987-03-28, 1987-03-29, 1987-04-01, 1987-04-04, 1987-04-05
    ## 9  1981-10-06, 1981-10-08, 1981-10-10, 1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-10-29, 1981-10-31, 1981-11-01, 1981-11-04, 1981-11-07, 1981-11-08, 1981-11-11, 1981-11-12, 1981-11-14, 1981-11-18, 1981-11-19, 1981-11-21, 1981-11-25, 1981-11-28, 1981-11-29, 1981-12-02, 1981-12-04, 1981-12-06, 1981-12-09, 1981-12-12, 1981-12-13, 1981-12-16, 1981-12-19, 1981-12-20, 1981-12-22, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-02, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-16, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25, 1982-01-27, 1982-01-30, 1982-01-31, 1982-02-02, 1982-02-05, 1982-02-06, 1982-02-10, 1982-02-13, 1982-02-14, 1982-02-16, 1982-02-19, 1982-02-20, 1982-02-22, 1982-02-24, 1982-02-27, 1982-02-28, 1982-03-03, 1982-03-06, 1982-03-07, 1982-03-10, 1982-03-13, 1982-03-14, 1982-03-16, 1982-03-18, 1982-03-20, 1982-03-21, 1982-03-24, 1982-03-27, 1982-03-28, 1982-03-30, 1982-03-31, 1982-04-03, 1982-04-04, 1982-10-06, 1982-10-09, 1982-10-10, 1982-10-14, 1982-10-16, 1982-10-20, 1982-10-21, 1982-10-23, 1982-10-26, 1982-10-30, 1982-11-02, 1982-11-04, 1982-11-06, 1982-11-07, 1982-11-10, 1982-11-13, 1982-11-16, 1982-11-17, 1982-11-20, 1982-11-24, 1982-11-26, 1982-11-27, 1982-12-01, 1982-12-03, 1982-12-04, 1982-12-06, 1982-12-08, 1982-12-11, 1982-12-12, 1982-12-14, 1982-12-17, 1982-12-18, 1982-12-21, 1982-12-23, 1982-12-26, 1982-12-28, 1982-12-30, 1983-01-01, 1983-01-02, 1983-01-06, 1983-01-08, 1983-01-09, 1983-01-11, 1983-01-13, 1983-01-15, 1983-01-18, 1983-01-20, 1983-01-22, 1983-01-23, 1983-01-27, 1983-01-29, 1983-02-01, 1983-02-02, 1983-02-05, 1983-02-09, 1983-02-12, 1983-02-13, 1983-02-15, 1983-02-17, 1983-02-19, 1983-02-20, 1983-02-23, 1983-02-25, 1983-02-27, 1983-03-01, 1983-03-05, 1983-03-06, 1983-03-08, 1983-03-10, 1983-03-12, 1983-03-15, 1983-03-16, 1983-03-20, 1983-03-22, 1983-03-23, 1983-03-26, 1983-03-27, 1983-03-29, 1983-04-02, 1983-04-03
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1986-10-11, 1986-10-12, 1986-10-16, 1986-10-18, 1986-10-24, 1986-10-25, 1986-10-28, 1986-10-30, 1986-11-01, 1986-11-02, 1986-11-05, 1986-11-08, 1986-11-12, 1986-11-13, 1986-11-15, 1986-11-19, 1986-11-21, 1986-11-22, 1986-11-26, 1986-11-29, 1986-12-01, 1986-12-03, 1986-12-04, 1986-12-06, 1986-12-10, 1986-12-13, 1986-12-14, 1986-12-17, 1986-12-18, 1986-12-20, 1986-12-21, 1986-12-23, 1986-12-26, 1986-12-27, 1986-12-30, 1986-12-31, 1987-01-03, 1987-01-04, 1987-01-07, 1987-01-09, 1987-01-10, 1987-01-12, 1987-01-14, 1987-01-15, 1987-01-17, 1987-01-19, 1987-01-21, 1987-01-23, 1987-01-24, 1987-01-27, 1987-01-29, 1987-01-31, 1987-02-01, 1987-02-04, 1987-02-06, 1987-02-07, 1987-02-14, 1987-02-17, 1987-02-18, 1987-02-21, 1987-02-22, 1987-02-25, 1987-02-28, 1987-03-01, 1987-03-03, 1987-03-05, 1987-03-07, 1987-03-10, 1987-03-11, 1987-03-13, 1987-03-15, 1987-03-18, 1987-03-21, 1987-03-22, 1987-03-25, 1987-03-28, 1987-03-29, 1987-04-01, 1987-04-04, 1987-04-05, 1987-10-16, 1987-10-17, 1987-10-21, 1987-10-24, 1987-10-28, 1987-10-31, 1987-11-01, 1987-11-04, 1987-11-06, 1987-11-07, 1987-11-11, 1987-11-14, 1987-11-16, 1987-11-18, 1987-11-21, 1987-11-25, 1987-11-27, 1987-11-28, 1987-12-02, 1987-12-03, 1987-12-05, 1987-12-08, 1987-12-09, 1987-12-12, 1987-12-15, 1987-12-17, 1987-12-19, 1987-12-20, 1987-12-22, 1987-12-26, 1987-12-27, 1987-12-30, 1988-01-02
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1986-10-11, 1986-10-12, 1986-10-16, 1986-10-18, 1986-10-24, 1986-10-25, 1986-10-28, 1986-10-30, 1986-11-01, 1986-11-02, 1986-11-05, 1986-11-08, 1986-11-12, 1986-11-13, 1986-11-15, 1986-11-19, 1986-11-21, 1986-11-22, 1986-11-26, 1986-11-29, 1986-12-01, 1986-12-03, 1986-12-04, 1986-12-06, 1986-12-10, 1986-12-13, 1986-12-14, 1986-12-17, 1986-12-18, 1986-12-20, 1986-12-21, 1986-12-23, 1986-12-26, 1986-12-27, 1986-12-30, 1986-12-31, 1987-01-03, 1987-01-04, 1987-01-07, 1987-01-09, 1987-01-10, 1987-01-12, 1987-01-14, 1987-01-15, 1987-01-17, 1987-01-19, 1987-01-21, 1987-01-23, 1987-01-24, 1987-01-27, 1987-01-29, 1987-01-31, 1987-02-01, 1987-02-04, 1987-02-06, 1987-02-07, 1987-02-14, 1987-02-17, 1987-02-18, 1987-02-21, 1987-02-22, 1987-02-25, 1987-02-28, 1987-03-01, 1987-03-03, 1987-03-05, 1987-03-07, 1987-03-10, 1987-03-11, 1987-03-13, 1987-03-15, 1987-03-18, 1987-03-21, 1987-03-22, 1987-03-25, 1987-03-28, 1987-03-29, 1987-04-01, 1987-04-04, 1987-04-05
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1988-02-27, 1988-03-01, 1988-03-02, 1988-03-05, 1988-03-08, 1988-03-09, 1988-03-12, 1988-03-13, 1988-03-15, 1988-03-19, 1988-03-20, 1988-03-22, 1988-03-24, 1988-03-26, 1988-03-27, 1988-03-31, 1988-04-02, 1988-04-03, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-11-08, 1989-11-10, 1989-11-12, 1989-11-14, 1989-11-15, 1989-11-18, 1989-11-22, 1989-11-25, 1989-11-26, 1989-11-28, 1989-11-30, 1989-12-02, 1989-12-06, 1989-12-07, 1989-12-09, 1989-12-13, 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1982-10-06, 1982-10-09, 1982-10-10, 1982-10-14, 1982-10-16, 1982-10-20, 1982-10-21, 1982-10-23, 1982-10-26, 1982-10-30, 1982-11-02, 1982-11-04, 1982-11-06, 1982-11-07, 1982-11-10, 1982-11-13, 1982-11-16, 1982-11-17, 1982-11-20, 1982-11-24, 1982-11-26, 1982-11-27, 1982-12-01, 1982-12-03, 1982-12-04, 1982-12-06, 1982-12-08, 1982-12-11, 1982-12-12, 1982-12-14, 1982-12-17, 1982-12-18, 1982-12-21, 1982-12-23, 1982-12-26, 1982-12-28, 1982-12-30, 1983-01-01, 1983-01-02, 1983-01-06, 1983-01-08, 1983-01-09, 1983-01-11, 1983-01-13, 1983-01-15, 1983-01-18, 1983-01-20, 1983-01-22, 1983-01-23, 1983-01-27, 1983-01-29, 1983-02-01, 1983-02-02, 1983-02-05, 1983-02-09, 1983-02-12, 1983-02-13, 1983-02-15, 1983-02-17, 1983-02-19, 1983-02-20, 1983-02-23, 1983-02-25, 1983-02-27, 1983-03-01, 1983-03-05, 1983-03-06, 1983-03-08, 1983-03-10, 1983-03-12, 1983-03-15, 1983-03-16, 1983-03-20, 1983-03-22, 1983-03-23, 1983-03-26, 1983-03-27, 1983-03-29, 1983-04-02, 1983-04-03
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1987-04-04, 1987-04-05
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1984-10-11, 1984-10-13, 1984-10-14, 1984-10-17, 1984-10-18, 1984-10-20, 1984-10-23, 1984-10-24, 1984-10-27, 1984-10-28, 1984-10-31, 1984-11-02, 1984-11-03, 1984-11-07, 1984-11-10, 1984-11-15, 1984-11-17, 1984-11-21, 1984-11-22, 1984-11-24, 1984-11-28, 1984-11-30, 1984-12-01, 1984-12-03, 1984-12-05, 1984-12-08, 1984-12-12, 1984-12-15, 1984-12-19, 1984-12-21, 1984-12-22, 1984-12-26, 1984-12-28, 1984-12-29, 1985-01-02, 1985-01-03, 1985-01-05, 1985-01-07, 1985-01-08, 1985-01-12, 1985-01-15, 1985-01-17, 1985-01-19, 1985-01-22, 1985-01-26, 1985-01-27, 1985-01-31, 1985-02-01, 1985-02-03, 1985-02-06, 1985-02-07, 1985-02-09, 1985-02-10, 1985-02-14, 1985-02-16, 1985-02-17, 1985-02-19, 1985-02-21, 1985-02-23, 1985-02-24, 1985-02-26, 1985-03-01, 1985-03-03, 1985-03-05, 1985-03-07, 1985-03-09, 1985-03-10, 1985-03-13, 1985-03-16, 1985-03-17, 1985-03-20, 1985-03-23, 1985-03-24, 1985-03-27, 1985-03-29, 1985-03-30, 1985-04-02, 1985-04-04, 1985-04-06, 1985-04-07
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1992-10-06, 1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-17, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-28, 1992-10-31, 1992-11-03, 1992-11-06, 1992-11-07, 1992-11-11, 1992-11-13, 1992-11-14, 1992-11-18, 1992-11-19, 1992-11-21, 1992-11-25, 1992-11-27, 1992-11-28, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-09, 1992-12-11, 1992-12-12, 1992-12-16, 1992-12-18, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-03, 1993-01-06, 1993-01-09, 1993-01-10, 1993-01-13, 1993-01-15, 1993-01-16, 1993-01-18, 1993-01-21, 1993-01-23, 1993-01-24, 1993-01-27, 1993-01-28, 1993-01-30, 1993-02-03, 1993-02-08, 1993-02-12, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-21, 1993-02-24, 1993-02-27, 1993-02-28, 1993-03-03, 1993-03-05, 1993-03-06, 1993-03-08, 1993-03-10, 1993-03-13, 1993-03-16, 1993-03-19, 1993-03-22, 1993-03-24, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-05, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14, 1993-04-16
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1989-10-08, 1989-10-11, 1989-10-13, 1989-10-14, 1989-10-18, 1989-10-19, 1989-10-21, 1989-10-23, 1989-10-25, 1989-10-26, 1989-10-28, 1989-11-01, 1989-11-03, 1989-11-04, 1989-11-08, 1989-11-10, 1989-11-12, 1989-11-14, 1989-11-15, 1989-11-18, 1989-11-22, 1989-11-25, 1989-11-26, 1989-11-28, 1989-11-30, 1989-12-02, 1989-12-06, 1989-12-07, 1989-12-09, 1989-12-13, 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25
    ## 24 1981-10-06, 1981-10-08, 1981-10-10, 1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-10-29, 1981-10-31, 1981-11-01, 1981-11-04, 1981-11-07, 1981-11-08, 1981-11-11, 1981-11-12, 1981-11-14, 1981-11-18, 1981-11-19, 1981-11-21, 1981-11-25, 1981-11-28, 1981-11-29, 1981-12-02, 1981-12-04, 1981-12-06, 1981-12-09, 1981-12-12, 1981-12-13, 1981-12-16, 1981-12-19, 1981-12-20, 1981-12-22, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-02, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-16, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25, 1982-01-27, 1982-01-30, 1982-01-31, 1982-02-02, 1982-02-05, 1982-02-06, 1982-02-10, 1982-02-13, 1982-02-14, 1982-02-16, 1982-02-19, 1982-02-20, 1982-02-22, 1982-02-24, 1982-02-27, 1982-02-28, 1982-03-03, 1982-03-06, 1982-03-07, 1982-03-10, 1982-03-13, 1982-03-14, 1982-03-16, 1982-03-18, 1982-03-20, 1982-03-21, 1982-03-24, 1982-03-27, 1982-03-28, 1982-03-30, 1982-03-31, 1982-04-03, 1982-04-04, 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1993-11-03, 1993-11-06, 1993-11-10, 1993-11-13, 1993-11-17, 1993-11-18, 1993-11-20, 1993-11-23, 1993-11-24, 1993-11-27, 1993-11-29, 1993-12-01, 1993-12-04, 1993-12-07, 1993-12-08, 1993-12-11, 1993-12-12, 1993-12-15, 1993-12-18, 1993-12-22, 1993-12-23, 1993-12-26, 1993-12-28, 1993-12-29, 1994-01-01, 1994-01-02, 1994-01-05, 1994-01-06, 1994-01-08, 1994-01-12, 1994-01-14, 1994-01-15, 1994-01-17, 1994-01-19, 1994-01-24, 1994-01-26, 1994-01-27, 1994-01-29, 1994-02-01, 1994-02-02, 1994-02-04, 1994-02-06, 1994-02-11, 1994-02-12, 1994-02-16, 1994-02-17, 1994-02-19, 1994-02-24, 1994-02-26, 1994-02-27, 1994-03-02, 1994-03-04, 1994-03-05, 1994-03-09, 1994-03-10, 1994-03-12, 1994-03-13, 1994-03-16, 1994-03-17, 1994-03-19, 1994-03-22, 1994-03-25, 1994-03-26, 1994-03-29, 1994-03-30, 1994-04-02, 1994-04-06, 1994-04-07, 1994-04-10, 1994-04-11, 1994-04-14
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1981-10-06, 1981-10-08, 1981-10-10, 1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-10-29, 1981-10-31, 1981-11-01, 1981-11-04, 1981-11-07, 1981-11-08, 1981-11-11, 1981-11-12, 1981-11-14, 1981-11-18, 1981-11-19, 1981-11-21, 1981-11-25, 1981-11-28, 1981-11-29, 1981-12-02, 1981-12-04, 1981-12-06, 1981-12-09, 1981-12-12, 1981-12-13, 1981-12-16, 1981-12-19, 1981-12-20, 1981-12-22, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-02, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-16, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25, 1982-01-27, 1982-01-30, 1982-01-31, 1982-02-02, 1982-02-05, 1982-02-06, 1982-02-10, 1982-02-13, 1982-02-14, 1982-02-16, 1982-02-19, 1982-02-20, 1982-02-22, 1982-02-24, 1982-02-27, 1982-02-28, 1982-03-03, 1982-03-06, 1982-03-07, 1982-03-10, 1982-03-13, 1982-03-14, 1982-03-16, 1982-03-18, 1982-03-20, 1982-03-21, 1982-03-24, 1982-03-27, 1982-03-28, 1982-03-30, 1982-03-31, 1982-04-03, 1982-04-04
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1985-10-10, 1985-10-12, 1985-10-15, 1985-10-17, 1985-10-19, 1985-10-23, 1985-10-24, 1985-10-26, 1985-10-29, 1985-10-30, 1985-11-02, 1985-11-05, 1985-11-07, 1985-11-09, 1985-11-13, 1985-11-16, 1985-11-19, 1985-11-21, 1985-11-23, 1985-11-27, 1985-11-29, 1985-11-30, 1985-12-04, 1985-12-07, 1985-12-11, 1985-12-13, 1985-12-14, 1985-12-16, 1985-12-18, 1985-12-19, 1985-12-21, 1985-12-23, 1985-12-26, 1985-12-28, 1985-12-29, 1985-12-31, 1986-01-02, 1986-01-04, 1986-01-07, 1986-01-10, 1986-01-12, 1986-01-15, 1986-01-17, 1986-01-18, 1986-01-20, 1986-01-23, 1986-01-25, 1986-01-27, 1986-01-29, 1986-02-01, 1986-02-02, 1986-02-06, 1986-02-08, 1986-02-09, 1986-02-11, 1986-02-14, 1986-02-15, 1986-02-18, 1986-02-19, 1986-02-22, 1986-02-23, 1986-02-26, 1986-03-01, 1986-03-02, 1986-03-05, 1986-03-07, 1986-03-08, 1986-03-10, 1986-03-13, 1986-03-15, 1986-03-18, 1986-03-19, 1986-03-22, 1986-03-23, 1986-03-26, 1986-03-29, 1986-04-01, 1986-04-03, 1986-04-05, 1986-04-06
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1991-10-16, 1991-10-19, 1991-10-23, 1991-10-26, 1991-10-27, 1991-10-30, 1991-11-01, 1991-11-02, 1991-11-06, 1991-11-09, 1991-11-10, 1991-11-12
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1991-03-10, 1991-03-12, 1991-03-14, 1991-03-16, 1991-03-17, 1991-03-19, 1991-03-23, 1991-03-25, 1991-03-27, 1991-03-30, 1991-03-31
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-03-19, 1989-03-22, 1989-03-23, 1989-03-25, 1989-03-28, 1989-03-30, 1989-04-01, 1989-04-02
    ## 31                                                                                                                                                                                                                                                                                                                                                             1987-10-08, 1987-10-10, 1987-10-11, 1987-10-14, 1987-10-16, 1987-10-17, 1987-10-21, 1987-10-24, 1987-10-28, 1987-10-31, 1987-11-01, 1987-11-04, 1987-11-06, 1987-11-07, 1987-11-11, 1987-11-14, 1987-11-16, 1987-11-18, 1987-11-21, 1987-11-25, 1987-11-27, 1987-11-28, 1987-12-02, 1987-12-03, 1987-12-05, 1987-12-08, 1987-12-09, 1987-12-12, 1987-12-15, 1987-12-17, 1987-12-19, 1987-12-20, 1987-12-22, 1987-12-26, 1987-12-27, 1987-12-30, 1988-01-02, 1988-01-06, 1988-01-08, 1988-01-09, 1988-01-11, 1988-01-13, 1988-01-14, 1988-01-16, 1988-01-19, 1988-01-21, 1988-01-23, 1988-01-24, 1988-01-27, 1988-01-29, 1988-01-30, 1988-02-01, 1988-02-03, 1988-02-06, 1988-02-07, 1988-02-13, 1988-02-15, 1988-02-17, 1988-02-20, 1988-02-21, 1988-02-23, 1988-02-25, 1988-02-27, 1988-03-01, 1988-03-02, 1988-03-05, 1988-03-08, 1988-03-09, 1988-03-12, 1988-03-13, 1988-03-15, 1988-03-19, 1988-03-20, 1988-03-22, 1988-03-24, 1988-03-26, 1988-03-27, 1988-03-31, 1988-04-02, 1988-04-03, 1988-12-15, 1988-12-17, 1988-12-19, 1988-12-21, 1988-12-23, 1988-12-26, 1988-12-28, 1988-12-30, 1988-12-31, 1989-01-02, 1989-01-04, 1989-01-10, 1989-01-14, 1989-01-16, 1989-01-18, 1989-01-19, 1989-01-21, 1989-01-23, 1989-01-25, 1989-01-27, 1989-01-28, 1989-01-31, 1989-02-03, 1989-02-04, 1989-02-09, 1989-02-11, 1989-02-15, 1989-02-18, 1989-02-19, 1989-02-21, 1989-02-23, 1989-02-25, 1989-02-26, 1989-02-28, 1989-03-02, 1989-03-04, 1989-03-05, 1989-03-08, 1989-03-11, 1989-03-12, 1989-03-14, 1989-03-16, 1989-03-18, 1989-03-19, 1989-03-22, 1989-03-23, 1989-03-25, 1989-03-28, 1989-03-30, 1989-04-01, 1989-04-02
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ##    mostGoalsOneGame mostGoalsOneSeason mostGoalsSeasonIds
    ## 1                 0                  0           19921993
    ## 2                 0                  0           19791980
    ## 3                 0                  0           19801981
    ## 4                 0                  0 19901991, 19911992
    ## 5                 0                  0           19831984
    ## 6                 0                  0           19891990
    ## 7                 0                  0           19871988
    ## 8                 0                  0           19861987
    ## 9                 0                  0 19811982, 19821983
    ## 10                0                  0 19861987, 19871988
    ## 11                0                  0           19861987
    ## 12                0                  0 19871988, 19891990
    ## 13                0                  0           19891990
    ## 14                0                  0           19821983
    ## 15                0                  0           19831984
    ## 16                0                  0           19791980
    ## 17                0                  0           19861987
    ## 18                0                  0           19841985
    ## 19                0                  0           19801981
    ## 20                0                  0           19921993
    ## 21                0                  0           19891990
    ## 22                0                  0           19791980
    ## 23                0                  0           19811982
    ## 24                0                  0 19811982, 19831984
    ## 25                0                  0           19931994
    ## 26                0                  0           19811982
    ## 27                0                  0           19851986
    ## 28                0                  0           19911992
    ## 29                0                  0           19901991
    ## 30                0                  0           19881989
    ## 31                0                  0 19871988, 19881989
    ## 32                0                  0           19791980
    ##    mostPenaltyMinutesOneSeason mostPenaltyMinutesSeasonIds
    ## 1                           68                    19921993
    ## 2                            2                    19791980
    ## 3                            0                    19801981
    ## 4                           14                    19901991
    ## 5                           25                    19831984
    ## 6                            7                    19891990
    ## 7                            0                    19871988
    ## 8                            0                    19861987
    ## 9                            2                    19811982
    ## 10                          78                    19861987
    ## 11                           0                    19861987
    ## 12                           6                    19891990
    ## 13                           2                    19891990
    ## 14                           0                    19821983
    ## 15                           0                    19831984
    ## 16                           6                    19791980
    ## 17                           2                    19861987
    ## 18                           6                    19841985
    ## 19                           4                    19801981
    ## 20                           7                    19921993
    ## 21                          43                    19891990
    ## 22                           4                    19791980
    ## 23                          11                    19811982
    ## 24                           4                    19831984
    ## 25                          41                    19931994
    ## 26                           2                    19811982
    ## 27                           0                    19851986
    ## 28                          16                    19911992
    ## 29                           2                    19901991
    ## 30                           5                    19881989
    ## 31                          31                    19871988
    ## 32                           0                    19791980
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               mostPointsGameDates
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1992-10-06, 1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-17, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-28, 1992-10-31, 1992-11-03, 1992-11-06, 1992-11-07, 1992-11-11, 1992-11-13, 1992-11-14, 1992-11-18, 1992-11-19, 1992-11-21, 1992-11-25, 1992-11-27, 1992-11-28, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-09, 1992-12-11, 1992-12-12, 1992-12-16, 1992-12-18, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-03, 1993-01-06, 1993-01-09, 1993-01-10, 1993-01-13, 1993-01-15, 1993-01-16, 1993-01-18, 1993-01-21, 1993-01-23, 1993-01-24, 1993-01-27, 1993-01-28, 1993-01-30, 1993-02-03, 1993-02-08, 1993-02-12, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-21, 1993-02-24, 1993-02-27, 1993-02-28, 1993-03-03, 1993-03-05, 1993-03-06, 1993-03-08, 1993-03-10, 1993-03-13, 1993-03-16, 1993-03-19, 1993-03-22, 1993-03-24, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-05, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14, 1993-04-16
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1979-10-13
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1990-10-27, 1991-02-24
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1988-03-15, 1988-03-19, 1988-03-20, 1988-03-22, 1988-03-24, 1988-03-26, 1988-03-27, 1988-03-31, 1988-04-02, 1988-04-03
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1986-10-11, 1986-10-12, 1986-10-16, 1986-10-18, 1986-10-24, 1986-10-25, 1986-10-28, 1986-10-30, 1986-11-01, 1986-11-02, 1986-11-05, 1986-11-08, 1986-11-12, 1986-11-13, 1986-11-15, 1986-11-19, 1986-11-21, 1986-11-22, 1986-11-26, 1986-11-29, 1986-12-01, 1986-12-03, 1986-12-04, 1986-12-06, 1986-12-10, 1986-12-13, 1986-12-14, 1986-12-17, 1986-12-18, 1986-12-20, 1986-12-21, 1986-12-23, 1986-12-26, 1986-12-27, 1986-12-30, 1986-12-31, 1987-01-03, 1987-01-04, 1987-01-07, 1987-01-09, 1987-01-10, 1987-01-12, 1987-01-14, 1987-01-15, 1987-01-17, 1987-01-19, 1987-01-21, 1987-01-23, 1987-01-24, 1987-01-27, 1987-01-29, 1987-01-31, 1987-02-01, 1987-02-04, 1987-02-06, 1987-02-07, 1987-02-14, 1987-02-17, 1987-02-18, 1987-02-21, 1987-02-22, 1987-02-25, 1987-02-28, 1987-03-01, 1987-03-03, 1987-03-05, 1987-03-07, 1987-03-10, 1987-03-11, 1987-03-13, 1987-03-15, 1987-03-18, 1987-03-21, 1987-03-22, 1987-03-25, 1987-03-28, 1987-03-29, 1987-04-01, 1987-04-04, 1987-04-05
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1982-04-03
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1987-02-01
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1986-10-11, 1986-10-12, 1986-10-16, 1986-10-18, 1986-10-24, 1986-10-25, 1986-10-28, 1986-10-30, 1986-11-01, 1986-11-02, 1986-11-05, 1986-11-08, 1986-11-12, 1986-11-13, 1986-11-15, 1986-11-19, 1986-11-21, 1986-11-22, 1986-11-26, 1986-11-29, 1986-12-01, 1986-12-03, 1986-12-04, 1986-12-06, 1986-12-10, 1986-12-13, 1986-12-14, 1986-12-17, 1986-12-18, 1986-12-20, 1986-12-21, 1986-12-23, 1986-12-26, 1986-12-27, 1986-12-30, 1986-12-31, 1987-01-03, 1987-01-04, 1987-01-07, 1987-01-09, 1987-01-10, 1987-01-12, 1987-01-14, 1987-01-15, 1987-01-17, 1987-01-19, 1987-01-21, 1987-01-23, 1987-01-24, 1987-01-27, 1987-01-29, 1987-01-31, 1987-02-01, 1987-02-04, 1987-02-06, 1987-02-07, 1987-02-14, 1987-02-17, 1987-02-18, 1987-02-21, 1987-02-22, 1987-02-25, 1987-02-28, 1987-03-01, 1987-03-03, 1987-03-05, 1987-03-07, 1987-03-10, 1987-03-11, 1987-03-13, 1987-03-15, 1987-03-18, 1987-03-21, 1987-03-22, 1987-03-25, 1987-03-28, 1987-03-29, 1987-04-01, 1987-04-04, 1987-04-05
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1990-03-03
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-11-08, 1989-11-10, 1989-11-12, 1989-11-14, 1989-11-15, 1989-11-18, 1989-11-22, 1989-11-25, 1989-11-26, 1989-11-28, 1989-11-30, 1989-12-02, 1989-12-06, 1989-12-07, 1989-12-09, 1989-12-13, 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1983-04-03
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1987-04-04, 1987-04-05
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1984-10-11, 1984-10-13, 1984-10-17, 1984-10-28
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1992-10-14, 1992-10-22, 1992-10-24, 1992-10-28, 1992-11-03, 1992-11-06
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1989-10-08, 1989-10-11, 1989-10-13, 1989-10-14, 1989-10-18, 1989-10-19, 1989-10-21, 1989-10-23, 1989-10-25, 1989-10-26, 1989-10-28, 1989-11-01, 1989-11-03, 1989-11-04, 1989-11-08, 1989-11-10, 1989-11-12, 1989-11-14, 1989-11-15, 1989-11-18, 1989-11-22, 1989-11-25, 1989-11-26, 1989-11-28, 1989-11-30, 1989-12-02, 1989-12-06, 1989-12-07, 1989-12-09, 1989-12-13, 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1979-10-19
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1981-12-27
    ## 24 1981-10-06, 1981-10-08, 1981-10-10, 1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-10-29, 1981-10-31, 1981-11-01, 1981-11-04, 1981-11-07, 1981-11-08, 1981-11-11, 1981-11-12, 1981-11-14, 1981-11-18, 1981-11-19, 1981-11-21, 1981-11-25, 1981-11-28, 1981-11-29, 1981-12-02, 1981-12-04, 1981-12-06, 1981-12-09, 1981-12-12, 1981-12-13, 1981-12-16, 1981-12-19, 1981-12-20, 1981-12-22, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-02, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-16, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25, 1982-01-27, 1982-01-30, 1982-01-31, 1982-02-02, 1982-02-05, 1982-02-06, 1982-02-10, 1982-02-13, 1982-02-14, 1982-02-16, 1982-02-19, 1982-02-20, 1982-02-22, 1982-02-24, 1982-02-27, 1982-02-28, 1982-03-03, 1982-03-06, 1982-03-07, 1982-03-10, 1982-03-13, 1982-03-14, 1982-03-16, 1982-03-18, 1982-03-20, 1982-03-21, 1982-03-24, 1982-03-27, 1982-03-28, 1982-03-30, 1982-03-31, 1982-04-03, 1982-04-04, 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1993-12-29, 1994-03-19, 1994-04-14
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1981-10-06, 1981-10-08, 1981-10-10, 1981-10-15, 1981-10-17, 1981-10-21, 1981-10-23, 1981-10-24, 1981-10-29, 1981-10-31, 1981-11-01, 1981-11-04, 1981-11-07, 1981-11-08, 1981-11-11, 1981-11-12, 1981-11-14, 1981-11-18, 1981-11-19, 1981-11-21, 1981-11-25, 1981-11-28, 1981-11-29, 1981-12-02, 1981-12-04, 1981-12-06, 1981-12-09, 1981-12-12, 1981-12-13, 1981-12-16, 1981-12-19, 1981-12-20, 1981-12-22, 1981-12-26, 1981-12-27, 1981-12-29, 1981-12-30, 1982-01-02, 1982-01-03, 1982-01-06, 1982-01-09, 1982-01-11, 1982-01-16, 1982-01-17, 1982-01-20, 1982-01-23, 1982-01-25, 1982-01-27, 1982-01-30, 1982-01-31, 1982-02-02, 1982-02-05, 1982-02-06, 1982-02-10, 1982-02-13, 1982-02-14, 1982-02-16, 1982-02-19, 1982-02-20, 1982-02-22, 1982-02-24, 1982-02-27, 1982-02-28, 1982-03-03, 1982-03-06, 1982-03-07, 1982-03-10, 1982-03-13, 1982-03-14, 1982-03-16, 1982-03-18, 1982-03-20, 1982-03-21, 1982-03-24, 1982-03-27, 1982-03-28, 1982-03-30, 1982-03-31, 1982-04-03, 1982-04-04
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1985-10-10, 1985-10-12, 1985-10-15, 1985-10-17, 1985-10-19, 1985-10-23, 1985-10-24, 1985-10-26, 1985-10-29, 1985-10-30, 1985-11-02, 1985-11-05, 1985-11-07, 1985-11-09, 1985-11-13, 1985-11-16, 1985-11-19, 1985-11-21, 1985-11-23, 1985-11-27, 1985-11-29, 1985-11-30, 1985-12-04, 1985-12-07, 1985-12-11, 1985-12-13, 1985-12-14, 1985-12-16, 1985-12-18, 1985-12-19, 1985-12-21, 1985-12-23, 1985-12-26, 1985-12-28, 1985-12-29, 1985-12-31, 1986-01-02, 1986-01-04, 1986-01-07, 1986-01-10, 1986-01-12, 1986-01-15, 1986-01-17, 1986-01-18, 1986-01-20, 1986-01-23, 1986-01-25, 1986-01-27, 1986-01-29, 1986-02-01, 1986-02-02, 1986-02-06, 1986-02-08, 1986-02-09, 1986-02-11, 1986-02-14, 1986-02-15, 1986-02-18, 1986-02-19, 1986-02-22, 1986-02-23, 1986-02-26, 1986-03-01, 1986-03-02, 1986-03-05, 1986-03-07, 1986-03-08, 1986-03-10, 1986-03-13, 1986-03-15, 1986-03-18, 1986-03-19, 1986-03-22, 1986-03-23, 1986-03-26, 1986-03-29, 1986-04-01, 1986-04-03, 1986-04-05, 1986-04-06
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1991-10-16, 1991-10-19, 1991-10-23, 1991-10-26, 1991-10-27, 1991-10-30, 1991-11-01, 1991-11-02, 1991-11-06, 1991-11-09, 1991-11-10, 1991-11-12
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1991-03-10, 1991-03-12, 1991-03-14, 1991-03-16, 1991-03-17, 1991-03-19, 1991-03-23, 1991-03-25, 1991-03-27, 1991-03-30, 1991-03-31
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-03-19, 1989-03-22, 1989-03-23, 1989-03-25, 1989-03-28, 1989-03-30, 1989-04-01, 1989-04-02
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1987-11-01, 1987-12-19, 1987-12-22, 1988-01-16, 1988-01-19, 1988-01-23, 1988-01-30, 1988-12-23, 1989-01-02
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ##    mostPointsOneGame mostPointsOneSeason
    ## 1                  0                   0
    ## 2                  1                   1
    ## 3                  0                   0
    ## 4                  1                   2
    ## 5                  0                   0
    ## 6                  0                   0
    ## 7                  0                   0
    ## 8                  0                   0
    ## 9                  1                   1
    ## 10                 1                   1
    ## 11                 0                   0
    ## 12                 1                   1
    ## 13                 0                   0
    ## 14                 1                   1
    ## 15                 0                   0
    ## 16                 0                   0
    ## 17                 0                   0
    ## 18                 1                   4
    ## 19                 0                   0
    ## 20                 1                   6
    ## 21                 0                   0
    ## 22                 1                   1
    ## 23                 2                   3
    ## 24                 0                   0
    ## 25                 1                   3
    ## 26                 0                   0
    ## 27                 0                   0
    ## 28                 0                   0
    ## 29                 0                   0
    ## 30                 0                   0
    ## 31                 1                   7
    ## 32                 0                   0
    ##    mostPointsSeasonIds penaltyMinutes playerId points
    ## 1             19921993             68  8444893      0
    ## 2             19791980              2  8445015      1
    ## 3             19801981              0  8445090      0
    ## 4             19901991             14  8445195      2
    ## 5             19831984             25  8445211      0
    ## 6             19891990              7  8445567      0
    ## 7             19871988              0  8445623      0
    ## 8             19861987              0  8445682      0
    ## 9             19811982              2  8445715      1
    ## 10            19861987             92  8446071      1
    ## 11            19861987              0  8446203      0
    ## 12            19891990              6  8446257      1
    ## 13            19891990              2  8446527      0
    ## 14            19821983              0  8447180      1
    ## 15            19831984              0  8447757      0
    ## 16            19791980              6  8447911      0
    ## 17            19861987              2  8448075      0
    ## 18            19841985              6  8448272      4
    ## 19            19801981              4  8448447      0
    ## 20            19921993              7  8448471      6
    ## 21            19891990             43  8448623      0
    ## 22            19791980              4  8448906      1
    ## 23            19811982             11  8449353      3
    ## 24  19811982, 19831984              4  8449418      0
    ## 25            19931994             41  8449442      3
    ## 26            19811982              2  8449565      0
    ## 27            19851986              0  8449871      0
    ## 28            19911992             16  8449905      0
    ## 29            19901991              2  8450199      0
    ## 30            19881989              5  8450257      0
    ## 31            19871988             37  8450741      9
    ## 32            19791980              0  8451145      0
    ##    positionCode rookieGamesPlayed rookiePoints seasons
    ## 1             D                NA           NA       1
    ## 2             C                NA           NA       1
    ## 3             D                 3            0       1
    ## 4             D                 9            2       2
    ## 5             D                NA           NA       1
    ## 6             L                 1            0       1
    ## 7             D                NA           NA       1
    ## 8             L                 1            0       1
    ## 9             D                 4            1       2
    ## 10            R                20            1       2
    ## 11            R                 4            0       1
    ## 12            D                 6            1       2
    ## 13            L                NA           NA       1
    ## 14            C                 2            1       1
    ## 15            D                NA           NA       1
    ## 16            D                 6            0       1
    ## 17            R                NA           NA       1
    ## 18            C                13            4       1
    ## 19            D                 3            0       1
    ## 20            R                NA           NA       1
    ## 21            D                 9            0       1
    ## 22            D                NA           NA       1
    ## 23            C                NA           NA       1
    ## 24            D                 5            0       2
    ## 25            D                NA           NA       1
    ## 26            L                 7            0       1
    ## 27            C                 3            0       1
    ## 28            D                NA           NA       1
    ## 29            R                NA           NA       1
    ## 30            D                NA           NA       1
    ## 31            R                NA           NA       2
    ## 32            C                NA           NA       1
    ##  [ reached 'max' / getOption("max.print") -- omitted 455 rows ]

``` r
getFranchiseDetail()
```

    ## No encoding supplied: defaulting to UTF-8.

    ##    id active
    ## 1   1   TRUE
    ## 2   2  FALSE
    ## 3   3  FALSE
    ## 4   4  FALSE
    ## 5   5   TRUE
    ## 6   6   TRUE
    ## 7   7  FALSE
    ## 8   8  FALSE
    ## 9   9  FALSE
    ## 10 10   TRUE
    ## 11 11   TRUE
    ## 12 12   TRUE
    ## 13 13  FALSE
    ## 14 14   TRUE
    ## 15 15   TRUE
    ## 16 16   TRUE
    ## 17 17   TRUE
    ## 18 18   TRUE
    ## 19 19   TRUE
    ## 20 20   TRUE
    ## 21 21   TRUE
    ## 22 22   TRUE
    ## 23 23   TRUE
    ## 24 24   TRUE
    ## 25 25   TRUE
    ## 26 26   TRUE
    ## 27 27   TRUE
    ## 28 28   TRUE
    ## 29 29   TRUE
    ## 30 30   TRUE
    ## 31 31   TRUE
    ## 32 32   TRUE
    ## 33 33   TRUE
    ## 34 34   TRUE
    ## 35 35   TRUE
    ## 36 36   TRUE
    ## 37 37   TRUE
    ## 38 38   TRUE
    ## 39 39   TRUE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                captainHistory
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <ul class="striped-list">\r\n\t<li>Shea Weber: 2018-19 &ndash; Present</li>\r\n\t<li>Max Pacioretty: 2015-16 &ndash;&nbsp;2017-18</li>\r\n\t<li>(No Captain): 2014-15</li>\r\n\t<li>Brian Gionta: 2010-11 &ndash;&nbsp;2013-14</li>\r\n\t<li>(No Captain): 2009-10</li>\r\n\t<li>Saku Koivu: 1999-00 &ndash;&nbsp;2008-09</li>\r\n\t<li>Vincent Damphousse: 1997-98 &ndash;&nbsp;1998-99</li>\r\n\t<li>Pierre Turgeon and Vincent Damphousse: 1996-97</li>\r\n\t<li>Mike Keane and Pierre Turgeon: 1995-96</li>\r\n\t<li>Kirk Muller and Mike Keane: 1994-95</li>\r\n\t<li>Guy Carbonneau: 1990-91 &ndash;&nbsp;1993-94</li>\r\n\t<li>Guy Carbonneau and Chris Chelios: 1989-90</li>\r\n\t<li>Bob Gainey: 1981-82 &ndash;&nbsp;1988-89</li>\r\n\t<li>Serge Savard: 1979-80 &ndash;&nbsp;1980-81</li>\r\n\t<li>Yvan Cournoyer and Serge Savard: 1978-79</li>\r\n\t<li>Yvan Cournoyer: 1975-76 &ndash;&nbsp;1977-78</li>\r\n\t<li>Henri Richard: 1971-72 &ndash;&nbsp;1974-75</li>\r\n\t<li>Jean Beliveau: 1961-62 &ndash;&nbsp;1970-71</li>\r\n\t<li>Doug Harvey: 1960-61</li>\r\n\t<li>Maurice Richard: 1956-57 &ndash;&nbsp;1959-60</li>\r\n\t<li>Butch Bouchard: 1948-49 &ndash;&nbsp;1955-56</li>\r\n\t<li>Toe Blake and Bill Durnan: 1947-48</li>\r\n\t<li>Toe Blake: 1940-41 &ndash;&nbsp;1946-47</li>\r\n\t<li>Walt Buswell: 1939-40</li>\r\n\t<li>Babe Siebert: 1936-37 &ndash;&nbsp;1938-39</li>\r\n\t<li>Sylvio Mantha: 1933-34 &ndash;&nbsp;1935-36</li>\r\n\t<li>George Hainsworth: 1932-33</li>\r\n\t<li>Sylvio Mantha: 1926-27 &ndash;&nbsp;1931-32</li>\r\n\t<li>Bill Coutu: 1925-26</li>\r\n\t<li>Sprague Cleghorn: 1922-23 &ndash;&nbsp;1924-25</li>\r\n\t<li>Newsy Lalonde: 1916-17&nbsp;&ndash;&nbsp;1921-22</li>\r\n</ul>\r\n
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <NA>
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <NA>
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <NA>
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <ul class="striped-list">\r\n\t<li>John Tavares: 2019-20 &ndash;&nbsp;Present</li>\r\n\t<li>(No Captain): 2016-17 &ndash;&nbsp;2018-19</li>\r\n\t<li>Dion Phaneuf and (No Captain): 2015-16</li>\r\n\t<li>Dion Phaneuf: 2010-11 &ndash;&nbsp;2014-15</li>\r\n\t<li>(No Captain): 2008-09 &ndash;&nbsp;2009-10</li>\r\n\t<li>Mats Sundin: 1997-98 &ndash;&nbsp;2007-08</li>\r\n\t<li>Doug Gilmour: 1994-95 &ndash;&nbsp;1996-97</li>\r\n\t<li>Wendel Clark: 1991-92 &ndash;&nbsp;1993-94</li>\r\n\t<li>Rob Ramage: 1989-90 &ndash;&nbsp;1990-91</li>\r\n\t<li>(No Captain): 1986-87 &ndash;&nbsp;1988-89</li>\r\n\t<li>Rick Vaive: 1981-82 &ndash;&nbsp;1985-86</li>\r\n\t<li>Darryl Sittler: 1975-76 &ndash;&nbsp;1980-81</li>\r\n\t<li>Dave Keon: 1969-70 &ndash;&nbsp;1974-75</li>\r\n\t<li>George Armstrong: 1957-58 &ndash;&nbsp;1968-69</li>\r\n\t<li>Jimmy Thomson and Ted Kennedy: 1956-57</li>\r\n\t<li>Sid Smith: 1955-56</li>\r\n\t<li>Ted Kennedy: 1948-49 &ndash;&nbsp;1954-55</li>\r\n\t<li>Syl Apps: 1945-46 &ndash;&nbsp;1947-48</li>\r\n\t<li>Bob Davidson: 1943-44 &ndash;&nbsp;1944-45</li>\r\n\t<li>Syl Apps: 1940-41 &ndash;&nbsp;1942-43</li>\r\n\t<li>Red Horner: 1938-39 &ndash;&nbsp;1939-40</li>\r\n\t<li>Charlie Conacher: 1937-38</li>\r\n\t<li>Hap Day: 1927-28 &ndash;&nbsp;1936-37</li>\r\n\t<li>Bert Corbeau: 1926-27</li>\r\n\t<li>Babe Dye: 1925-26</li>\r\n\t<li>John Ross Roach: 1924-25</li>\r\n\t<li>Jack Adams: 1923-24</li>\r\n\t<li>Reg Noble and Jack Adams: 1922-23</li>\r\n\t<li>Reg Noble: 1920-21 &ndash;&nbsp;1921-22</li>\r\n\t<li>Frank Heffernan: 1919-20</li>\r\n\t<li>Ken Randall: 1917-18 &ndash;&nbsp;1918-19</li>\r\n</ul>\r\n
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Patrice Bergeron: 2020-21 &ndash; Present</li>\r\n\t<li>Zdeno Chara: 2006-07 &ndash;&nbsp;2019-20</li>\r\n\t<li>Joe Thornton and (No Captain): 2005-06</li>\r\n\t<li>Joe Thornton: 2002-03 &ndash;&nbsp;2004-05</li>\r\n\t<li>(No Captain): 2001-02</li>\r\n\t<li>Jason Allison: 2000-01</li>\r\n\t<li>Ray&nbsp;Bourque: 1988-89 &ndash;&nbsp;1999-00</li>\r\n\t<li>Ray&nbsp;Bourque and Rick Middleton (Co-Captains): 1985-86 &ndash;&nbsp;1987-88</li>\r\n\t<li>Terry O&rsquo;Reilly: 1983-84 &ndash;&nbsp;1984-85</li>\r\n\t<li>Wayne Cashman: 1977-78 &ndash;&nbsp;1982-83</li>\r\n\t<li>Johnny Bucyk: 1973-74 &ndash;&nbsp;1976-77</li>\r\n\t<li>(No Captain): 1967-68 &ndash;&nbsp;1972-73</li>\r\n\t<li>Johnny Bucyk: 1966-67</li>\r\n\t<li>Leo Boivin: 1963-64 &ndash;&nbsp;1965-66</li>\r\n\t<li>Don McKenney: 1961-62 &ndash;&nbsp;1962-63</li>\r\n\t<li>Fern Flaman: 1955-56 &ndash;&nbsp;1960-61</li>\r\n\t<li>Milt Schmidt and Ed Sanford: 1954-55</li>\r\n\t<li>Milt Schmidt: 1950-51 &ndash;&nbsp;1953-54</li>\r\n\t<li>John Crawford: 1947-48 &ndash;&nbsp;1949-50</li>\r\n\t<li>Dit Clapper and John Crawford: 1946-47</li>\r\n\t<li>Dit Clapper: 1939-40 &ndash;&nbsp;1945-46</li>\r\n\t<li>Cooney Weiland: 1938-39</li>\r\n\t<li>Dit Clapper: 1932-33 &ndash;&nbsp;1937-38</li>\r\n\t<li>George Owen: 1931-32</li>\r\n\t<li>Lionel Hitchman: 1927-28 &ndash;&nbsp;1930-31</li>\r\n\t<li>Sprague Cleghorn: 1925-26&nbsp;&ndash;&nbsp;1926-27</li>\r\n\t<li>(No Captain): 1924-25</li>\r\n</ul>\r\n
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <NA>
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <NA>
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <NA>
    ## 10                                                                                                                                                                  <ul class="striped-list">\r\n\t<li>(No Captain): 2018-19 &ndash;&nbsp;Present</li>\r\n\t<li>Ryan McDonagh and (No Captain): 2017-18</li>\r\n\t<li>Ryan McDonagh: 2014-15 &ndash;&nbsp;2016-17</li>\r\n\t<li>Ryan Callahan and (No Captain): 2013-14</li>\r\n\t<li>Ryan Callahan: 2011-12 &ndash;&nbsp;2012-13</li>\r\n\t<li>Chris Drury: 2008-09 &ndash;&nbsp;2010-11</li>\r\n\t<li>Jaromir Jagr: 2006-07 &ndash;&nbsp;2007-08</li>\r\n\t<li>(No Captain): 2005-06</li>\r\n\t<li>Mark Messier: 2000-01 &ndash;&nbsp;2003-04</li>\r\n\t<li>Brian Leetch: 1997-98 &ndash;&nbsp;1999-00</li>\r\n\t<li>Mark Messier: 1991-92 &ndash;&nbsp;1996-97</li>\r\n\t<li>Kelly Kisio: 1988-89 &ndash;&nbsp;1990-91</li>\r\n\t<li>Ron Greschner and Kelly Kisio: 1987-88</li>\r\n\t<li>Ron Greschner: 1986-87</li>\r\n\t<li>Barry Beck: 1981-82 &ndash;&nbsp;1985-86</li>\r\n\t<li>Dave Maloney, Walt Tkaczuk and Barry Beck: 1980-81</li>\r\n\t<li>Dave Maloney: 1978-79 &ndash;&nbsp;1979-80</li>\r\n\t<li>Phil Esposito: 1976-77 &ndash;&nbsp;1977-78</li>\r\n\t<li>Brad Park and Phil Esposito: 1975-76</li>\r\n\t<li>Brad Park: 1974-75</li>\r\n\t<li>Vic Hadfield: 1971-72 &ndash;&nbsp;1973-74</li>\r\n\t<li>Bob Nevin: 1965-66 &ndash;&nbsp;1970-71</li>\r\n\t<li>Camille Henry and Bob Nevin: 1964-65</li>\r\n\t<li>Andy Bathgate and Camille Henry: 1963-64</li>\r\n\t<li>Andy Bathgate: 1961-62 &ndash;&nbsp;1962-63</li>\r\n\t<li>Red Sullivan: 1957-58 &ndash;&nbsp;1960-61</li>\r\n\t<li>Harry Howell: 1955-56 &ndash;&nbsp;1956-57</li>\r\n\t<li>Don Raleigh: 1954-55</li>\r\n\t<li>Allan Stanley and Don Raleigh: 1953-54</li>\r\n\t<li>Allan Stanley: 1952-53</li>\r\n\t<li>Frank Eddolls and Allan Stanley: 1951-52</li>\r\n\t<li>Frank Eddolls: 1950-51</li>\r\n\t<li>Buddy O&rsquo;Connor: 1949-50</li>\r\n\t<li>Neil Colville: 1945-46 &ndash;&nbsp;1948-49</li>\r\n\t<li>Ott Heller: 1942-43 &ndash;&nbsp;1944-45</li>\r\n\t<li>Art Coulter: 1937-38 &ndash;&nbsp;1941-42</li>\r\n\t<li>Bill Cook: 1926-27 &ndash;&nbsp;1936-37</li>\r\n</ul>\r\n
    ## 11 <ul class="striped-list">\r\n\t<li>Jonathan Toews: 2008-09 &ndash;&nbsp;Present</li>\r\n\t<li>(No Captain): 2007-08</li>\r\n\t<li>Adrian Aucoin and Martin Lapointe: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Alexei Zhamnov: 2002-03 &ndash;&nbsp;2003-04</li>\r\n\t<li>Tony Amonte: 2000-01 &ndash;&nbsp;2001-02</li>\r\n\t<li>Doug Gilmour: 1999-00</li>\r\n\t<li>Chris Chelios: 1995-96 &ndash;&nbsp;1998-99</li>\r\n\t<li>Dirk Graham: 1989-90 &ndash;&nbsp;1994-95</li>\r\n\t<li>Denis Savard and Dirk Graham: 1988-89</li>\r\n\t<li>(No Captain): 1987-88</li>\r\n\t<li>Darryl Sutter: 1986-87</li>\r\n\t<li>Darryl Sutter and Bob Murray: 1985-86</li>\r\n\t<li>Darryl Sutter: 1982-83 &ndash;&nbsp;1984-85</li>\r\n\t<li>Terry Ruskowski: 1980-81 &ndash; 1981-82</li>\r\n\t<li>Keith Magnuson and Terry Ruskowski: 1979-80</li>\r\n\t<li>Keith Magnuson: 1977-78 &ndash;&nbsp;1978-79</li>\r\n\t<li>Stan Mikita, Pit Martin&nbsp;and Keith Magnuson: 1976-77</li>\r\n\t<li>Stan Mikita and Pit Martin: 1975-76</li>\r\n\t<li>(No Captain): 1970-71 &ndash;&nbsp;1974-75</li>\r\n\t<li>Pat Stapleton: 1969-70</li>\r\n\t<li>(No Captain): 1968-69</li>\r\n\t<li>Pierre Pilote: 1961-62 &ndash;&nbsp;1967-68</li>\r\n\t<li>Ed Litzenberger: 1958-59 &ndash;&nbsp;1960-61</li>\r\n\t<li>(No Captain): 1957-58</li>\r\n\t<li>Gus Mortson: 1954-55 &ndash;&nbsp;1956-57</li>\r\n\t<li>Bill Gadsby: 1952-53 &ndash;&nbsp;1953-54</li>\r\n\t<li>Jack Stewart: 1950-51 &ndash;&nbsp;1951-52</li>\r\n\t<li>Doug Bentley: 1949-50</li>\r\n\t<li>Gaye Stewart: 1948-49</li>\r\n\t<li>John Mariucci: 1947-48</li>\r\n\t<li>Red Hamill: 1946-47</li>\r\n\t<li>John Mariucci: 1945-46</li>\r\n\t<li>Clint Smith: 1944-45</li>\r\n\t<li>Doug Bentley: 1942-43 &ndash;&nbsp;1943-44</li>\r\n\t<li>Earl Seibert: 1940-41 &ndash;&nbsp;1941-42</li>\r\n\t<li>Johnny Gottselig: 1935-36 &ndash;&nbsp;1939-40</li>\r\n\t<li>(No Captain): 1934-35</li>\r\n\t<li>Charlie Gardiner: 1933-34</li>\r\n\t<li>Helge Bostrom and Teddy Graham: 1932-33</li>\r\n\t<li>Cy Wentworth: 1931-32</li>\r\n\t<li>Ty Arbour: 1930-31</li>\r\n\t<li>Duke Dukowski: 1929-30</li>\r\n\t<li>Dick Irvin: 1926-27 &ndash;&nbsp;1928-29</li>\r\n</ul>\r\n
    ## 12                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Dylan Larkin: 2020-21 &ndash; Present</li>\r\n\t<li>(No Captain): 2018-19 &ndash;&nbsp;2019-20</li>\r\n\t<li>Henrik Zetterberg: 2012-13 &ndash;&nbsp;2017-18</li>\r\n\t<li>Nicklas Lidstrom: 2006-07 &ndash;&nbsp;2011-12</li>\r\n\t<li>Steve Yzerman: 1986-87 &ndash;&nbsp;2005-06</li>\r\n\t<li>Danny Gare: 1982-83 &ndash;&nbsp;1985-86</li>\r\n\t<li>Reed Larson: 1981-82</li>\r\n\t<li>Errol Thompson and Reed Larson: 1980-81</li>\r\n\t<li>Dale McCourt: 1979-80</li>\r\n\t<li>Dennis Hextall, Nick Libett and Paul Woods: 1978-79</li>\r\n\t<li>Dan Maloney and Dennis Hextall: 1977-78</li>\r\n\t<li>Danny Grant and Dennis Polonich: 1976-77</li>\r\n\t<li>Danny Grant and Terry Harper: 1975-76</li>\r\n\t<li>Marcel Dionne: 1974-75</li>\r\n\t<li>Alex Delvecchio, Nick Libett, Red Berenson, Gary Bergman, Ted Harris, Mickey Redmond and Larry Johnston: 1973-74</li>\r\n\t<li>Alex Delvecchio: 1962-63 &ndash;&nbsp;1972-73</li>\r\n\t<li>Gordie Howe: 1958-59 &ndash;&nbsp;1961-62</li>\r\n\t<li>Red Kelly: 1956-57 &ndash;&nbsp;1957-58</li>\r\n\t<li>Ted Lindsay: 1952-53 &ndash;&nbsp;1955-56</li>\r\n\t<li>Sid Abel: 1946-47 &ndash;&nbsp;1951-52</li>\r\n\t<li>Flash Hollett and Sid Abel: 1945-46</li>\r\n\t<li>Flash Hollett: 1944-45</li>\r\n\t<li>Mud Bruneteau and&nbsp;Flash Hollett: 1943-44</li>\r\n\t<li>Sid Abel: 1942-43</li>\r\n\t<li>Ebbie Goodfellow and Syd Howe: 1941-42</li>\r\n\t<li>Ebbie Goodfellow: 1938-39 &ndash;&nbsp;1940-41</li>\r\n\t<li>Doug Young: 1935-36 &ndash;&nbsp;1937-38</li>\r\n\t<li>Ebbie Goodfellow: 1934-35</li>\r\n\t<li>Herbie Lewis: 1933-34</li>\r\n\t<li>Larry Aurie: 1932-33</li>\r\n\t<li>Carson Cooper: 1931-32</li>\r\n\t<li>George Hay: 1930-31</li>\r\n\t<li>Reg Noble: 1927-28 &ndash;&nbsp;1929-30</li>\r\n\t<li>Art Duncan: 1926-27</li>\r\n</ul>\r\n
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <NA>
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <ul class="striped-list">\r\n\t<li>Anze Kopitar: 2016-17 &ndash;&nbsp;Present</li>\r\n\t<li>Dustin Brown: 2008-09 &ndash;&nbsp;2015-16</li>\r\n\t<li>Rob Blake: 2007-08</li>\r\n\t<li>Mattias Norstrom: 2001-02 &ndash;&nbsp;2006-07</li>\r\n\t<li>Rob Blake: 1996-97 &ndash;&nbsp;2000-01</li>\r\n\t<li>Wayne Gretzky and Rob Blake: 1995-96</li>\r\n\t<li>Wayne Gretzky: 1993-94 &ndash;&nbsp;1994-95</li>\r\n\t<li>Wayne Gretzky and Luc Robitaille: 1992-93</li>\r\n\t<li>Wayne Gretzky: 1989-90 &ndash;&nbsp;1991-92</li>\r\n\t<li>Dave Taylor: 1985-86 &ndash;&nbsp;1988-89</li>\r\n\t<li>Terry Ruskowski: 1983-84 &ndash;&nbsp;1984-85</li>\r\n\t<li>Dave Lewis: 1981-82 &ndash;&nbsp;1982-83</li>\r\n\t<li>Mike Murphy: 1975-76 &ndash;&nbsp;1980-81</li>\r\n\t<li>Terry Harper: 1973-74 &ndash;&nbsp;1974-75</li>\r\n\t<li>Bob Pulford: 1971-72 &ndash;&nbsp;1972-73</li>\r\n\t<li>Larry Cahan: 1969-70 &ndash;&nbsp;1970-71</li>\r\n\t<li>Bob Wall: 1967-68 &ndash;&nbsp;1968-69</li>\r\n</ul>\r\n
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>Jamie Benn: 2013-14 &ndash;&nbsp;Present</li>\r\n\t<li>Brenden Morrow: 2006-07 &ndash;&nbsp;2012-13</li>\r\n\t<li>Mike Modano: 2003-04 &ndash;&nbsp;2005-06</li>\r\n\t<li>Derian Hatcher: 1995-96 &ndash;&nbsp;2002-03</li>\r\n\t<li>Neal Broten and Derian Hatcher: 1994-95</li>\r\n\t<li>Mark Tinordi: 1991-92 &ndash;&nbsp;1993-94</li>\r\n\t<li>Curt Giles: 1989-90 &ndash;&nbsp;1990-91</li>\r\n\t<li>Curt Fraser, Bob Rouse and Curt Giles: 1988-89</li>\r\n\t<li>Craig Hartsburg: 1984-85 &ndash;&nbsp;1987-88</li>\r\n\t<li>Craig Hartsburg and Brian Bellows: 1983-84</li>\r\n\t<li>Craig Hartsburg: 1982-83</li>\r\n\t<li>Tim Young: 1981-82</li>\r\n\t<li>Paul Shmyr: 1979-80 &ndash;&nbsp;1980-81</li>\r\n\t<li>J.P. Parise: 1978-79</li>\r\n\t<li>Nick Beverley: 1977-78</li>\r\n\t<li>Bill Hogaboam: 1976-77</li>\r\n\t<li>Bill Goldsworthy: 1974-75 &ndash;&nbsp;1975-76</li>\r\n\t<li>Ted Harris: 1970-71 &ndash;&nbsp;1973-74</li>\r\n\t<li>Claude Larose: 1969-70</li>\r\n\t<li>Moose Vasko: 1968-69</li>\r\n\t<li>Bob Woytowich: 1967-68</li>\r\n</ul>\r\n
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <ul class="striped-list">\r\n\t<li>Claude Giroux: 2012-13 &ndash;&nbsp;Present</li>\r\n\t<li>Chris Pronger: 2011-12</li>\r\n\t<li>Mike Richards: 2008-09 &ndash;&nbsp;2010-11</li>\r\n\t<li>Jason Smith: 2007-08</li>\r\n\t<li>Peter Forsberg: 2006-07</li>\r\n\t<li>Keith Primeau and Derian Hatcher: 2005-06</li>\r\n\t<li>Keith Primeau: 2002-03 &ndash;&nbsp;2003-04</li>\r\n\t<li>Eric Desjardins and Keith Primeau: 2001-02</li>\r\n\t<li>Eric Desjardins: 2000-01</li>\r\n\t<li>Eric Lindros and Eric Desjardins: 1999-00</li>\r\n\t<li>Eric Lindros: 1994-95 &ndash;&nbsp;1998-99</li>\r\n\t<li>Kevin Dineen: 1993-94</li>\r\n\t<li>(No Captain): 1992-93</li>\r\n\t<li>Rick Tocchet: 1991-92</li>\r\n\t<li>Ron Sutter: 1990-91</li>\r\n\t<li>Dave Poulin and Ron Sutter: 1989-90</li>\r\n\t<li>Dave Poulin: 1984-85 &ndash;&nbsp;1988-89</li>\r\n\t<li>Bobby Clarke: 1983-84</li>\r\n\t<li>Bill Barber and Bobby Clarke: 1982-83</li>\r\n\t<li>Bill Barber: 1981-82</li>\r\n\t<li>Mel Bridgman: 1979-80 &ndash;&nbsp;1980-81</li>\r\n\t<li>Bobby Clarke: 1973-74 &ndash;&nbsp;1978-79</li>\r\n\t<li>Ed Van Impe and Bobby Clarke: 1972-73</li>\r\n\t<li>Ed Van Impe: 1968-69 &ndash;&nbsp;1971-72</li>\r\n\t<li>Lou Angotti: 1967-68</li>\r\n</ul>\r\n
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <ul class="striped-list">\r\n\t<li>Sidney Crosby: 2007-08 &ndash;&nbsp;Present</li>\r\n\t<li>(No Captain): 2006-07</li>\r\n\t<li>Mario Lemieux and (No Captain): 2005-06</li>\r\n\t<li>Mario Lemieux: 2001-02 &ndash;&nbsp;2004-05</li>\r\n\t<li>Jaromir Jagr: 1998-99 &ndash;&nbsp;2000-01</li>\r\n\t<li>Ron Francis: 1997-98</li>\r\n\t<li>Mario Lemieux: 1995-96 &ndash;&nbsp;1996-97</li>\r\n\t<li>Ron Francis: 1994-95</li>\r\n\t<li>Mario Lemieux: 1988-89 &ndash;&nbsp;1993-94</li>\r\n\t<li>Dan Frawley and Mario Lemieux: 1987-88</li>\r\n\t<li>Mike Bullard and Terry Ruskowski: 1986-87</li>\r\n\t<li>Mike Bullard: 1984-85 &ndash;&nbsp;1985-86</li>\r\n\t<li>Randy Carlyle: 1981-82 &ndash;&nbsp;1983-84</li>\r\n\t<li>Orest Kindrachuk: 1978-79 &ndash;&nbsp;1980-81</li>\r\n\t<li>Jean Pronovost: 1977-78</li>\r\n\t<li>Ron Schock: 1973-74 &ndash;&nbsp;1976-77</li>\r\n\t<li>(No Captain): 1969-70 &ndash;&nbsp;1972-73</li>\r\n\t<li>Earl Ingarfield and (No Captain): 1968-69</li>\r\n\t<li>Ab McDonald: 1967-68</li>\r\n</ul>\r\n
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>Ryan O&rsquo;Reilly: 2020-21 &ndash;&nbsp;Present</li>\r\n\t<li>Alex Pietrangelo: 2016-17 &ndash;&nbsp;2019-20</li>\r\n\t<li>David Backes: 2011-12 &ndash;&nbsp;2015-16</li>\r\n\t<li>Eric Brewer: 2007-08 &ndash;&nbsp;2010-11</li>\r\n\t<li>Dallas Drake: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Al MacInnis: 2002-03 &ndash;&nbsp;2003-04</li>\r\n\t<li>Chris Pronger: 1997-98 &ndash;&nbsp;2001-02</li>\r\n\t<li>(No Captain): 1996-97</li>\r\n\t<li>Brett Hull, Shayne Corson and Wayne Gretzky: 1995-96</li>\r\n\t<li>Brett Hull: 1992-93 &ndash;&nbsp;1994-95</li>\r\n\t<li>Garth Butcher: 1991-92</li>\r\n\t<li>Scott Stevens: 1990-91</li>\r\n\t<li>Rick Meagher: 1989-90</li>\r\n\t<li>Bernie Federko: 1988-89</li>\r\n\t<li>Brian Sutter: 1979-80 &ndash;&nbsp;1987-88</li>\r\n\t<li>Barry Gibbs: 1978-79</li>\r\n\t<li>Red Berenson: 1977-78</li>\r\n\t<li>(Captain Appointed on a Game-by-Game Basis): 1976-77</li>\r\n\t<li>Barclay Plager: 1972-73 &ndash;&nbsp;1975-76</li>\r\n\t<li>Barclay Plager and Jim Roberts: 1971-72</li>\r\n\t<li>Red Berenson and Barclay Plager: 1970-71</li>\r\n\t<li>Al Arbour: 1967-68 &ndash;&nbsp;1969-70</li>\r\n</ul>\r\n
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Jack Eichel: 2018-19 &ndash; Present</li>\r\n\t<li>(No Captain): 2017-18</li>\r\n\t<li>Brian Gionta: 2014-15 &ndash;&nbsp;2016-17</li>\r\n\t<li>(No Captain): 2013-14</li>\r\n\t<li>Jason Pominville: 2011-12 &ndash;&nbsp;2012-13</li>\r\n\t<li>Craig Rivet: 2008-09 &ndash;&nbsp;2010-11</li>\r\n\t<li>Jochen Hecht, Toni Lydman, Brian Campbell, Jaroslav Spacek and&nbsp;Jason Pominville: 2007-08</li>\r\n\t<li>Daniel Briere and Chris Drury: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Miroslav Satan, Chris Drury, James Patrick, J.P. Dumont and&nbsp;Daniel Briere: 2003-04</li>\r\n\t<li>Stu Barnes: 2001-02 &ndash;&nbsp;2002-03</li>\r\n\t<li>(No Captain): 2000-01</li>\r\n\t<li>Michael Peca: 1998-99 &ndash;&nbsp;1999-00</li>\r\n\t<li>Donald Audette and Michael Peca: 1997-98</li>\r\n\t<li>Pat LaFontaine: 1994-95 &ndash;&nbsp;1996-97</li>\r\n\t<li>Pat LaFontaine and Alexander Mogilny: 1993-94</li>\r\n\t<li>Mike Ramsey and Pat LaFontaine: 1992-93</li>\r\n\t<li>Mike Ramsey: 1991-92</li>\r\n\t<li>Mike Foligno and Mike Ramsey: 1990-91</li>\r\n\t<li>Mike Foligno: 1989-90</li>\r\n\t<li>Lindy Ruff and Mike Foligno: 1988-89</li>\r\n\t<li>Lindy Ruff: 1987-88</li>\r\n\t<li>Gilbert Perreault and Lindy Ruff: 1986-87</li>\r\n\t<li>Gilbert Perreault: 1982-83 &ndash;&nbsp;1985-86</li>\r\n\t<li>Danny Gare and Gilbert Perreault: 1981-82</li>\r\n\t<li>Danny Gare: 1977-78 &ndash;&nbsp;1980-81</li>\r\n\t<li>Jim Schoenfeld: 1975-76 &ndash;&nbsp;1976-77</li>\r\n\t<li>Gerry Meehan and Jim Schoenfeld: 1974-75</li>\r\n\t<li>Gerry Meehan: 1971-72 &ndash;&nbsp;1973-74</li>\r\n\t<li>Floyd Smith: 1970-71</li>\r\n</ul>\r\n
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>(No Captain) and Bo Horvat: 2019-20</li>\r\n\t<li>(No Captain): 2018-19</li>\r\n\t<li>Henrik Sedin: 2010-11 &ndash;&nbsp;2017-18</li>\r\n\t<li>Roberto Luongo: 2008-09 &ndash;&nbsp;2009-10</li>\r\n\t<li>Markus Naslund: 2000-01 &ndash;&nbsp;2007-08</li>\r\n\t<li>Mark Messier: 1997-98 &ndash;&nbsp;1999-00</li>\r\n\t<li>Trevor Linden: 1991-92 &ndash;&nbsp;1996-97</li>\r\n\t<li>Dan Quinn, Doug Lidster and Trevor Linden: 1990-91</li>\r\n\t<li>Stan Smyl: 1982-83 &ndash;&nbsp;1989-90</li>\r\n\t<li>Kevin McCarthy: 1979-80 &ndash;&nbsp;1981-82</li>\r\n\t<li>Don Lever: 1977-78 &ndash;&nbsp;1978-79</li>\r\n\t<li>Chris Oddleifson: 1976-77</li>\r\n\t<li>Andre Boudrias: 1975-76</li>\r\n\t<li>(No Captain): 1974-75</li>\r\n\t<li>Orland Kurtenbach: 1970-71 &ndash;&nbsp;1973-74</li>\r\n</ul>\r\n
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Mark Giordano: 2013-14 &ndash;&nbsp;Present</li>\r\n\t<li>Jarome Iginla: 2003-04 &ndash;&nbsp;2012-13</li>\r\n\t<li>Bob Boughner and Craig Conroy: 2002-03</li>\r\n\t<li>Dave Lowry,&nbsp;Bob Boughner and Craig Conroy: 2001-02</li>\r\n\t<li>Steve Smith&nbsp;and Dave Lowry: 2000-01</li>\r\n\t<li>Steve Smith: 1999-00</li>\r\n\t<li>Todd Simpson: 1997-98 &ndash; 1998-99</li>\r\n\t<li>Theo&nbsp;Fleury: 1995-96 &ndash; 1996-97</li>\r\n\t<li>Joe Nieuwendyk: 1991-92 &ndash;&nbsp;1994-95</li>\r\n\t<li>Alternating Captains: 1990-91</li>\r\n\t<li>Brad McCrimmon: 1989-90</li>\r\n\t<li>Lanny McDonald, Jim Peplinski and Tim Hunter: 1988-89</li>\r\n\t<li>Lanny McDonald and&nbsp;Jim Peplinski: 1987-88</li>\r\n\t<li>Lanny McDonald, Doug Risebrough and&nbsp;Jim Peplinski: 1984-85 &ndash;&nbsp;1986-87</li>\r\n\t<li>Lanny McDonald and&nbsp;Doug Risebrough: 1983-84</li>\r\n\t<li>Phil Russell: 1981-82 &ndash;&nbsp;1982-83</li>\r\n\t<li>Brad Marsh: 1980-81</li>\r\n\t<li>Jean Pronovost: 1979-80</li>\r\n\t<li>Tom Lysiak: 1977-78 &ndash;&nbsp;1978-79</li>\r\n\t<li>Pat Quinn: 1975-76 &ndash;&nbsp;1976-77</li>\r\n\t<li>Keith McCreary: 1972-73 &ndash;&nbsp;1974-75</li>\r\n</ul>\r\n
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Anders Lee: 2018-19 &ndash;&nbsp;Present</li>\r\n\t<li>John Tavares: 2013-14 &ndash;&nbsp;2017-18</li>\r\n\t<li>Mark Streit: 2011-12 &ndash;&nbsp;2012-13</li>\r\n\t<li>Doug Weight: 2009-10 &ndash;&nbsp;2010-11</li>\r\n\t<li>Bill Guerin and (No Captain): 2008-09</li>\r\n\t<li>Bill Guerin: 2007-08</li>\r\n\t<li>Alexei Yashin: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Michael Peca: 2001-02 &ndash;&nbsp;2003-04</li>\r\n\t<li>Kenny Jonsson: 1999-00 &ndash;&nbsp;2000-01</li>\r\n\t<li>Trevor Linden: 1998-99</li>\r\n\t<li>Bryan McCabe and Trevor Linden: 1997-98</li>\r\n\t<li>(No Captain): 1996-97</li>\r\n\t<li>Patrick Flatley: 1992-93 &ndash;&nbsp;1995-96</li>\r\n\t<li>Brent Sutter and Patrick Flatley: 1991-92</li>\r\n\t<li>Brent Sutter: 1987-88 &ndash;&nbsp;1990-91</li>\r\n\t<li>Denis Potvin: 1979-80 &ndash;&nbsp;1986-87</li>\r\n\t<li>Clark Gillies: 1977-78 &ndash;&nbsp;1978-79</li>\r\n\t<li>Ed Westfall and Clark Gillies: 1976-77</li>\r\n\t<li>Ed Westfall: 1972-73 &ndash;&nbsp;1975-76</li>\r\n</ul>\r\n
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <ul class="striped-list">\r\n\t<li>(No Captain) and Nico Hischier: 2020-21</li>\r\n\t<li>Andy Greene and (No Captain): 2019-20</li>\r\n\t<li>Andy Greene: 2015-16 &ndash;&nbsp;2018-19</li>\r\n\t<li>Bryce Salvador: 2012-13 &ndash;&nbsp;2014-15</li>\r\n\t<li>Zach Parise: 2011-12</li>\r\n\t<li>Jamie Langenbrunner: 2008-09 &ndash;&nbsp;2010-11</li>\r\n\t<li>Patrik Elias and Jamie Langenbrunner: 2007-08</li>\r\n\t<li>Patrik Elias: 2006-07</li>\r\n\t<li>(No Captain): 2005-06</li>\r\n\t<li>Scott Stevens and Scott Niedermayer: 2003-04</li>\r\n\t<li>Scott Stevens: 1992-93 &ndash;&nbsp;2002-03</li>\r\n\t<li>Bruce Driver: 1991-92</li>\r\n\t<li>Kirk Muller: 1987-88 &ndash;&nbsp;1990-91</li>\r\n\t<li>Mel Bridgman: 1984-85 &ndash;&nbsp;1986-87</li>\r\n\t<li>Don Lever and Mel Bridgman: 1983-84</li>\r\n\t<li>Don Lever: 1982-83</li>\r\n\t<li>Lanny McDonald and Rob Ramage: 1981-82</li>\r\n\t<li>Lanny McDonald: 1980-81</li>\r\n\t<li>Mike Christie, Rene Robert and Lanny McDonald: 1979-80</li>\r\n\t<li>Gary Croteau: 1978-79</li>\r\n\t<li>Wilf Paiement: 1977-78</li>\r\n\t<li>Simon Nolet: 1974-75 &ndash;&nbsp;1976-77</li>\r\n</ul>\r\n
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Alex Ovechkin: 2010-11 &ndash;&nbsp;Present</li>\r\n\t<li>Chris Clark and Alex Ovechkin: 2009-10</li>\r\n\t<li>Chris Clark: 2006-07 &ndash;&nbsp;2008-09</li>\r\n\t<li>Jeff Halpern: 2005-06</li>\r\n\t<li>Steve Konowalchuk and (No Captain): 2003-04</li>\r\n\t<li>Steve Konowalchuk: 2002-03</li>\r\n\t<li>Brendan Witt and Steve Konowalchuk: 2001-02</li>\r\n\t<li>Adam Oates: 1999-00 &ndash;&nbsp;2000-01</li>\r\n\t<li>Dale Hunter: 1994-95 &ndash;&nbsp;1998-99</li>\r\n\t<li>Kevin Hatcher: 1993-94</li>\r\n\t<li>Rod Langway and Kevin Hatcher: 1992-93</li>\r\n\t<li>Rod Langway: 1982-83 &ndash;&nbsp;1991-92</li>\r\n\t<li>Ryan Walter: 1979-80 &ndash;&nbsp;1981-82</li>\r\n\t<li>Guy Charron: 1978-79</li>\r\n\t<li>Yvon Labre: 1976-77 &ndash;&nbsp;1977-78</li>\r\n\t<li>Bill Clement and Yvon Labre: 1975-76</li>\r\n\t<li>Doug Mohns: 1974-75</li>\r\n</ul>\r\n
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <ul class="striped-list">\r\n\t<li>Connor McDavid: 2016-17 &ndash;&nbsp;Present</li>\r\n\t<li>(No Captain): 2015-16</li>\r\n\t<li>Andrew Ference: 2013-14 &ndash;&nbsp;2014-15</li>\r\n\t<li>Shawn Horcoff: 2010-11 &ndash;&nbsp;2012-13</li>\r\n\t<li>Ethan Moreau: 2007-08 &ndash;&nbsp;2009-10</li>\r\n\t<li>Jason Smith: 2001-02 &ndash;&nbsp;2006-07</li>\r\n\t<li>Doug Weight: 1999-00 &ndash;&nbsp;2000-01</li>\r\n\t<li>Kelly Buchberger: 1995-96 &ndash;&nbsp;1998-99</li>\r\n\t<li>Shayne Corson: 1994-95</li>\r\n\t<li>Craig MacTavish: 1992-93 &ndash;&nbsp;1993-94</li>\r\n\t<li>Kevin Lowe: 1991-92</li>\r\n\t<li>Mark Messier: 1988-89 &ndash;&nbsp;1990-91</li>\r\n\t<li>Wayne Gretzky: 1983-84 &ndash;&nbsp;1987-88</li>\r\n\t<li>Lee Fogolin&nbsp;Jr.: 1981-82 &ndash;&nbsp;1982-83</li>\r\n\t<li>Blair MacDonald and Lee Fogolin&nbsp;Jr.: 1980-81</li>\r\n\t<li>Ron Chipperfield: 1979-80</li>\r\n</ul>\r\n
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <ul class="striped-list">\r\n\t<li>Jordan Staal: 2019-20 &ndash; Present</li>\r\n\t<li>Justin Williams: 2018-19</li>\r\n\t<li>Justin Faulk and Jordan Staal: 2017-18</li>\r\n\t<li>(No Captain): 2016-17</li>\r\n\t<li>Eric Staal: 2010-11 &ndash;&nbsp;2015-16</li>\r\n\t<li>Rod Brind&rsquo;Amour and Eric Staal: 2009-10</li>\r\n\t<li>Rod Brind&rsquo;Amour: 2005-06 &ndash;&nbsp;2008-09</li>\r\n\t<li>Ron Francis: 2000-01 &ndash;&nbsp;2003-04</li>\r\n\t<li>Keith Primeau and Ron Francis: 1999-00</li>\r\n\t<li>Keith Primeau: 1998-99</li>\r\n\t<li>Kevin Dineen: 1996-97 &ndash;&nbsp;1997-98</li>\r\n\t<li>Brendan Shanahan: 1995-96</li>\r\n\t<li>Pat Verbeek: 1992-93 &ndash;&nbsp;1994-95</li>\r\n\t<li>Randy Ladouceur: 1991-92</li>\r\n\t<li>Ron Francis: 1985-86 &ndash;&nbsp;1990-91</li>\r\n\t<li>Mark Johnson and Ron Francis: 1984-85</li>\r\n\t<li>Mark Johnson: 1983-84</li>\r\n\t<li>Russ Anderson: 1982-83</li>\r\n\t<li>Dave Keon: 1981-82</li>\r\n\t<li>Rick Ley and Mike Rogers: 1980-81</li>\r\n\t<li>Rick Ley: 1979-80</li>\r\n</ul>\r\n
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <ul class="striped-list">\r\n\t<li>Gabriel Landeskog: 2012-13 &ndash;&nbsp;Present</li>\r\n\t<li>Milan Hejduk: 2011-12</li>\r\n\t<li>Adam Foote: 2009-10 &ndash;&nbsp;2010-11</li>\r\n\t<li>Joe Sakic: 1992-93 &ndash;&nbsp;2008-09</li>\r\n\t<li>Mike Hough: 1991-92</li>\r\n\t<li>Joe Sakic and Steven Finn: 1990-91</li>\r\n\t<li>Peter Stastny: 1986-87 &ndash;&nbsp;1989-90</li>\r\n\t<li>Mario Marois and Peter Stastny: 1985-86</li>\r\n\t<li>Mario Marois: 1982-83 &ndash;&nbsp;1984-85</li>\r\n\t<li>Robbie Ftorek and Andre Dupont: 1981-82</li>\r\n\t<li>Marc Tardif: 1979-80 &ndash;&nbsp;1980-81</li>\r\n</ul>\r\n
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <ul class="striped-list">\r\n\t<li>Oliver Ekman-Larsson: 2018-19 &ndash;&nbsp;Present</li>\r\n\t<li>(No Captain): 2017-18</li>\r\n\t<li>Shane Doan: 2003-04 &ndash;&nbsp;2016-17</li>\r\n\t<li>Teppo Numminen: 2001-02 &ndash;&nbsp;2002-03</li>\r\n\t<li>Keith Tkachuk: 1996-97 &ndash;&nbsp;2000-01</li>\r\n\t<li>Kris King: 1995-96</li>\r\n\t<li>Keith Tkachuk: 1994-95</li>\r\n\t<li>Dean Kennedy and Keith Tkachuk: 1993-94</li>\r\n\t<li>Troy Murray and Dean Kennedy: 1992-93</li>\r\n\t<li>Troy Murray: 1991-92</li>\r\n\t<li>Randy Carlyle and Thomas Steen (Co-Captains): 1990-91</li>\r\n\t<li>Randy Carlyle, Dale Hawerchuk and Thomas Steen (Tri-Captains): 1989-90</li>\r\n\t<li>Dale Hawerchuk: 1984-85 &ndash;&nbsp;1988-89</li>\r\n\t<li>Lucien DeBlois: 1983-84</li>\r\n\t<li>Dave Christian and Lucien DeBlois: 1982-83</li>\r\n\t<li>Dave Christian and Barry Long: 1981-82</li>\r\n\t<li>Morris Lukowich and Scott Campbell: 1980-81</li>\r\n\t<li>Lars-Erik Sjoberg: 1979-80</li>\r\n</ul>\r\n
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Logan Couture: 2019-20 &ndash; Present</li>\r\n\t<li>Joe Pavelski: 2015-16 &ndash;&nbsp;2018-19</li>\r\n\t<li>(No Captain): 2014-15</li>\r\n\t<li>Joe Thornton: 2010-11 &ndash;&nbsp;2013-14</li>\r\n\t<li>Rob Blake: 2009-10</li>\r\n\t<li>Patrick Marleau: 2005-06 &ndash;&nbsp;2008-09</li>\r\n\t<li>Mike Ricci, Vincent Damphousse, Alyn McCauley and&nbsp;Patrick Marleau: 2003-04</li>\r\n\t<li>Owen Nolan: 1998-99 &ndash;&nbsp;2002-03</li>\r\n\t<li>Todd Gill: 1996-97 &ndash;&nbsp;1997-98</li>\r\n\t<li>Jeff Odgers: 1995-96</li>\r\n\t<li>Bob Errey and Jeff Odgers: 1994-95</li>\r\n\t<li>Bob Errey: 1993-94</li>\r\n\t<li>Doug Wilson: 1991-92 &ndash;&nbsp;1992-93</li>\r\n</ul>\r\n
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <ul class="striped-list">\r\n\t<li>(No Captain): 2018-19&nbsp;&ndash; Present</li>\r\n\t<li>Erik Karlsson: 2014-15 &ndash;&nbsp;2017-18</li>\r\n\t<li>Jason Spezza: 2013-14</li>\r\n\t<li>Daniel Alfredsson: 1999-00 &ndash;&nbsp;2012-13</li>\r\n\t<li>Alexei Yashin: 1998-99</li>\r\n\t<li>Randy Cunneyworth: 1994-95 &ndash;&nbsp;1997-98</li>\r\n\t<li>Brad Shaw, Mark Lamb and Gord Dineen: 1993-94</li>\r\n\t<li>Laurie Boschman: 1992-93</li>\r\n</ul>\r\n
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <ul class="striped-list">\r\n\t<li>Steven Stamkos: 2014-15 &ndash;&nbsp;Present</li>\r\n\t<li>Martin St. Louis and Steven Stamkos: 2013-14</li>\r\n\t<li>Vincent Lecavalier: 2008-09 &ndash;&nbsp;2012-13</li>\r\n\t<li>Tim Taylor: 2006-07 &ndash;&nbsp;2007-08</li>\r\n\t<li>Dave Andreychuk and (No Captain): 2005-06</li>\r\n\t<li>Dave Andreychuk: 2002-03 &ndash;&nbsp;2004-05</li>\r\n\t<li>(No Captain): 2001-02</li>\r\n\t<li>Vincent Lecavalier: 2000-01</li>\r\n\t<li>Bill Houlder, Chris Gratton and Vincent Lecavalier: 1999-00</li>\r\n\t<li>Rob Zamuner: 1998-99</li>\r\n\t<li>Paul Ysebaert and Mikael Renberg: 1997-98</li>\r\n\t<li>Paul Ysebaert: 1995-96 &ndash;&nbsp;1996-97</li>\r\n\t<li>(No Captain): 1992-93 &ndash;&nbsp;1994-95</li>\r\n</ul>\r\n
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <ul class="striped-list">\r\n\t<li>Ryan Getzlaf: 2010-11 &ndash;&nbsp;Present</li>\r\n\t<li>Scott Niedermayer: 2008-09 &ndash;&nbsp;2009-10</li>\r\n\t<li>Chris Pronger: 2007-08</li>\r\n\t<li>Scott Niedermayer: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Steve Rucchin: 2003-04</li>\r\n\t<li>Paul Kariya: 1998-99 &ndash;&nbsp;2002-03</li>\r\n\t<li>Paul Kariya and Teemu Selanne: 1997-98</li>\r\n\t<li>Paul Kariya: 1996-97</li>\r\n\t<li>Randy Ladouceur: 1994-95 &ndash;&nbsp;1995-96</li>\r\n\t<li>Troy Loney: 1993-94</li>\r\n</ul>\r\n
    ## 33                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <ul class="striped-list">\r\n\t<li>Aleksander Barkov: 2018-19 &ndash;&nbsp;Present</li>\r\n\t<li>Derek MacKenzie: 2016-17 &ndash;&nbsp;2017-18</li>\r\n\t<li>Willie Mitchell: 2014-15 &ndash;&nbsp;2015-16</li>\r\n\t<li>Ed Jovanovski: 2012-13 &ndash;&nbsp;2013-14</li>\r\n\t<li>(No Captain): 2011-12</li>\r\n\t<li>Bryan McCabe: 2009-10 &ndash;&nbsp;2010-11</li>\r\n\t<li>(No Captain): 2008-09</li>\r\n\t<li>Olli Jokinen: 2003-04 &ndash;&nbsp;2007-08</li>\r\n\t<li>(No Captain): 2002-03</li>\r\n\t<li>Pavel Bure: 2001-02</li>\r\n\t<li>Scott Mellanby: 1997-98 &ndash;&nbsp;2000-01</li>\r\n\t<li>Brian Skrudland: 1993-94 &ndash;&nbsp;1996-97</li>\r\n</ul>\r\n
    ## 34                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <ul class="striped-list">\r\n\t<li>Roman Josi: 2017-18 &ndash;&nbsp;Present</li>\r\n\t<li>Mike Fisher: 2016-17</li>\r\n\t<li>Shea Weber: 2010-11 &ndash;&nbsp;2015-16</li>\r\n\t<li>Jason Arnott: 2007-08 &ndash;&nbsp;2009-10</li>\r\n\t<li>Kimmo Timonen: 2006-07</li>\r\n\t<li>Greg Johnson: 2002-03 &ndash;&nbsp;2005-06</li>\r\n\t<li>Tom Fitzgerald: 1998-99 &ndash;&nbsp;2001-02</li>\r\n</ul>\r\n
    ## 35                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <ul class="striped-list">\r\n\t<li>Blake Wheeler: 2016-17 &ndash;&nbsp;Present</li>\r\n\t<li>Andrew Ladd: 2010-11 &ndash;&nbsp;2015-16</li>\r\n\t<li>Ilya Kovalchuk: 2009-10</li>\r\n\t<li>(No Captain) and Ilya Kovalchuk: 2008-09</li>\r\n\t<li>Bobby Holik: 2007-08</li>\r\n\t<li>Scott Mellanby: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Shawn McEachern: 2002-03 &ndash;&nbsp;2003-04</li>\r\n\t<li>Ray Ferraro: 2001-02</li>\r\n\t<li>Steve Staios: 2000-01</li>\r\n\t<li>Kelly Buchberger: 1999-00</li>\r\n</ul>\r\n
    ## 36                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <ul class="striped-list">\r\n\t<li>Nick Foligno and (No Captain): 2020-21</li>\r\n\t<li>Nick Foligno: 2015-16 &ndash;&nbsp;2019-20</li>\r\n\t<li>(No Captain): 2012-13 &ndash;&nbsp;2014-15</li>\r\n\t<li>Rick Nash: 2008-09 &ndash;&nbsp;2011-12</li>\r\n\t<li>Adam Foote and Rick Nash: 2007-08</li>\r\n\t<li>Adam Foote: 2006-07</li>\r\n\t<li>Luke Richardson and Adam Foote: 2005-06</li>\r\n\t<li>Luke Richardson: 2003-04</li>\r\n\t<li>Ray Whitney: 2002-03</li>\r\n\t<li>Lyle Odelein: 2000-01 &ndash;&nbsp;2001-02</li>\r\n</ul>\r\n
    ## 37                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <ul class="striped-list">\r\n\t<li>Jared Spurgeon: 2020-21 &ndash;&nbsp;Present</li>\r\n\t<li>Mikko Koivu: 2009-10 &ndash;&nbsp;2019-20</li>\r\n\t<li>Mikko Koivu, Kim Johnsson and Andrew Brunette: 2008-09</li>\r\n\t<li>Pavol Demitra, Brian Rolston, Mark Parrish, Nick Schultz and Marian Gaborik: 2007-08</li>\r\n\t<li>Brian Rolston, Keith Carney and Mark Parrish: 2006-07</li>\r\n\t<li>Alex Henry, Filip Kuba, Willie Mitchell, Brian Rolston and Wes Walz: 2005-06</li>\r\n\t<li>Brad Brown, Andrew Brunette, Richard Park, Brad Bombardir and Jim Dowd: 2003-04</li>\r\n\t<li>Brad Bombardir, Matt Johnson and Sergei Zholtok: 2002-03</li>\r\n\t<li>Jim Dowd, Filip Kuba, Brad Brown and Andrew Brunette: 2001-02</li>\r\n\t<li>Sean O&rsquo;Donnell, Scott Pellerin, Wes Walz, Brad Bombardir and Darby Hendrickson: 2000-01</li>\r\n</ul>\r\n
    ## 38                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <ul class="striped-list">\r\n\t<li>Mark Stone: 2020-21 &ndash; Present</li>\r\n\t<li>(No Captain): 2017-18 &ndash; 2019-20</li>\r\n</ul>\r\n
    ## 39                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <NA>
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       coachingHistory
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>Dominique Ducharme: Feb. 25, 2021&nbsp;&ndash; Present</li>\r\n\t<li>Claude Julien: Feb. 18, 2017 &ndash; Feb. 23, 2021</li>\r\n\t<li>Michel Therrien: Jan. 19, 2013 &ndash; Feb. 12, 2017</li>\r\n\t<li>Randy Cunneyworth: Dec. 17, 2011 &ndash; April 7, 2012</li>\r\n\t<li>Jacques Martin: Oct. 1, 2009 &ndash; Dec. 15, 2011</li>\r\n\t<li>Bob Gainey: March 10&nbsp;&ndash; April 22, 2009</li>\r\n\t<li>Guy Carbonneau: Oct. 6, 2006 &ndash; March 8, 2009</li>\r\n\t<li>Bob Gainey: Jan. 14&nbsp;&ndash; May 2, 2006</li>\r\n\t<li>Claude Julien: Jan. 18, 2003 &ndash; Jan. 11, 2006</li>\r\n\t<li>Michel Therrien: Nov. 21, 2000 &ndash; Jan. 16, 2003</li>\r\n\t<li>Alain Vigneault: Oct. 1, 1997 &ndash; Nov. 18, 2000</li>\r\n\t<li>Mario Tremblay: Oct. 21, 1995 &ndash; April 26, 1997</li>\r\n\t<li>Jacques Laperriere: Oct. 20, 1995</li>\r\n\t<li>Jacques Demers: Oct. 6, 1992 &ndash; Oct. 14, 1995</li>\r\n\t<li>Pat Burns: Oct. 6, 1988 &ndash; May 9, 1992</li>\r\n\t<li>Jean Perron: Oct. 10, 1985 &ndash; April 26, 1988</li>\r\n\t<li>Jacques Lemaire: Feb. 25, 1984 &ndash; May 2, 1985</li>\r\n\t<li>Bob Berry: Oct. 8, 1981 &ndash; Feb. 23, 1984</li>\r\n\t<li>Claude Ruel: Dec. 14, 1979 &ndash; April 11, 1981</li>\r\n\t<li>Bernie Geoffrion: Oct. 11&nbsp;&ndash; Dec. 11, 1979</li>\r\n\t<li>Scotty Bowman: Oct. 9, 1971 &ndash; May 21, 1979</li>\r\n\t<li>Al MacNeil: Dec. 3, 1970 &ndash; May 18, 1971</li>\r\n\t<li>Claude Ruel: Oct. 12, 1968 &ndash; Dec. 2, 1970</li>\r\n\t<li>Toe Blake: Oct. 6, 1955 &ndash; May 11, 1968</li>\r\n\t<li>Dick Irvin: Nov. 3, 1940 &ndash; April 14, 1955</li>\r\n\t<li>Pit Lepine: Nov. 5, 1939 &ndash; March 17, 1940</li>\r\n\t<li>Jules Dugal: Jan. 29&nbsp;&ndash; March 26, 1939^</li>\r\n\t<li>Cecil Hart: Nov. 7, 1936 &ndash; Jan. 24, 1939</li>\r\n\t<li>Sylvio Mantha: Nov. 12, 1935 &ndash; March 19, 1936</li>\r\n\t<li>Leo Dandurand: Jan. 1&nbsp;&ndash; March 26, 1935</li>\r\n\t<li>Newsy Lalonde: Nov. 12, 1932 &ndash; Dec. 29, 1934</li>\r\n\t<li>Cecil Hart: Nov. 16, 1926 &ndash; March 29, 1932</li>\r\n\t<li>Leo Dandurand: Jan. 11, 1922 &ndash; March 16, 1926</li>\r\n\t<li>Newsy Lalonde: Dec. 19, 1917 &ndash; Jan. 7, 1922</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n\t<li>^ <em>The Canadiens named Babe Siebert head coach in the summer of 1939, but he died before the 1939-40 season began</em></li>\r\n</ul>\r\n
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 5                                                                                                                                                                                                                                                                                               <ul class="striped-list">\r\n\t<li>Sheldon Keefe: Nov. 21, 2019 &ndash; Present</li>\r\n\t<li>Mike Babcock: Oct. 7, 2015 &ndash; Nov. 19, 2019</li>\r\n\t<li>Peter Horachek: Jan. 7&nbsp;&ndash; April 11, 2015</li>\r\n\t<li>Randy Carlyle: March 3, 2012 &ndash; Jan. 3, 2015</li>\r\n\t<li>Ron Wilson: Oct. 9, 2008 &ndash; Feb. 29, 2012</li>\r\n\t<li>Paul Maurice: Oct. 4, 2006 &ndash; April 5, 2008</li>\r\n\t<li>Pat Quinn: Oct. 10, 1998 &ndash; April 18, 2006</li>\r\n\t<li>Mike Murphy: Oct. 5, 1996 &ndash; April 19, 1998</li>\r\n\t<li>Nick Beverley: March 6&nbsp;&ndash; April 27, 1996</li>\r\n\t<li>Pat Burns: Oct. 7, 1992 &ndash; March 3, 1996</li>\r\n\t<li>Tom Watt: Oct. 27, 1990 &ndash; April 15, 1992</li>\r\n\t<li>Doug Carpenter: Oct. 5, 1989 &ndash; Oct. 25, 1990</li>\r\n\t<li>George Armstrong: Dec. 19, 1988 &ndash; April 2, 1989</li>\r\n\t<li>John Brophy: Oct. 9, 1986 &ndash; Dec. 17, 1988</li>\r\n\t<li>Dan Maloney: Oct. 11, 1984 &ndash; April 30, 1986</li>\r\n\t<li>Mike Nykoluk: Jan. 10, 1981 &ndash; April 1, 1984</li>\r\n\t<li>Joe Crozier: Oct. 11, 1980 &ndash; Jan. 7, 1981</li>\r\n\t<li>Punch Imlach: March 19&nbsp;&ndash; April 11, 1980</li>\r\n\t<li>Dick Duff: March 15-17, 1980</li>\r\n\t<li>Floyd Smith: Oct. 10, 1979 &ndash; March 12, 1980</li>\r\n\t<li>Roger Neilson: Oct. 13, 1977 &ndash; April 22, 1979</li>\r\n\t<li>Red Kelly: Oct. 10, 1973 &ndash; April 21, 1977</li>\r\n\t<li>John McLellan: Oct. 11, 1969 &ndash; April 1, 1973</li>\r\n\t<li>Punch Imlach: Nov. 29, 1958 &ndash; April 6, 1969</li>\r\n\t<li>Billy Reay: Oct. 8, 1957 &ndash; Nov. 27, 1958</li>\r\n\t<li>Howie Meeker: Oct. 11, 1956 &ndash; March 24, 1957</li>\r\n\t<li>King Clancy: Oct. 10, 1953 &ndash; March 29, 1956</li>\r\n\t<li>Joe Primeau: Oct. 14, 1950 &ndash; March 22, 1953</li>\r\n\t<li>Hap Day: Nov. 2, 1940 &ndash; April 9, 1950</li>\r\n\t<li>Dick Irvin: Dec. 1, 1931 &ndash; April 13, 1940</li>\r\n\t<li>Conn Smythe: Nov. 28, 1931</li>\r\n\t<li>Art Duncan: Nov. 18, 1930 &ndash; Nov. 26, 1931</li>\r\n\t<li>Conn Smythe: Nov. 15, 1927 &ndash; Nov. 15, 1930</li>\r\n\t<li>Alex Romeril: Feb. 17&nbsp;&ndash; March 26, 1927</li>\r\n\t<li>Mike Rodden: Feb. 12-15, 1927</li>\r\n\t<li>Charles Querrie: Nov. 17, 1926 &ndash; Feb. 10, 1927</li>\r\n\t<li>Eddie Powers: Nov. 29, 1924 &ndash; March 17, 1926</li>\r\n\t<li>Charles Querrie: Jan. 3, 1923 &ndash; March 5, 1924</li>\r\n\t<li>George O&#39;Donoghue: Dec. 17, 1921 &ndash; Dec. 30, 1922</li>\r\n\t<li>Frank Carroll: Dec. 22, 1920 &ndash; March 14, 1921</li>\r\n\t<li>Harvey Sproule: Feb. 4&nbsp;&ndash; March 13, 1920</li>\r\n\t<li>Frank Heffernan: Dec. 23, 1919 &ndash; Jan. 31, 1920</li>\r\n\t<li>Dick Carroll: Dec. 19, 1917 &ndash; Feb. 20, 1919</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <ul class="striped-list">\r\n\t<li>Bruce Cassidy: Feb. 9, 2017 &ndash; Present</li>\r\n\t<li>Claude Julien: Oct. 5, 2007 &ndash; Feb. 4, 2017</li>\r\n\t<li>Dave Lewis: Oct. 6, 2006 &ndash; April 7, 2007</li>\r\n\t<li>Mike Sullivan: Oct. 8, 2003 &ndash; April 15, 2006</li>\r\n\t<li>Mike O&rsquo;Connell: March 21&nbsp;&ndash; April 17, 2003</li>\r\n\t<li>Robbie Ftorek: Oct. 4, 2001 &ndash; March 18, 2003</li>\r\n\t<li>Mike Keenan: Oct. 26, 2000 &ndash; April 7, 2001</li>\r\n\t<li>Pat Burns: Oct. 2, 1997 &ndash; Oct. 20, 2000</li>\r\n\t<li>Steve Kasper: Oct. 7, 1995 &ndash; April 13, 1997</li>\r\n\t<li>Brian Sutter: Oct. 8, 1992 &ndash; May 14, 1995</li>\r\n\t<li>Rick Bowness: Oct. 3, 1991 &ndash; May 23, 1992</li>\r\n\t<li>Mike Milbury: Oct. 5, 1989 &ndash; May 11, 1991</li>\r\n\t<li>Terry O&rsquo;Reilly: Nov. 8, 1986 &ndash; April 25, 1989</li>\r\n\t<li>Butch Goring: Oct. 10, 1985 &ndash; Nov. 5, 1986</li>\r\n\t<li>Harry Sinden: Feb. 14&nbsp;&ndash; April 16, 1985</li>\r\n\t<li>Gerry Cheevers: Oct. 9, 1980 &ndash; Feb. 10, 1985</li>\r\n\t<li>Harry Sinden: March 22&nbsp;&ndash; April 22, 1980</li>\r\n\t<li>Fred Creighton: Oct. 11, 1979 &ndash; March 19, 1980</li>\r\n\t<li>Don Cherry: Oct. 10, 1974 &ndash; May 10, 1979</li>\r\n\t<li>Bep Guidolin: Feb. 7, 1973 &ndash; May 19, 1974</li>\r\n\t<li>Tom Johnson: Oct. 11, 1970 - Feb. 4, 1973</li>\r\n\t<li>Harry Sinden: Oct. 19, 1966 &ndash; May 10, 1970</li>\r\n\t<li>Milt Schmidt: Nov. 21, 1962 &ndash; April 3, 1966</li>\r\n\t<li>Phil Watson: Oct. 11, 1961 &ndash; Nov. 18, 1962</li>\r\n\t<li>Milt Schmidt: Dec. 25, 1954 &ndash; March 19, 1961</li>\r\n\t<li>Lynn Patrick: Oct. 14, 1950 &ndash; Dec. 19, 1954</li>\r\n\t<li>George Boucher: Oct. 12, 1949 &ndash; March 26, 1950</li>\r\n\t<li>Dit Clapper: Oct. 24, 1945 &ndash;&nbsp;March 30, 1949</li>\r\n\t<li>Art Ross: Nov. 8, 1941 &ndash; April 3, 1945</li>\r\n\t<li>Cooney Weiland: Nov. 4, 1939 &ndash; April 12, 1941</li>\r\n\t<li>Art Ross: Nov. 7, 1936 &ndash; April 16, 1939</li>\r\n\t<li>Frank Patrick: Nov. 8, 1934 &ndash; March 26, 1936</li>\r\n\t<li>Art Ross: Dec. 1, 1924 &ndash; March 18, 1934</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 10 <ul class="striped-list">\r\n\t<li>Gerard Gallant: Present</li>\r\n\t<li>David Quinn: Oct. 4, 2018 &ndash; May 8, 2021</li>\r\n\t<li>Alain Vigneault: Oct. 3, 2013 &ndash; April 7, 2018</li>\r\n\t<li>John Tortorella: Feb. 25, 2009 &ndash; May 25, 2013</li>\r\n\t<li>Tom Renney: Feb. 26, 2004 &ndash; Feb. 22, 2009</li>\r\n\t<li>Glen Sather: Jan. 30, 2003 &ndash; Feb. 23, 2004</li>\r\n\t<li>Bryan Trottier: Oct. 9, 2002 &ndash; Jan. 28, 2003</li>\r\n\t<li>Ron Low: Oct. 7, 2000 &ndash; April 13, 2002</li>\r\n\t<li>John Tortorella: April 1-9, 2000</li>\r\n\t<li>John Muckler: Feb. 26, 1998 &ndash; March 27, 2000</li>\r\n\t<li>Colin Campbell: Jan. 20, 1995 &ndash; Feb. 7, 1998</li>\r\n\t<li>Mike Keenan: Oct. 5, 1993 &ndash; June 14, 1994</li>\r\n\t<li>Ron Smith: Jan. 4&nbsp;&ndash; April 16, 1993</li>\r\n\t<li>Roger Neilson: Oct. 6, 1989 &ndash; Jan. 2, 1993</li>\r\n\t<li>Phil Esposito: April 1-9, 1989</li>\r\n\t<li>Michel Bergeron: Oct. 8, 1987 &ndash; March 29, 1989</li>\r\n\t<li>Phil Esposito: Jan. 26&nbsp;&ndash; April 16, 1987</li>\r\n\t<li>Tom Webster: Jan. 19-23, 1987</li>\r\n\t<li>Phil Esposito: Jan. 14, 1987</li>\r\n\t<li>Tom Webster: Jan. 5-12, 1987</li>\r\n\t<li>Phil Esposito: Dec. 21, 1986 &ndash; Jan. 3, 1987</li>\r\n\t<li>Tom Webster: Nov. 29&nbsp;&ndash; Dec. 20, 1986</li>\r\n\t<li>Phil Esposito: Nov. 21-26, 1986</li>\r\n\t<li>Ted Sator: Oct. 10, 1985 &ndash; Nov. 19, 1986</li>\r\n\t<li>Craig Patrick: Jan. 24&nbsp;&ndash; April 13, 1985</li>\r\n\t<li>Herb Brooks: Oct. 6, 1981 &ndash; Jan. 19, 1985</li>\r\n\t<li>Craig Patrick: Nov. 22, 1980 &ndash; May 5, 1981</li>\r\n\t<li>Fred Shero: Oct. 12, 1978 &ndash; Nov. 19, 1980</li>\r\n\t<li>Jean-Guy Talbot: Oct. 12, 1977 &ndash; April 15, 1978</li>\r\n\t<li>John Ferguson Sr.: Jan. 10, 1976 &ndash; April 3, 1977</li>\r\n\t<li>Ron Stewart: Oct. 8, 1975 &ndash; Jan. 6, 1976</li>\r\n\t<li>Emile Francis: Jan. 12, 1974 &ndash; April 11, 1975</li>\r\n\t<li>Larry Popein: Oct. 10, 1973 &ndash; Jan. 10, 1974</li>\r\n\t<li>Emile Francis: Jan. 18, 1969 &ndash; April 24, 1973</li>\r\n\t<li>Bernie Geoffrion: Oct. 13, 1968 &ndash; Jan. 17, 1969</li>\r\n\t<li>Emile Francis: Dec. 8, 1965 &ndash; April 16, 1968</li>\r\n\t<li>Red Sullivan: Dec. 30, 1962 &ndash; Dec. 5, 1965</li>\r\n\t<li>Muzz Patrick: Oct. 11&nbsp;&ndash; Dec. 27, 1962</li>\r\n\t<li>Doug Harvey: Oct. 11, 1961 &ndash; April 7, 1962</li>\r\n\t<li>Alf Pike: Nov. 18, 1959 &ndash; March 19, 1961</li>\r\n\t<li>Muzz Patrick: Nov. 14-15, 1959</li>\r\n\t<li>Phil Watson: Oct. 7, 1955 &ndash; Nov. 11, 1959</li>\r\n\t<li>Muzz Patrick: Jan. 13, 1954 &ndash; March 20, 1955</li>\r\n\t<li>Frank Boucher: Oct. 8, 1953 &ndash; Jan. 10, 1954</li>\r\n\t<li>Bill Cook: Dec. 9, 1951 &ndash; March 22, 1953</li>\r\n\t<li>Neil Colville: Oct. 11, 1950 &ndash; Dec. 5, 1951</li>\r\n\t<li>Lynn Patrick: Dec. 23, 1948 &ndash; April 23, 1950</li>\r\n\t<li>Frank Boucher: Nov. 5, 1939 &ndash; Dec. 19, 1948</li>\r\n\t<li>Lester Patrick: Nov. 16, 1926 &ndash; April 2, 1939</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 11                                                                                                                                                        <ul class="striped-list">\r\n\t<li>Jeremy Colliton: Nov. 8, 2018 &ndash; Present</li>\r\n\t<li>Joel Quenneville: Oct. 18, 2008 &ndash; Nov. 3, 2018</li>\r\n\t<li>Denis Savard: Nov. 29, 2006 &ndash; Oct. 15, 2008</li>\r\n\t<li>Trent Yawney: Oct. 5, 2005 &ndash; Nov. 24, 2006</li>\r\n\t<li>Brian Sutter: Oct. 4, 2001 &ndash; April 4, 2004</li>\r\n\t<li>Alpo Suhonen: Oct. 5, 2000 &ndash; April 8, 2001</li>\r\n\t<li>Bob Pulford: Dec. 3, 1999 &ndash; April 9, 2000</li>\r\n\t<li>Lorne Molleken: Feb. 24&nbsp;&ndash; Nov. 30, 1999</li>\r\n\t<li>Dirk Graham: Oct. 10, 1998 &ndash; Feb. 21, 1999</li>\r\n\t<li>Craig Hartsburg: Oct. 7, 1995 &ndash; April 18, 1998</li>\r\n\t<li>Darryl Sutter: Oct. 7, 1992 &ndash; June 11, 1995</li>\r\n\t<li>Mike Keenan: Oct. 6, 1988 &ndash; June 1, 1992</li>\r\n\t<li>Bob Murdoch: Oct. 8, 1987 &ndash; April 12, 1988</li>\r\n\t<li>Bob Pulford: Feb. 6, 1985 &ndash; April 12, 1987</li>\r\n\t<li>Orval Tessier: Oct. 6, 1982 &ndash; Feb. 3, 1985</li>\r\n\t<li>Bob Pulford: Feb. 17&nbsp;&ndash; May 6, 1982</li>\r\n\t<li>Keith Magnuson: Oct. 9, 1980 &ndash; Feb. 15, 1982</li>\r\n\t<li>Eddie Johnston: Oct. 10, 1979 &ndash; April 20, 1980</li>\r\n\t<li>Bob Pulford: Oct. 13, 1977 &ndash; April 22, 1979</li>\r\n\t<li>Bill White: Dec. 22, 1976 &ndash; April 7, 1977</li>\r\n\t<li>Billy Reay: Oct. 9, 1963 &ndash; Dec. 21, 1976</li>\r\n\t<li>Rudy Pilous: Dec. 31, 1957 &ndash; April 7, 1963</li>\r\n\t<li>Tommy Ivan: Oct. 11, 1956 &ndash; Dec. 29, 1957</li>\r\n\t<li>Dick Irvin: Oct. 6, 1955 &ndash; March 18, 1956</li>\r\n\t<li>Frank Eddolls: Oct. 7, 1954 &ndash; March 20, 1955</li>\r\n\t<li>Sid Abel: Oct. 9, 1952 &ndash; March 21, 1954</li>\r\n\t<li>Ebbie Goodfellow: Oct. 12, 1950 &ndash; March 23, 1952</li>\r\n\t<li>Charlie Conacher: Dec. 31, 1947 &ndash; March 26, 1950</li>\r\n\t<li>Johnny Gottselig: Feb. 27, 1945 &ndash; Dec. 27, 1947</li>\r\n\t<li>Paul Thompson: Jan. 5, 1939 &ndash; Feb. 25, 1945</li>\r\n\t<li>Bill Stewart: Nov. 4, 1937 &ndash; Jan. 1, 1939</li>\r\n\t<li>Clem Loughlin: Nov. 8, 1934 &ndash; March 21, 1937</li>\r\n\t<li>Tommy Gorman: Jan. 15, 1933 &ndash; April 10, 1934</li>\r\n\t<li>Emil Iverson: Nov. 12, 1931 &ndash; Jan. 12, 1933</li>\r\n\t<li>Godfrey Matheson^</li>\r\n\t<li>Dick Irvin: Nov. 16, 1930 &ndash; April 14, 1931</li>\r\n\t<li>Bill Tobin: Jan. 14&nbsp;&ndash; March 26, 1930</li>\r\n\t<li>Tom Shaughnessy: Nov. 14, 1929 &ndash; Jan. 12, 1930</li>\r\n\t<li>Dick Irvin: Feb. 14&nbsp;&ndash; March 14, 1929</li>\r\n\t<li>Herb Gardiner: Nov. 15, 1928 &ndash; Feb. 9, 1929</li>\r\n\t<li>Hugh Lehman: Jan. 21&nbsp;&ndash; March 21, 1928</li>\r\n\t<li>Barney Stanley: Nov. 15, 1927 &ndash; Jan. 18, 1928</li>\r\n\t<li>Pete Muldoon: Nov. 17, 1926 &ndash; March 31, 1927</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n\t<li>^ <i>Named coach Oct. 14, 1931, but replaced after training camp due to health reasons</i></li>\r\n</ul>\r\n
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Jeff Blashill: Oct. 9, 2015 &ndash; Present</li>\r\n\t<li>Mike Babcock: Oct. 5, 2005 &ndash; April 29, 2015</li>\r\n\t<li>Dave Lewis: Oct. 10, 2002 &ndash; May&nbsp;3, 2004</li>\r\n\t<li>Scotty Bowman: Oct. 23, 1998 &ndash; June 13, 2002</li>\r\n\t<li>Dave Lewis and Barry Smith (Co-Coaches): Oct. 10-21, 1998</li>\r\n\t<li>Scotty Bowman: Oct. 5, 1993 &ndash; June 16, 1998</li>\r\n\t<li>Bryan Murray: Oct. 4, 1990 &ndash; May 1, 1993</li>\r\n\t<li>Jacques Demers: Oct. 9, 1986 &ndash; April 1, 1990</li>\r\n\t<li>Brad Park: Dec. 31, 1985 &ndash; April 6, 1986</li>\r\n\t<li>Harry Neale: Oct 10&nbsp;&ndash; Dec. 29, 1985</li>\r\n\t<li>Nick Polano: Oct. 6, 1982 &ndash; April 13, 1985</li>\r\n\t<li>Billy Dea: March 11&nbsp;&ndash; April 4, 1982</li>\r\n\t<li>Wayne Maxner: Nov. 26, 1980 &ndash; March 8, 1982</li>\r\n\t<li>Ted Lindsay: March 21&nbsp;&ndash; Nov. 22, 1980</li>\r\n\t<li>Bobby Kromm: Oct. 13, 1977 &ndash; March 19, 1980</li>\r\n\t<li>Larry Wilson: Jan. 20&nbsp;&ndash; April 3, 1977</li>\r\n\t<li>Alex Delvecchio: Dec. 5, 1975 &ndash; Jan. 15, 1977</li>\r\n\t<li>Doug Barkley: Oct. 8&nbsp;&ndash; Dec. 3, 1975</li>\r\n\t<li>Alex Delvecchio: Nov. 7, 1973 &ndash; April 6, 1975</li>\r\n\t<li>Ted Garvin: Oct. 10&nbsp;&ndash; Nov. 4, 1973</li>\r\n\t<li>Johnny Wilson: Nov. 1, 1971 &ndash; April 1, 1973</li>\r\n\t<li>Doug Barkley: Jan. 9&nbsp;&ndash; Oct. 31, 1971</li>\r\n\t<li>Ned Harkness: Oct. 10, 1970 &ndash; Jan. 7, 1971</li>\r\n\t<li>Sid Abel: Oct. 16, 1969 &ndash; April 12, 1970</li>\r\n\t<li>Bill Gadsby: Oct. 11, 1968 &ndash; Oct. 15, 1969</li>\r\n\t<li>Sid Abel: Jan. 4, 1958 &ndash; March 31, 1968</li>\r\n\t<li>Jimmy Skinner: Oct. 7, 1954 &ndash; Jan. 1, 1958</li>\r\n\t<li>Tommy Ivan: Oct. 15, 1947 &ndash; April 16, 1954</li>\r\n\t<li>Jack Adams: Nov. 15, 1927 &ndash; April 5, 1947</li>\r\n\t<li>Duke Keats: Feb. 24&nbsp;&ndash; March 26, 1927</li>\r\n\t<li>Art Duncan: Nov. 18, 1926 &ndash; Feb. 22, 1927</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <ul class="striped-list">\r\n\t<li>Todd McLellan: Oct. 5, 2019 &ndash; Present</li>\r\n\t<li>Willie Desjardins: Nov. 6, 2018 &ndash; April 6, 2019</li>\r\n\t<li>John Stevens: Oct. 5, 2017 &ndash; Nov. 3, 2018</li>\r\n\t<li>Darryl Sutter: Dec. 22, 2011 &ndash; April 9, 2017</li>\r\n\t<li>John Stevens: Dec. 13-19, 2011</li>\r\n\t<li>Terry Murray: Oct. 11, 2008 &ndash; Dec. 10, 2011</li>\r\n\t<li>Marc Crawford: Oct. 6, 2006 &ndash; April 5, 2008</li>\r\n\t<li>John Torchetti: March 25&nbsp;&ndash; April 17, 2006</li>\r\n\t<li>Andy Murray: Oct. 2, 1999 &ndash; March 20, 2006</li>\r\n\t<li>Larry Robinson: Oct. 7, 1995 &ndash; April 18, 1999</li>\r\n\t<li>Rogie Vachon: April 21&nbsp;&ndash; May 3, 1995</li>\r\n\t<li>Barry Melrose: Oct. 6, 1992 &ndash; April 19, 1995</li>\r\n\t<li>Tom Webster: Oct. 5, 1989 &ndash; April 28, 1992</li>\r\n\t<li>Robbie Ftorek: Dec. 9, 1987 &ndash; April 24, 1989</li>\r\n\t<li>Rogie Vachon: Dec. 6,&nbsp;1987</li>\r\n\t<li>Mike Murphy: Jan. 10&nbsp;&ndash; Dec. 5, 1987</li>\r\n\t<li>Pat Quinn: Oct. 11, 1984 &ndash; Jan. 8, 1987</li>\r\n\t<li>Roger Neilson: Feb. 2&nbsp;&ndash; March 31, 1984</li>\r\n\t<li>Rogie Vachon: Jan. 27-28, 1984</li>\r\n\t<li>Don Perry: Jan. 12, 1982 &ndash; Jan. 25, 1984</li>\r\n\t<li>Parker MacDonald: Oct. 7, 1981 &ndash; Jan. 10, 1982</li>\r\n\t<li>Bob Berry: Oct. 11, 1978 &ndash; April 12, 1981</li>\r\n\t<li>Ron Stewart: Oct. 12, 1977 &ndash; April 13, 1978</li>\r\n\t<li>Bob Pulford: Oct. 7, 1972 &ndash; April 21, 1977</li>\r\n\t<li>Fred Glover: Oct. 30, 1971 &ndash; April 1, 1972</li>\r\n\t<li>Larry Regan: Oct. 9, 1970 &ndash; Oct. 27, 1971</li>\r\n\t<li>Johnny Wilson: Dec. 15, 1969 &ndash; April 4, 1970</li>\r\n\t<li>Hal Laycoe: Oct. 11&nbsp;&ndash; Dec. 13, 1969</li>\r\n\t<li>Red Kelly: Oct. 14, 1967 &ndash; April 20, 1969</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>Rick Bowness: Dec. 10, 2019 &ndash; Present</li>\r\n\t<li>Jim Montgomery: Oct. 4, 2018 &ndash; Dec. 7, 2019</li>\r\n\t<li>Ken Hitchcock: Oct. 6, 2017 &ndash; April 7, 2018</li>\r\n\t<li>Lindy Ruff: Oct. 3, 2013 &ndash; April 8, 2017</li>\r\n\t<li>Glen Gulutzan: Oct. 7, 2011 &ndash; April 27, 2013</li>\r\n\t<li>Marc Crawford: Oct. 3, 2009 &ndash; April 10, 2011</li>\r\n\t<li>Dave Tippett: Oct. 9, 2002 &ndash; April 10, 2009</li>\r\n\t<li>Rick Wilson: Jan. 25&nbsp;&ndash; April 14, 2002</li>\r\n\t<li>Ken Hitchcock: Jan. 10, 1996 &ndash; Jan. 23, 2002</li>\r\n\t<li>Bob Gainey: Oct. 4, 1990 &ndash; Jan. 8, 1996</li>\r\n\t<li>Pierre Page: Oct. 6, 1988 &ndash; April 16, 1990</li>\r\n\t<li>Herb Brooks: Oct. 8, 1987 &ndash; April 3, 1988</li>\r\n\t<li>Glen Sonmor: April 1-4, 1987</li>\r\n\t<li>Lorne Henning: Oct. 10, 1985 &ndash; March 30, 1987</li>\r\n\t<li>Glen Sonmor: Nov. 10, 1984 &ndash; April 30, 1985</li>\r\n\t<li>Bill Mahoney: Oct. 5, 1983 &ndash; Nov. 7, 1984</li>\r\n\t<li>Murray Oliver: Jan. 13&nbsp;&ndash; April 20, 1983</li>\r\n\t<li>Glen Sonmor: Nov. 8, 1978 &ndash; Jan. 12, 1983</li>\r\n\t<li>Harry Howell: Oct. 11&nbsp;&ndash; Nov. 7, 1978</li>\r\n\t<li>Lou Nanne: Feb. 11&nbsp;&ndash; April 9, 1978</li>\r\n\t<li>Andre Beaulieu: Nov. 26, 1977 &ndash; Feb. 8, 1978</li>\r\n\t<li>Ted Harris: Oct. 8, 1975 &ndash; Nov. 23, 1977</li>\r\n\t<li>Charlie Burns: Jan. 6&nbsp;&ndash; April 6, 1975</li>\r\n\t<li>Jack Gordon: Oct. 9, 1974 &ndash; Jan. 4, 1975</li>\r\n\t<li>Parker MacDonald: Nov. 21, 1973 &ndash; April 7, 1974</li>\r\n\t<li>Jack Gordon: Oct. 10, 1970 &ndash; Nov. 17, 1973</li>\r\n\t<li>Charlie Burns: Dec. 30, 1969 &ndash; April 16, 1970</li>\r\n\t<li>Wren Blair: Jan. 23&nbsp;&ndash; Dec. 27, 1969</li>\r\n\t<li>John Muckler: Nov. 6, 1968 &ndash; Jan. 19, 1969</li>\r\n\t<li>Wren Blair: Oct. 11, 1967 &ndash; Nov. 3, 1968</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Alain Vigneault: Oct. 4, 2019 &ndash; Present</li>\r\n\t<li>Scott Gordon: Dec. 18, 2018 &ndash; April 6, 2019</li>\r\n\t<li>Dave Hakstol: Oct. 8, 2015 &ndash; Dec. 15, 2018</li>\r\n\t<li>Craig Berube: Oct. 8, 2013 &ndash; April 11, 2015</li>\r\n\t<li>Peter Laviolette: Dec. 5, 2009 &ndash; Oct. 6, 2013</li>\r\n\t<li>John Stevens: Oct. 26, 2006 &ndash; Dec. 3, 2009</li>\r\n\t<li>Ken Hitchcock: Oct. 10, 2002 &ndash; Oct. 20, 2006</li>\r\n\t<li>Bill Barber: Dec. 10, 2000 &ndash; April 26, 2002</li>\r\n\t<li>Craig Ramsay: Oct. 5&nbsp;&ndash; Dec. 8, 2000</li>\r\n\t<li>Roger Neilson: March 10, 1998 &ndash; May 26, 2000</li>\r\n\t<li>Wayne Cashman: Oct. 1, 1997 &ndash; March 8, 1998</li>\r\n\t<li>Terry Murray: Jan. 21, 1995 &ndash; June 7, 1997</li>\r\n\t<li>Terry Simpson: Oct. 5, 1993 &ndash; April 14, 1994</li>\r\n\t<li>Bill Dineen: Dec. 5, 1991 &ndash; April 16, 1993</li>\r\n\t<li>Paul Holmgren: Oct. 6, 1988 &ndash; Dec. 2, 1991</li>\r\n\t<li>Mike Keenan: Oct. 11, 1984 &ndash; April 16, 1988</li>\r\n\t<li>Bob McCammon: March 20, 1982 &ndash; April 7, 1984</li>\r\n\t<li>Pat Quinn: Feb. 1, 1979 &ndash; March 18, 1982</li>\r\n\t<li>Bob McCammon: Oct. 12, 1978 &ndash; Jan. 29, 1979</li>\r\n\t<li>Fred Shero: Oct. 9, 1971 &ndash; May 11, 1978</li>\r\n\t<li>Vic Stasiuk: Oct. 11, 1969 &ndash; April 11, 1971</li>\r\n\t<li>Keith Allen: Oct. 11, 1967 &ndash; April 6, 1969</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>Mike Sullivan: Dec. 14, 2015 &ndash; Present</li>\r\n\t<li>Mike Johnston: Oct. 9, 2014 &ndash; Dec. 11, 2015</li>\r\n\t<li>Dan Bylsma: Feb. 16, 2009 &ndash; May 13, 2014</li>\r\n\t<li>Michel Therrien: Dec. 16, 2005 &ndash; Feb. 14, 2009</li>\r\n\t<li>Eddie Olczyk: Oct. 10, 2003 &ndash; Dec. 13, 2005</li>\r\n\t<li>Rick Kehoe: Oct. 16, 2001 &ndash; April 5, 2003</li>\r\n\t<li>Ivan Hlinka: Oct. 6, 2000 &ndash; Oct. 14, 2001</li>\r\n\t<li>Herb Brooks: Dec. 9, 1999 &ndash; May 9, 2000</li>\r\n\t<li>Kevin Constantine: Oct. 1, 1997 &ndash; Dec. 7, 1999</li>\r\n\t<li>Craig Patrick: March 4&nbsp;&ndash; April 26, 1997</li>\r\n\t<li>Eddie Johnston: Oct. 5, 1993 &ndash; March 1, 1997</li>\r\n\t<li>Scotty Bowman: Oct. 4, 1991 &ndash; May 14, 1993</li>\r\n\t<li>Bob Johnson: Oct. 5, 1990 &ndash; May 25, 1991</li>\r\n\t<li>Craig Patrick: Dec. 6, 1989 &ndash; March 31, 1990</li>\r\n\t<li>Gene Ubriaco: Oct. 7, 1988 &ndash; Dec. 2, 1989</li>\r\n\t<li>Pierre Creamer: Oct. 8, 1987 &ndash; April 3, 1988</li>\r\n\t<li>Bob Berry: Oct. 11, 1984 &ndash; April 4, 1987</li>\r\n\t<li>Lou Angotti: Oct. 4, 1983 &ndash; April 1, 1984</li>\r\n\t<li>Eddie Johnston: Oct. 9, 1980 &ndash; April 3, 1983</li>\r\n\t<li>Johnny Wilson: Oct. 12, 1977 &ndash; April 14, 1980</li>\r\n\t<li>Ken Schinkel: Jan. 17, 1976 &ndash; April 9, 1977</li>\r\n\t<li>Marc Boileau: Feb. 7, 1974 &ndash; Jan. 15, 1976</li>\r\n\t<li>Ken Schinkel: Jan. 13, 1973 &ndash; Feb. 3, 1974</li>\r\n\t<li>Red Kelly: Oct. 11, 1969 &ndash; Jan. 10, 1973</li>\r\n\t<li>Red Sullivan: Oct. 11, 1967 &ndash; March 30, 1969</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <ul class="striped-list">\r\n\t<li>Craig Berube: Nov. 21, 2018 &ndash; Present</li>\r\n\t<li>Mike Yeo: Feb. 2, 2017 &ndash; Nov. 19, 2018</li>\r\n\t<li>Ken Hitchcock: Nov. 8, 2011 &ndash; Jan. 31, 2017</li>\r\n\t<li>Davis Payne: Jan. 2, 2010 &ndash; Nov. 5, 2011</li>\r\n\t<li>Andy Murray: Dec. 12, 2006 &ndash; Dec. 31, 2009</li>\r\n\t<li>Mike Kitchen: Feb. 26, 2004 &ndash; Dec. 9, 2006</li>\r\n\t<li>Joel Quenneville: Jan. 7, 1997 &ndash; Feb. 22, 2004</li>\r\n\t<li>Jim Roberts: Dec. 19, 1996 &ndash; Jan. 5, 1997</li>\r\n\t<li>Mike Keenan: Jan. 20, 1995 &ndash; Dec. 17, 1996</li>\r\n\t<li>Bob Berry: Oct. 31, 1992 &ndash; April 24, 1994</li>\r\n\t<li>Bob Plager: Oct. 6-29, 1992</li>\r\n\t<li>Brian Sutter: Oct. 6, 1988 &ndash; April 28, 1992</li>\r\n\t<li>Jacques Martin: Oct. 9, 1986 &ndash; April 27, 1988</li>\r\n\t<li>Jacques Demers: Oct. 4, 1983 &ndash; May 14, 1986</li>\r\n\t<li>Barclay Plager: Dec. 14, 1982 &ndash; April 10, 1983</li>\r\n\t<li>Emile Francis: March 9&nbsp;&ndash; Dec. 11, 1982</li>\r\n\t<li>Red Berenson: Dec. 11, 1979 &ndash; March 8, 1982</li>\r\n\t<li>Barclay Plager: Feb. 18, 1978 &ndash; Dec. 8, 1979</li>\r\n\t<li>Leo Boivin: Oct. 12, 1977 &ndash; Feb. 15, 1978</li>\r\n\t<li>Emile Francis: Oct. 7, 1976 &ndash; April 17, 1977</li>\r\n\t<li>Leo Boivin: Jan. 6&nbsp;&ndash; April 9, 1976</li>\r\n\t<li>Lynn Patrick: Dec. 14, 1975 &ndash; Jan. 3, 1976</li>\r\n\t<li>Garry Young: Nov. 5, 1974 &ndash; Dec. 12, 1975</li>\r\n\t<li>Lynn Patrick: Oct. 31&nbsp;&ndash; Nov. 2, 1974</li>\r\n\t<li>Lou Angotti: Feb. 16&nbsp;&ndash; Oct. 27, 1974</li>\r\n\t<li>Jean-Guy Talbot: Nov. 11, 1972 &ndash; Feb. 14, 1974</li>\r\n\t<li>Al Arbour: Dec. 25, 1971 &ndash; Nov. 7, 1972</li>\r\n\t<li>Bill McCreary: Nov. 3&nbsp;&ndash; Dec. 22, 1971</li>\r\n\t<li>Sid Abel: Oct. 9-30, 1971</li>\r\n\t<li>Scotty Bowman: Feb. 7&nbsp;&ndash; April 15, 1971</li>\r\n\t<li>Al Arbour: Oct. 10, 1970 &ndash; Feb. 5, 1971</li>\r\n\t<li>Scotty Bowman: Nov. 22, 1967 &ndash; May 10, 1970</li>\r\n\t<li>Lynn Patrick: Oct. 11&nbsp;&ndash; Nov. 19, 1967</li>\r\n\t<li>*&nbsp;<em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <ul class="striped-list">\r\n\t<li>Don Granato: March 18, 2021 &ndash; Present</li>\r\n\t<li>Ralph Krueger: Oct. 3, 2019 &ndash; March 16, 2021</li>\r\n\t<li>Phil Housley: Oct. 5, 2017 &ndash; April 6, 2019</li>\r\n\t<li>Dan Bylsma: Oct. 8, 2015 &ndash; April 9, 2017</li>\r\n\t<li>Ted Nolan: Nov. 15, 2013&nbsp;&ndash; April 11, 2015</li>\r\n\t<li>Ron Rolston: Feb. 21&nbsp;&ndash; Nov. 12, 2013</li>\r\n\t<li>Lindy Ruff: Oct. 1, 1997 &ndash; Feb. 19, 2013</li>\r\n\t<li>Ted Nolan: Oct. 7, 1995 &ndash; May 11, 1997</li>\r\n\t<li>John Muckler: Dec. 13, 1991 &ndash; May 14, 1995</li>\r\n\t<li>Rick Dudley: Oct. 5, 1989 &ndash; Dec. 11, 1991</li>\r\n\t<li>Ted Sator: Dec. 23, 1986 &ndash; April 11, 1989</li>\r\n\t<li>Craig Ramsay: Nov. 5&nbsp;&ndash; Dec. 20, 1986</li>\r\n\t<li>Scotty Bowman: Jan. 17&nbsp;&ndash; Nov. 2, 1986</li>\r\n\t<li>Jim Schoenfeld: Oct. 10, 1985 &ndash; Jan. 15, 1986</li>\r\n\t<li>Scotty Bowman: March 20, 1982 &ndash; April 16, 1985</li>\r\n\t<li>Jim Roberts: Dec. 9, 1981 &ndash; March 18, 1982</li>\r\n\t<li>Scotty Bowman: Oct. 7&nbsp;&ndash; Dec. 6, 1981</li>\r\n\t<li>Roger Neilson: Oct. 9, 1980 &ndash; April 22, 1981</li>\r\n\t<li>Scotty Bowman: Oct. 11, 1979 &ndash; May 10, 1980</li>\r\n\t<li>Billy Inglis: Dec. 7, 1978 &ndash; April 14, 1979</li>\r\n\t<li>Marcel Pronovost: Oct. 13, 1977 &ndash; Dec. 3, 1978</li>\r\n\t<li>Floyd Smith: Oct. 10, 1974 &ndash; April 17, 1977</li>\r\n\t<li>Joe Crozier: Jan. 13, 1972 &ndash; April 7, 1974</li>\r\n\t<li>Floyd Smith: Jan. 9, 1972</li>\r\n\t<li>Punch Imlach: Oct. 10, 1970 &ndash; Jan. 6, 1972</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>Travis Green: Oct. 7, 2017 &ndash; Present</li>\r\n\t<li>Willie Desjardins: Oct. 8, 2014 &ndash; April 9, 2017</li>\r\n\t<li>John Tortorella: Oct. 3, 2013 &ndash; April 13, 2014</li>\r\n\t<li>Alain Vigneault: Oct. 5, 2006 &ndash; May 7, 2013</li>\r\n\t<li>Marc Crawford: Jan. 28, 1999 &ndash; April 15, 2006</li>\r\n\t<li>Mike Keenan: Nov. 14, 1997 &ndash; Jan. 19, 1999</li>\r\n\t<li>Tom Renney: Oct. 5, 1996 &ndash; Nov. 12, 1997</li>\r\n\t<li>Pat Quinn: March 29&nbsp;&ndash; April 27, 1996</li>\r\n\t<li>Rick Ley: Jan. 20, 1995 &ndash; March 27, 1996</li>\r\n\t<li>Pat Quinn: Feb. 2, 1991 &ndash; June 14, 1994</li>\r\n\t<li>Bob McCammon: Oct. 8, 1987 &ndash; Jan. 31, 1991</li>\r\n\t<li>Tom Watt: Oct. 10, 1985 &ndash; April 5, 1987</li>\r\n\t<li>Harry Neale: Nov. 21, 1984 &ndash; April 6, 1985</li>\r\n\t<li>Bill LaForge: Oct. 11&nbsp;&ndash; Nov. 20, 1984</li>\r\n\t<li>Harry Neale: Jan. 20&nbsp;&ndash; April 8, 1984</li>\r\n\t<li>Roger Neilson: March 27, 1982 &ndash; Jan. 18, 1984</li>\r\n\t<li>Harry Neale: Oct. 11, 1978 &ndash; March 24, 1982</li>\r\n\t<li>Orland Kurtenbach: Dec. 22, 1976 &ndash; April 8, 1978</li>\r\n\t<li>Phil Maloney: Jan. 15, 1974 &ndash; Dec. 20, 1976</li>\r\n\t<li>Bill McCreary: Oct. 10, 1973 &ndash; Jan. 12, 1974</li>\r\n\t<li>Vic Stasiuk: Oct. 7, 1972 &ndash; March 31, 1973</li>\r\n\t<li>Hal Laycoe: Oct. 9, 1970 &ndash; April 2, 1972</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <ul class="striped-list">\r\n\t<li>Darryl Sutter: March 11, 2021 &ndash; Present</li>\r\n\t<li>Ryan Huska: March 6-7, 2021</li>\r\n\t<li>Geoff Ward: Nov. 27, 2019 &ndash; March 4, 2021</li>\r\n\t<li>Bill Peters: Oct. 3, 2018 &ndash; Nov. 25, 2019</li>\r\n\t<li>Glen Gulutzan: Oct. 12, 2016 &ndash; April 7, 2018</li>\r\n\t<li>Bob Hartley: Jan. 20, 2013 &ndash; April 9, 2016</li>\r\n\t<li>Brent Sutter: Oct. 1, 2009 &ndash; April 7, 2012</li>\r\n\t<li>Mike Keenan: Oct. 4, 2007 &ndash; April 27, 2009</li>\r\n\t<li>Jim Playfair: Oct. 5, 2006 &ndash; April 22, 2007</li>\r\n\t<li>Darryl Sutter: Dec. 29, 2002 &ndash; May 3, 2006</li>\r\n\t<li>Al MacNeil: Dec. 3-27, 2002</li>\r\n\t<li>Greg Gilbert: March 14, 2001 &ndash; Dec. 1, 2002</li>\r\n\t<li>Don Hay: Oct. 5, 2000 &ndash; March 11, 2001</li>\r\n\t<li>Brian Sutter: Oct. 1, 1997 &ndash; April 8, 2000</li>\r\n\t<li>Pierre Page: Oct. 7, 1995 &ndash; April 12, 1997</li>\r\n\t<li>Dave King: Oct. 6, 1992 &ndash; May 19, 1995</li>\r\n\t<li>Guy Charron: March 3&nbsp;&ndash; April 16, 1992</li>\r\n\t<li>Doug Risebrough: Oct. 4, 1990 &ndash; March 1, 1992</li>\r\n\t<li>Terry Crisp: Oct. 8, 1987 &ndash; April 14, 1990</li>\r\n\t<li>Bob Johnson: Oct. 5, 1982 &ndash; April 16, 1987</li>\r\n\t<li>Al MacNeil: Oct. 10, 1979 &ndash; April 10, 1982</li>\r\n\t<li>Fred Creighton: Feb. 5, 1975 &ndash; April 12, 1979</li>\r\n\t<li>Bernie Geoffrion: Oct. 7, 1972 &ndash; Feb. 2, 1975</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Barry Trotz: Oct. 4, 2018 &ndash; Present</li>\r\n\t<li>Doug Weight: Jan. 19, 2017 &ndash; April 7, 2018</li>\r\n\t<li>Jack Capuano: Nov. 17, 2010 &ndash; Jan. 16, 2017</li>\r\n\t<li>Scott Gordon: Oct. 10, 2008 &ndash; Nov. 13, 2010</li>\r\n\t<li>Ted Nolan: Nov. 3, 2007 &ndash; April 4, 2008</li>\r\n\t<li>Al Arbour: Nov. 1, 2007</li>\r\n\t<li>Ted Nolan: Oct. 5, 2006 &ndash; Oct. 27, 2007</li>\r\n\t<li>Brad Shaw: Jan. 12&nbsp;&ndash; April 18, 2006</li>\r\n\t<li>Steve Stirling: Oct. 9, 2003 &ndash; Jan. 10, 2006</li>\r\n\t<li>Peter Laviolette: Oct. 5, 2001 &ndash; April 17, 2003</li>\r\n\t<li>Lorne Henning: March 5&nbsp;&ndash; April 7, 2001</li>\r\n\t<li>Butch Goring: Oct. 2, 1999 &ndash; March 3, 2001</li>\r\n\t<li>Bill Stewart: Jan. 21&nbsp;&ndash; April 17, 1999</li>\r\n\t<li>Mike Milbury: March 12, 1998 &ndash; Jan. 20, 1999</li>\r\n\t<li>Rick Bowness: Jan. 24, 1997 &ndash; March 10, 1998</li>\r\n\t<li>Mike Milbury: Oct. 7, 1995 &ndash; Jan. 22, 1997</li>\r\n\t<li>Lorne Henning: Jan. 21&nbsp;&ndash; May 2, 1995</li>\r\n\t<li>Al Arbour: Dec. 9, 1988 &ndash; April 24, 1994</li>\r\n\t<li>Terry Simpson: Oct. 9, 1986 &ndash; Dec. 6, 1988</li>\r\n\t<li>Al Arbour: Oct. 10, 1973 &ndash; April 12, 1986</li>\r\n\t<li>Earl Ingarfield: Jan. 31&nbsp;&ndash; April 1, 1973</li>\r\n\t<li>Phil Goyette: Oct. 7, 1972 &ndash; Jan. 26, 1973</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <ul class="striped-list">\r\n\t<li>Lindy Ruff: Present</li>\r\n\t<li>Alain Nasreddine: Dec. 3, 2019 &ndash; March 10, 2020</li>\r\n\t<li>John Hynes: Oct. 9, 2015 &ndash; Dec. 2, 2019</li>\r\n\t<li>Peter DeBoer: Oct. 8, 2011 &ndash; Dec. 23, 2014</li>\r\n\t<li>Jacques Lemaire: Dec. 23, 2010 &ndash; April 10, 2011</li>\r\n\t<li>John MacLean: Oct. 8&nbsp;&ndash; Dec. 21, 2010</li>\r\n\t<li>Jacques Lemaire: Oct. 3, 2009 &ndash; April 22, 2010</li>\r\n\t<li>Brent Sutter: Oct. 4, 2007 &ndash; April 28, 2009</li>\r\n\t<li>Lou Lamoriello: April 3&nbsp;&ndash; May 5, 2007</li>\r\n\t<li>Claude Julien: Oct. 6, 2006 &ndash; April 1, 2007</li>\r\n\t<li>Lou Lamoriello: Dec. 20, 2005 &ndash; May 14, 2006</li>\r\n\t<li>Larry Robinson: Oct. 5&nbsp;&ndash; Dec. 17, 2005</li>\r\n\t<li>Pat Burns: Oct. 10, 2002 &ndash; April 17, 2004</li>\r\n\t<li>Kevin Constantine: Jan. 29&nbsp;&ndash; April 27, 2002</li>\r\n\t<li>Larry Robinson: March 24, 2000 &ndash; Jan. 26, 2002</li>\r\n\t<li>Robbie Ftorek: Oct. 10, 1998 &ndash; March 21, 2000</li>\r\n\t<li>Jacques Lemaire: Oct. 6, 1993 &ndash; May 2, 1998</li>\r\n\t<li>Herb Brooks: Oct. 6, 1992 &ndash; April 26, 1993</li>\r\n\t<li>Tom McVie: March 5, 1991 &ndash; May 1, 1992</li>\r\n\t<li>John Cunniff: Nov. 8, 1989 &ndash; March 3, 1991</li>\r\n\t<li>Jim Schoenfeld: Jan. 28, 1988 &ndash; Nov. 4, 1989</li>\r\n\t<li>Doug Carpenter: Oct. 12, 1984 &ndash; Jan. 25, 1988</li>\r\n\t<li>Tom McVie: Nov. 23, 1983 &ndash; April 1, 1984</li>\r\n\t<li>Bill MacMillan: Oct. 5, 1982 &ndash; Nov. 19, 1983</li>\r\n\t<li>Marshall Johnston: Nov. 30, 1981 &ndash; April 3, 1982</li>\r\n\t<li>Bert Marshall: Oct. 6&nbsp;&ndash; Nov. 28, 1981</li>\r\n\t<li>Bill MacMillan: Oct. 11, 1980 &ndash; April 4, 1981</li>\r\n\t<li>Don Cherry: Oct. 11, 1979 &ndash; April 6, 1980</li>\r\n\t<li>Aldo Guidolin: Nov. 25, 1978 &ndash; April 8, 1979</li>\r\n\t<li>Pat Kelly: Oct. 14, 1977 &ndash; Nov. 23, 1978</li>\r\n\t<li>Johnny Wilson: Oct. 5, 1976 &ndash; April 3, 1977</li>\r\n\t<li>Eddie Bush: Jan. 28&nbsp;&ndash; April 4, 1976</li>\r\n\t<li>Sid Abel: Jan. 21-25, 1976</li>\r\n\t<li>Bep Guidolin: Oct. 9, 1974 &ndash; Jan. 17, 1976</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <ul class="striped-list">\r\n\t<li>Peter Laviolette: Present</li>\r\n\t<li>Todd Reirden: Oct. 3, 2018 &ndash; Aug. 20, 2020</li>\r\n\t<li>Barry Trotz: Oct. 9, 2014 &ndash; June 7, 2018</li>\r\n\t<li>Adam Oates: Jan. 19, 2013 &ndash; April 13, 2014</li>\r\n\t<li>Dale Hunter: Nov. 29, 2011 &ndash; May 12, 2012</li>\r\n\t<li>Bruce Boudreau: Nov. 23, 2007 &ndash; Nov. 26, 2011</li>\r\n\t<li>Glen Hanlon: Dec. 11, 2003 &ndash; Nov. 21, 2007</li>\r\n\t<li>Bruce Cassidy: Oct. 11, 2002 &ndash; Dec. 8, 2003</li>\r\n\t<li>Ron Wilson: Oct. 1, 1997 &ndash; April 13, 2002</li>\r\n\t<li>Jim Schoenfeld: Jan. 27, 1994 &ndash; April 13, 1997</li>\r\n\t<li>Terry Murray: Jan. 16, 1990 &ndash; Jan. 25, 1994</li>\r\n\t<li>Bryan Murray: Nov. 11, 1981 &ndash; Jan. 13, 1990</li>\r\n\t<li>Roger Crozier: Nov. 7, 1981</li>\r\n\t<li>Gary Green: Nov. 15, 1979 &ndash; Nov. 4, 1981</li>\r\n\t<li>Danny Belisle: Oct. 11, 1978 &ndash; Nov. 13, 1979</li>\r\n\t<li>Tom McVie: Dec. 31, 1975 &ndash; April 9, 1978</li>\r\n\t<li>Milt Schmidt: March 22&nbsp;&ndash; Dec. 29, 1975</li>\r\n\t<li>Red Sullivan: Feb. 11&nbsp;&ndash; March 20, 1975</li>\r\n\t<li>Jim Anderson: Oct. 9, 1974 &ndash; Feb. 9, 1975</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>Dave Tippett: Oct. 2, 2019 &ndash; Present</li>\r\n\t<li>Ken Hitchcock: Nov. 20, 2018 &ndash; April 6, 2019</li>\r\n\t<li>Todd McLellan: Oct. 8, 2015 &ndash; Nov. 18, 2018</li>\r\n\t<li>Todd Nelson: Dec. 30, 2014 &ndash; April 11, 2015</li>\r\n\t<li>Craig MacTavish: Dec. 16-27, 2014</li>\r\n\t<li>Dallas Eakins: Oct. 1, 2013 &ndash; Dec. 14, 2014</li>\r\n\t<li>Ralph Krueger: Jan. 20&nbsp;&ndash; April 27, 2013</li>\r\n\t<li>Tom Renney: Oct. 7, 2010 &ndash; April 7, 2012</li>\r\n\t<li>Pat Quinn: Oct. 3, 2009 &ndash; April 11, 2010</li>\r\n\t<li>Craig MacTavish: Oct. 6, 2000 &ndash; April 11, 2009</li>\r\n\t<li>Kevin Lowe: Oct. 1, 1999 &ndash; April 21, 2000</li>\r\n\t<li>Ron Low: April 7, 1995 &ndash; April 27, 1999</li>\r\n\t<li>George Burnett: Jan. 20&nbsp;&ndash; April 5, 1995</li>\r\n\t<li>Glen Sather: Nov. 27, 1993 &ndash; April 14, 1994</li>\r\n\t<li>Ted Green: Oct. 4, 1991 &ndash; Nov. 24, 1993</li>\r\n\t<li>John Muckler: Oct. 5, 1989 &ndash; May 10, 1991</li>\r\n\t<li>Glen Sather: Nov. 23, 1980 &ndash; April 15, 1989</li>\r\n\t<li>Bryan Watson: Oct. 10&nbsp;&ndash; Nov. 19, 1980</li>\r\n\t<li>Glen Sather: Oct. 10, 1979 &ndash; April 11, 1980</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <ul class="striped-list">\r\n\t<li>Rod Brind&rsquo;Amour: Oct. 4, 2018 &ndash; Present</li>\r\n\t<li>Bill Peters: Oct. 10, 2014 &ndash; April 7, 2018</li>\r\n\t<li>Kirk Muller: Nov. 29, 2011 &ndash; April 13, 2014</li>\r\n\t<li>Paul Maurice: Dec. 4, 2008 &ndash; Nov. 27, 2011</li>\r\n\t<li>Peter Laviolette: Dec. 18, 2003 &ndash; Nov. 30, 2008</li>\r\n\t<li>Paul Maurice: Nov. 7, 1995 &ndash; Dec. 14, 2003</li>\r\n\t<li>Paul Holmgren: Jan. 21&nbsp;&ndash; Nov. 5, 1995</li>\r\n\t<li>Pierre McGuire: Nov. 17, 1993 &ndash; April 14, 1994</li>\r\n\t<li>Paul Holmgren: Oct. 6, 1992 &ndash; Nov. 13, 1993</li>\r\n\t<li>Jim Roberts:&nbsp; Oct. 5, 1991 &ndash; May 1, 1992</li>\r\n\t<li>Rick Ley: Oct. 5, 1989 &ndash; April 13, 1991</li>\r\n\t<li>Larry Pleau: Feb. 7, 1988 &ndash; April 9, 1989</li>\r\n\t<li>Jack Evans: Oct. 5, 1983 &ndash; Feb. 6, 1988</li>\r\n\t<li>John Cunniff: March 8&nbsp;&ndash; April 3, 1983</li>\r\n\t<li>Larry Pleau: Jan. 27&nbsp;&ndash; March 6, 1983</li>\r\n\t<li>Larry Kish: Oct. 6, 1982 &ndash; Jan. 23, 1983</li>\r\n\t<li>Larry Pleau: Feb. 22, 1981 &ndash; April 4, 1982</li>\r\n\t<li>Don Blackburn: Oct. 11, 1979 &ndash; Feb. 19, 1981</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>Jared Bednar: Oct. 15, 2016 &ndash; Present</li>\r\n\t<li>Patrick Roy: Oct. 2, 2013 &ndash; April 9, 2016</li>\r\n\t<li>Joe Sacco: Oct. 1, 2009 &ndash; April 27, 2013</li>\r\n\t<li>Tony Granato: Oct. 9, 2008 &ndash; April 12, 2009</li>\r\n\t<li>Joel Quenneville: Oct. 5, 2005 &ndash; May 1, 2008</li>\r\n\t<li>Tony Granato: Dec. 19, 2002 &ndash; May 4, 2004</li>\r\n\t<li>Bob Hartley: Oct. 10, 1998 &ndash; Dec. 16, 2002</li>\r\n\t<li>Marc Crawford: Jan. 21, 1995 &ndash; May 4, 1998</li>\r\n\t<li>Pierre Page: Nov. 18, 1991 &ndash; April 14, 1994</li>\r\n\t<li>Dave Chambers: Oct. 4, 1990 &ndash; Nov. 16, 1991</li>\r\n\t<li>Michel Bergeron: Oct. 5, 1989 &ndash; April 1, 1990</li>\r\n\t<li>Jean Perron: Dec. 17, 1988 &ndash; April 2, 1989</li>\r\n\t<li>Ron Lapointe: Dec. 5, 1987 &ndash; Dec. 15, 1988</li>\r\n\t<li>Andre Savard: Oct. 8&nbsp;&ndash; Dec. 3, 1987</li>\r\n\t<li>Michel Bergeron: Oct. 22, 1980 &ndash; May 2, 1987</li>\r\n\t<li>Maurice Filion: Oct. 9-19, 1980</li>\r\n\t<li>Jacques Demers: Oct. 10, 1979 &ndash; April 6, 1980</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <ul class="striped-list">\r\n\t<li>Rick Tocchet: Oct. 5, 2017 &ndash; May 8, 2021</li>\r\n\t<li>Dave Tippett: Oct. 3, 2009 &ndash; April 8, 2017</li>\r\n\t<li>Wayne Gretzky: Oct. 5, 2005 &ndash; April 11, 2009</li>\r\n\t<li>Rick Bowness: Feb. 25&nbsp;&ndash; April 3, 2004</li>\r\n\t<li>Bob Francis: Oct. 2, 1999 &ndash; Feb. 23, 2004</li>\r\n\t<li>Jim Schoenfeld: Oct. 1, 1997 &ndash; May 4, 1999</li>\r\n\t<li>Don Hay: Oct. 5, 1996 &ndash; April 29, 1997</li>\r\n\t<li>Terry Simpson: April 5, 1995 &ndash; April 28, 1996</li>\r\n\t<li>John Paddock: Oct. 4, 1991 &ndash; April 1, 1995</li>\r\n\t<li>Bob Murdoch: Oct. 6, 1989 &ndash; March 31, 1991</li>\r\n\t<li>Rick Bowness: Feb. 9&nbsp;&ndash; April 2, 1989</li>\r\n\t<li>Dan Maloney: Oct. 9, 1986 &ndash; Feb. 5, 1989</li>\r\n\t<li>John Ferguson Sr.: March 5&nbsp;&ndash; April 12, 1986</li>\r\n\t<li>Barry Long: Nov. 23, 1983 &ndash; March 3, 1986</li>\r\n\t<li>Tom Watt: Oct. 6, 1981 &ndash; Nov. 21, 1983</li>\r\n\t<li>Mike Smith: Feb. 17&nbsp;&ndash; April 5, 1981</li>\r\n\t<li>Bill Sutherland: Dec. 13, 1980 &ndash; Feb. 15, 1981</li>\r\n\t<li>Tom McVie: Nov. 18, 1979 &ndash; Dec. 10, 1980</li>\r\n\t<li>Bill Sutherland: Nov. 10-16, 1979</li>\r\n\t<li>Tom McVie: Oct. 10&nbsp;&ndash; Nov. 7, 1979</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <ul class="striped-list">\r\n\t<li>Bob Boughner: Dec. 12, 2019 &ndash; Present</li>\r\n\t<li>Peter DeBoer: Oct. 7, 2015 &ndash; Dec. 10, 2019</li>\r\n\t<li>Todd McLellan: Oct. 9, 2008 &ndash; April 11, 2015</li>\r\n\t<li>Ron Wilson: Dec. 6, 2002 &ndash; May 4, 2008</li>\r\n\t<li>Cap Raeder: Dec. 3, 2002</li>\r\n\t<li>Darryl Sutter: Oct. 1, 1997 &ndash; Nov. 30, 2002</li>\r\n\t<li>Al Sims: Oct. 5, 1996 &ndash; April 12, 1997</li>\r\n\t<li>Jim Wiley: Dec. 2, 1995 &ndash; April 12, 1996</li>\r\n\t<li>Kevin Constantine: Oct. 6, 1993 &ndash; Dec. 1, 1995</li>\r\n\t<li>George Kingston: Oct. 4, 1991 &ndash; April 15, 1993</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>D.J. Smith: Oct. 2, 2019 &ndash; Present</li>\r\n\t<li>Marc Crawford: March 2&nbsp;&ndash; April 6, 2019</li>\r\n\t<li>Guy Boucher: Oct. 12, 2016 &ndash; Feb. 28, 2019</li>\r\n\t<li>Dave Cameron: Dec. 11, 2014 &ndash; April 9, 2016</li>\r\n\t<li>Paul MacLean: Oct. 7, 2011 &ndash; Dec. 7, 2014</li>\r\n\t<li>Cory Clouston: Feb. 3, 2009 &ndash; April 9, 2011</li>\r\n\t<li>Craig Hartsburg: Oct. 4, 2008 &ndash; Feb. 1, 2009</li>\r\n\t<li>Bryan Murray: Feb. 28&nbsp;&ndash; April 16, 2008</li>\r\n\t<li>John Paddock: Oct. 3, 2007 &ndash; Feb. 26, 2008</li>\r\n\t<li>Bryan Murray: Oct. 5, 2005 &ndash; June 6, 2007</li>\r\n\t<li>Jacques Martin: April 17, 2002 &ndash; April 20, 2004</li>\r\n\t<li>Roger Neilson: April 11-13, 2002</li>\r\n\t<li>Jacques Martin: Jan. 24, 1996 &ndash; April 9, 2002</li>\r\n\t<li>Dave Allison: Nov. 22, 1995 &ndash; Jan. 22, 1996</li>\r\n\t<li>Rick Bowness: Oct. 8, 1992 &ndash; Nov. 19, 1995</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <ul class="striped-list">\r\n\t<li>Jon Cooper: March 29, 2013^ &ndash; Present</li>\r\n\t<li>Guy Boucher: Oct. 9, 2010 &ndash; March 23, 2013</li>\r\n\t<li>Rick Tocchet: Nov. 16, 2008 &ndash; April 11, 2010</li>\r\n\t<li>Barry Melrose: Oct. 4&nbsp;&ndash; Nov. 13, 2008</li>\r\n\t<li>John Tortorella: Jan. 7, 2001 &ndash; April 5, 2008</li>\r\n\t<li>Steve Ludzik: Oct. 2, 1999 &ndash; Jan. 4, 2001</li>\r\n\t<li>Jacques Demers: Nov. 14, 1997 &ndash; April 17, 1999</li>\r\n\t<li>Rick Paterson: Oct. 26&nbsp;&ndash; Nov. 11, 1997</li>\r\n\t<li>Terry Crisp: Oct. 7, 1992 &ndash; Oct. 24, 1997</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n\t<li>^ <i>The Lightning hired Cooper on March 25, 2013, but he did not coach his first game until March&nbsp;29. No interim head coach was used for games on March 24 or March 26.</i></li>\r\n</ul>\r\n
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <ul class="striped-list">\r\n\t<li>Dallas Eakins: Oct. 3, 2019 &ndash; Present</li>\r\n\t<li>Bob Murray: Feb. 13&nbsp;&ndash; April 5, 2019</li>\r\n\t<li>Randy Carlyle: Oct. 13, 2016 &ndash; Feb. 9, 2019</li>\r\n\t<li>Bruce Boudreau: Dec. 2, 2011 &ndash; April 27, 2016</li>\r\n\t<li>Randy Carlyle: Oct. 5, 2005 &ndash; Nov. 30, 2011</li>\r\n\t<li>Mike Babcock: Oct. 10, 2002 &ndash; April 4, 2004</li>\r\n\t<li>Bryan Murray: Oct. 4, 2001 &ndash; April 14, 2002</li>\r\n\t<li>Guy Charron: Dec. 15, 2000 &ndash; April 8, 2001</li>\r\n\t<li>Craig Hartsburg: Oct. 10, 1998 &ndash; Dec. 13, 2000</li>\r\n\t<li>Pierre Page: Oct. 3, 1997 &ndash; April 19, 1998</li>\r\n\t<li>Ron Wilson: Oct. 8, 1993 &ndash; May 8, 1997</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 33                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Joel Quenneville: Oct. 3, 2019 &ndash; Present</li>\r\n\t<li>Bob Boughner: Oct. 6, 2017 &ndash; April 6, 2019</li>\r\n\t<li>Tom Rowe: Nov. 29, 2016 &ndash; April 9, 2017</li>\r\n\t<li>Gerard Gallant: Oct. 9, 2014 &ndash; Nov. 27, 2016</li>\r\n\t<li>Peter Horachek: Nov. 9, 2013 &ndash; April 12, 2014</li>\r\n\t<li>Kevin Dineen: Oct. 8, 2011 &ndash; Nov. 7, 2013</li>\r\n\t<li>Peter DeBoer: Oct. 10, 2008 &ndash; April 9, 2011</li>\r\n\t<li>Jacques Martin: Oct. 5, 2005 &ndash; April 5, 2008</li>\r\n\t<li>John Torchetti: Feb. 10&nbsp;&ndash; April 4, 2004</li>\r\n\t<li>Rick Dudley: Nov. 11, 2003 &ndash; Feb. 4, 2004</li>\r\n\t<li>Mike Keenan: Dec. 5, 2001 &ndash; Nov. 8, 2003</li>\r\n\t<li>Duane Sutter: Dec. 29, 2000 &ndash; Dec. 1, 2001</li>\r\n\t<li>Terry Murray: Oct. 9, 1998 &ndash; Dec. 27, 2000</li>\r\n\t<li>Bryan Murray: Nov. 26, 1997 &ndash; April 18, 1998</li>\r\n\t<li>Doug MacLean: Oct. 7, 1995 &ndash; Nov. 23, 1997</li>\r\n\t<li>Roger Neilson: Oct. 6, 1993 &ndash; May 3, 1995</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 34                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <ul class="striped-list">\r\n\t<li>John Hynes: Jan. 7, 2019 &ndash; Present</li>\r\n\t<li>Peter Laviolette: Oct. 9, 2014 &ndash; Jan. 5, 2019</li>\r\n\t<li>Barry Trotz: Oct. 10, 1998 &ndash; April 13, 2014</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 35                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <ul class="striped-list">\r\n\t<li>Paul Maurice: Jan. 13, 2014 &ndash; Present</li>\r\n\t<li>Claude Noel: Oct. 9, 2011 &ndash; Jan. 11, 2014</li>\r\n\t<li>Craig Ramsay: Oct. 8, 2010 &ndash; April 10, 2011</li>\r\n\t<li>John Anderson: Oct. 10, 2008 &ndash; April 10, 2010</li>\r\n\t<li>Don Waddell: Oct. 18, 2007 &ndash; April 5, 2008</li>\r\n\t<li>Bob Hartley: Jan. 15, 2003 &ndash; Oct. 16, 2007</li>\r\n\t<li>Don Waddell: Dec. 27, 2002 &ndash; Jan. 13, 2003</li>\r\n\t<li>Curt Fraser: Oct. 2, 1999 &ndash; Dec. 23, 2002</li>\r\n\t<li>*&nbsp;<i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 36                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <ul class="striped-list">\r\n\t<li>Brad Larsen: Present</li>\r\n\t<li>John Tortorella: Oct. 22, 2015 &ndash; May 8, 2021</li>\r\n\t<li>Todd Richards: Jan. 10, 2012 &ndash; Oct. 20, 2015</li>\r\n\t<li>Scott Arniel: Oct. 8, 2010 &ndash; Jan. 8, 2012</li>\r\n\t<li>Claude Noel: Feb. 4&nbsp;&ndash; April 9, 2010</li>\r\n\t<li>Ken Hitchcock: Nov. 24, 2006 &ndash; Feb. 2, 2010</li>\r\n\t<li>Gary Agnew: Nov. 15-22, 2006</li>\r\n\t<li>Gerard Gallant: Jan. 2, 2004 &ndash; Nov. 12, 2006</li>\r\n\t<li>Doug MacLean: Jan. 8&nbsp;&ndash; Dec. 31, 2003</li>\r\n\t<li>Dave King: Oct. 7, 2000 &ndash; Jan. 6, 2003</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 37                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>Dean Evason: Feb. 15, 2020 &ndash; Present</li>\r\n\t<li>Bruce Boudreau: Oct. 13, 2016 &ndash; Feb. 13, 2020</li>\r\n\t<li>John Torchetti: Feb. 15&nbsp;&ndash; April 24, 2016</li>\r\n\t<li>Mike Yeo: Oct. 8, 2011 &ndash; Feb. 13, 2016</li>\r\n\t<li>Todd Richards: Oct. 3, 2009 &ndash; April 10, 2011</li>\r\n\t<li>Jacques Lemaire: Oct. 6, 2000 &ndash; April 11, 2009</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ## 38                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <ul class="striped-list">\r\n\t<li>Peter DeBoer: Jan. 16, 2020 &ndash; Present</li>\r\n\t<li>Gerard Gallant: Oct. 6, 2017 &ndash; Jan. 14, 2020</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ## 39                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
    ##            dateAwarded
    ## 1  1917-11-26T00:00:00
    ## 2  1917-11-26T00:00:00
    ## 3  1917-11-26T00:00:00
    ## 4  1917-11-26T00:00:00
    ## 5  1917-11-26T00:00:00
    ## 6  1924-11-01T00:00:00
    ## 7  1924-11-01T00:00:00
    ## 8  1925-09-22T00:00:00
    ## 9  1925-11-07T00:00:00
    ## 10 1926-05-15T00:00:00
    ## 11 1926-09-25T00:00:00
    ## 12 1926-09-25T00:00:00
    ## 13 1967-06-05T00:00:00
    ## 14 1967-06-05T00:00:00
    ## 15 1967-06-05T00:00:00
    ## 16 1967-06-05T00:00:00
    ## 17 1967-06-05T00:00:00
    ## 18 1967-06-05T00:00:00
    ## 19 1970-05-22T00:00:00
    ## 20 1970-05-22T00:00:00
    ## 21 1972-06-06T00:00:00
    ## 22 1972-06-06T00:00:00
    ## 23 1974-06-11T00:00:00
    ## 24 1974-06-11T00:00:00
    ## 25 1979-06-22T00:00:00
    ## 26 1979-06-22T00:00:00
    ## 27 1979-06-22T00:00:00
    ## 28 1979-06-22T00:00:00
    ## 29 1990-05-09T00:00:00
    ## 30 1991-12-16T00:00:00
    ## 31 1991-12-16T00:00:00
    ## 32 1993-02-15T00:00:00
    ## 33 1993-06-14T00:00:00
    ## 34 1997-06-25T00:00:00
    ## 35 1997-06-25T00:00:00
    ## 36 1997-06-25T00:00:00
    ## 37 1997-06-25T00:00:00
    ## 38 2017-03-01T00:00:00
    ## 39                <NA>
    ##                                             directoryUrl
    ## 1      https://www.nhl.com/canadiens/team/administration
    ## 2                                                   <NA>
    ## 3                                                   <NA>
    ## 4                                                   <NA>
    ## 5         https://www.nhl.com/mapleleafs/team/management
    ## 6         https://www.nhl.com/bruins/team/club-directory
    ## 7                                                   <NA>
    ## 8                                                   <NA>
    ## 9                                                   <NA>
    ## 10         https://www.nhl.com/rangers/team/front-office
    ## 11      https://www.nhl.com/blackhawks/team/front-office
    ## 12 https://www.nhl.com/redwings/team/business-operations
    ## 13                                                  <NA>
    ## 14        https://www.nhl.com/kings/team/staff-directory
    ## 15           https://www.nhl.com/stars/team/front-office
    ## 16          https://www.nhl.com/flyers/team/front-office
    ## 17               https://www.nhl.com/penguins/team/staff
    ## 18           https://www.nhl.com/blues/team/front-office
    ## 19                 https://www.nhl.com/sabres/team/staff
    ## 20         https://www.nhl.com/canucks/team/front-office
    ## 21       https://www.nhl.com/flames/team/staff-directory
    ## 22 https://www.nhl.com/islanders/team/business-directory
    ## 23       https://www.nhl.com/devils/team/staff-directory
    ## 24               https://www.nhl.com/capitals/team/staff
    ## 25          https://www.nhl.com/oilers/team/front-office
    ## 26             https://www.nhl.com/hurricanes/team/staff
    ## 27 http://www.pepsicenter.com/kse/company/executive-team
    ## 28                https://www.nhl.com/coyotes/team/staff
    ## 29          https://www.nhl.com/sharks/team/front-office
    ## 30        https://www.nhl.com/senators/team/front-office
    ## 31       https://www.nhl.com/lightning/team/front-office
    ## 32        https://www.nhl.com/ducks/team/staff-directory
    ## 33           https://www.nhl.com/panthers/team/directory
    ## 34      https://www.nhl.com/predators/team/staff-listing
    ## 35             https://www.nhl.com/jets/team/frontoffice
    ## 36  https://www.nhl.com/bluejackets/team/staff-directory
    ## 37          https://www.nhl.com/wild/team/business-staff
    ## 38   https://www.nhl.com/goldenknights/team/front-office
    ## 39                                                      
    ##    firstSeasonId
    ## 1       19171918
    ## 2       19171918
    ## 3       19171918
    ## 4       19191920
    ## 5       19171918
    ## 6       19241925
    ## 7       19241925
    ## 8       19251926
    ## 9       19251926
    ## 10      19261927
    ## 11      19261927
    ## 12      19261927
    ## 13      19671968
    ## 14      19671968
    ## 15      19671968
    ## 16      19671968
    ## 17      19671968
    ## 18      19671968
    ## 19      19701971
    ## 20      19701971
    ## 21      19721973
    ## 22      19721973
    ## 23      19741975
    ## 24      19741975
    ## 25      19791980
    ## 26      19791980
    ## 27      19791980
    ## 28      19791980
    ## 29      19911992
    ## 30      19921993
    ## 31      19921993
    ## 32      19931994
    ## 33      19931994
    ## 34      19981999
    ## 35      19992000
    ## 36      20002001
    ## 37      20002001
    ## 38      20172018
    ## 39      20212022
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 generalManagerHistory
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>Marc Bergevin: May 2, 2012 &ndash; Present</li>\r\n\t<li>Pierre Gauthier: Feb. 8, 2010 &ndash; March 29, 2012</li>\r\n\t<li>Bob Gainey: June 2, 2003 &ndash; Feb. 8, 2010</li>\r\n\t<li>Andre Savard: Nov. 20, 2000 &ndash; June 2, 2003</li>\r\n\t<li>Rejean Houle: Oct. 21, 1995 &ndash; Nov. 20, 2000</li>\r\n\t<li>Serge Savard: April 28, 1983 &ndash; Oct. 17, 1995</li>\r\n\t<li>Irving Grundman: Sept. 4, 1978 &ndash; April 14, 1983</li>\r\n\t<li>Sam Pollock: May 15, 1964 &ndash; Sept. 4, 1978</li>\r\n\t<li>Frank Selke: July 26, 1946 &ndash; May 15, 1964</li>\r\n\t<li>Tommy Gorman: April 8, 1940 &ndash; July 26, 1946</li>\r\n\t<li>Jules Dugal: Jan. 27, 1939 &ndash; April 8, 1940</li>\r\n\t<li>Cecil Hart: July 30, 1936 &ndash; Jan. 27, 1939</li>\r\n\t<li>Ernest Savard: Sept. 17, 1935 &ndash; July 30, 1936</li>\r\n\t<li>Leo Dandurand: Nov. 2, 1921 &ndash; Sept. 17, 1935</li>\r\n\t<li>George Kennedy: 1910 &ndash; Oct. 19, 1921</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 5  <ul class="striped-list">\r\n\t<li>Kyle Dubas: May 11, 2018 &ndash; Present</li>\r\n\t<li>Lou Lamoriello: July 23, 2015 &ndash; April 30, 2018</li>\r\n\t<li>Dave Nonis: Jan. 9, 2013 &ndash; April 12, 2015</li>\r\n\t<li>Brian Burke: Nov. 29, 2008 &ndash; Jan. 9, 2013</li>\r\n\t<li>Cliff Fletcher: Jan. 22&nbsp;&ndash; Nov. 29, 2008</li>\r\n\t<li>John Ferguson Jr.: Aug. 29, 2003 &ndash; Jan. 22, 2008</li>\r\n\t<li>Pat Quinn: July 15, 1999 &ndash; Aug. 29, 2003</li>\r\n\t<li>Ken Dryden: Aug. 21, 1997 &ndash; July 15, 1999</li>\r\n\t<li>Bill Watters: May 25&nbsp;&ndash; Aug. 21, 1997</li>\r\n\t<li>Cliff Fletcher: July 1, 1991 &ndash; May 25, 1997</li>\r\n\t<li>Floyd Smith: Aug. 15, 1989 &ndash; July 1, 1991</li>\r\n\t<li>Gord Stellick: April 28, 1988 &ndash; Aug. 11, 1989</li>\r\n\t<li>John Brophy, Dick Duff and Gord Stellick: Feb. 7&nbsp;&ndash; April 28, 1988</li>\r\n\t<li>Gerry McNamara: Oct. 26, 1981 &ndash; Feb. 7, 1988</li>\r\n\t<li>Punch Imlach: July 4, 1979 &ndash; Oct. 26, 1981</li>\r\n\t<li>Jim Gregory: April 6, 1969 &ndash; July 4, 1979</li>\r\n\t<li>Punch Imlach: Nov. 21, 1958 &ndash; April 6, 1969</li>\r\n\t<li>Stafford Smythe: Oct. 3, 1957 &ndash; Nov. 21, 1958</li>\r\n\t<li>Howie Meeker: May 13&nbsp;&ndash; Oct. 3, 1957</li>\r\n\t<li>Hap Day: Oct. 8, 1954 &ndash; March 25, 1957</li>\r\n\t<li>Conn Smythe: Nov. 15, 1927 &ndash; Oct. 8, 1954</li>\r\n\t<li>Charles Querrie: 1917 &ndash; Feb. 14, 1927</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>Don Sweeney: May 20, 2015 &ndash; Present</li>\r\n\t<li>Peter Chiarelli: May 26, 2006 &ndash; April 15, 2015</li>\r\n\t<li>Jeff Gorton: March 25&nbsp;&ndash; May 26, 2006</li>\r\n\t<li>Mike O&rsquo;Connell: Nov. 1, 2000 &ndash; March 25, 2006</li>\r\n\t<li>Harry Sinden: Oct. 5, 1972 &ndash; Nov. 1, 2000</li>\r\n\t<li>Milt Schmidt: May 1, 1967 &ndash; Oct. 5, 1972</li>\r\n\t<li>Hap Emms: April 5, 1965 &ndash; May 1, 1967</li>\r\n\t<li>Lynn Patrick: April 1, 1954 &ndash; April 5, 1965</li>\r\n\t<li>Art Ross: Nov. 1, 1924 &ndash; April 1, 1954</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <ul class="striped-list">\r\n\t<li>Chris Drury: May 5, 2021 &ndash; Present</li>\r\n\t<li>Jeff Gorton: July 1, 2015 &ndash; May 5, 2021</li>\r\n\t<li>Glen Sather: June 1, 2000 &ndash; July 1, 2015</li>\r\n\t<li>Neil Smith: July 17, 1989 &ndash; March 28, 2000</li>\r\n\t<li>Phil Esposito: July 14, 1986 &ndash; May 24, 1989</li>\r\n\t<li>Craig Patrick: Nov. 21, 1980 &ndash; July 14, 1986</li>\r\n\t<li>Fred Shero: June 2, 1978 &ndash; Nov. 21, 1980</li>\r\n\t<li>John Ferguson Sr.: Jan. 7, 1976 &ndash; June 2, 1978</li>\r\n\t<li>Emile Francis: Oct. 30, 1964 &ndash; Jan. 6, 1976</li>\r\n\t<li>Muzz Patrick: April 22, 1955 &ndash; Oct. 30, 1964</li>\r\n\t<li>Frank Boucher: Feb. 21, 1946 &ndash; April 22, 1955</li>\r\n\t<li>Lester Patrick: Oct. 27, 1926 &ndash; Feb. 21, 1946^</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n\t<li>^ <em>The Rangers hired&nbsp;Conn Smythe as their first GM, but he left the team before the start of their inaugural season</em></li>\r\n</ul>\r\n
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>Stan Bowman: July 14, 2009 &ndash; Present</li>\r\n\t<li>Dale Tallon: June 21, 2005 &ndash; July 14, 2009</li>\r\n\t<li>Bob Pulford: Oct. 24, 2003 &ndash; June 21, 2005</li>\r\n\t<li>Mike Smith: Sept. 22, 2000 &ndash; Oct. 24, 2003</li>\r\n\t<li>Bob Pulford: Dec. 2, 1999 &ndash; Sept. 22, 2000</li>\r\n\t<li>Bob Murray: July 3, 1997 &ndash; Dec. 2, 1999</li>\r\n\t<li>Bob Pulford: Nov. 6, 1992 &ndash; July 3, 1997</li>\r\n\t<li>Mike Keenan: June 5, 1990 &ndash; Nov. 6, 1992</li>\r\n\t<li>Bob Pulford: July 6, 1977 &ndash; June 5, 1990</li>\r\n\t<li>Tommy Ivan: July 8, 1954 &ndash; July 6, 1977</li>\r\n\t<li>Bill Tobin: May 26, 1936 &ndash; July 7, 1954</li>\r\n\t<li>Clem Loughlin: May 11, 1934 &ndash; May 26, 1936</li>\r\n\t<li>Tommy Gorman: Jan. 14, 1933 &ndash; May 11, 1934</li>\r\n\t<li>Frederic McLaughlin: Sept. 25, 1926 &ndash; Jan. 14, 1933</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Steve Yzerman: April 19, 2019 &ndash; Present</li>\r\n\t<li>Ken Holland: July 18, 1997 &ndash; April 19, 2019</li>\r\n\t<li>Jim Devellano: June 3, 1994&nbsp;&ndash; July 18, 1997</li>\r\n\t<li>Bryan Murray: July 13, 1990 &ndash; June 3, 1994</li>\r\n\t<li>Jim Devellano: July 12, 1982 &ndash; July 11, 1990</li>\r\n\t<li>Jimmy Skinner: April 11, 1980 &ndash; July 12, 1982</li>\r\n\t<li>Ted Lindsay: March 16, 1977 &ndash; April 11, 1980</li>\r\n\t<li>Alex Delvecchio: May 21, 1974 &ndash; March 16, 1977</li>\r\n\t<li>Jimmy Skinner: Feb. 6&nbsp;&ndash; May 21, 1974</li>\r\n\t<li>Ned Harkness: Jan. 8, 1971 &ndash; Feb. 6, 1974</li>\r\n\t<li>Sid Abel: April 26, 1962 &ndash; Jan. 6, 1971</li>\r\n\t<li>Jack Adams: May 14, 1927 &ndash; April 26, 1962</li>\r\n\t<li>Art Duncan: Oct. 18, 1926 &ndash; May 14, 1927</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Rob Blake: April 10, 2017 &ndash; Present</li>\r\n\t<li>Dean Lombardi: April 21, 2006 &ndash; April 10, 2017</li>\r\n\t<li>Dave Taylor: April 22, 1997 &ndash; April 18, 2006</li>\r\n\t<li>Sam McMaster: May 24, 1994 &ndash; April 22, 1997</li>\r\n\t<li>Nick Beverley: June 25, 1992 &ndash; May 18, 1994</li>\r\n\t<li>Rogie Vachon: Jan. 30, 1984 &ndash; June 25, 1992</li>\r\n\t<li>George Maguire: May 28, 1977 &ndash; Jan. 30, 1984</li>\r\n\t<li>Jake Milford: Dec. 17, 1973 &ndash; May 26, 1977</li>\r\n\t<li>Larry Regan: May 17, 1967 &ndash; Dec. 17, 1973</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <ul class="striped-list">\r\n\t<li>Jim Nill: April 29, 2013 &ndash; Present</li>\r\n\t<li>Joe Nieuwendyk: May 31, 2009 &ndash; April 28, 2013</li>\r\n\t<li>Brett Hull and Les Jackson (Co-GMs): Nov. 13, 2007 &ndash; May 31, 2009</li>\r\n\t<li>Doug Armstrong: Jan. 25, 2002 &ndash; Nov. 13, 2007</li>\r\n\t<li>Bob Gainey: June 9, 1992 &ndash; Jan. 25, 2002</li>\r\n\t<li>Bobby Clarke: June 8, 1990 &ndash; June 9, 1992</li>\r\n\t<li>Jack Ferreira: June 14, 1988 &ndash; May 10, 1990</li>\r\n\t<li>Lou Nanne: Feb. 10, 1978 &ndash; June 14, 1988</li>\r\n\t<li>Jack Gordon: April 25, 1974 &ndash; Feb. 10, 1978</li>\r\n\t<li>Wren Blair: May 20, 1966 &ndash; April 19, 1974</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>Chuck Fletcher: Dec. 3, 2018 &ndash; Present</li>\r\n\t<li>Ron Hextall: May 7, 2014 &ndash; Nov. 26, 2018</li>\r\n\t<li>Paul Holmgren: Oct. 22, 2006 &ndash; May 7, 2014</li>\r\n\t<li>Bobby Clarke: June 15, 1994 &ndash; Oct. 22, 2006</li>\r\n\t<li>Russ Farwell: June 6, 1990 &ndash; June 15, 1994</li>\r\n\t<li>Bobby Clarke: May 15, 1984 &ndash; April 16, 1990</li>\r\n\t<li>Bob McCammon: May 28, 1983 &ndash; April 25, 1984</li>\r\n\t<li>Keith Allen: Dec. 22, 1969 &ndash; May 28, 1983</li>\r\n\t<li>Bud Poile: May 31, 1966 &ndash; Dec. 19, 1969</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <ul class="striped-list">\r\n\t<li>Ron Hextall: Feb. 9, 2021&nbsp;&ndash; Present</li>\r\n\t<li>Patrik Allvin: Jan. 27&nbsp;&ndash; Feb. 9, 2021</li>\r\n\t<li>Jim Rutherford: June 6, 2014 &ndash; Jan. 27, 2021</li>\r\n\t<li>Jason Botterill: May 16&nbsp;&ndash; June 6, 2014</li>\r\n\t<li>Ray Shero: May 25, 2006 &ndash; May 16, 2014</li>\r\n\t<li>Craig Patrick: Dec. 5, 1989 &ndash; April 20, 2006</li>\r\n\t<li>Tony Esposito: April 14, 1988 &ndash; Dec. 5, 1989</li>\r\n\t<li>Eddie Johnston: May 27, 1983 &ndash; April 14, 1988</li>\r\n\t<li>Baz Bastien: Dec. 3, 1976 &ndash; March 15, 1983</li>\r\n\t<li>Wren Blair: July 1, 1975 &ndash; Dec. 3, 1976</li>\r\n\t<li>Jack Button: Jan. 13, 1974 &ndash; July 1, 1975</li>\r\n\t<li>Jack Riley: Jan. 29, 1972 &ndash; Jan. 13, 1974</li>\r\n\t<li>Red Kelly: May 1, 1970 &ndash; Jan. 29, 1972</li>\r\n\t<li>Jack Riley: June 6, 1967 &ndash; May 1, 1970</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Doug Armstrong: July 1, 2010 &ndash; Present</li>\r\n\t<li>Larry Pleau: June 9, 1997 &ndash; June 30, 2010</li>\r\n\t<li>Ron Caron: Dec. 19, 1996 &ndash; June 9, 1997</li>\r\n\t<li>Mike Keenan: July 17, 1994 &ndash; Dec. 19, 1996</li>\r\n\t<li>Ron Caron: Aug. 13, 1983 &ndash; July 17, 1994</li>\r\n\t<li>Emile Francis: April 12, 1976 &ndash; May 2, 1983</li>\r\n\t<li>Sid Salomon III: Aug. 24, 1974 &ndash; April 12, 1976</li>\r\n\t<li>Lou Angotti: April 7&nbsp;&ndash; Aug. 24, 1974</li>\r\n\t<li>Charles Catto: May 7, 1973 &ndash; April 7, 1974</li>\r\n\t<li>Sid Abel: Oct. 30, 1971 &ndash; April 17, 1973</li>\r\n\t<li>Lynn Patrick: May 7&nbsp;&ndash; Oct. 30, 1971</li>\r\n\t<li>Scotty Bowman: May 29, 1968 &ndash; April 30, 1971</li>\r\n\t<li>Lynn Patrick: May 5, 1966 &ndash; May 29, 1968</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <ul class="striped-list">\r\n\t<li>Kevyn Adams: June 16, 2020&nbsp;&ndash; Present</li>\r\n\t<li>Jason Botterill: May 11, 2017 &ndash; June 16, 2020</li>\r\n\t<li>Tim Murray: Jan. 9, 2014 &ndash; April 20, 2017</li>\r\n\t<li>Darcy Regier: June 11, 1997 &ndash; Nov. 13, 2013</li>\r\n\t<li>John Muckler: July 31, 1993 &ndash; May 14, 1997</li>\r\n\t<li>Gerry Meehan: Dec. 2, 1986 &ndash; July 31, 1993</li>\r\n\t<li>Scotty Bowman: June 11, 1979 &ndash; Dec. 2, 1986</li>\r\n\t<li>John Andersen: Dec. 4, 1978 &ndash; June 11, 1979</li>\r\n\t<li>Punch Imlach: Jan. 16, 1970 &ndash; Dec. 4, 1978</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <ul class="striped-list">\r\n\t<li>Jim Benning: May 21, 2014 &ndash; Present</li>\r\n\t<li>Mike Gillis: April 23, 2008 &ndash; April 8, 2014</li>\r\n\t<li>Dave Nonis: May 6, 2004 &ndash; April 14, 2008</li>\r\n\t<li>Brian Burke: June 22, 1998 &ndash; May 3, 2004</li>\r\n\t<li>Mike Keenan: Jan. 25&nbsp;&ndash; June 22, 1998</li>\r\n\t<li>Pat Quinn: May 1, 1987 &ndash; Nov. 4, 1997</li>\r\n\t<li>Jack Gordon: June 4, 1985 &ndash; June 1, 1987</li>\r\n\t<li>Harry Neale: June 1, 1982 &ndash; May 23, 1985</li>\r\n\t<li>Jake Milford: May 31, 1977 &ndash; June 1, 1982</li>\r\n\t<li>Phil Maloney: Feb. 1, 1974 &ndash; May 31, 1977</li>\r\n\t<li>Hal Laycoe: Dec. 16, 1972 &ndash; Jan. 31, 1974</li>\r\n\t<li>Bud Poile: Feb. 25, 1970 &ndash; Dec. 16, 1972</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <ul class="striped-list">\r\n\t<li>Brad Treliving: April 28, 2014 &ndash; Present</li>\r\n\t<li>Brian Burke: Dec. 12, 2013 &ndash; April 28, 2014</li>\r\n\t<li>Jay Feaster: Dec. 28, 2010 &ndash; Dec. 12, 2013</li>\r\n\t<li>Darryl Sutter: April 11, 2003 &ndash; Dec. 28, 2010</li>\r\n\t<li>Craig Button: June 6, 2000 &ndash; April 11, 2003</li>\r\n\t<li>Al Coates: Nov. 3, 1995 &ndash; April 11, 2000</li>\r\n\t<li>Doug Risebrough: May 16, 1991 &ndash; Nov. 3, 1995</li>\r\n\t<li>Cliff Fletcher: Jan. 12, 1972 &ndash; May 16, 1991</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Lou Lamoriello: June 5, 2018 &ndash; Present</li>\r\n\t<li>Garth Snow: July 18, 2006 &ndash; June 5, 2018</li>\r\n\t<li>Neil Smith: June 6&nbsp;&ndash; July 18, 2006</li>\r\n\t<li>Mike Milbury: Dec. 12, 1995 &ndash; June 6, 2006</li>\r\n\t<li>Darcy Regier: Dec. 2-12, 1995</li>\r\n\t<li>Don Maloney: Aug. 17, 1992 &ndash; Dec. 2, 1995</li>\r\n\t<li>Bill Torrey: Feb. 14, 1972 &ndash; Aug. 17, 1992</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>Tom Fitzgerald: Jan. 12, 2020 &ndash; Present</li>\r\n\t<li>Ray Shero: May 4, 2015 &ndash; Jan. 12, 2020</li>\r\n\t<li>Lou Lamoriello: Sept. 10, 1987 &ndash; May 4, 2015</li>\r\n\t<li>Max McNab: Nov. 22, 1983 &ndash; Sept. 10, 1987</li>\r\n\t<li>Bill MacMillan: May 4, 1981 &ndash; Nov. 22, 1983</li>\r\n\t<li>Ray Miron: Aug. 23, 1976 &ndash; May 1, 1981</li>\r\n\t<li>Baz Bastien: Feb. 14&nbsp;&ndash; April 5, 1976</li>\r\n\t<li>Sid Abel: June 1, 1973 &ndash; Feb. 13, 1976</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>Brian MacLellan: May 26, 2014 &ndash; Present</li>\r\n\t<li>George McPhee: June 9, 1997 &ndash; May 26, 2014</li>\r\n\t<li>David Poile: Aug. 30, 1982 &ndash; June 9, 1997</li>\r\n\t<li>Roger Crozier: Nov. 5, 1981 &ndash; Aug. 30, 1982</li>\r\n\t<li>Max McNab: Dec. 31, 1975 &ndash; Nov. 5, 1981</li>\r\n\t<li>Milt Schmidt: April 20, 1973 &ndash; Dec. 29, 1975</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>Ken Holland: May 7, 2019 &ndash; Present</li>\r\n\t<li>Keith Gretzky: Jan. 23 &ndash; May 7, 2019</li>\r\n\t<li>Peter Chiarelli: April 23, 2015 &ndash; Jan. 22, 2019</li>\r\n\t<li>Craig MacTavish: April 15, 2013 &ndash; April 23, 2015</li>\r\n\t<li>Steve Tambellini: July 31, 2008 &ndash; April 15, 2013</li>\r\n\t<li>Kevin Lowe: June 9, 2000 &ndash; July 31, 2008</li>\r\n\t<li>Glen Sather: May 1980 &ndash; June 1, 2000</li>\r\n\t<li>Larry Gordon: April 26, 1978 &ndash; May 1980</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <ul class="striped-list">\r\n\t<li>Don Waddell: May 8, 2018 &ndash; Present</li>\r\n\t<li>Ron Francis: April 28, 2014 &ndash; March 7, 2018</li>\r\n\t<li>Jim Rutherford: June 28, 1994 &ndash; April 28, 2014</li>\r\n\t<li>Paul Holmgren: Sept. 8, 1993 &ndash; June 28, 1994</li>\r\n\t<li>Brian Burke: May 26, 1992 &ndash; Sept. 1, 1993</li>\r\n\t<li>Eddie Johnston: May 11, 1989 &ndash; May 12, 1992</li>\r\n\t<li>Emile Francis: May 2, 1983 &ndash; May 11, 1989</li>\r\n\t<li>Larry Pleau: April 2, 1981 &ndash; May 2, 1983</li>\r\n\t<li>Jack Kelley: May 6, 1977 &ndash; April 2, 1981</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <ul class="striped-list">\r\n\t<li>Joe Sakic: Sept. 19, 2014 &ndash; Present</li>\r\n\t<li>Greg Sherman: June 3, 2009 &ndash; Sept. 19, 2014</li>\r\n\t<li>Francois Giguere: May 25, 2006 &ndash; April 13, 2009</li>\r\n\t<li>Pierre Lacroix: May 24, 1994 &ndash; May 25, 2006</li>\r\n\t<li>Pierre Page: May 4, 1990 &ndash; May 24, 1994</li>\r\n\t<li>Maurice Filion: Feb. 2&nbsp;&ndash; May 4, 1990</li>\r\n\t<li>Martin Madden: June 27, 1988 &ndash; Feb. 2, 1990</li>\r\n\t<li>Maurice Filion: May 7, 1974 &ndash; April 19, 1988</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <ul class="striped-list">\r\n\t<li>Bill Armstrong: Sept. 17, 2020 &ndash; Present</li>\r\n\t<li>Steve Sullivan: July 26&nbsp;&ndash; Sept. 17, 2020</li>\r\n\t<li>John Chayka: May 5, 2016 &ndash; July 26, 2020</li>\r\n\t<li>Don Maloney: May 29, 2007 &ndash; April 11, 2016</li>\r\n\t<li>Mike Barnett: Aug. 28, 2001 &ndash; April 11, 2007</li>\r\n\t<li>Cliff Fletcher: Feb. 17&nbsp;&ndash; Aug. 28, 2001</li>\r\n\t<li>Bobby Smith: Dec. 11, 1996 &ndash; Feb. 17, 2001</li>\r\n\t<li>John Paddock: Jan. 19, 1994 &ndash; Dec. 11, 1996</li>\r\n\t<li>Mike Smith: Oct. 31, 1988 &ndash; Jan. 19, 1994</li>\r\n\t<li>John Ferguson Sr.: Nov. 22, 1978 &ndash; Oct. 31, 1988</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <ul class="striped-list">\r\n\t<li>Doug Wilson: May 13, 2003 &ndash; Present</li>\r\n\t<li>Dean Lombardi: March 6, 1996 &ndash; March 18, 2003</li>\r\n\t<li>Chuck Grillo and Dean Lombardi (Co-GMs): June 26, 1992 &ndash; March 6, 1996</li>\r\n\t<li>Jack Ferreira: May 9, 1990 &ndash; June 26, 1992</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <ul class="striped-list">\r\n\t<li>Pierre Dorion: April 10, 2016 &ndash; Present</li>\r\n\t<li>Bryan Murray: June 18, 2007 &ndash; April 10, 2016</li>\r\n\t<li>John Muckler: July 1, 2002 &ndash; June 18, 2007</li>\r\n\t<li>Marshall Johnston: June 8, 1999 &ndash; June 30, 2002</li>\r\n\t<li>Rick Dudley: June 30, 1998 &ndash; June 8, 1999</li>\r\n\t<li>Pierre Gauthier: Dec. 11, 1995 &ndash; June 29, 1998</li>\r\n\t<li>Randy Sexton: April 15, 1993 &ndash; Dec. 11, 1995</li>\r\n\t<li>Mel Bridgman: Aug. 30, 1991 &ndash; April 15, 1993</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>Julien BriseBois: Sept. 11, 2018 &ndash; Present</li>\r\n\t<li>Steve Yzerman: May 25, 2010 &ndash; Sept. 11, 2018</li>\r\n\t<li>Tom Kurvers: April 12&nbsp;&ndash; May 25, 2010</li>\r\n\t<li>Brian Lawton: Oct. 22, 2008 &ndash; April 12, 2010</li>\r\n\t<li>Jay Feaster: Feb. 10, 2002 &ndash; July 11, 2008</li>\r\n\t<li>Rick Dudley: June 28, 1999 &ndash; Feb. 10, 2002</li>\r\n\t<li>Jacques Demers: Oct. 13, 1998 &ndash; July 14, 1999</li>\r\n\t<li>Phil Esposito: April 4, 1991 &ndash; Oct. 13, 1998</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <ul class="striped-list">\r\n\t<li>Bob Murray: Nov. 12, 2008 &ndash; Present</li>\r\n\t<li>Brian Burke: June 20, 2005 &ndash; Nov. 12, 2008</li>\r\n\t<li>Al Coates: June 8, 2004 &ndash; June 20, 2005</li>\r\n\t<li>Bryan Murray: May 2, 2002 &ndash; June 8, 2004</li>\r\n\t<li>Pierre Gauthier: Aug. 6, 1998 &ndash; April 19, 2002</li>\r\n\t<li>Jack Ferreira: March 23, 1993 &ndash; Aug. 6, 1998</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 33                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <ul class="striped-list">\r\n\t<li>Bill Zito: Sept. 2, 2020 &ndash; Present</li>\r\n\t<li>Dale Tallon: April 10, 2017 &ndash; Aug. 10, 2020</li>\r\n\t<li>Tom Rowe: May 16, 2016 &ndash; April 10, 2017</li>\r\n\t<li>Dale Tallon: May 17, 2010 &ndash; May 16, 2016</li>\r\n\t<li>Randy Sexton: Oct. 2, 2009 &ndash; May 17, 2010</li>\r\n\t<li>Jacques Martin: Sept. 3, 2006 &ndash; June 1, 2009</li>\r\n\t<li>Mike Keenan: May 26, 2004 &ndash; Sept. 3, 2006</li>\r\n\t<li>Rick Dudley: May 10, 2002 &ndash; May 24, 2004</li>\r\n\t<li>Chuck Fletcher: Dec. 3, 2001 &ndash; May 10, 2002</li>\r\n\t<li>Bill Torrey: Dec. 28, 2000 &ndash; Dec. 3, 2001</li>\r\n\t<li>Bryan Murray: Aug. 1, 1994 &ndash; Dec. 28, 2000</li>\r\n\t<li>Bobby Clarke: March 1, 1993 &ndash; June 15, 1994</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 34                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>David Poile: July 9, 1997 &ndash; Present</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 35                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <ul class="striped-list">\r\n\t<li>Kevin Cheveldayoff: June 8, 2011 &ndash; Present</li>\r\n\t<li>Rick Dudley: April 14, 2010 &ndash; June 5, 2011</li>\r\n\t<li>Don Waddell: June 23, 1998 &ndash; April 14, 2010</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 36                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>Jarmo Kekalainen: Feb. 13, 2013 &ndash; Present</li>\r\n\t<li>Scott Howson: June 15, 2007 &ndash; Feb. 12, 2013</li>\r\n\t<li>Jim Clark: April 19&nbsp;&ndash; June 15, 2007</li>\r\n\t<li>Doug MacLean: Feb. 11, 1998 &ndash; April 19, 2007</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 37                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>Bill Guerin: Aug. 21, 2019&nbsp;&ndash; Present</li>\r\n\t<li>Tom Kurvers: July 30&nbsp;&ndash; Aug. 21, 2019</li>\r\n\t<li>Paul Fenton: May 21, 2018 &ndash; July 30, 2019</li>\r\n\t<li>Chuck Fletcher: May 22, 2009 &ndash; April 23, 2018</li>\r\n\t<li>Doug Risebrough: Sept. 2, 1999 &ndash; April 16, 2009</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 38                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <ul class="striped-list">\r\n\t<li>Kelly McCrimmon: Sept. 1, 2019 &ndash; Present</li>\r\n\t<li>George McPhee: July 13, 2016 &ndash; Sept. 1, 2019</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ## 39                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <NA>
    ##                                                                                             heroImageUrl
    ## 1                            https://records.nhl.com/site/asset/public/ext/hero/Team Pages/MTL/Price.jpg
    ## 2  https://records.nhl.com/site/asset/public/images/hero/teams/defunct-franchises/montreal-wanderers.jpg
    ## 3                              https://records.nhl.com/site/asset/public/ext/hero/Team Pages/1927SEN.JPG
    ## 4     https://records.nhl.com/site/asset/public/images/hero/teams/defunct-franchises/hamilton-tigers.jpg
    ## 5                   https://records.nhl.com/site/asset/public/ext/hero/Team Pages/TOR/MatthewsMarner.jpg
    ## 6                 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/BOS/BergeronPastrnak.jpg
    ## 7    https://records.nhl.com/site/asset/public/images/hero/teams/defunct-franchises/montreal-maroons.jpg
    ## 8                          https://records.nhl.com/site/asset/public/ext/hero/Team Pages/NYAmericans.jpg
    ## 9  https://records.nhl.com/site/asset/public/images/hero/teams/defunct-franchises/pittsburgh-pirates.jpg
    ## 10                https://records.nhl.com/site/asset/public/ext/hero/Team Pages/NYR/PanarinZibanejad.jpg
    ## 11                           https://records.nhl.com/site/asset/public/ext/hero/Team Pages/ToewsKane.jpg
    ## 12                        https://records.nhl.com/site/asset/public/ext/hero/Team Pages/DET/WingsWin.jpg
    ## 13                               https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Seals.jpg
    ## 14                               https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Kings.jpg
    ## 15                               https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Stars.jpg
    ## 16                              https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Giroux.jpg
    ## 17                            https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Penguins.jpg
    ## 18                https://records.nhl.com/site/asset/public/ext/hero/Team Pages/STL/PerronCelebrates.jpg
    ## 19                              https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Sabres.jpg
    ## 20                             https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Canucks.jpg
    ## 21                              https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Flames.jpg
    ## 22                          https://records.nhl.com/site/asset/public/ext/hero/Team Pages/NYI/Barzal.jpg
    ## 23                          https://records.nhl.com/site/asset/public/ext/hero/Team Pages/NJD/Subban.jpg
    ## 24                            https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Capitals.jpg
    ## 25                              https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Oilers.jpg
    ## 26                                 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Aho.jpg
    ## 27                  https://records.nhl.com/site/asset/public/ext/hero/Team Pages/MacKinnonLandeskog.jpg
    ## 28                      https://records.nhl.com/site/asset/public/ext/hero/Team Pages/ARI/OELKuemper.jpg
    ## 29                      https://records.nhl.com/site/asset/public/ext/hero/Team Pages/SJS/SharksGoal.jpg
    ## 30                            https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Senators.jpg
    ## 31                          https://records.nhl.com/site/asset/public/ext/hero/LightningCupTeamPhoto.jpg
    ## 32                             https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Getzlaf.jpg
    ## 33               https://records.nhl.com/site/asset/public/ext/hero/Team Pages/FLA/PanthersCelebrate.jpg
    ## 34                       https://records.nhl.com/site/asset/public/ext/hero/Team Pages/PredatorsGoal.jpg
    ## 35                                https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Jets.jpg
    ## 36                         https://records.nhl.com/site/asset/public/ext/hero/Team Pages/BlueJackets.jpg
    ## 37                        https://records.nhl.com/site/asset/public/ext/hero/Team Pages/MIN/WildTrio.jpg
    ## 38                 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/VGK/StonePacioretty.jpg
    ## 39                                                                                                  <NA>
    ##    mostRecentTeamId
    ## 1                 8
    ## 2                41
    ## 3                45
    ## 4                37
    ## 5                10
    ## 6                 6
    ## 7                43
    ## 8                51
    ## 9                39
    ## 10                3
    ## 11               16
    ## 12               17
    ## 13               49
    ## 14               26
    ## 15               25
    ## 16                4
    ## 17                5
    ## 18               19
    ## 19                7
    ## 20               23
    ## 21               20
    ## 22                2
    ## 23                1
    ## 24               15
    ## 25               22
    ## 26               12
    ## 27               21
    ## 28               53
    ## 29               28
    ## 30                9
    ## 31               14
    ## 32               24
    ## 33               13
    ## 34               18
    ## 35               52
    ## 36               29
    ## 37               30
    ## 38               54
    ## 39               55
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  retiredNumbersSummary
    ## 1                                                                                                <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Jacques Plante (1952-63)</li>\r\n\t<li>2 &ndash;&nbsp;Doug Harvey (1947-61)</li>\r\n\t<li>3 &ndash;&nbsp;Butch Bouchard (1941-56)</li>\r\n\t<li>4 &ndash;&nbsp;Jean Beliveau (1950-71)</li>\r\n\t<li>5 &ndash;&nbsp;Bernie&nbsp;Geoffrion (1950-64)</li>\r\n\t<li>5 &ndash;&nbsp;Guy Lapointe (1968-82)</li>\r\n\t<li>7 &ndash;&nbsp;Howie Morenz (1923-37)</li>\r\n\t<li>9 &ndash;&nbsp;Maurice Richard (1942-60)</li>\r\n\t<li>10 &ndash;&nbsp;Guy Lafleur (1971-84)</li>\r\n\t<li>12 &ndash;&nbsp;Dickie Moore (1951-63)</li>\r\n\t<li>12 &ndash;&nbsp;Yvan Cournoyer (1963-79)</li>\r\n\t<li>16 &ndash;&nbsp;Elmer Lach (1940-54)</li>\r\n\t<li>16 &ndash;&nbsp;Henri Richard (1955-75)</li>\r\n\t<li>18 &ndash;&nbsp;Serge Savard (1966-81)</li>\r\n\t<li>19 &ndash;&nbsp;Larry Robinson (1972-89)</li>\r\n\t<li>23 &ndash;&nbsp;Bob Gainey (1973-89)</li>\r\n\t<li>29 &ndash;&nbsp;Ken Dryden (1970-79)</li>\r\n\t<li>33 &ndash;&nbsp;Patrick Roy (1984-95)</li>\r\n</ul>\r\n
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
    ## 5  <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Turk Broda (1936-43, 1945-52)</li>\r\n\t<li>1 &ndash;&nbsp;Johnny Bower (1958-70)</li>\r\n\t<li>4 &ndash;&nbsp;Hap Day (1926-37)</li>\r\n\t<li>4 &ndash;&nbsp;Red Kelly (1959-67)</li>\r\n\t<li>5 &ndash;&nbsp;Bill Barilko (1946-51)</li>\r\n\t<li>6 &ndash;&nbsp;Ace Bailey (1926-34)</li>\r\n\t<li>7 &ndash;&nbsp;King Clancy (1930-37)</li>\r\n\t<li>7 &ndash;&nbsp;Tim Horton (1949-50, 1951-70)</li>\r\n\t<li>9 &ndash;&nbsp;Charlie Conacher (1929-38)</li>\r\n\t<li>9 &ndash;&nbsp;Ted Kennedy (1942-55, 1956-57)</li>\r\n\t<li>10 &ndash;&nbsp;Syl Apps (1936-43, 1945-48)</li>\r\n\t<li>10 &ndash;&nbsp;George Armstrong (1949-50, 1951-71)</li>\r\n\t<li>13 &ndash;&nbsp;Mats Sundin (1994-08)</li>\r\n\t<li>14 &ndash;&nbsp;Dave Keon (1960-75)</li>\r\n\t<li>17 &ndash;&nbsp;Wendel Clark (1985-94, 1996-98, 2000)</li>\r\n\t<li>21 &ndash;&nbsp;Borje Salming (1973-89)</li>\r\n\t<li>27 &ndash;&nbsp;Frank Mahovlich (1956-68)</li>\r\n\t<li>27 &ndash;&nbsp;Darryl Sittler (1970-82)</li>\r\n\t<li>93 &ndash;&nbsp;Doug Gilmour (1992-97, 2003)</li>\r\n</ul>\r\n
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <ul class="striped-list">\r\n\t<li>2 &ndash;&nbsp;Eddie Shore (1926-40)</li>\r\n\t<li>3 &ndash;&nbsp;Lionel Hitchman (1925-34)</li>\r\n\t<li>4 &ndash;&nbsp;Bobby Orr (1966-76)</li>\r\n\t<li>5 &ndash;&nbsp;Dit Clapper (1927-47)</li>\r\n\t<li>7 &ndash;&nbsp;Phil Esposito (1967-75)</li>\r\n\t<li>8 &ndash;&nbsp;Cam Neely (1986-96)</li>\r\n\t<li>9 &ndash;&nbsp;Johnny Bucyk (1957-78)</li>\r\n\t<li>15 &ndash;&nbsp;Milt Schmidt (1936-55)</li>\r\n\t<li>16 &ndash;&nbsp;Rick Middleton (1976-88)</li>\r\n\t<li>24 &ndash;&nbsp;Terry O&rsquo;Reilly (1971-85)</li>\r\n\t<li>77 &ndash;&nbsp;Ray&nbsp;Bourque (1979-00)</li>\r\n</ul>\r\n
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <NA>
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Ed Giacomin (1965-75)</li>\r\n\t<li>2 &ndash;&nbsp;Brian Leetch (1987-04)</li>\r\n\t<li>3 &ndash;&nbsp;Harry Howell (1952-69)</li>\r\n\t<li>7 &ndash;&nbsp;Rod Gilbert (1960-77)</li>\r\n\t<li>9 &ndash;&nbsp;Andy Bathgate (1952-64)</li>\r\n\t<li>9 &ndash;&nbsp;Adam Graves (1991-01)</li>\r\n\t<li>11 &ndash;&nbsp;Vic Hadfield (1961-74)</li>\r\n\t<li>11 &ndash;&nbsp;Mark Messier (1991-97, 2000-04)</li>\r\n\t<li>19 &ndash;&nbsp;Jean Ratelle (1960-76)</li>\r\n\t<li>35 &ndash;&nbsp;Mike Richter (1989-03)</li>\r\n</ul>\r\n
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Glenn Hall (1957-67)</li>\r\n\t<li>3 &ndash;&nbsp;Pierre Pilote (1955-68)</li>\r\n\t<li>3 &ndash;&nbsp;Keith Magnuson (1969-80)</li>\r\n\t<li>9 &ndash;&nbsp;Bobby Hull (1957-72)</li>\r\n\t<li>18 &ndash;&nbsp;Denis Savard (1980-90, 1995-97)</li>\r\n\t<li>21 &ndash;&nbsp;Stan Mikita (1958-80)</li>\r\n\t<li>35 &ndash;&nbsp;Tony Esposito (1969-84)</li>\r\n</ul>\r\n
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Terry Sawchuk (1949-55, 1957-64, 1968-69)</li>\r\n\t<li>4 &ndash;&nbsp;Red Kelly (1947-60)</li>\r\n\t<li>5 &ndash;&nbsp;Nicklas Lidstrom (1991-12)</li>\r\n\t<li>7 &ndash;&nbsp;Ted Lindsay (1947-57, 1964-65)</li>\r\n\t<li>9 &ndash;&nbsp;Gordie Howe (1946-71)</li>\r\n\t<li>10 &ndash;&nbsp;Alex Delvecchio (1951-73)</li>\r\n\t<li>12 &ndash;&nbsp;Sid Abel (1938-43, 1945-52)</li>\r\n\t<li>19 &ndash;&nbsp;Steve Yzerman (1983-06)</li>\r\n</ul>\r\n
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <ul class="striped-list">\r\n\t<li>4 &ndash;&nbsp;Rob Blake (1990-01, 2006-08)</li>\r\n\t<li>16 &ndash;&nbsp;Marcel Dionne (1975-87)</li>\r\n\t<li>18 &ndash;&nbsp;Dave Taylor (1977-94)</li>\r\n\t<li>20 &ndash;&nbsp;Luc Robitaille (1986-94, 1997-01, 2003-06)</li>\r\n\t<li>30 &ndash;&nbsp;Rogie Vachon (1971-78)</li>\r\n\t<li>99 &ndash;&nbsp;Wayne Gretzky (1988-96)</li>\r\n</ul>\r\n
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <ul class="striped-list">\r\n\t<li>7 &ndash;&nbsp;Neal Broten (1980-95, 1996-97)</li>\r\n\t<li>8 &ndash;&nbsp;Bill Goldsworthy&nbsp;(1967-76)</li>\r\n\t<li>9 &ndash;&nbsp;Mike Modano (1989-10)</li>\r\n\t<li>19 &ndash;&nbsp;Bill Masterton&nbsp;(1967-68)</li>\r\n\t<li>26 &ndash;&nbsp;Jere Lehtinen (1995-10)</li>\r\n</ul>\r\n
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Bernie Parent (1967-71, 1973-79)</li>\r\n\t<li>2 &ndash;&nbsp;Mark Howe (1982-92)</li>\r\n\t<li>4 &ndash;&nbsp;Barry Ashbee (1970-74)</li>\r\n\t<li>7 &ndash;&nbsp;Bill Barber (1972-85)</li>\r\n\t<li>16 &ndash;&nbsp;Bobby Clarke (1969-84)</li>\r\n\t<li>88 &ndash;&nbsp;Eric Lindros (1992-00)</li>\r\n</ul>\r\n
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>21 &ndash;&nbsp;Michel Briere (1969-70)</li>\r\n\t<li>66 &ndash;&nbsp;Mario Lemieux (1984-97, 2000-06)</li>\r\n</ul>\r\n
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <ul class="striped-list">\r\n\t<li>2 &ndash;&nbsp;Al MacInnis (1994-04)</li>\r\n\t<li>3 &ndash;&nbsp;Bob Gassoff (1973-77)</li>\r\n\t<li>5 &ndash;&nbsp;Bob Plager (1967-78)</li>\r\n\t<li>8 &ndash;&nbsp;Barclay Plager (1967-77)</li>\r\n\t<li>11 &ndash;&nbsp;Brian Sutter (1976-88)</li>\r\n\t<li>16 &ndash;&nbsp;Brett Hull (1987-98)</li>\r\n\t<li>24 &ndash;&nbsp;Bernie Federko (1976-89)</li>\r\n</ul>\r\n
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>2 &ndash;&nbsp;Tim Horton (1972-74)</li>\r\n\t<li>7 &ndash;&nbsp;Rick Martin (1971-81)</li>\r\n\t<li>11 &ndash;&nbsp;Gilbert Perreault (1970-87)</li>\r\n\t<li>14 &ndash;&nbsp;Rene Robert (1971-79)</li>\r\n\t<li>16 &ndash;&nbsp;Pat LaFontaine (1991-96)</li>\r\n\t<li>18 &ndash;&nbsp;Danny Gare (1974-81)</li>\r\n\t<li>39 &ndash;&nbsp;Dominik Hasek (1992-01)</li>\r\n</ul>\r\n
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>10 &ndash;&nbsp;Pavel Bure (1991-98)</li>\r\n\t<li>12 &ndash;&nbsp;Stan Smyl (1978-91)</li>\r\n\t<li>16 &ndash;&nbsp;Trevor Linden (1988-98, 2001-08)</li>\r\n\t<li>19 &ndash;&nbsp;Markus Naslund (1996-08)</li>\r\n\t<li>22 &ndash;&nbsp;Daniel Sedin (2000-18)</li>\r\n\t<li>33 &ndash;&nbsp;Henrik Sedin (2000-18)</li>\r\n</ul>\r\n
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <ul class="striped-list">\r\n\t<li>9 &ndash;&nbsp;Lanny McDonald (1981-89)</li>\r\n\t<li>12 &ndash;&nbsp;Jarome Iginla (1996-13)</li>\r\n\t<li>30 &ndash;&nbsp;Mike Vernon (1982-94, 2000-02)</li>\r\n</ul>\r\n\r\n<p><strong>Honored Numbers</strong></p>\r\n\r\n<ul class="striped-list">\r\n\t<li>2 &ndash;&nbsp;Al MacInnis (1981-94)</li>\r\n\t<li>25 &ndash;&nbsp;Joe Nieuwendyk (1986-95)</li>\r\n</ul>\r\n
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>5 &ndash;&nbsp;Denis Potvin (1973-88)</li>\r\n\t<li>9 &ndash;&nbsp;Clark Gillies (1974-86)</li>\r\n\t<li>19 &ndash;&nbsp;Bryan Trottier (1975-90)</li>\r\n\t<li>22 &ndash;&nbsp;Mike Bossy (1977-87)</li>\r\n\t<li>23 &ndash;&nbsp;Bobby Nystrom (1972-86)</li>\r\n\t<li>27 &ndash;&nbsp;John Tonelli (1978-86)</li>\r\n\t<li>31 &ndash;&nbsp;Billy Smith (1972-89)</li>\r\n\t<li>91 &ndash;&nbsp;Butch Goring (1980-84)</li>\r\n</ul>\r\n
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <ul class="striped-list">\r\n\t<li>3 &ndash;&nbsp;Ken Daneyko (1982-03)</li>\r\n\t<li>4 &ndash;&nbsp;Scott Stevens (1991-05)</li>\r\n\t<li>26 &ndash;&nbsp;Patrik Elias (1996-16)</li>\r\n\t<li>27 &ndash;&nbsp;Scott Niedermayer (1991-04)</li>\r\n\t<li>30 &ndash;&nbsp;Martin Brodeur (1992-14)</li>\r\n</ul>\r\n
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <ul class="striped-list">\r\n\t<li>5 &ndash;&nbsp;Rod Langway (1982-93)</li>\r\n\t<li>7 &ndash;&nbsp;Yvon Labre (1974-81)</li>\r\n\t<li>11 &ndash;&nbsp;Mike Gartner (1979-89)</li>\r\n\t<li>32 &ndash;&nbsp;Dale Hunter (1987-99)</li>\r\n</ul>\r\n
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <ul class="striped-list">\r\n\t<li>3 &ndash;&nbsp;Al Hamilton&nbsp;(1972-80)*</li>\r\n\t<li>7 &ndash;&nbsp;Paul Coffey&nbsp;(1980-87)</li>\r\n\t<li>9 &ndash;&nbsp;Glenn Anderson&nbsp;(1980-91, 1996)</li>\r\n\t<li>11 &ndash;&nbsp;Mark Messier&nbsp;(1980-91)</li>\r\n\t<li>17 &ndash;&nbsp;Jari Kurri&nbsp;(1980-90)</li>\r\n\t<li>31 &ndash;&nbsp;Grant Fuhr&nbsp;(1981-91)</li>\r\n\t<li>99 &ndash;&nbsp;Wayne Gretzky (1979-88)</li>\r\n\t<li>* <em>Includes time with WHA&rsquo;s Edmonton Oilers</em></li>\r\n</ul>\r\n
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <ul class="striped-list">\r\n\t<li>2 &ndash;&nbsp;Glen Wesley (1994-08)</li>\r\n\t<li>10 &ndash;&nbsp;Ron Francis (1981-91, 1998-04)</li>\r\n\t<li>17 &ndash;&nbsp;Rod Brind&rsquo;Amour (2000-10) &nbsp;</li>\r\n</ul>\r\n
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                       <ul class="striped-list">\r\n\t<li>3 &ndash;&nbsp;J.C. Tremblay (1972-79)*</li>\r\n\t<li>8 &ndash;&nbsp;Marc Tardif (1974-83)*</li>\r\n\t<li>16 &ndash;&nbsp;Michel Goulet (1979-90)</li>\r\n\t<li>19 &ndash;&nbsp;Joe Sakic (1988-08)</li>\r\n\t<li>21 &ndash;&nbsp;Peter Forsberg (1994-04, 2008, 2011)</li>\r\n\t<li>23 &ndash;&nbsp;Milan Hejduk (1998-13)</li>\r\n\t<li>26 &ndash;&nbsp;Peter Stastny (1980-90)</li>\r\n\t<li>33 &ndash;&nbsp;Patrick Roy (1995-03)</li>\r\n\t<li>52 &ndash;&nbsp;Adam Foote (1991-04, 2008-11)</li>\r\n\t<li>77 &ndash;&nbsp;Ray&nbsp;Bourque (2000-01)</li>\r\n\t<li>* <em>Includes time with WHA&rsquo;s&nbsp;Quebec Nordiques</em></li>\r\n</ul>\r\n
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>19 &ndash;&nbsp;Shane Doan (1995-17)</li>\r\n</ul>\r\n
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>None</li>\r\n</ul>\r\n
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <ul class="striped-list">\r\n\t<li>4 &ndash;&nbsp;Chris Phillips&nbsp;(1997-2015)</li>\r\n\t<li>8 &ndash;&nbsp;Frank Finnigan&nbsp;(1924-34)*</li>\r\n\t<li>11 &ndash;&nbsp;Daniel Alfredsson (1995-13)</li>\r\n\t<li>* <em>Played for original Ottawa Senators (1917-34)</em></li>\r\n</ul>\r\n
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>4 &ndash;&nbsp;Vincent Lecavalier (1998-13)</li>\r\n\t<li>26 &ndash;&nbsp;Martin St. Louis (2000-14)</li>\r\n</ul>\r\n
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <ul class="striped-list">\r\n\t<li>8 &ndash;&nbsp;Teemu Selanne (1996-01, 2005-14)</li>\r\n\t<li>9 &ndash; Paul Kariya (1994-03)</li>\r\n\t<li>27 &ndash;&nbsp;Scott Niedermayer (2005-10)</li>\r\n</ul>\r\n
    ## 33                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Roberto Luongo (2000-06, 2014-19)</li>\r\n\t<li>37 &ndash;&nbsp;H. Wayne Huizenga (Owner:&nbsp;1993-01)</li>\r\n\t<li>93 &ndash;&nbsp;William A. Torrey (President/Governor/Alternate Governor:&nbsp;1993-18)</li>\r\n</ul>\r\n
    ## 34                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>None</li>\r\n</ul>\r\n
    ## 35                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>None</li>\r\n</ul>\r\n
    ## 36                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>None</li>\r\n</ul>\r\n
    ## 37                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <ul class="striped-list">\r\n\t<li>1&nbsp;&ndash; In Honor of Wild Fans</li>\r\n</ul>\r\n
    ## 38                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <ul class="striped-list">\r\n\t<li>58 &ndash;&nbsp;In Honor of the Victims of the Oct. 1, 2017, Las Vegas Shooting</li>\r\n</ul>\r\n
    ## 39                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <NA>
    ##    teamAbbrev          teamFullName
    ## 1         MTL    Montréal Canadiens
    ## 2         MWN    Montreal Wanderers
    ## 3         SLE      St. Louis Eagles
    ## 4         HAM       Hamilton Tigers
    ## 5         TOR   Toronto Maple Leafs
    ## 6         BOS         Boston Bruins
    ## 7         MMR      Montreal Maroons
    ## 8         BRK    Brooklyn Americans
    ## 9         QUA  Philadelphia Quakers
    ## 10        NYR      New York Rangers
    ## 11        CHI    Chicago Blackhawks
    ## 12        DET     Detroit Red Wings
    ## 13        CLE      Cleveland Barons
    ## 14        LAK     Los Angeles Kings
    ## 15        DAL          Dallas Stars
    ## 16        PHI   Philadelphia Flyers
    ## 17        PIT   Pittsburgh Penguins
    ## 18        STL       St. Louis Blues
    ## 19        BUF        Buffalo Sabres
    ## 20        VAN     Vancouver Canucks
    ## 21        CGY        Calgary Flames
    ## 22        NYI    New York Islanders
    ## 23        NJD     New Jersey Devils
    ## 24        WSH   Washington Capitals
    ## 25        EDM       Edmonton Oilers
    ## 26        CAR   Carolina Hurricanes
    ## 27        COL    Colorado Avalanche
    ## 28        ARI       Arizona Coyotes
    ## 29        SJS       San Jose Sharks
    ## 30        OTT       Ottawa Senators
    ## 31        TBL   Tampa Bay Lightning
    ## 32        ANA         Anaheim Ducks
    ## 33        FLA      Florida Panthers
    ## 34        NSH   Nashville Predators
    ## 35        WPG         Winnipeg Jets
    ## 36        CBJ Columbus Blue Jackets
    ## 37        MIN        Minnesota Wild
    ## 38        VGK  Vegas Golden Knights
    ## 39        SEA        Seattle Kraken

``` r
getFranchiseDetail("Hurricanes")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##   id active
    ## 1 26   TRUE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            captainHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Jordan Staal: 2019-20 &ndash; Present</li>\r\n\t<li>Justin Williams: 2018-19</li>\r\n\t<li>Justin Faulk and Jordan Staal: 2017-18</li>\r\n\t<li>(No Captain): 2016-17</li>\r\n\t<li>Eric Staal: 2010-11 &ndash;&nbsp;2015-16</li>\r\n\t<li>Rod Brind&rsquo;Amour and Eric Staal: 2009-10</li>\r\n\t<li>Rod Brind&rsquo;Amour: 2005-06 &ndash;&nbsp;2008-09</li>\r\n\t<li>Ron Francis: 2000-01 &ndash;&nbsp;2003-04</li>\r\n\t<li>Keith Primeau and Ron Francis: 1999-00</li>\r\n\t<li>Keith Primeau: 1998-99</li>\r\n\t<li>Kevin Dineen: 1996-97 &ndash;&nbsp;1997-98</li>\r\n\t<li>Brendan Shanahan: 1995-96</li>\r\n\t<li>Pat Verbeek: 1992-93 &ndash;&nbsp;1994-95</li>\r\n\t<li>Randy Ladouceur: 1991-92</li>\r\n\t<li>Ron Francis: 1985-86 &ndash;&nbsp;1990-91</li>\r\n\t<li>Mark Johnson and Ron Francis: 1984-85</li>\r\n\t<li>Mark Johnson: 1983-84</li>\r\n\t<li>Russ Anderson: 1982-83</li>\r\n\t<li>Dave Keon: 1981-82</li>\r\n\t<li>Rick Ley and Mike Rogers: 1980-81</li>\r\n\t<li>Rick Ley: 1979-80</li>\r\n</ul>\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     coachingHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Rod Brind&rsquo;Amour: Oct. 4, 2018 &ndash; Present</li>\r\n\t<li>Bill Peters: Oct. 10, 2014 &ndash; April 7, 2018</li>\r\n\t<li>Kirk Muller: Nov. 29, 2011 &ndash; April 13, 2014</li>\r\n\t<li>Paul Maurice: Dec. 4, 2008 &ndash; Nov. 27, 2011</li>\r\n\t<li>Peter Laviolette: Dec. 18, 2003 &ndash; Nov. 30, 2008</li>\r\n\t<li>Paul Maurice: Nov. 7, 1995 &ndash; Dec. 14, 2003</li>\r\n\t<li>Paul Holmgren: Jan. 21&nbsp;&ndash; Nov. 5, 1995</li>\r\n\t<li>Pierre McGuire: Nov. 17, 1993 &ndash; April 14, 1994</li>\r\n\t<li>Paul Holmgren: Oct. 6, 1992 &ndash; Nov. 13, 1993</li>\r\n\t<li>Jim Roberts:&nbsp; Oct. 5, 1991 &ndash; May 1, 1992</li>\r\n\t<li>Rick Ley: Oct. 5, 1989 &ndash; April 13, 1991</li>\r\n\t<li>Larry Pleau: Feb. 7, 1988 &ndash; April 9, 1989</li>\r\n\t<li>Jack Evans: Oct. 5, 1983 &ndash; Feb. 6, 1988</li>\r\n\t<li>John Cunniff: March 8&nbsp;&ndash; April 3, 1983</li>\r\n\t<li>Larry Pleau: Jan. 27&nbsp;&ndash; March 6, 1983</li>\r\n\t<li>Larry Kish: Oct. 6, 1982 &ndash; Jan. 23, 1983</li>\r\n\t<li>Larry Pleau: Feb. 22, 1981 &ndash; April 4, 1982</li>\r\n\t<li>Don Blackburn: Oct. 11, 1979 &ndash; Feb. 19, 1981</li>\r\n\t<li>* <i>Date range indicates first and last games coached during tenure (regular season or playoffs)</i></li>\r\n</ul>\r\n
    ##           dateAwarded
    ## 1 1979-06-22T00:00:00
    ##                                directoryUrl firstSeasonId
    ## 1 https://www.nhl.com/hurricanes/team/staff      19791980
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 generalManagerHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Don Waddell: May 8, 2018 &ndash; Present</li>\r\n\t<li>Ron Francis: April 28, 2014 &ndash; March 7, 2018</li>\r\n\t<li>Jim Rutherford: June 28, 1994 &ndash; April 28, 2014</li>\r\n\t<li>Paul Holmgren: Sept. 8, 1993 &ndash; June 28, 1994</li>\r\n\t<li>Brian Burke: May 26, 1992 &ndash; Sept. 1, 1993</li>\r\n\t<li>Eddie Johnston: May 11, 1989 &ndash; May 12, 1992</li>\r\n\t<li>Emile Francis: May 2, 1983 &ndash; May 11, 1989</li>\r\n\t<li>Larry Pleau: April 2, 1981 &ndash; May 2, 1983</li>\r\n\t<li>Jack Kelley: May 6, 1977 &ndash; April 2, 1981</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ##                                                            heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Aho.jpg
    ##   mostRecentTeamId
    ## 1               12
    ##                                                                                                                                                                                                         retiredNumbersSummary
    ## 1 <ul class="striped-list">\r\n\t<li>2 &ndash;&nbsp;Glen Wesley (1994-08)</li>\r\n\t<li>10 &ndash;&nbsp;Ron Francis (1981-91, 1998-04)</li>\r\n\t<li>17 &ndash;&nbsp;Rod Brind&rsquo;Amour (2000-10) &nbsp;</li>\r\n</ul>\r\n
    ##   teamAbbrev        teamFullName
    ## 1        CAR Carolina Hurricanes

\#\#Stats API function fix name id match

``` r
getTeamIdStats <- function(team){
  Id <- teamNameIdMatch %>% filter(str_detect(fullName, team)) %>%
    select(mostRecentTeamId)
  if (nrow(Id) == 1){
    return(as.numeric(Id))
  } else {
    stop("try again entering team name or ID")
  }
}

getStats <- function(team = NULL){
  if(is.null(team)){
    teamStats <- GET("https://statsapi.web.nhl.com/api/v1/teams?expand=team.stats") %>%
    content("text") %>%
    fromJSON(flatten = TRUE) %>%
    as.data.frame()
    allTeams <- data.frame()
    for (i in seq_along(teamStats$teams.id)){
      allTeams <- bind_rows(allTeams, teamStats[[9]][[i]][[1]][[1]])
    }
    allTeams <- allTeams %>% relocate(starts_with("team"))
    return(allTeams)
  } else{
    if(!is.numeric(team)) {team <- getTeamIdStats(team)}
    fullURL <- paste0("https://statsapi.web.nhl.com/api/v1/teams", "/", team, "?expand=team.stats")
    teamStats <- GET(fullURL) %>%
      content("text") %>%
      fromJSON(flatten = TRUE) %>%
      as.data.frame()
    oneTeam <- teamStats[[9]][[1]][[1]][[1]] %>%
      relocate(starts_with("team"))
    return(oneTeam)
  }
}


stats <- getStats()
activeStats <- stats %>% select(team.name) %>% rename(teamName = team.name) %>% distinct
setdiff(activeTeams, activeStats)
```

    ##                 teamName
    ## 1      Atlanta Thrashers
    ## 2        Phoenix Coyotes
    ## 3  Minnesota North Stars
    ## 4       Quebec Nordiques
    ## 5   Winnipeg Jets (1979)
    ## 6       Hartford Whalers
    ## 7       Colorado Rockies
    ## 8        Detroit Cougars
    ## 9         Atlanta Flames
    ## 10    Kansas City Scouts
    ## 11       Detroit Falcons
    ## 12        Toronto Arenas
    ## 13  Toronto St. Patricks

``` r
teamTotals %>% filter(teamName %in% as_vector(setdiff(activeTeams, activeStats))) %>% select(teamName) %>% n_distinct()
```

    ## [1] 13

``` r
getStats("Devils")
```

    ##   team.id         team.name       team.link
    ## 1       1 New Jersey Devils /api/v1/teams/1
    ## 2       1 New Jersey Devils /api/v1/teams/1
    ##   stat.gamesPlayed stat.wins stat.losses stat.ot stat.pts
    ## 1               56        19          30       7       45
    ## 2               NA      28th        29th    15th     29th
    ##   stat.ptPctg stat.goalsPerGame stat.goalsAgainstPerGame
    ## 1        40.2             2.589                    3.375
    ## 2        29th              26th                     28th
    ##   stat.evGGARatio stat.powerPlayPercentage
    ## 1          0.8293                     14.2
    ## 2            21st                     28th
    ##   stat.powerPlayGoals stat.powerPlayGoalsAgainst
    ## 1                  22                         43
    ## 2                28th                       30th
    ##   stat.powerPlayOpportunities stat.penaltyKillPercentage
    ## 1                         155                       71.0
    ## 2                        23rd                       31st
    ##   stat.shotsPerGame stat.shotsAllowed stat.winScoreFirst
    ## 1           28.7857           31.0179              0.552
    ## 2              24th              22nd               22nd
    ##   stat.winOppScoreFirst stat.winLeadFirstPer
    ## 1                 0.111                0.737
    ## 2                  31st                 19th
    ##   stat.winLeadSecondPer stat.winOutshootOpp
    ## 1                 0.733               0.211
    ## 2                  28th                31st
    ##   stat.winOutshotByOpp stat.faceOffsTaken stat.faceOffsWon
    ## 1                0.417               3180             1481
    ## 2                 31st                8th             27th
    ##   stat.faceOffsLost stat.faceOffWinPercentage
    ## 1              1699                      46.6
    ## 2              30th                      27th
    ##   stat.shootingPctg stat.savePctg
    ## 1                 9         0.891
    ## 2                NA            NA
    ##   stat.penaltyKillOpportunities stat.savePctRank
    ## 1                          <NA>             <NA>
    ## 2                           6th             29th
    ##   stat.shootingPctRank
    ## 1                 <NA>
    ## 2                 24th

``` r
#for stats without rankings
#str(testStats2[[9]][[11]][[1]][[1]][1,]) %>%
#  relocate(starts_with("team"))
```

## Wrapper function

``` r
getChoice <- function(what, team = NULL){
  switch(what,
         franchise = getFranchiseInfo(team),
         totals = getTeamTotals(team),
         season = getSeasonRecords(team),
         goalie = getGoalieRecords(team),
         skater = getSkaterRecords(team),
         detail = getFranchiseDetail(team),
         stats = getStats(team),
         stop("try again entering what you want")
         )
}

getChoice("franchise", 26)
```

    ## No encoding supplied: defaulting to UTF-8.

    ##   id firstSeasonId            fullName lastSeasonId
    ## 1 26      19791980 Carolina Hurricanes           NA
    ##   mostRecentTeamId teamAbbrev teamCommonName teamPlaceName
    ## 1               12        CAR     Hurricanes      Carolina

``` r
getChoice("totals")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##    id activeFranchise firstSeasonId franchiseId gameTypeId
    ## 1   1               1      19821983          23          2
    ## 2   2               1      19821983          23          3
    ## 3   3               1      19721973          22          2
    ## 4   4               1      19721973          22          3
    ## 5   5               1      19261927          10          2
    ## 6   6               1      19261927          10          3
    ## 7   7               1      19671968          16          3
    ## 8   8               1      19671968          16          2
    ## 9   9               1      19671968          17          2
    ## 10 10               1      19671968          17          3
    ## 11 11               1      19241925           6          2
    ## 12 12               1      19241925           6          3
    ## 13 13               1      19701971          19          2
    ## 14 14               1      19701971          19          3
    ## 15 15               1      19171918           1          3
    ## 16 16               1      19171918           1          2
    ## 17 17               1      19921993          30          2
    ## 18 18               1      19921993          30          3
    ## 19 19               1      19271928           5          2
    ## 20 20               1      19271928           5          3
    ## 21 21               1      19992000          35          2
    ## 22 22               1      19992000          35          3
    ## 23 23               1      19971998          26          3
    ## 24 24               1      19971998          26          2
    ## 25 25               1      19931994          33          2
    ## 26 26               1      19931994          33          3
    ## 27 27               1      19921993          31          2
    ## 28 28               1      19921993          31          3
    ## 29 29               1      19741975          24          2
    ## 30 30               1      19741975          24          3
    ## 31 31               1      19261927          11          3
    ## 32 32               1      19261927          11          2
    ## 33 33               1      19321933          12          2
    ##    gamesPlayed goalsAgainst goalsFor homeLosses
    ## 1         2993         8902     8792        525
    ## 2          257          634      697         53
    ## 3         3788        11907    12045        678
    ## 4          308          895      982         52
    ## 5         6560        20020    20041       1143
    ## 6          518         1447     1404        104
    ## 7          449         1332     1335         97
    ## 8         4171        12255    13690        584
    ## 9         4171        14049    13874        683
    ## 10         391         1131     1190         85
    ## 11        6626        19137    21112        960
    ## 12         675         1907     1956        151
    ## 13        3945        11966    12471        639
    ## 14         256          765      763         54
    ## 15         771         1955     2300        133
    ## 16        6787        18260    21791        881
    ## 17        2195         6580     6250        413
    ## 18         151          372      357         35
    ## 19        6516        19953    19980       1082
    ## 20         545         1491     1398        120
    ## 21         902         3014     2465        204
    ## 22           4           17        6          2
    ## 23         112          282      272         24
    ## 24        1812         5140     4914        323
    ## 25        2109         6122     5665        390
    ## 26          54          152      132         15
    ## 27        2194         6646     6216        414
    ## 28         175          457      483         43
    ## 29        3633        11553    11516        620
    ## 30         295          837      836         77
    ## 31         548         1669     1566        104
    ## 32        6560        19687    19537       1128
    ## 33        6293        18881    19550        940
    ##    homeOvertimeLosses homeTies homeWins lastSeasonId losses
    ## 1                  85       96      790           NA   1211
    ## 2                   0       NA       74           NA    120
    ## 3                  84      170      963           NA   1587
    ## 4                   1       NA       94           NA    138
    ## 5                  76      448     1614           NA   2716
    ## 6                   0        1      137           NA    266
    ## 7                   0       NA      135           NA    218
    ## 8                  93      193     1216           NA   1452
    ## 9                  60      205     1138           NA   1734
    ## 10                  0       NA      113           NA    182
    ## 11                 92      376     1885           NA   2403
    ## 12                  2        3      194           NA    337
    ## 13                 84      197     1053           NA   1564
    ## 14                  0       NA       73           NA    132
    ## 15                  0        3      257           NA    321
    ## 16                 95      381     2038           NA   2302
    ## 17                 93       60      533           NA    940
    ## 18                  0       NA       37           NA     79
    ## 19                 85      388     1702           NA   2696
    ## 20                  0        2      149           NA    283
    ## 21                 38       26      183     20102011    437
    ## 22                  0       NA        0     20102011      4
    ## 23                  0       NA       32           NA     54
    ## 24                 77       52      453           NA    725
    ## 25                115       65      485           NA    870
    ## 26                  0       NA       13           NA     33
    ## 27                 67       56      559           NA    947
    ## 28                  0       NA       48           NA     75
    ## 29                 83      153      959           NA   1467
    ## 30                  1       NA       75           NA    156
    ## 31                  0        1      166           NA    275
    ## 32                 86      410     1655           NA   2761
    ## 33                 99      368     1741           NA   2446
    ##    overtimeLosses penaltyMinutes pointPctg points
    ## 1             169          44773    0.5306   3176
    ## 2               0           4266    0.0039      2
    ## 3             166          57792    0.5133   3889
    ## 4               0           5687    0.0130      8
    ## 5             153          86129    0.5127   6727
    ## 6               0           8181    0.0000      0
    ## 7               0           9104    0.0045      4
    ## 8             183          76208    0.5752   4798
    ## 9             151          66221    0.5203   4340
    ## 10              0           6106    0.0153     12
    ## 11            191          88570    0.5632   7464
    ## 12              0          10607    0.0296     40
    ## 13            167          60671    0.5305   4186
    ## 14              0           4692    0.0000      0
    ## 15              0          12138    0.0000      0
    ## 16            175          87484    0.5863   7958
    ## 17            169          29684    0.5071   2226
    ## 18              0           2102    0.0000      0
    ## 19            174          92331    0.5136   6693
    ## 20              0           8550    0.0110     12
    ## 21             78          13727    0.4473    807
    ## 22              0            115    0.0000      0
    ## 23              0           1310    0.0714     16
    ## 24            174          19429    0.5281   1914
    ## 25            208          29171    0.5045   2128
    ## 26              0            775    0.0000      0
    ## 27            150          31086    0.5087   2232
    ## 28              0           2442    0.0629     22
    ## 29            163          57455    0.5321   3866
    ## 30              1           5152    0.0644     38
    ## 31              0           8855    0.0000      0
    ## 32            173          92285    0.5039   6611
    ## 33            183          84403    0.5354   6738
    ##    roadLosses roadOvertimeLosses roadTies roadWins
    ## 1         686                 84      123      604
    ## 2          67                  0       NA       63
    ## 3         909                 82      177      725
    ## 4          86                  2       NA       76
    ## 5        1573                 77      360     1269
    ## 6         162                  0        7      107
    ## 7         121                  0       NA       96
    ## 8         868                 90      264      863
    ## 9        1051                 91      178      765
    ## 10         97                  1       NA       96
    ## 11       1443                 99      415     1356
    ## 12        186                  2        3      138
    ## 13        925                 83      212      752
    ## 14         78                  0       NA       51
    ## 15        188                  0        5      185
    ## 16       1421                 80      456     1435
    ## 17        527                 76       55      438
    ## 18         44                  0       NA       35
    ## 19       1614                 89      385     1171
    ## 20        163                  1        1      110
    ## 21        233                 40       19      159
    ## 22          2                  0       NA        0
    ## 23         30                  2       NA       26
    ## 24        402                 97       34      374
    ## 25        480                 93       77      404
    ## 26         18                  0       NA        8
    ## 27        533                 83       56      426
    ## 28         32                  0       NA       52
    ## 29        847                 80      150      741
    ## 30         79                  2       NA       63
    ## 31        171                  1        4      102
    ## 32       1633                 87      404     1157
    ## 33       1506                 84      405     1150
    ##    shootoutLosses shootoutWins shutouts teamId
    ## 1              84           78      196      1
    ## 2               0            0       25      1
    ## 3              70           86      177      2
    ## 4               0            0       12      2
    ## 5              68           79      408      3
    ## 6               0            0       44      3
    ## 7               0            0       33      4
    ## 8              92           53      248      4
    ## 9              54           83      189      5
    ## 10              0            0       30      5
    ## 11             82           68      506      6
    ## 12              0            0       49      6
    ## 13             74           81      194      7
    ## 14              0            0       18      7
    ## 15              0            0       68      8
    ## 16             66           69      543      8
    ## 17             79           58      137      9
    ## 18              0            0       12      9
    ## 19             77           59      422     10
    ## 20              0            0       50     10
    ## 21             29           37       41     11
    ## 22              0            0        0     11
    ## 23              0            0       11     12
    ## 24             61           50       99     12
    ## 25             97           71      115     13
    ## 26              0            0        3     13
    ## 27             59           68      124     14
    ## 28              0            1       14     14
    ## 29             71           68      178     15
    ## 30              1            0       19     15
    ## 31              0            0       32     16
    ## 32             70           75      439     16
    ## 33             76           71      423     17
    ##               teamName ties triCode wins
    ## 1    New Jersey Devils  219     NJD 1394
    ## 2    New Jersey Devils   NA     NJD  137
    ## 3   New York Islanders  347     NYI 1688
    ## 4   New York Islanders   NA     NYI  170
    ## 5     New York Rangers  808     NYR 2883
    ## 6     New York Rangers    8     NYR  244
    ## 7  Philadelphia Flyers   NA     PHI  231
    ## 8  Philadelphia Flyers  457     PHI 2079
    ## 9  Pittsburgh Penguins  383     PIT 1903
    ## 10 Pittsburgh Penguins   NA     PIT  209
    ## 11       Boston Bruins  791     BOS 3241
    ## 12       Boston Bruins    6     BOS  332
    ## 13      Buffalo Sabres  409     BUF 1805
    ## 14      Buffalo Sabres   NA     BUF  124
    ## 15  Montréal Canadiens    8     MTL  442
    ## 16  Montréal Canadiens  837     MTL 3473
    ## 17     Ottawa Senators  115     OTT  971
    ## 18     Ottawa Senators   NA     OTT   72
    ## 19 Toronto Maple Leafs  773     TOR 2873
    ## 20 Toronto Maple Leafs    3     TOR  259
    ## 21   Atlanta Thrashers   45     ATL  342
    ## 22   Atlanta Thrashers   NA     ATL    0
    ## 23 Carolina Hurricanes   NA     CAR   58
    ## 24 Carolina Hurricanes   86     CAR  827
    ## 25    Florida Panthers  142     FLA  889
    ## 26    Florida Panthers   NA     FLA   21
    ## 27 Tampa Bay Lightning  112     TBL  985
    ## 28 Tampa Bay Lightning   NA     TBL  100
    ## 29 Washington Capitals  303     WSH 1700
    ## 30 Washington Capitals   NA     WSH  138
    ## 31  Chicago Blackhawks    5     CHI  268
    ## 32  Chicago Blackhawks  814     CHI 2812
    ## 33   Detroit Red Wings  773     DET 2891
    ##  [ reached 'max' / getOption("max.print") -- omitted 72 rows ]

``` r
getChoice("season", "Caro")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##   id fewestGoals fewestGoalsAgainst
    ## 1 12         171                202
    ##   fewestGoalsAgainstSeasons fewestGoalsSeasons fewestLosses
    ## 1              1998-99 (82)       2002-03 (82)           22
    ##   fewestLossesSeasons fewestPoints fewestPointsSeasons
    ## 1        2005-06 (82)           45        1982-83 (80)
    ##   fewestTies fewestTiesSeasons fewestWins fewestWinsSeasons
    ## 1          4      1985-86 (80)         19      1982-83 (80)
    ##   franchiseId       franchiseName homeLossStreak
    ## 1          26 Carolina Hurricanes              8
    ##         homeLossStreakDates homePointStreak
    ## 1 Mar 14 2013 - Apr 09 2013              15
    ##        homePointStreakDates homeWinStreak
    ## 1 Dec 13 2005 - Jan 28 2006            12
    ##          homeWinStreakDates homeWinlessStreak
    ## 1 Feb 20 2009 - Apr 07 2009                13
    ##      homeWinlessStreakDates lossStreak
    ## 1 Jan 15 1985 - Mar 10 1985          9
    ##             lossStreakDates mostGameGoals
    ## 1 Feb 19 1983 - Mar 08 1983            11
    ##                                                                                                       mostGameGoalsDates
    ## 1 Feb 12 1984 - EDM 0 @ HFD 11, Oct 19 1985 - MTL 6 @ HFD 11, Jan 17 1986 - QUE 6 @ HFD 11, Mar 15 1986 - CHI 4 @ HFD 11
    ##   mostGoals mostGoalsAgainst mostGoalsAgainstSeasons
    ## 1       332              403            1982-83 (80)
    ##   mostGoalsSeasons mostLosses mostLossesSeasons
    ## 1     1985-86 (80)         54      1982-83 (80)
    ##   mostPenaltyMinutes mostPenaltyMinutesSeasons mostPoints
    ## 1               2354              1992-93 (84)        112
    ##   mostPointsSeasons mostShutouts mostShutoutsSeasons
    ## 1      2005-06 (82)            8        1998-99 (82)
    ##   mostTies mostTiesSeasons mostWins mostWinsSeasons
    ## 1       19    1979-80 (80)       52    2005-06 (82)
    ##   pointStreak
    ## 1          13
    ##                                       pointStreakDates
    ## 1 Mar 09 2017 - Mar 30 2017, Apr 15 2021 - May 06 2021
    ##   roadLossStreak       roadLossStreakDates roadPointStreak
    ## 1             13 Dec 18 1982 - Feb 05 1983              12
    ##        roadPointStreakDates roadWinStreak
    ## 1 Feb 23 2004 - Mar 27 2004             6
    ##                                     roadWinStreakDates
    ## 1 Nov 10 1990 - Dec 07 1990, May 09 2002 - Jun 04 2002
    ##   roadWinlessStreak
    ## 1                15
    ##                                 roadWinlessStreakDates
    ## 1 Nov 11 1979 - Jan 09 1980, Jan 07 2003 - Mar 02 2003
    ##   winStreak
    ## 1         9
    ##                                                                    winStreakDates
    ## 1 Oct 22 2005 - Nov 11 2005, Dec 31 2005 - Jan 19 2006, Mar 18 2009 - Apr 07 2009
    ##   winlessStreak
    ## 1            14
    ##                                     winlessStreakDates
    ## 1 Jan 04 1992 - Feb 09 1992, Oct 10 2009 - Nov 13 2009

``` r
getChoice("goalie", "Rangers")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##      id activePlayer     firstName franchiseId
    ## 1   388        FALSE          John          10
    ## 2   417        FALSE           Bob          10
    ## 3   442        FALSE          Glen          10
    ## 4   454        FALSE         Glenn          10
    ## 5   456        FALSE           Guy          10
    ## 6   507        FALSE          Kirk          10
    ## 7   520        FALSE         Eddie          10
    ## 8   540        FALSE          Bill          10
    ## 9   546        FALSE        Johnny          10
    ## 10 1211        FALSE         Lorne          10
    ## 11  580        FALSE         Emile          10
    ## 12  582        FALSE           Jim          10
    ## 13  594        FALSE        Gilles          10
    ## 14  608        FALSE         Percy          10
    ## 15  615        FALSE          Dave          10
    ## 16  621        FALSE        Cesare          10
    ## 17 1237        FALSE           Ken          10
    ## 18  629        FALSE       Jacques          10
    ## 19  631        FALSE         Chuck          10
    ## 20  632        FALSE     John Ross          10
    ## 21  636        FALSE            Al          10
    ## 22  640        FALSE         Terry          10
    ## 23  643        FALSE           Don          10
    ## 24  652        FALSE          Dunc          10
    ## 25  306        FALSE          Gump          10
    ## 26  260        FALSE          Mike          10
    ## 27  736        FALSE         Wayne          10
    ## 28  778        FALSE          Mike          10
    ## 29  793        FALSE         Jason          10
    ## 30  815        FALSE         Jamie          10
    ## 31  823        FALSE         Milan          10
    ## 32  848        FALSE Jean-Francois          10
    ## 33  869        FALSE         Kevin          10
    ## 34  915        FALSE        Martin          10
    ##       franchiseName gameTypeId gamesPlayed  lastName losses
    ## 1  New York Rangers          2         222  Davidson     90
    ## 2  New York Rangers          2          98    Froese     43
    ## 3  New York Rangers          2         138    Hanlon     56
    ## 4  New York Rangers          2         113     Healy     44
    ## 5  New York Rangers          2          13    Hebert      7
    ## 6  New York Rangers          2          45    McLean     18
    ## 7  New York Rangers          2          66       Mio     24
    ## 8  New York Rangers          2          17 Beveridge     10
    ## 9  New York Rangers          2          77     Bower     35
    ## 10 New York Rangers          2          81    Chabot     25
    ## 11 New York Rangers          2          22   Francis     10
    ## 12 New York Rangers          2          23    Franks     14
    ## 13 New York Rangers          2          41   Gratton     18
    ## 14 New York Rangers          2           1   Jackson      1
    ## 15 New York Rangers          2         324      Kerr    110
    ## 16 New York Rangers          2          34   Maniago     15
    ## 17 New York Rangers          2          96   McAuley     64
    ## 18 New York Rangers          2          98    Plante     53
    ## 19 New York Rangers          2         377    Rayner    179
    ## 20 New York Rangers          2         180     Roach     63
    ## 21 New York Rangers          2          10   Rollins      4
    ## 22 New York Rangers          2           8   Sawchuk      1
    ## 23 New York Rangers          2          22   Simmons     10
    ## 24 New York Rangers          2          23    Wilson     11
    ## 25 New York Rangers          2         581   Worsley    270
    ## 26 New York Rangers          2         666   Richter    258
    ## 27 New York Rangers          2          94    Thomas     43
    ## 28 New York Rangers          2         100    Dunham     47
    ## 29 New York Rangers          2           6  Muzzatti      3
    ## 30 New York Rangers          2           4  McLennan      3
    ## 31 New York Rangers          2           2  Hnilicka      1
    ## 32 New York Rangers          2           1     Labbe      1
    ## 33 New York Rangers          2          46    Weekes     20
    ## 34 New York Rangers          2          46     Biron     15
    ##                 mostGoalsAgainstDates
    ## 1                          1975-10-22
    ## 2                          1988-12-04
    ## 3                          1983-12-14
    ## 4                          1996-03-24
    ## 5  2001-03-24, 2001-03-14, 2001-03-09
    ## 6                          2000-10-14
    ## 7              1982-10-16, 1982-02-07
    ## 8                          1943-03-16
    ## 9                          1953-12-12
    ## 10                         1927-01-18
    ## 11                         1950-03-22
    ## 12             1942-12-20, 1942-11-28
    ## 13                         1976-12-16
    ## 14                         1934-11-15
    ## 15                         1935-12-28
    ## 16 1966-03-19, 1966-02-13, 1966-02-10
    ## 17                         1944-01-23
    ## 18                         1964-03-19
    ## 19                         1947-02-12
    ## 20             1930-03-18, 1929-02-23
    ## 21                         1960-03-19
    ## 22                         1970-03-14
    ## 23                         1965-12-09
    ## 24                         1975-11-29
    ## 25                         1957-03-16
    ## 26                         1993-03-06
    ## 27 1978-12-23, 1978-04-02, 1977-12-11
    ## 28                         2004-02-14
    ## 29             1997-11-19, 1997-10-15
    ## 30                         2004-03-23
    ## 31                         2000-04-09
    ## 32                         2000-04-05
    ## 33                         2006-10-14
    ## 34 2013-10-08, 2012-03-15, 2010-10-27
    ##    mostGoalsAgainstOneGame
    ## 1                        9
    ## 2                        9
    ## 3                        9
    ## 4                        8
    ## 5                        5
    ## 6                        8
    ## 7                        8
    ## 8                       11
    ## 9                        7
    ## 10                       7
    ## 11                       8
    ## 12                       8
    ## 13                       7
    ## 14                       8
    ## 15                       9
    ## 16                       6
    ## 17                      15
    ## 18                       9
    ## 19                      10
    ## 20                       9
    ## 21                       6
    ## 22                       7
    ## 23                       7
    ## 24                       8
    ## 25                      14
    ## 26                      10
    ## 27                       8
    ## 28                       6
    ## 29                       5
    ## 30                       5
    ## 31                       3
    ## 32                       3
    ## 33                       7
    ## 34                       5
    ##                        mostSavesDates mostSavesOneGame
    ## 1                          1978-04-06               47
    ## 2                          1987-10-10               44
    ## 3                          1985-01-27               50
    ## 4                          1996-01-27               41
    ## 5                          2001-03-31               38
    ## 6                          2001-02-26               39
    ## 7                          1982-03-28               40
    ## 8                                <NA>               NA
    ## 9                          1956-12-23               25
    ## 10                               <NA>               NA
    ## 11                               <NA>               NA
    ## 12                               <NA>               NA
    ## 13                         1976-12-31               41
    ## 14                               <NA>               NA
    ## 15                               <NA>               NA
    ## 16                         1966-02-19               39
    ## 17                               <NA>               NA
    ## 18                         1963-10-12               53
    ## 19                               <NA>               NA
    ## 20                               <NA>               NA
    ## 21                         1960-02-20               46
    ## 22                         1969-10-29               33
    ## 23 1966-01-01, 1965-12-23, 1965-12-19               36
    ## 24                         1975-11-29               38
    ## 25 1963-01-23, 1962-03-11, 1961-02-11               52
    ## 26                         1991-01-31               59
    ## 27                         1977-11-09               40
    ## 28                         2004-03-15               42
    ## 29                         1997-12-16               29
    ## 30                         2004-03-13               29
    ## 31                         2000-04-09               36
    ## 32                         2000-04-05               19
    ## 33                         2006-03-24               40
    ## 34                         2011-01-02               33
    ##                 mostShotsAgainstDates
    ## 1                          1976-03-24
    ## 2                          1987-10-10
    ## 3                          1985-01-27
    ## 4                          1996-01-27
    ## 5                          2001-03-31
    ## 6                          2001-02-26
    ## 7              1982-03-28, 1982-02-07
    ## 8                                <NA>
    ## 9                          1956-12-23
    ## 10                               <NA>
    ## 11                               <NA>
    ## 12                               <NA>
    ## 13                         1976-12-31
    ## 14                               <NA>
    ## 15                               <NA>
    ## 16 1966-03-23, 1966-03-13, 1966-02-19
    ## 17                               <NA>
    ## 18                         1963-10-12
    ## 19                               <NA>
    ## 20                               <NA>
    ## 21                         1960-02-20
    ## 22                         1969-10-29
    ## 23                         1966-01-01
    ## 24                         1975-11-29
    ## 25                         1963-01-30
    ## 26                         1991-01-31
    ## 27                         1979-03-21
    ## 28                         2004-03-15
    ## 29                         1997-12-16
    ## 30                         2004-03-13
    ## 31                         2000-04-09
    ## 32                         2000-04-05
    ## 33                         2006-03-24
    ## 34             2011-02-07, 2011-01-02
    ##    mostShotsAgainstOneGame mostShutoutsOneSeason
    ## 1                       52                     3
    ## 2                       46                     1
    ## 3                       52                     1
    ## 4                       44                     2
    ## 5                       41                     0
    ## 6                       42                     0
    ## 7                       43                     2
    ## 8                       NA                     1
    ## 9                       27                     5
    ## 10                      NA                    11
    ## 11                      NA                     0
    ## 12                      NA                     0
    ## 13                      45                     0
    ## 14                      NA                     0
    ## 15                      NA                     8
    ## 16                      40                     2
    ## 17                      NA                     1
    ## 18                      59                     3
    ## 19                      NA                     7
    ## 20                      NA                    13
    ## 21                      49                     0
    ## 22                      34                     1
    ## 23                      41                     0
    ## 24                      46                     0
    ## 25                      56                     4
    ## 26                      62                     5
    ## 27                      45                     4
    ## 28                      45                     5
    ## 29                      33                     0
    ## 30                      32                     0
    ## 31                      39                     0
    ## 32                      22                     0
    ## 33                      42                     0
    ## 34                      35                     2
    ##                     mostShutoutsSeasonIds mostWinsOneSeason
    ## 1                                19751976                22
    ## 2                                19881989                14
    ## 3                                19831984                28
    ## 4                      19931994, 19951996                17
    ## 5                                20002001                 5
    ## 6                      19992000, 20002001                 8
    ## 7                                19821983                16
    ## 8                                19421943                 4
    ## 9                                19531954                29
    ## 10                               19271928                22
    ## 11 19481949, 19491950, 19501951, 19511952                 4
    ## 12                               19421943                 5
    ## 13                               19761977                11
    ## 14                               19341935                 0
    ## 15           19351936, 19371938, 19391940                27
    ## 16                               19651966                 9
    ## 17                               19441945                11
    ## 18                               19631964                22
    ## 19                               19481949                28
    ## 20                               19281929                23
    ## 21                               19591960                 3
    ## 22                               19691970                 3
    ## 23           19651966, 19671968, 19681969                 2
    ## 24                     19741975, 19751976                 5
    ## 25           19541955, 19551956, 19571958                32
    ## 26                               19931994                42
    ## 27                               19771978                15
    ## 28                               20022003                19
    ## 29                               19971998                 0
    ## 30                               20032004                 1
    ## 31                               19992000                 0
    ## 32                     19992000, 20002001                 0
    ## 33                     20052006, 20062007                14
    ## 34                               20112012                12
    ##     mostWinsSeasonIds overtimeLosses playerId positionCode
    ## 1            19751976             NA  8446323            G
    ## 2            19861987             NA  8446982            G
    ## 3            19831984             NA  8447505            G
    ## 4            19951996             NA  8447709            G
    ## 5            20002001             NA  8447715            G
    ## 6            20002001             NA  8449474            G
    ## 7            19821983             NA  8449618            G
    ## 8            19421943             NA  8449821            G
    ## 9            19531954             NA  8449835            G
    ## 10           19261927             NA  8449850            G
    ## 11           19511952             NA  8449922            G
    ## 12           19421943             NA  8449923            G
    ## 13           19761977             NA  8449984            G
    ## 14           19341935             NA  8450003            G
    ## 15 19371938, 19391940             NA  8450009            G
    ## 16           19651966             NA  8450020            G
    ## 17           19441945             NA  8450027            G
    ## 18           19631964             NA  8450066            G
    ## 19           19491950             NA  8450096            G
    ## 20           19311932             NA  8450103            G
    ## 21           19591960             NA  8450108            G
    ## 22           19691970             NA  8450111            G
    ## 23           19671968             NA  8450113            G
    ## 24           19751976             NA  8450148            G
    ## 25           19551956             NA  8450152            G
    ## 26           19931994             NA  8450833            G
    ## 27           19781979             NA  8451891            G
    ## 28           20022003             NA  8456137            G
    ## 29           19971998             NA  8457463            G
    ## 30           20032004             NA  8458562            G
    ## 31           19992000             NA  8458584            G
    ## 32 19992000, 20002001             NA  8459239            G
    ## 33           20052006              5  8459463            G
    ## 34           20112012              3  8462047            G
    ##    rookieGamesPlayed rookieShutouts rookieWins seasons
    ## 1                 NA             NA         NA       8
    ## 2                 NA             NA         NA       4
    ## 3                 NA             NA         NA       4
    ## 4                 NA             NA         NA       4
    ## 5                 NA             NA         NA       1
    ## 6                 NA             NA         NA       2
    ## 7                 NA             NA         NA       2
    ## 8                 NA             NA         NA       1
    ## 9                 70              5         29       3
    ## 10                37             10         22       2
    ## 11                NA             NA         NA       4
    ## 12                NA             NA         NA       1
    ## 13                NA             NA         NA       1
    ## 14                NA             NA         NA       1
    ## 15                NA             NA         NA       7
    ## 16                NA             NA         NA       2
    ## 17                50              0          6       2
    ## 18                NA             NA         NA       2
    ## 19                NA             NA         NA       8
    ## 20                NA             NA         NA       4
    ## 21                NA             NA         NA       1
    ## 22                NA             NA         NA       1
    ## 23                NA             NA         NA       3
    ## 24                NA             NA         NA       2
    ## 25                50              2         13      10
    ## 26                45              0         21      14
    ## 27                NA             NA         NA       4
    ## 28                NA             NA         NA       2
    ## 29                NA             NA         NA       1
    ## 30                NA             NA         NA       1
    ## 31                NA             NA         NA       1
    ## 32                NA             NA         NA       2
    ## 33                NA             NA         NA       2
    ## 34                NA             NA         NA       4
    ##    shutouts ties wins
    ## 1         7   25   93
    ## 2         1    8   36
    ## 3         1   13   56
    ## 4         6   18   40
    ## 5         0    1    5
    ## 6         0    5   15
    ## 7         2   11   29
    ## 8         1    3    4
    ## 9         5   11   31
    ## 10       21   14   41
    ## 11        0    4    7
    ## 12        0    4    5
    ## 13        0    7   11
    ## 14        0    0    0
    ## 15       40   57  157
    ## 16        2    5    9
    ## 17        1   15   17
    ## 18        5   12   32
    ## 19       24   73  123
    ## 20       30   37   80
    ## 21        0    3    3
    ## 22        1    2    3
    ## 23        0    4    4
    ## 24        0    3    6
    ## 25       24  101  204
    ## 26       24   73  301
    ## 27        5   11   34
    ## 28        7   11   35
    ## 29        0    2    0
    ## 30        0    0    1
    ## 31        0    0    0
    ## 32        0    0    0
    ## 33        0   NA   18
    ## 34        2   NA   22
    ##  [ reached 'max' / getOption("max.print") -- omitted 9 rows ]

``` r
getChoice("skater", "Philadelphia Q")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##       id activePlayer assists firstName franchiseId
    ## 1  17095        FALSE       0    D'arcy           9
    ## 2  17507        FALSE       0     Louis           9
    ## 3  17607        FALSE       0    Edmond           9
    ## 4  18459        FALSE       0      Stan           9
    ## 5  20759        FALSE       0      Fred           9
    ## 6  22090        FALSE       0       Sam           9
    ## 7  22856        FALSE       0       Alf           9
    ## 8  17770        FALSE       2     Marty           9
    ## 9  20080        FALSE       0      Bill           9
    ## 10 20810        FALSE       0    Mickey           9
    ## 11 18073        FALSE       1      Odie           9
    ## 12 20789        FALSE       4       Ron           9
    ## 13 20875        FALSE       3  Rennison           9
    ## 14 20981        FALSE       0     Eddie           9
    ## 15 21103        FALSE       0    Mickey           9
    ## 16 17658        FALSE       3    Archie           9
    ## 17 19292        FALSE      14     Frank           9
    ## 18 19936        FALSE       0    Albert           9
    ## 19 23017        FALSE       0     Jesse           9
    ## 20 16894        FALSE      13      Herb           9
    ## 21 17443        FALSE      11     Cliff           9
    ## 22 19286        FALSE       4      Gord           9
    ## 23 20516        FALSE       1   Charlie           9
    ## 24 20965        FALSE       8      Bert           9
    ## 25 17339        FALSE       8        Ty           9
    ## 26 22809        FALSE       4        Al           9
    ## 27 16922        FALSE      13       Tex           9
    ## 28 17147        FALSE      11     Wally           9
    ## 29 16918        FALSE       4    Rodger           9
    ## 30 17142        FALSE      11       Syd           9
    ## 31 18105        FALSE       7    Lionel           9
    ## 32 18257        FALSE       7     Baldy           9
    ##           franchiseName gameTypeId gamesPlayed goals
    ## 1  Philadelphia Quakers          2          28     0
    ## 2  Philadelphia Quakers          2          30     0
    ## 3  Philadelphia Quakers          2          11     0
    ## 4  Philadelphia Quakers          2          21     0
    ## 5  Philadelphia Quakers          2          16     0
    ## 6  Philadelphia Quakers          2           6     0
    ## 7  Philadelphia Quakers          2           7     0
    ## 8  Philadelphia Quakers          2          35     1
    ## 9  Philadelphia Quakers          2          21     1
    ## 10 Philadelphia Quakers          2          12     1
    ## 11 Philadelphia Quakers          2          22     2
    ## 12 Philadelphia Quakers          2          22     2
    ## 13 Philadelphia Quakers          2          37     3
    ## 14 Philadelphia Quakers          2          16     3
    ## 15 Philadelphia Quakers          2          36     3
    ## 16 Philadelphia Quakers          2          29     4
    ## 17 Philadelphia Quakers          2          40     7
    ## 18 Philadelphia Quakers          2          44     4
    ## 19 Philadelphia Quakers          2          58     6
    ## 20 Philadelphia Quakers          2         216    24
    ## 21 Philadelphia Quakers          2          82    10
    ## 22 Philadelphia Quakers          2          34     6
    ## 23 Philadelphia Quakers          2          44     6
    ## 24 Philadelphia Quakers          2          93    10
    ## 25 Philadelphia Quakers          2          47     7
    ## 26 Philadelphia Quakers          2          43     7
    ## 27 Philadelphia Quakers          2         194    32
    ## 28 Philadelphia Quakers          2          44     8
    ## 29 Philadelphia Quakers          2         210    20
    ## 30 Philadelphia Quakers          2          44     9
    ## 31 Philadelphia Quakers          2          41     9
    ## 32 Philadelphia Quakers          2         144    24
    ##        lastName
    ## 1       Coulson
    ## 2  Berlinquette
    ## 3      Bouchard
    ## 4      Crossett
    ## 5        Lowrey
    ## 6    Rothschild
    ## 7       Skinner
    ## 8         Burke
    ## 9        Hutton
    ## 10       MacKay
    ## 11     Cleghorn
    ## 12        Lyons
    ## 13      Manners
    ## 14     McCalmon
    ## 15      McGuire
    ## 16       Briden
    ## 17  Fredrickson
    ## 18       Holway
    ## 19       Spring
    ## 20        Drury
    ## 21       Barton
    ## 22       Fraser
    ## 23     Langlois
    ## 24    McCaffrey
    ## 25       Arbour
    ## 26      Shields
    ## 27        White
    ## 28       Kilrea
    ## 29        Smith
    ## 30         Howe
    ## 31     Conacher
    ## 32       Cotton
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              mostAssistsGameDates
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1930-11-11, 1930-11-15, 1930-11-16, 1930-11-18, 1930-11-23, 1930-11-25, 1930-11-29, 1930-12-02, 1930-12-04, 1930-12-06, 1930-12-09, 1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-20, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14, 1931-02-17, 1931-02-22, 1931-02-24, 1931-02-28, 1931-03-03, 1931-03-07, 1931-03-10, 1931-03-12, 1931-03-14, 1931-03-17, 1931-03-21
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1925-11-26, 1925-11-28, 1925-12-02, 1925-12-05, 1925-12-09, 1925-12-11, 1925-12-16, 1925-12-18, 1925-12-19, 1925-12-23, 1925-12-26, 1925-12-30, 1926-01-01, 1926-01-05, 1926-01-13, 1926-01-15, 1926-01-19, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-06, 1926-02-09, 1926-02-13, 1926-02-16, 1926-02-19, 1926-02-20, 1926-02-23, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              1929-02-16, 1929-02-17, 1929-02-19, 1929-02-21, 1929-02-23, 1929-02-26, 1929-03-02, 1929-03-05, 1929-03-09, 1929-03-16, 1929-03-17
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1930-11-11, 1930-11-15, 1930-11-16, 1930-11-18, 1930-11-23, 1930-11-25, 1930-11-29, 1930-12-02, 1930-12-04, 1930-12-06, 1930-12-09, 1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-20, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14, 1931-02-17, 1931-02-22, 1931-02-24, 1931-02-28, 1931-03-03, 1931-03-07, 1931-03-10, 1931-03-12, 1931-03-14, 1931-03-17, 1931-03-21
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1926-01-13, 1926-01-15, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-09, 1926-02-13, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1927-11-26, 1927-11-29, 1927-12-01, 1927-12-06, 1927-12-10, 1927-12-22
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1925-11-26, 1925-11-28, 1925-12-02, 1925-12-05, 1925-12-09, 1925-12-11, 1925-12-16, 1925-12-18, 1925-12-19, 1925-12-23, 1925-12-26, 1925-12-30, 1926-01-01, 1926-01-05, 1926-01-13, 1926-01-15, 1926-01-19, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-06, 1926-02-09, 1926-02-13, 1926-02-16, 1926-02-19, 1926-02-20, 1926-02-23, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1928-02-08, 1928-03-10
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1928-11-15, 1928-11-18, 1928-11-20, 1928-11-22, 1928-11-25, 1928-11-27, 1928-11-29, 1928-12-06, 1928-12-08, 1928-12-15, 1928-12-18, 1928-12-20
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1925-12-09
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1930-12-28
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1929-12-15, 1930-03-01, 1930-03-16
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1930-11-11, 1930-11-15, 1930-11-16, 1930-11-18, 1930-11-23, 1930-11-25, 1930-11-29, 1930-12-02, 1930-12-04, 1930-12-06, 1930-12-09, 1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-20, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14, 1931-02-17, 1931-02-22, 1931-02-24, 1931-02-28, 1931-03-03, 1931-03-07, 1931-03-10, 1931-03-12, 1931-03-14, 1931-03-17, 1931-03-21
    ## 15 1926-11-16, 1926-11-20, 1926-11-25, 1926-11-27, 1926-11-30, 1926-12-04, 1926-12-09, 1926-12-11, 1926-12-18, 1926-12-21, 1926-12-23, 1926-12-25, 1926-12-30, 1927-01-01, 1927-01-04, 1927-01-06, 1927-01-08, 1927-01-13, 1927-01-15, 1927-01-18, 1927-01-20, 1927-01-22, 1927-01-25, 1927-01-27, 1927-01-29, 1927-02-06, 1927-02-08, 1927-02-10, 1927-02-12, 1927-02-15, 1927-02-19, 1927-02-22, 1927-02-26, 1927-03-01, 1927-03-03, 1927-03-05, 1927-03-08, 1927-03-10, 1927-03-15, 1927-03-17, 1927-03-20, 1927-03-22, 1927-03-24, 1927-03-26, 1927-11-15, 1927-11-19, 1927-11-22, 1927-11-26, 1927-11-29, 1927-12-01, 1927-12-06, 1927-12-10, 1927-12-17, 1927-12-18, 1927-12-20, 1927-12-22, 1927-12-24, 1927-12-31, 1928-01-03, 1928-01-05, 1928-01-07, 1928-01-12, 1928-01-14, 1928-01-16, 1928-01-19, 1928-01-22, 1928-01-24, 1928-01-28, 1928-01-31, 1928-02-04, 1928-02-08, 1928-02-11, 1928-02-12, 1928-02-14, 1928-02-16, 1928-02-18, 1928-02-21, 1928-02-23, 1928-02-25, 1928-03-01, 1928-03-03, 1928-03-06, 1928-03-10, 1928-03-12, 1928-03-17, 1928-03-18, 1928-03-22, 1928-03-24
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1929-11-19, 1929-12-17, 1930-01-02
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1929-11-19
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1928-11-15, 1928-11-18, 1928-11-20, 1928-11-22, 1928-11-25, 1928-11-27, 1928-11-29, 1928-12-01, 1928-12-06, 1928-12-08, 1928-12-15, 1928-12-18, 1928-12-20, 1928-12-22, 1928-12-27, 1928-12-29, 1929-01-01, 1929-01-03, 1929-01-05, 1929-01-08, 1929-01-10, 1929-01-12, 1929-01-13, 1929-01-19, 1929-01-20, 1929-01-24, 1929-01-26, 1929-02-02, 1929-02-05, 1929-02-09, 1929-02-10, 1929-02-12, 1929-02-14, 1929-02-16, 1929-02-17, 1929-02-19, 1929-02-21, 1929-02-23, 1929-02-26, 1929-03-02, 1929-03-05, 1929-03-09, 1929-03-16, 1929-03-17
    ## 19             1925-11-26, 1925-11-28, 1925-12-02, 1925-12-05, 1925-12-09, 1925-12-11, 1925-12-16, 1925-12-18, 1925-12-19, 1925-12-23, 1925-12-26, 1925-12-30, 1926-01-01, 1926-01-05, 1926-01-13, 1926-01-15, 1926-01-19, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-06, 1926-02-09, 1926-02-13, 1926-02-16, 1926-02-19, 1926-02-20, 1926-02-23, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15, 1929-02-17, 1929-02-19, 1929-02-21, 1929-02-23, 1929-02-26, 1929-03-02, 1929-03-16, 1929-11-16, 1929-11-19, 1929-11-23, 1929-11-26, 1929-11-30, 1929-12-03, 1929-12-05, 1929-12-08, 1929-12-10, 1929-12-14, 1929-12-15, 1929-12-17, 1929-12-21, 1929-12-22, 1929-12-26, 1929-12-29, 1930-01-02, 1930-01-05, 1930-01-09, 1930-01-11, 1930-01-12, 1930-01-14, 1930-01-18, 1930-01-21, 1930-01-25, 1930-01-26, 1930-01-28, 1930-02-01, 1930-02-04, 1930-02-09, 1930-02-12, 1930-02-13, 1930-02-15, 1930-02-18, 1930-02-20, 1930-02-22, 1930-02-25, 1930-03-01, 1930-03-02, 1930-03-08, 1930-03-11, 1930-03-13, 1930-03-16, 1930-03-18
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1926-02-06, 1926-03-04, 1926-11-20, 1927-03-08, 1927-12-31, 1928-03-17, 1929-02-14, 1929-02-21, 1929-03-05, 1929-03-17, 1930-12-09, 1931-02-05, 1931-02-14
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1930-01-21, 1930-02-04, 1930-03-13, 1930-03-18, 1930-12-06, 1930-12-13, 1931-02-14, 1931-02-17, 1931-03-07, 1931-03-10, 1931-03-12
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1930-02-01
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1927-02-22
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1928-01-12, 1928-01-19, 1928-02-08, 1928-02-14, 1929-11-19, 1929-12-08, 1929-12-14, 1929-12-26
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1926-12-04, 1926-12-25, 1927-01-27
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1930-11-23, 1931-01-24, 1931-01-29, 1931-01-31
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1926-02-23, 1927-01-01, 1927-02-15, 1927-03-17, 1927-03-26, 1927-12-22, 1928-01-16, 1928-03-24, 1928-11-18, 1928-12-01, 1929-01-01, 1929-01-24, 1930-01-25
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1931-01-03, 1931-03-12
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1925-12-09, 1929-01-03, 1929-03-05, 1930-01-02
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1931-03-12
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1926-03-04
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1928-03-17
    ##    mostAssistsOneGame mostAssistsOneSeason
    ## 1                   0                    0
    ## 2                   0                    0
    ## 3                   0                    0
    ## 4                   0                    0
    ## 5                   0                    0
    ## 6                   0                    0
    ## 7                   0                    0
    ## 8                   1                    2
    ## 9                   0                    0
    ## 10                  0                    0
    ## 11                  1                    1
    ## 12                  2                    4
    ## 13                  1                    3
    ## 14                  0                    0
    ## 15                  0                    0
    ## 16                  1                    3
    ## 17                  5                    7
    ## 18                  0                    0
    ## 19                  0                    0
    ## 20                  1                    4
    ## 21                  1                    7
    ## 22                  2                    4
    ## 23                  1                    1
    ## 24                  1                    4
    ## 25                  2                    8
    ## 26                  1                    4
    ## 27                  1                    4
    ## 28                  2                   11
    ## 29                  1                    2
    ## 30                  3                   11
    ## 31                  2                    6
    ## 32                  2                    3
    ##            mostAssistsSeasonIds
    ## 1                      19301931
    ## 2                      19251926
    ## 3                      19281929
    ## 4                      19301931
    ## 5                      19251926
    ## 6                      19271928
    ## 7                      19251926
    ## 8                      19271928
    ## 9                      19301931
    ## 10                     19281929
    ## 11                     19251926
    ## 12                     19301931
    ## 13                     19291930
    ## 14                     19301931
    ## 15           19261927, 19271928
    ## 16                     19291930
    ## 17           19281929, 19291930
    ## 18                     19281929
    ## 19 19251926, 19281929, 19291930
    ## 20                     19281929
    ## 21                     19301931
    ## 22                     19291930
    ## 23                     19261927
    ## 24           19271928, 19291930
    ## 25                     19261927
    ## 26                     19301931
    ## 27           19261927, 19281929
    ## 28                     19301931
    ## 29                     19281929
    ## 30                     19301931
    ## 31                     19251926
    ## 32                     19271928
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                mostGoalsGameDates
    ## 1  1930-11-11, 1930-11-15, 1930-11-16, 1930-11-18, 1930-11-23, 1930-11-25, 1930-11-29, 1930-12-02, 1930-12-04, 1930-12-06, 1930-12-09, 1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-20, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14, 1931-02-17, 1931-02-22, 1931-02-24, 1931-02-28, 1931-03-03, 1931-03-07, 1931-03-10, 1931-03-12, 1931-03-14, 1931-03-17, 1931-03-21
    ## 2                                                                                                  1925-11-26, 1925-11-28, 1925-12-02, 1925-12-05, 1925-12-09, 1925-12-11, 1925-12-16, 1925-12-18, 1925-12-19, 1925-12-23, 1925-12-26, 1925-12-30, 1926-01-01, 1926-01-05, 1926-01-13, 1926-01-15, 1926-01-19, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-06, 1926-02-09, 1926-02-13, 1926-02-16, 1926-02-19, 1926-02-20, 1926-02-23, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                              1929-02-16, 1929-02-17, 1929-02-19, 1929-02-21, 1929-02-23, 1929-02-26, 1929-03-02, 1929-03-05, 1929-03-09, 1929-03-16, 1929-03-17
    ## 4  1930-11-11, 1930-11-15, 1930-11-16, 1930-11-18, 1930-11-23, 1930-11-25, 1930-11-29, 1930-12-02, 1930-12-04, 1930-12-06, 1930-12-09, 1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-20, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14, 1931-02-17, 1931-02-22, 1931-02-24, 1931-02-28, 1931-03-03, 1931-03-07, 1931-03-10, 1931-03-12, 1931-03-14, 1931-03-17, 1931-03-21
    ## 5                                                                                                                                                                                                                                                                                                                                                  1926-01-13, 1926-01-15, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-09, 1926-02-13, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1927-11-26, 1927-11-29, 1927-12-01, 1927-12-06, 1927-12-10, 1927-12-22
    ## 7                                                                                                  1925-11-26, 1925-11-28, 1925-12-02, 1925-12-05, 1925-12-09, 1925-12-11, 1925-12-16, 1925-12-18, 1925-12-19, 1925-12-23, 1925-12-26, 1925-12-30, 1926-01-01, 1926-01-05, 1926-01-13, 1926-01-15, 1926-01-19, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-06, 1926-02-09, 1926-02-13, 1926-02-16, 1926-02-19, 1926-02-20, 1926-02-23, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1927-12-18
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1931-01-10
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1928-11-18
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1925-12-11, 1926-01-21
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1931-01-03, 1931-01-20
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1929-12-26, 1930-01-09, 1930-03-11
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1930-12-28
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1927-03-26
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1929-12-29, 1930-01-12, 1930-01-14, 1930-01-26
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1929-11-19
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1928-11-29, 1929-01-12, 1929-02-21, 1929-03-17
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1926-01-19
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1925-12-09, 1928-02-14, 1930-03-18
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                         1930-02-18, 1930-02-22, 1930-03-01, 1930-03-08, 1930-11-18, 1930-12-23, 1931-01-27, 1931-03-10, 1931-03-12, 1931-03-21
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1930-01-18, 1930-02-09, 1930-02-12, 1930-02-15, 1930-02-18, 1930-03-08
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1926-12-25, 1927-01-15, 1927-01-18, 1927-01-20, 1927-02-26, 1927-03-10
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1928-03-24, 1929-12-14
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1927-02-22
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                             1931-01-01, 1931-01-13, 1931-01-17, 1931-02-22, 1931-03-07, 1931-03-10, 1931-03-14
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1929-12-14
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1931-03-12
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1926-01-15
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                     1930-11-23, 1930-11-29, 1931-01-01, 1931-01-03, 1931-01-27, 1931-02-17, 1931-02-28, 1931-03-10, 1931-03-12
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                     1925-11-26, 1925-12-02, 1925-12-09, 1925-12-26, 1926-02-06, 1926-02-09, 1926-02-23, 1926-03-02, 1926-03-15
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1928-02-04
    ##    mostGoalsOneGame mostGoalsOneSeason mostGoalsSeasonIds
    ## 1                 0                  0           19301931
    ## 2                 0                  0           19251926
    ## 3                 0                  0           19281929
    ## 4                 0                  0           19301931
    ## 5                 0                  0           19251926
    ## 6                 0                  0           19271928
    ## 7                 0                  0           19251926
    ## 8                 1                  1           19271928
    ## 9                 1                  1           19301931
    ## 10                1                  1           19281929
    ## 11                1                  2           19251926
    ## 12                1                  2           19301931
    ## 13                1                  3           19291930
    ## 14                2                  3           19301931
    ## 15                2                  3           19261927
    ## 16                1                  4           19291930
    ## 17                2                  4           19291930
    ## 18                1                  4           19281929
    ## 19                2                  5           19251926
    ## 20                2                  6 19251926, 19271928
    ## 21                1                  6           19301931
    ## 22                1                  6           19291930
    ## 23                1                  6           19261927
    ## 24                2                  6           19271928
    ## 25                3                  7           19261927
    ## 26                1                  7           19301931
    ## 27                2                  8           19291930
    ## 28                2                  8           19301931
    ## 29                3                  9           19251926
    ## 30                1                  9           19301931
    ## 31                1                  9           19251926
    ## 32                3                  9           19271928
    ##    mostPenaltyMinutesOneSeason mostPenaltyMinutesSeasonIds
    ## 1                          103                    19301931
    ## 2                            8                    19251926
    ## 3                            2                    19281929
    ## 4                           10                    19301931
    ## 5                            2                    19251926
    ## 6                            0                    19271928
    ## 7                            2                    19251926
    ## 8                           57                    19271928
    ## 9                            2                    19301931
    ## 10                           2                    19281929
    ## 11                           4          19251926, 19271928
    ## 12                          11                    19301931
    ## 13                          14                    19291930
    ## 14                           6                    19301931
    ## 15                           6                    19261927
    ## 16                          20                    19291930
    ## 17                          26                    19281929
    ## 18                          20                    19281929
    ## 19                          23                    19251926
    ## 20                          54                    19261927
    ## 21                          21                    19301931
    ## 22                          37                    19291930
    ## 23                          30                    19261927
    ## 24                          36                    19281929
    ## 25                          10                    19261927
    ## 26                         102                    19301931
    ## 27                          54                    19271928
    ## 28                          26                    19301931
    ## 29                          69                    19291930
    ## 30                          22                    19301931
    ## 31                          66                    19251926
    ## 32                          40                    19271928
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               mostPointsGameDates
    ## 1  1930-11-11, 1930-11-15, 1930-11-16, 1930-11-18, 1930-11-23, 1930-11-25, 1930-11-29, 1930-12-02, 1930-12-04, 1930-12-06, 1930-12-09, 1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-20, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14, 1931-02-17, 1931-02-22, 1931-02-24, 1931-02-28, 1931-03-03, 1931-03-07, 1931-03-10, 1931-03-12, 1931-03-14, 1931-03-17, 1931-03-21
    ## 2                                                                                                  1925-11-26, 1925-11-28, 1925-12-02, 1925-12-05, 1925-12-09, 1925-12-11, 1925-12-16, 1925-12-18, 1925-12-19, 1925-12-23, 1925-12-26, 1925-12-30, 1926-01-01, 1926-01-05, 1926-01-13, 1926-01-15, 1926-01-19, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-06, 1926-02-09, 1926-02-13, 1926-02-16, 1926-02-19, 1926-02-20, 1926-02-23, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                              1929-02-16, 1929-02-17, 1929-02-19, 1929-02-21, 1929-02-23, 1929-02-26, 1929-03-02, 1929-03-05, 1929-03-09, 1929-03-16, 1929-03-17
    ## 4  1930-11-11, 1930-11-15, 1930-11-16, 1930-11-18, 1930-11-23, 1930-11-25, 1930-11-29, 1930-12-02, 1930-12-04, 1930-12-06, 1930-12-09, 1930-12-13, 1930-12-16, 1930-12-20, 1930-12-23, 1930-12-25, 1930-12-28, 1931-01-01, 1931-01-03, 1931-01-04, 1931-01-08, 1931-01-10, 1931-01-13, 1931-01-17, 1931-01-20, 1931-01-22, 1931-01-24, 1931-01-27, 1931-01-29, 1931-01-31, 1931-02-05, 1931-02-10, 1931-02-14, 1931-02-17, 1931-02-22, 1931-02-24, 1931-02-28, 1931-03-03, 1931-03-07, 1931-03-10, 1931-03-12, 1931-03-14, 1931-03-17, 1931-03-21
    ## 5                                                                                                                                                                                                                                                                                                                                                  1926-01-13, 1926-01-15, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-09, 1926-02-13, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                          1927-11-26, 1927-11-29, 1927-12-01, 1927-12-06, 1927-12-10, 1927-12-22
    ## 7                                                                                                  1925-11-26, 1925-11-28, 1925-12-02, 1925-12-05, 1925-12-09, 1925-12-11, 1925-12-16, 1925-12-18, 1925-12-19, 1925-12-23, 1925-12-26, 1925-12-30, 1926-01-01, 1926-01-05, 1926-01-13, 1926-01-15, 1926-01-19, 1926-01-21, 1926-01-23, 1926-01-27, 1926-01-29, 1926-02-02, 1926-02-06, 1926-02-09, 1926-02-13, 1926-02-16, 1926-02-19, 1926-02-20, 1926-02-23, 1926-02-26, 1926-03-02, 1926-03-04, 1926-03-08, 1926-03-09, 1926-03-12, 1926-03-15
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              1927-12-18, 1928-02-08, 1928-03-10
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1931-01-10
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1928-11-18
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1925-12-09, 1925-12-11, 1926-01-21
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1930-12-28
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1929-12-15, 1929-12-26, 1930-01-09, 1930-03-01, 1930-03-11, 1930-03-16
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1930-12-28
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1927-03-26
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                             1929-11-19, 1929-12-17, 1929-12-29, 1930-01-02, 1930-01-12, 1930-01-14, 1930-01-26
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1929-11-19
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1928-11-29, 1929-01-12, 1929-02-21, 1929-03-17
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1926-01-19
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1925-12-09, 1926-02-06, 1928-02-14, 1930-03-18
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1931-03-10, 1931-03-12
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1930-02-01
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                                                             1926-12-25, 1927-01-15, 1927-01-18, 1927-01-20, 1927-02-22, 1927-02-26, 1927-03-10
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1929-12-14
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1927-01-27, 1927-02-22
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                             1930-11-23, 1931-01-01, 1931-01-13, 1931-01-17, 1931-01-24, 1931-01-29, 1931-01-31, 1931-02-22, 1931-03-07, 1931-03-10, 1931-03-14
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1927-03-17, 1927-03-26, 1929-12-14
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1931-03-12
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1926-01-15
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1931-03-12
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1926-03-02, 1926-03-04
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1928-02-04
    ##    mostPointsOneGame mostPointsOneSeason
    ## 1                  0                   0
    ## 2                  0                   0
    ## 3                  0                   0
    ## 4                  0                   0
    ## 5                  0                   0
    ## 6                  0                   0
    ## 7                  0                   0
    ## 8                  1                   3
    ## 9                  1                   1
    ## 10                 1                   1
    ## 11                 1                   3
    ## 12                 2                   6
    ## 13                 1                   6
    ## 14                 2                   3
    ## 15                 2                   3
    ## 16                 1                   7
    ## 17                 7                  11
    ## 18                 1                   4
    ## 19                 2                   5
    ## 20                 2                   9
    ## 21                 2                  13
    ## 22                 2                  10
    ## 23                 1                   7
    ## 24                 3                  10
    ## 25                 3                  15
    ## 26                 1                  11
    ## 27                 2                   9
    ## 28                 4                  19
    ## 29                 3                  10
    ## 30                 4                  20
    ## 31                 2                  15
    ## 32                 3                  12
    ##    mostPointsSeasonIds penaltyMinutes playerId points
    ## 1             19301931            103  8445741      0
    ## 2             19251926              8  8445071      0
    ## 3             19281929              2  8445131      0
    ## 4             19301931             10  8445818      0
    ## 5             19251926              2  8447521      0
    ## 6             19271928              0  8448462      0
    ## 7             19251926              2  8449049      0
    ## 8             19271928             57  8445262      3
    ## 9             19301931              2  8447012      1
    ## 10            19281929              2  8447545      1
    ## 11            19251926              8  8445496      3
    ## 12            19301931             11  8447534      6
    ## 13            19291930             14  8447602      6
    ## 14            19301931              6  8447671      3
    ## 15            19261927              6  8447779      3
    ## 16            19291930             20  8445162      7
    ## 17            19291930             46  8446493     21
    ## 18            19281929             20  8446907      4
    ## 19            19251926             47  8449141      6
    ## 20            19281929            223  8445955     37
    ## 21            19301931             25  8445029     21
    ## 22            19291930             66  8446489     10
    ## 23            19261927             40  8447323      7
    ## 24            19271928             60  8447655     18
    ## 25            19261927             10  8444967     15
    ## 26            19301931            102  8449019     11
    ## 27  19261927, 19291930            135  8449392     45
    ## 28            19301931             26  8447183     19
    ## 29            19251926            180  8449082     24
    ## 30            19301931             22  8446924     20
    ## 31            19251926             78  8445514     16
    ## 32            19271928            128  8445648     31
    ##    positionCode rookieGamesPlayed rookiePoints seasons
    ## 1             D                28            0       1
    ## 2             L                NA           NA       1
    ## 3             L                NA           NA       1
    ## 4             D                21            0       1
    ## 5             R                NA           NA       1
    ## 6             L                NA           NA       1
    ## 7             R                NA           NA       1
    ## 8             D                35            3       1
    ## 9             D                NA           NA       1
    ## 10            C                NA           NA       1
    ## 11            R                NA           NA       3
    ## 12            L                22            6       1
    ## 13            C                33            6       2
    ## 14            R                NA           NA       1
    ## 15            L                32            3       2
    ## 16            L                NA           NA       1
    ## 17            C                NA           NA       2
    ## 18            D                NA           NA       1
    ## 19            D                NA           NA       3
    ## 20            D                35            8       6
    ## 21            R                39            8       2
    ## 22            D                NA           NA       2
    ## 23            R                NA           NA       2
    ## 24            R                NA           NA       3
    ## 25            L                41           15       2
    ## 26            D                NA           NA       1
    ## 27            R                36            8       6
    ## 28            R                NA           NA       1
    ## 29            D                36           10       6
    ## 30            C                NA           NA       1
    ## 31            D                33           15       2
    ## 32            L                33            9       4
    ##  [ reached 'max' / getOption("max.print") -- omitted 6 rows ]

``` r
getChoice("detail", "Maple")
```

    ## No encoding supplied: defaulting to UTF-8.

    ##   id active
    ## 1  5   TRUE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         captainHistory
    ## 1 <ul class="striped-list">\r\n\t<li>John Tavares: 2019-20 &ndash;&nbsp;Present</li>\r\n\t<li>(No Captain): 2016-17 &ndash;&nbsp;2018-19</li>\r\n\t<li>Dion Phaneuf and (No Captain): 2015-16</li>\r\n\t<li>Dion Phaneuf: 2010-11 &ndash;&nbsp;2014-15</li>\r\n\t<li>(No Captain): 2008-09 &ndash;&nbsp;2009-10</li>\r\n\t<li>Mats Sundin: 1997-98 &ndash;&nbsp;2007-08</li>\r\n\t<li>Doug Gilmour: 1994-95 &ndash;&nbsp;1996-97</li>\r\n\t<li>Wendel Clark: 1991-92 &ndash;&nbsp;1993-94</li>\r\n\t<li>Rob Ramage: 1989-90 &ndash;&nbsp;1990-91</li>\r\n\t<li>(No Captain): 1986-87 &ndash;&nbsp;1988-89</li>\r\n\t<li>Rick Vaive: 1981-82 &ndash;&nbsp;1985-86</li>\r\n\t<li>Darryl Sittler: 1975-76 &ndash;&nbsp;1980-81</li>\r\n\t<li>Dave Keon: 1969-70 &ndash;&nbsp;1974-75</li>\r\n\t<li>George Armstrong: 1957-58 &ndash;&nbsp;1968-69</li>\r\n\t<li>Jimmy Thomson and Ted Kennedy: 1956-57</li>\r\n\t<li>Sid Smith: 1955-56</li>\r\n\t<li>Ted Kennedy: 1948-49 &ndash;&nbsp;1954-55</li>\r\n\t<li>Syl Apps: 1945-46 &ndash;&nbsp;1947-48</li>\r\n\t<li>Bob Davidson: 1943-44 &ndash;&nbsp;1944-45</li>\r\n\t<li>Syl Apps: 1940-41 &ndash;&nbsp;1942-43</li>\r\n\t<li>Red Horner: 1938-39 &ndash;&nbsp;1939-40</li>\r\n\t<li>Charlie Conacher: 1937-38</li>\r\n\t<li>Hap Day: 1927-28 &ndash;&nbsp;1936-37</li>\r\n\t<li>Bert Corbeau: 1926-27</li>\r\n\t<li>Babe Dye: 1925-26</li>\r\n\t<li>John Ross Roach: 1924-25</li>\r\n\t<li>Jack Adams: 1923-24</li>\r\n\t<li>Reg Noble and Jack Adams: 1922-23</li>\r\n\t<li>Reg Noble: 1920-21 &ndash;&nbsp;1921-22</li>\r\n\t<li>Frank Heffernan: 1919-20</li>\r\n\t<li>Ken Randall: 1917-18 &ndash;&nbsp;1918-19</li>\r\n</ul>\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         coachingHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Sheldon Keefe: Nov. 21, 2019 &ndash; Present</li>\r\n\t<li>Mike Babcock: Oct. 7, 2015 &ndash; Nov. 19, 2019</li>\r\n\t<li>Peter Horachek: Jan. 7&nbsp;&ndash; April 11, 2015</li>\r\n\t<li>Randy Carlyle: March 3, 2012 &ndash; Jan. 3, 2015</li>\r\n\t<li>Ron Wilson: Oct. 9, 2008 &ndash; Feb. 29, 2012</li>\r\n\t<li>Paul Maurice: Oct. 4, 2006 &ndash; April 5, 2008</li>\r\n\t<li>Pat Quinn: Oct. 10, 1998 &ndash; April 18, 2006</li>\r\n\t<li>Mike Murphy: Oct. 5, 1996 &ndash; April 19, 1998</li>\r\n\t<li>Nick Beverley: March 6&nbsp;&ndash; April 27, 1996</li>\r\n\t<li>Pat Burns: Oct. 7, 1992 &ndash; March 3, 1996</li>\r\n\t<li>Tom Watt: Oct. 27, 1990 &ndash; April 15, 1992</li>\r\n\t<li>Doug Carpenter: Oct. 5, 1989 &ndash; Oct. 25, 1990</li>\r\n\t<li>George Armstrong: Dec. 19, 1988 &ndash; April 2, 1989</li>\r\n\t<li>John Brophy: Oct. 9, 1986 &ndash; Dec. 17, 1988</li>\r\n\t<li>Dan Maloney: Oct. 11, 1984 &ndash; April 30, 1986</li>\r\n\t<li>Mike Nykoluk: Jan. 10, 1981 &ndash; April 1, 1984</li>\r\n\t<li>Joe Crozier: Oct. 11, 1980 &ndash; Jan. 7, 1981</li>\r\n\t<li>Punch Imlach: March 19&nbsp;&ndash; April 11, 1980</li>\r\n\t<li>Dick Duff: March 15-17, 1980</li>\r\n\t<li>Floyd Smith: Oct. 10, 1979 &ndash; March 12, 1980</li>\r\n\t<li>Roger Neilson: Oct. 13, 1977 &ndash; April 22, 1979</li>\r\n\t<li>Red Kelly: Oct. 10, 1973 &ndash; April 21, 1977</li>\r\n\t<li>John McLellan: Oct. 11, 1969 &ndash; April 1, 1973</li>\r\n\t<li>Punch Imlach: Nov. 29, 1958 &ndash; April 6, 1969</li>\r\n\t<li>Billy Reay: Oct. 8, 1957 &ndash; Nov. 27, 1958</li>\r\n\t<li>Howie Meeker: Oct. 11, 1956 &ndash; March 24, 1957</li>\r\n\t<li>King Clancy: Oct. 10, 1953 &ndash; March 29, 1956</li>\r\n\t<li>Joe Primeau: Oct. 14, 1950 &ndash; March 22, 1953</li>\r\n\t<li>Hap Day: Nov. 2, 1940 &ndash; April 9, 1950</li>\r\n\t<li>Dick Irvin: Dec. 1, 1931 &ndash; April 13, 1940</li>\r\n\t<li>Conn Smythe: Nov. 28, 1931</li>\r\n\t<li>Art Duncan: Nov. 18, 1930 &ndash; Nov. 26, 1931</li>\r\n\t<li>Conn Smythe: Nov. 15, 1927 &ndash; Nov. 15, 1930</li>\r\n\t<li>Alex Romeril: Feb. 17&nbsp;&ndash; March 26, 1927</li>\r\n\t<li>Mike Rodden: Feb. 12-15, 1927</li>\r\n\t<li>Charles Querrie: Nov. 17, 1926 &ndash; Feb. 10, 1927</li>\r\n\t<li>Eddie Powers: Nov. 29, 1924 &ndash; March 17, 1926</li>\r\n\t<li>Charles Querrie: Jan. 3, 1923 &ndash; March 5, 1924</li>\r\n\t<li>George O&#39;Donoghue: Dec. 17, 1921 &ndash; Dec. 30, 1922</li>\r\n\t<li>Frank Carroll: Dec. 22, 1920 &ndash; March 14, 1921</li>\r\n\t<li>Harvey Sproule: Feb. 4&nbsp;&ndash; March 13, 1920</li>\r\n\t<li>Frank Heffernan: Dec. 23, 1919 &ndash; Jan. 31, 1920</li>\r\n\t<li>Dick Carroll: Dec. 19, 1917 &ndash; Feb. 20, 1919</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ##           dateAwarded
    ## 1 1917-11-26T00:00:00
    ##                                     directoryUrl
    ## 1 https://www.nhl.com/mapleleafs/team/management
    ##   firstSeasonId
    ## 1      19171918
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                generalManagerHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Kyle Dubas: May 11, 2018 &ndash; Present</li>\r\n\t<li>Lou Lamoriello: July 23, 2015 &ndash; April 30, 2018</li>\r\n\t<li>Dave Nonis: Jan. 9, 2013 &ndash; April 12, 2015</li>\r\n\t<li>Brian Burke: Nov. 29, 2008 &ndash; Jan. 9, 2013</li>\r\n\t<li>Cliff Fletcher: Jan. 22&nbsp;&ndash; Nov. 29, 2008</li>\r\n\t<li>John Ferguson Jr.: Aug. 29, 2003 &ndash; Jan. 22, 2008</li>\r\n\t<li>Pat Quinn: July 15, 1999 &ndash; Aug. 29, 2003</li>\r\n\t<li>Ken Dryden: Aug. 21, 1997 &ndash; July 15, 1999</li>\r\n\t<li>Bill Watters: May 25&nbsp;&ndash; Aug. 21, 1997</li>\r\n\t<li>Cliff Fletcher: July 1, 1991 &ndash; May 25, 1997</li>\r\n\t<li>Floyd Smith: Aug. 15, 1989 &ndash; July 1, 1991</li>\r\n\t<li>Gord Stellick: April 28, 1988 &ndash; Aug. 11, 1989</li>\r\n\t<li>John Brophy, Dick Duff and Gord Stellick: Feb. 7&nbsp;&ndash; April 28, 1988</li>\r\n\t<li>Gerry McNamara: Oct. 26, 1981 &ndash; Feb. 7, 1988</li>\r\n\t<li>Punch Imlach: July 4, 1979 &ndash; Oct. 26, 1981</li>\r\n\t<li>Jim Gregory: April 6, 1969 &ndash; July 4, 1979</li>\r\n\t<li>Punch Imlach: Nov. 21, 1958 &ndash; April 6, 1969</li>\r\n\t<li>Stafford Smythe: Oct. 3, 1957 &ndash; Nov. 21, 1958</li>\r\n\t<li>Howie Meeker: May 13&nbsp;&ndash; Oct. 3, 1957</li>\r\n\t<li>Hap Day: Oct. 8, 1954 &ndash; March 25, 1957</li>\r\n\t<li>Conn Smythe: Nov. 15, 1927 &ndash; Oct. 8, 1954</li>\r\n\t<li>Charles Querrie: 1917 &ndash; Feb. 14, 1927</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ##                                                                           heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/TOR/MatthewsMarner.jpg
    ##   mostRecentTeamId
    ## 1               10
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 retiredNumbersSummary
    ## 1 <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Turk Broda (1936-43, 1945-52)</li>\r\n\t<li>1 &ndash;&nbsp;Johnny Bower (1958-70)</li>\r\n\t<li>4 &ndash;&nbsp;Hap Day (1926-37)</li>\r\n\t<li>4 &ndash;&nbsp;Red Kelly (1959-67)</li>\r\n\t<li>5 &ndash;&nbsp;Bill Barilko (1946-51)</li>\r\n\t<li>6 &ndash;&nbsp;Ace Bailey (1926-34)</li>\r\n\t<li>7 &ndash;&nbsp;King Clancy (1930-37)</li>\r\n\t<li>7 &ndash;&nbsp;Tim Horton (1949-50, 1951-70)</li>\r\n\t<li>9 &ndash;&nbsp;Charlie Conacher (1929-38)</li>\r\n\t<li>9 &ndash;&nbsp;Ted Kennedy (1942-55, 1956-57)</li>\r\n\t<li>10 &ndash;&nbsp;Syl Apps (1936-43, 1945-48)</li>\r\n\t<li>10 &ndash;&nbsp;George Armstrong (1949-50, 1951-71)</li>\r\n\t<li>13 &ndash;&nbsp;Mats Sundin (1994-08)</li>\r\n\t<li>14 &ndash;&nbsp;Dave Keon (1960-75)</li>\r\n\t<li>17 &ndash;&nbsp;Wendel Clark (1985-94, 1996-98, 2000)</li>\r\n\t<li>21 &ndash;&nbsp;Borje Salming (1973-89)</li>\r\n\t<li>27 &ndash;&nbsp;Frank Mahovlich (1956-68)</li>\r\n\t<li>27 &ndash;&nbsp;Darryl Sittler (1970-82)</li>\r\n\t<li>93 &ndash;&nbsp;Doug Gilmour (1992-97, 2003)</li>\r\n</ul>\r\n
    ##   teamAbbrev        teamFullName
    ## 1        TOR Toronto Maple Leafs

``` r
getChoice("stats", "Detroit")
```

    ##   team.id         team.name        team.link
    ## 1      17 Detroit Red Wings /api/v1/teams/17
    ## 2      17 Detroit Red Wings /api/v1/teams/17
    ##   stat.gamesPlayed stat.wins stat.losses stat.ot stat.pts
    ## 1               56        19          27      10       48
    ## 2               NA      27th        24th     4th     28th
    ##   stat.ptPctg stat.goalsPerGame stat.goalsAgainstPerGame
    ## 1        42.9             2.232                        3
    ## 2        28th              30th                     20th
    ##   stat.evGGARatio stat.powerPlayPercentage
    ## 1          0.7768                     11.4
    ## 2            29th                     30th
    ##   stat.powerPlayGoals stat.powerPlayGoalsAgainst
    ## 1                  17                         33
    ## 2                30th                       18th
    ##   stat.powerPlayOpportunities stat.penaltyKillPercentage
    ## 1                         149                       78.7
    ## 2                        27th                       22nd
    ##   stat.shotsPerGame stat.shotsAllowed stat.winScoreFirst
    ## 1           27.2857           31.8929                0.5
    ## 2              30th              25th               29th
    ##   stat.winOppScoreFirst stat.winLeadFirstPer
    ## 1                 0.219                0.643
    ## 2                  22nd                 27th
    ##   stat.winLeadSecondPer stat.winOutshootOpp
    ## 1                 0.833               0.318
    ## 2                  22nd                28th
    ##   stat.winOutshotByOpp stat.faceOffsTaken stat.faceOffsWon
    ## 1                0.353               3041             1523
    ## 2                 28th               25th             19th
    ##   stat.faceOffsLost stat.faceOffWinPercentage
    ## 1              1518                      50.1
    ## 2              10th                      17th
    ##   stat.shootingPctg stat.savePctg
    ## 1               8.2         0.906
    ## 2                NA            NA
    ##   stat.penaltyKillOpportunities stat.savePctRank
    ## 1                          <NA>             <NA>
    ## 2                          12th             14th
    ##   stat.shootingPctRank
    ## 1                 <NA>
    ## 2                 31st
