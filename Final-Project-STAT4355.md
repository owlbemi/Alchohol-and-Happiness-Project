STAT4355: Alcohol, Wealth, and Happiness
================
Ysabella SalasTeddy WestJake LeeWhitney Humecky

``` r
#Packages
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.1
    ## ✔ tidyr   1.2.1     ✔ stringr 1.4.1
    ## ✔ readr   2.1.3     ✔ forcats 0.5.2
    ## ✔ purrr   0.3.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggthemes)
library(reshape2)
```

    ## 
    ## 다음의 패키지를 부착합니다: 'reshape2'
    ## 
    ##  以下のオブジェクトは 'package:tidyr' からマスクされています:
    ## 
    ##     smiths

``` r
library(car)
```

    ## 필요한 패키지를 로딩중입니다: carData
    ## 
    ## 다음의 패키지를 부착합니다: 'car'
    ## 
    ##  以下のオブジェクトは 'package:dplyr' からマスクされています:
    ## 
    ##     recode
    ## 
    ##  以下のオブジェクトは 'package:purrr' からマスクされています:
    ## 
    ##     some

``` r
#Read CSV file
df <- read.csv("HAC.csv")
df <- as.data.frame(df)
df
```

    ##                    Country                          Region Hemisphere
    ## 1                  Denmark                  Western Europe      north
    ## 2              Switzerland                  Western Europe      north
    ## 3                  Iceland                  Western Europe      north
    ## 4                   Norway                  Western Europe      north
    ## 5                  Finland                  Western Europe      north
    ## 6                   Canada                   North America      north
    ## 7              Netherlands                  Western Europe      north
    ## 8              New Zealand       Australia and New Zealand      south
    ## 9                Australia       Australia and New Zealand      south
    ## 10                  Sweden                  Western Europe      north
    ## 11                  Israel Middle East and Northern Africa      north
    ## 12                 Austria                  Western Europe      north
    ## 13           United States                   North America      north
    ## 14              Costa Rica     Latin America and Caribbean      north
    ## 15                 Germany                  Western Europe      north
    ## 16                  Brazil     Latin America and Caribbean       both
    ## 17                 Belgium                  Western Europe      north
    ## 18                 Ireland                  Western Europe      north
    ## 19              Luxembourg                  Western Europe      north
    ## 20                  Mexico     Latin America and Caribbean      north
    ## 21               Singapore               Southeastern Asia      north
    ## 22          United Kingdom                  Western Europe      north
    ## 23                   Chile     Latin America and Caribbean      south
    ## 24                  Panama     Latin America and Caribbean      north
    ## 25               Argentina     Latin America and Caribbean      south
    ## 26          Czech Republic      Central and Eastern Europe      north
    ## 27    United Arab Emirates Middle East and Northern Africa      north
    ## 28                 Uruguay     Latin America and Caribbean      south
    ## 29                   Malta                  Western Europe      north
    ## 30                Colombia     Latin America and Caribbean       both
    ## 31                  France                  Western Europe      north
    ## 32                Thailand               Southeastern Asia      north
    ## 33                   Qatar Middle East and Northern Africa      north
    ## 34                   Spain                  Western Europe      north
    ## 35               Guatemala     Latin America and Caribbean      north
    ## 36                Suriname     Latin America and Caribbean      north
    ## 37                 Bahrain Middle East and Northern Africa      north
    ## 38     Trinidad and Tobago     Latin America and Caribbean      north
    ## 39               Venezuela     Latin America and Caribbean      north
    ## 40                Slovakia      Central and Eastern Europe      north
    ## 41             El Salvador     Latin America and Caribbean      north
    ## 42               Nicaragua     Latin America and Caribbean      north
    ## 43              Uzbekistan      Central and Eastern Europe      north
    ## 44                   Italy                  Western Europe      north
    ## 45                 Ecuador     Latin America and Caribbean       both
    ## 46                  Belize     Latin America and Caribbean      north
    ## 47                   Japan                    Eastern Asia       noth
    ## 48              Kazakhstan      Central and Eastern Europe      north
    ## 49                 Moldova      Central and Eastern Europe      north
    ## 50      Russian Federation      Central and Eastern Europe      north
    ## 51                  Poland      Central and Eastern Europe      north
    ## 52             South Korea                    Eastern Asia       noth
    ## 53                 Bolivia     Latin America and Caribbean      south
    ## 54               Lithuania      Central and Eastern Europe      north
    ## 55                 Belarus      Central and Eastern Europe      north
    ## 56                Slovenia      Central and Eastern Europe      north
    ## 57                    Peru     Latin America and Caribbean       both
    ## 58            Turkmenistan      Central and Eastern Europe      north
    ## 59               Mauritius              Sub-Saharan Africa      north
    ## 60                  Latvia      Central and Eastern Europe      north
    ## 61                  Cyprus                  Western Europe      north
    ## 62                Paraguay     Latin America and Caribbean      south
    ## 63                 Romania      Central and Eastern Europe      north
    ## 64                 Estonia      Central and Eastern Europe      north
    ## 65                 Jamaica     Latin America and Caribbean      north
    ## 66                 Croatia      Central and Eastern Europe      north
    ## 67                  Turkey Middle East and Northern Africa      north
    ## 68                  Jordan Middle East and Northern Africa      north
    ## 69              Azerbaijan      Central and Eastern Europe      north
    ## 70             Philippines               Southeastern Asia      north
    ## 71                   China                    Eastern Asia       noth
    ## 72              Kyrgyzstan      Central and Eastern Europe      north
    ## 73                  Serbia      Central and Eastern Europe      north
    ## 74  Bosnia and Herzegovina      Central and Eastern Europe      north
    ## 75              Montenegro      Central and Eastern Europe      north
    ## 76      Dominican Republic     Latin America and Caribbean      north
    ## 77                 Morocco Middle East and Northern Africa      north
    ## 78                 Hungary      Central and Eastern Europe      north
    ## 79                 Lebanon Middle East and Northern Africa      north
    ## 80                Portugal                  Western Europe      north
    ## 81               Macedonia      Central and Eastern Europe      north
    ## 82                 Vietnam               Southeastern Asia      north
    ## 83                 Tunisia Middle East and Northern Africa      north
    ## 84                  Greece                  Western Europe      north
    ## 85                Mongolia                    Eastern Asia       noth
    ## 86                 Nigeria              Sub-Saharan Africa      north
    ## 87                Honduras     Latin America and Caribbean      north
    ## 88                  Zambia              Sub-Saharan Africa      south
    ## 89                 Albania      Central and Eastern Europe      north
    ## 90            Sierra Leone              Sub-Saharan Africa      north
    ## 91                 Namibia              Sub-Saharan Africa      south
    ## 92                Cameroon              Sub-Saharan Africa      south
    ## 93            South Africa              Sub-Saharan Africa      south
    ## 94                   Egypt Middle East and Northern Africa      north
    ## 95                 Armenia      Central and Eastern Europe      north
    ## 96                   Kenya              Sub-Saharan Africa       both
    ## 97                 Ukraine      Central and Eastern Europe      north
    ## 98                   Ghana              Sub-Saharan Africa      north
    ## 99         Dem. Rep. Congo              Sub-Saharan Africa      south
    ## 100                Georgia      Central and Eastern Europe      north
    ## 101             Rep. Congo              Sub-Saharan Africa      south
    ## 102                Senegal              Sub-Saharan Africa      north
    ## 103               Bulgaria      Central and Eastern Europe      north
    ## 104               Zimbabwe              Sub-Saharan Africa      south
    ## 105                 Malawi              Sub-Saharan Africa      south
    ## 106                  Gabon              Sub-Saharan Africa      south
    ## 107                   Mali              Sub-Saharan Africa      north
    ## 108                  Haiti     Latin America and Caribbean      north
    ## 109               Botswana              Sub-Saharan Africa      south
    ## 110                Comoros              Sub-Saharan Africa      south
    ## 111          Cote d'Ivoire              Sub-Saharan Africa      north
    ## 112               Cambodia               Southeastern Asia      north
    ## 113                 Angola              Sub-Saharan Africa      south
    ## 114                  Niger              Sub-Saharan Africa      north
    ## 115                   Chad              Sub-Saharan Africa      north
    ## 116           Burkina Faso              Sub-Saharan Africa      north
    ## 117             Madagascar              Sub-Saharan Africa      south
    ## 118               Tanzania              Sub-Saharan Africa      south
    ## 119                Liberia              Sub-Saharan Africa      north
    ## 120                  Benin              Sub-Saharan Africa      north
    ## 121                   Togo              Sub-Saharan Africa      north
    ## 122                  Syria Middle East and Northern Africa      north
    ##     HappinessScore HDI GDP_PerCapita Beer_PerCapita Spirit_PerCapita
    ## 1            7.526 928        53.579            224               81
    ## 2            7.509 943        79.866            185              100
    ## 3            7.501 933        60.530            233               61
    ## 4            7.498 951        70.890            169               71
    ## 5            7.413 918        43.433            263              133
    ## 6            7.404 922        42.349            240              122
    ## 7            7.339 928        45.638            251               88
    ## 8            7.334 915        40.332            203               79
    ## 9            7.313 938        49.897            261               72
    ## 10           7.291 932        51.845            152               60
    ## 11           7.267 902        37.181             63               69
    ## 12           7.119 906        44.731            279               75
    ## 13           7.104 922        57.589            249              158
    ## 14           7.087 791        11.733            149               87
    ## 15           6.994 934        42.233            346              117
    ## 16           6.952 758         8.639            245              145
    ## 17           6.929 915        41.261            295               84
    ## 18           6.907 934        64.100            313              118
    ## 19           6.871 904       100.739            236              133
    ## 20           6.778 772         8.444            238               68
    ## 21           6.739 930        55.243             60               12
    ## 22           6.725 920        40.412            219              126
    ## 23           6.705 842        13.961            130              124
    ## 24           6.701 785        14.333            285              104
    ## 25           6.650 822        12.654            193               25
    ## 26           6.596 885        18.484            361              170
    ## 27           6.573 862        38.518             16              135
    ## 28           6.545 802        15.298            115               35
    ## 29           6.488 875        24.771            149              100
    ## 30           6.481 747         5.757            159               76
    ## 31           6.478 899        36.870            127              151
    ## 32           6.474 748         5.979             99              258
    ## 33           6.375 855        59.324              1               42
    ## 34           6.361 889        26.617            284              157
    ## 35           6.324 649         4.141             53               69
    ## 36           6.269 719         5.871            128              178
    ## 37           6.218 846        22.561             42               63
    ## 38           6.168 785        16.352            197              156
    ## 39           6.084 766        15.692            333              100
    ## 40           6.078 853        16.530            196              293
    ## 41           6.068 679         3.769             52               69
    ## 42           5.992 657         2.144             78              118
    ## 43           5.987 703         2.106             25              101
    ## 44           5.977 878        30.669             85               42
    ## 45           5.976 749         6.019            162               74
    ## 46           5.956 709         4.960            263              114
    ## 47           5.921 907        38.972             77              202
    ## 48           5.919 797         7.715            124              246
    ## 49           5.897 697         1.913            109              226
    ## 50           5.856 815         8.748            247              326
    ## 51           5.835 860        12.415            343              215
    ## 52           5.835 900        27.105            140               16
    ## 53           5.822 689         3.117            167               41
    ## 54           5.813 855        14.913            343              244
    ## 55           5.802 805         5.023            142              373
    ## 56           5.768 894        21.650            270               51
    ## 57           5.743 748         6.031            163              160
    ## 58           5.658 705         6.389             19               71
    ## 59           5.648 788         9.682             98               31
    ## 60           5.560 844        14.070            281              216
    ## 61           5.546 867        23.541            192              154
    ## 62           5.538 702         4.078            213              117
    ## 63           5.528 807         9.532            297              122
    ## 64           5.517 868        17.737            224              194
    ## 65           5.510 732         4.879             82               97
    ## 66           5.488 828        12.299            230               87
    ## 67           5.389 787        10.863             51               22
    ## 68           5.303 735         4.088              6               21
    ## 69           5.291 757         3.881             21               46
    ## 70           5.279 696         2.951             71              186
    ## 71           5.245 748         8.117             79              192
    ## 72           5.185 669         1.220             31               97
    ## 73           5.177 785         5.426            283              131
    ## 74           5.163 766         4.809             76              173
    ## 75           5.161 810         7.029             31              114
    ## 76           5.155 733         6.794            193              147
    ## 77           5.151 662         2.893             12                6
    ## 78           5.145 835        12.820            234              215
    ## 79           5.129 753         8.257             20               55
    ## 80           5.123 845        19.872            194               67
    ## 81           5.121 756         4.834            106               27
    ## 82           5.061 689         2.171            111                2
    ## 83           5.045 732         3.689             51                3
    ## 84           5.033 868        17.882            133              112
    ## 85           4.907 743         3.694             77              189
    ## 86           4.875 530         2.176             42                5
    ## 87           4.871 614         2.375             69               98
    ## 88           4.795 586         1.263             32               19
    ## 89           4.655 782         4.132             89              132
    ## 90           4.635 413       481.000             25                3
    ## 91           4.574 645         4.561            376                3
    ## 92           4.513 553         1.375            147                1
    ## 93           4.459 696         5.280            225               76
    ## 94           4.362 694         3.548              6                4
    ## 95           4.360 749         3.606             21              179
    ## 96           4.356 585         1.463             58               22
    ## 97           4.324 746         2.186            206              237
    ## 98           4.276 588         1.517             31                3
    ## 99           4.272 452         1.712             32                3
    ## 100          4.252 776         3.866             52              100
    ## 101          4.236 612       498.000             76                1
    ## 102          4.219 499       953.000              9                1
    ## 103          4.217 810         7.469            231              252
    ## 104          4.193 532         1.029             64               18
    ## 105          4.156 474       300.000              8               11
    ## 106          4.121 698         7.079            347               98
    ## 107          4.073 421       780.000              5                1
    ## 108          4.028 496       735.000              1              326
    ## 109          3.974 712         6.954            173               35
    ## 110          3.956 502       775.000              1                3
    ## 111          3.916 486         1.535             37                1
    ## 112          3.907 576         1.270             57               65
    ## 113          3.866 577         3.309            217               57
    ## 114          3.856 351       368.000              3                2
    ## 115          3.763 405       651.000             15                1
    ## 116          3.739 420       614.000             25                7
    ## 117          3.695 517       402.000             26               15
    ## 118          3.666 533       878.000             36                6
    ## 119          3.622 432       455.000             19              152
    ## 120          3.484 512       789.000             34                4
    ## 121          3.303 500       577.000             36                2
    ## 122          3.069 536         2.058              5               35
    ##     Wine_PerCapita
    ## 1              278
    ## 2              280
    ## 3               78
    ## 4              129
    ## 5               97
    ## 6              100
    ## 7              190
    ## 8              175
    ## 9              212
    ## 10             186
    ## 11               9
    ## 12             191
    ## 13              84
    ## 14              11
    ## 15             175
    ## 16              16
    ## 17             212
    ## 18             165
    ## 19             271
    ## 20               5
    ## 21              11
    ## 22             195
    ## 23             172
    ## 24              18
    ## 25             221
    ## 26             134
    ## 27               5
    ## 28             220
    ## 29             120
    ## 30               3
    ## 31             370
    ## 32               1
    ## 33               7
    ## 34             112
    ## 35               2
    ## 36               7
    ## 37               7
    ## 38               7
    ## 39               3
    ## 40             116
    ## 41               2
    ## 42               1
    ## 43               8
    ## 44             237
    ## 45               3
    ## 46               8
    ## 47              16
    ## 48              12
    ## 49              18
    ## 50              73
    ## 51              56
    ## 52               9
    ## 53               8
    ## 54              56
    ## 55              42
    ## 56             276
    ## 57              21
    ## 58              32
    ## 59              18
    ## 60              62
    ## 61             113
    ## 62              74
    ## 63             167
    ## 64              59
    ## 65               9
    ## 66             254
    ## 67               7
    ## 68               1
    ## 69               5
    ## 70               1
    ## 71               8
    ## 72               6
    ## 73             127
    ## 74               8
    ## 75             128
    ## 76               9
    ## 77              10
    ## 78             185
    ## 79              31
    ## 80             339
    ## 81              86
    ## 82               1
    ## 83              20
    ## 84             218
    ## 85               8
    ## 86               2
    ## 87               2
    ## 88               4
    ## 89              54
    ## 90               2
    ## 91               1
    ## 92               4
    ## 93              81
    ## 94               1
    ## 95              11
    ## 96               2
    ## 97              45
    ## 98              10
    ## 99               1
    ## 100            149
    ## 101              9
    ## 102              7
    ## 103             94
    ## 104              4
    ## 105              1
    ## 106             59
    ## 107              1
    ## 108              1
    ## 109             35
    ## 110              1
    ## 111              7
    ## 112              1
    ## 113             45
    ## 114              1
    ## 115              1
    ## 116              7
    ## 117              4
    ## 118              1
    ## 119              2
    ## 120             13
    ## 121             19
    ## 122             16

``` r
#Data Cleaning

#Fixing GDP
  #new column by row name order (1 to 122)
  #Macedonia(81) == North Macedonia

GDP_PerCapita2 <- c(67803.0, 93457.4, 68383.8, 89202.8, 53982.6, 
                   52051.4, 58061.0, 48801.7, 59934.1, 60239.0,
                   51430.1, 53267.9, 69287.5, 12508.6, 50801.8,
                   7518.8, 51767.8, 99152.1, 135682.8, 9926.4,
                   72794.0, 47334.4, 16502.8, 14516.5, 10729.2,
                   26378.5, 36284.6, 17020.6, 33257.4, 6131.2,
                   43518.5, 7233.4, 61276.0, 30115.7, 5025.6,
                   4836.3, 22232.3, 15243.1, 16055.6, 21087.8,
                   4408.5, 2090.8, 1983.1, 35551.3, 5934.9, 
                   4420.5, 39285.2, 10041.5, 5314.5, 12172.8,
                   17840.9, 34757.7, 3414.9, 23433.4, 7303.7, 
                   29200.8, 6692.2, 7612.0, 8812.1, 20642.2,
                   30798.5, 5400.1, 14861.9, 27280.7, 4586.7, 
                   17398.8, 9586.6, 4405.8, 5384.0, 3548.8,
                   12556.3, 1276.2, 9215.0, 6916.4, 9367.0, 
                   8603.8, 3496.8, 18772.7, 2670.4, 24262.2,
                   6720.9, 3694.0, 3924.3, 20276.5, 4534.9,
                   2085.0, 2831.0, 1120.6, 6494.4, 515.9,
                   4729.3, 1661.7, 6994.2, 3876.4, 4670.0,
                   2006.8, 4835.6, 2445.3, 584.1, 5042.4,
                   2213.9, 1606.5, 11635.0, 1737.2, 642.7,
                   8017.0, 917.9, 1814.7, 7347.6, 1494.7,
                   2578.8, 1591.0, 2137.9, 594.9, 696.4,
                   918.2, 514.9, 1135.5, 673.1, 1428.4,
                   992.3, 1265.6)

df$GDP_PerCapita <- GDP_PerCapita2

#Sum of all alcohol consumption
Total_Consumption <- df$Beer_PerCapita + df$Spirit_PerCapita + df$Wine_PerCapita

df$Total_Consumption <- Total_Consumption

#Fix unit scale
df$HDI <- (df$HDI / 1000)


#GDP Transformation
GDP_log <- log(df$GDP_PerCapita)
df$GDP_log <- GDP_log

df
```

    ##                    Country                          Region Hemisphere
    ## 1                  Denmark                  Western Europe      north
    ## 2              Switzerland                  Western Europe      north
    ## 3                  Iceland                  Western Europe      north
    ## 4                   Norway                  Western Europe      north
    ## 5                  Finland                  Western Europe      north
    ## 6                   Canada                   North America      north
    ## 7              Netherlands                  Western Europe      north
    ## 8              New Zealand       Australia and New Zealand      south
    ## 9                Australia       Australia and New Zealand      south
    ## 10                  Sweden                  Western Europe      north
    ## 11                  Israel Middle East and Northern Africa      north
    ## 12                 Austria                  Western Europe      north
    ## 13           United States                   North America      north
    ## 14              Costa Rica     Latin America and Caribbean      north
    ## 15                 Germany                  Western Europe      north
    ## 16                  Brazil     Latin America and Caribbean       both
    ## 17                 Belgium                  Western Europe      north
    ## 18                 Ireland                  Western Europe      north
    ## 19              Luxembourg                  Western Europe      north
    ## 20                  Mexico     Latin America and Caribbean      north
    ## 21               Singapore               Southeastern Asia      north
    ## 22          United Kingdom                  Western Europe      north
    ## 23                   Chile     Latin America and Caribbean      south
    ## 24                  Panama     Latin America and Caribbean      north
    ## 25               Argentina     Latin America and Caribbean      south
    ## 26          Czech Republic      Central and Eastern Europe      north
    ## 27    United Arab Emirates Middle East and Northern Africa      north
    ## 28                 Uruguay     Latin America and Caribbean      south
    ## 29                   Malta                  Western Europe      north
    ## 30                Colombia     Latin America and Caribbean       both
    ## 31                  France                  Western Europe      north
    ## 32                Thailand               Southeastern Asia      north
    ## 33                   Qatar Middle East and Northern Africa      north
    ## 34                   Spain                  Western Europe      north
    ## 35               Guatemala     Latin America and Caribbean      north
    ## 36                Suriname     Latin America and Caribbean      north
    ## 37                 Bahrain Middle East and Northern Africa      north
    ## 38     Trinidad and Tobago     Latin America and Caribbean      north
    ## 39               Venezuela     Latin America and Caribbean      north
    ## 40                Slovakia      Central and Eastern Europe      north
    ## 41             El Salvador     Latin America and Caribbean      north
    ## 42               Nicaragua     Latin America and Caribbean      north
    ## 43              Uzbekistan      Central and Eastern Europe      north
    ## 44                   Italy                  Western Europe      north
    ## 45                 Ecuador     Latin America and Caribbean       both
    ## 46                  Belize     Latin America and Caribbean      north
    ## 47                   Japan                    Eastern Asia       noth
    ## 48              Kazakhstan      Central and Eastern Europe      north
    ## 49                 Moldova      Central and Eastern Europe      north
    ## 50      Russian Federation      Central and Eastern Europe      north
    ## 51                  Poland      Central and Eastern Europe      north
    ## 52             South Korea                    Eastern Asia       noth
    ## 53                 Bolivia     Latin America and Caribbean      south
    ## 54               Lithuania      Central and Eastern Europe      north
    ## 55                 Belarus      Central and Eastern Europe      north
    ## 56                Slovenia      Central and Eastern Europe      north
    ## 57                    Peru     Latin America and Caribbean       both
    ## 58            Turkmenistan      Central and Eastern Europe      north
    ## 59               Mauritius              Sub-Saharan Africa      north
    ## 60                  Latvia      Central and Eastern Europe      north
    ## 61                  Cyprus                  Western Europe      north
    ## 62                Paraguay     Latin America and Caribbean      south
    ## 63                 Romania      Central and Eastern Europe      north
    ## 64                 Estonia      Central and Eastern Europe      north
    ## 65                 Jamaica     Latin America and Caribbean      north
    ## 66                 Croatia      Central and Eastern Europe      north
    ## 67                  Turkey Middle East and Northern Africa      north
    ## 68                  Jordan Middle East and Northern Africa      north
    ## 69              Azerbaijan      Central and Eastern Europe      north
    ## 70             Philippines               Southeastern Asia      north
    ## 71                   China                    Eastern Asia       noth
    ## 72              Kyrgyzstan      Central and Eastern Europe      north
    ## 73                  Serbia      Central and Eastern Europe      north
    ## 74  Bosnia and Herzegovina      Central and Eastern Europe      north
    ## 75              Montenegro      Central and Eastern Europe      north
    ## 76      Dominican Republic     Latin America and Caribbean      north
    ## 77                 Morocco Middle East and Northern Africa      north
    ## 78                 Hungary      Central and Eastern Europe      north
    ## 79                 Lebanon Middle East and Northern Africa      north
    ## 80                Portugal                  Western Europe      north
    ## 81               Macedonia      Central and Eastern Europe      north
    ## 82                 Vietnam               Southeastern Asia      north
    ## 83                 Tunisia Middle East and Northern Africa      north
    ## 84                  Greece                  Western Europe      north
    ## 85                Mongolia                    Eastern Asia       noth
    ## 86                 Nigeria              Sub-Saharan Africa      north
    ## 87                Honduras     Latin America and Caribbean      north
    ## 88                  Zambia              Sub-Saharan Africa      south
    ## 89                 Albania      Central and Eastern Europe      north
    ## 90            Sierra Leone              Sub-Saharan Africa      north
    ## 91                 Namibia              Sub-Saharan Africa      south
    ## 92                Cameroon              Sub-Saharan Africa      south
    ## 93            South Africa              Sub-Saharan Africa      south
    ## 94                   Egypt Middle East and Northern Africa      north
    ## 95                 Armenia      Central and Eastern Europe      north
    ## 96                   Kenya              Sub-Saharan Africa       both
    ## 97                 Ukraine      Central and Eastern Europe      north
    ## 98                   Ghana              Sub-Saharan Africa      north
    ## 99         Dem. Rep. Congo              Sub-Saharan Africa      south
    ## 100                Georgia      Central and Eastern Europe      north
    ## 101             Rep. Congo              Sub-Saharan Africa      south
    ## 102                Senegal              Sub-Saharan Africa      north
    ## 103               Bulgaria      Central and Eastern Europe      north
    ## 104               Zimbabwe              Sub-Saharan Africa      south
    ## 105                 Malawi              Sub-Saharan Africa      south
    ## 106                  Gabon              Sub-Saharan Africa      south
    ## 107                   Mali              Sub-Saharan Africa      north
    ## 108                  Haiti     Latin America and Caribbean      north
    ## 109               Botswana              Sub-Saharan Africa      south
    ## 110                Comoros              Sub-Saharan Africa      south
    ## 111          Cote d'Ivoire              Sub-Saharan Africa      north
    ## 112               Cambodia               Southeastern Asia      north
    ## 113                 Angola              Sub-Saharan Africa      south
    ## 114                  Niger              Sub-Saharan Africa      north
    ## 115                   Chad              Sub-Saharan Africa      north
    ## 116           Burkina Faso              Sub-Saharan Africa      north
    ## 117             Madagascar              Sub-Saharan Africa      south
    ## 118               Tanzania              Sub-Saharan Africa      south
    ## 119                Liberia              Sub-Saharan Africa      north
    ## 120                  Benin              Sub-Saharan Africa      north
    ## 121                   Togo              Sub-Saharan Africa      north
    ## 122                  Syria Middle East and Northern Africa      north
    ##     HappinessScore   HDI GDP_PerCapita Beer_PerCapita Spirit_PerCapita
    ## 1            7.526 0.928       67803.0            224               81
    ## 2            7.509 0.943       93457.4            185              100
    ## 3            7.501 0.933       68383.8            233               61
    ## 4            7.498 0.951       89202.8            169               71
    ## 5            7.413 0.918       53982.6            263              133
    ## 6            7.404 0.922       52051.4            240              122
    ## 7            7.339 0.928       58061.0            251               88
    ## 8            7.334 0.915       48801.7            203               79
    ## 9            7.313 0.938       59934.1            261               72
    ## 10           7.291 0.932       60239.0            152               60
    ## 11           7.267 0.902       51430.1             63               69
    ## 12           7.119 0.906       53267.9            279               75
    ## 13           7.104 0.922       69287.5            249              158
    ## 14           7.087 0.791       12508.6            149               87
    ## 15           6.994 0.934       50801.8            346              117
    ## 16           6.952 0.758        7518.8            245              145
    ## 17           6.929 0.915       51767.8            295               84
    ## 18           6.907 0.934       99152.1            313              118
    ## 19           6.871 0.904      135682.8            236              133
    ## 20           6.778 0.772        9926.4            238               68
    ## 21           6.739 0.930       72794.0             60               12
    ## 22           6.725 0.920       47334.4            219              126
    ## 23           6.705 0.842       16502.8            130              124
    ## 24           6.701 0.785       14516.5            285              104
    ## 25           6.650 0.822       10729.2            193               25
    ## 26           6.596 0.885       26378.5            361              170
    ## 27           6.573 0.862       36284.6             16              135
    ## 28           6.545 0.802       17020.6            115               35
    ## 29           6.488 0.875       33257.4            149              100
    ## 30           6.481 0.747        6131.2            159               76
    ## 31           6.478 0.899       43518.5            127              151
    ## 32           6.474 0.748        7233.4             99              258
    ## 33           6.375 0.855       61276.0              1               42
    ## 34           6.361 0.889       30115.7            284              157
    ## 35           6.324 0.649        5025.6             53               69
    ## 36           6.269 0.719        4836.3            128              178
    ## 37           6.218 0.846       22232.3             42               63
    ## 38           6.168 0.785       15243.1            197              156
    ## 39           6.084 0.766       16055.6            333              100
    ## 40           6.078 0.853       21087.8            196              293
    ## 41           6.068 0.679        4408.5             52               69
    ## 42           5.992 0.657        2090.8             78              118
    ## 43           5.987 0.703        1983.1             25              101
    ## 44           5.977 0.878       35551.3             85               42
    ## 45           5.976 0.749        5934.9            162               74
    ## 46           5.956 0.709        4420.5            263              114
    ## 47           5.921 0.907       39285.2             77              202
    ## 48           5.919 0.797       10041.5            124              246
    ## 49           5.897 0.697        5314.5            109              226
    ## 50           5.856 0.815       12172.8            247              326
    ## 51           5.835 0.860       17840.9            343              215
    ## 52           5.835 0.900       34757.7            140               16
    ## 53           5.822 0.689        3414.9            167               41
    ## 54           5.813 0.855       23433.4            343              244
    ## 55           5.802 0.805        7303.7            142              373
    ## 56           5.768 0.894       29200.8            270               51
    ## 57           5.743 0.748        6692.2            163              160
    ## 58           5.658 0.705        7612.0             19               71
    ## 59           5.648 0.788        8812.1             98               31
    ## 60           5.560 0.844       20642.2            281              216
    ## 61           5.546 0.867       30798.5            192              154
    ## 62           5.538 0.702        5400.1            213              117
    ## 63           5.528 0.807       14861.9            297              122
    ## 64           5.517 0.868       27280.7            224              194
    ## 65           5.510 0.732        4586.7             82               97
    ## 66           5.488 0.828       17398.8            230               87
    ## 67           5.389 0.787        9586.6             51               22
    ## 68           5.303 0.735        4405.8              6               21
    ## 69           5.291 0.757        5384.0             21               46
    ## 70           5.279 0.696        3548.8             71              186
    ## 71           5.245 0.748       12556.3             79              192
    ## 72           5.185 0.669        1276.2             31               97
    ## 73           5.177 0.785        9215.0            283              131
    ## 74           5.163 0.766        6916.4             76              173
    ## 75           5.161 0.810        9367.0             31              114
    ## 76           5.155 0.733        8603.8            193              147
    ## 77           5.151 0.662        3496.8             12                6
    ## 78           5.145 0.835       18772.7            234              215
    ## 79           5.129 0.753        2670.4             20               55
    ## 80           5.123 0.845       24262.2            194               67
    ## 81           5.121 0.756        6720.9            106               27
    ## 82           5.061 0.689        3694.0            111                2
    ## 83           5.045 0.732        3924.3             51                3
    ## 84           5.033 0.868       20276.5            133              112
    ## 85           4.907 0.743        4534.9             77              189
    ## 86           4.875 0.530        2085.0             42                5
    ## 87           4.871 0.614        2831.0             69               98
    ## 88           4.795 0.586        1120.6             32               19
    ## 89           4.655 0.782        6494.4             89              132
    ## 90           4.635 0.413         515.9             25                3
    ## 91           4.574 0.645        4729.3            376                3
    ## 92           4.513 0.553        1661.7            147                1
    ## 93           4.459 0.696        6994.2            225               76
    ## 94           4.362 0.694        3876.4              6                4
    ## 95           4.360 0.749        4670.0             21              179
    ## 96           4.356 0.585        2006.8             58               22
    ## 97           4.324 0.746        4835.6            206              237
    ## 98           4.276 0.588        2445.3             31                3
    ## 99           4.272 0.452         584.1             32                3
    ## 100          4.252 0.776        5042.4             52              100
    ## 101          4.236 0.612        2213.9             76                1
    ## 102          4.219 0.499        1606.5              9                1
    ## 103          4.217 0.810       11635.0            231              252
    ## 104          4.193 0.532        1737.2             64               18
    ## 105          4.156 0.474         642.7              8               11
    ## 106          4.121 0.698        8017.0            347               98
    ## 107          4.073 0.421         917.9              5                1
    ## 108          4.028 0.496        1814.7              1              326
    ## 109          3.974 0.712        7347.6            173               35
    ## 110          3.956 0.502        1494.7              1                3
    ## 111          3.916 0.486        2578.8             37                1
    ## 112          3.907 0.576        1591.0             57               65
    ## 113          3.866 0.577        2137.9            217               57
    ## 114          3.856 0.351         594.9              3                2
    ## 115          3.763 0.405         696.4             15                1
    ## 116          3.739 0.420         918.2             25                7
    ## 117          3.695 0.517         514.9             26               15
    ## 118          3.666 0.533        1135.5             36                6
    ## 119          3.622 0.432         673.1             19              152
    ## 120          3.484 0.512        1428.4             34                4
    ## 121          3.303 0.500         992.3             36                2
    ## 122          3.069 0.536        1265.6              5               35
    ##     Wine_PerCapita Total_Consumption   GDP_log
    ## 1              278               583 11.124362
    ## 2              280               565 11.445261
    ## 3               78               372 11.132891
    ## 4              129               369 11.398668
    ## 5               97               493 10.896417
    ## 6              100               462 10.859987
    ## 7              190               529 10.969249
    ## 8              175               457 10.795520
    ## 9              212               545 11.001001
    ## 10             186               398 11.006075
    ## 11               9               141 10.847979
    ## 12             191               545 10.883089
    ## 13              84               491 11.146020
    ## 14              11               247  9.434172
    ## 15             175               638 10.835687
    ## 16              16               406  8.925162
    ## 17             212               591 10.854524
    ## 18             165               596 11.504410
    ## 19             271               640 11.818075
    ## 20               5               311  9.202953
    ## 21              11                83 11.195389
    ## 22             195               540 10.764993
    ## 23             172               426  9.711285
    ## 24              18               407  9.583041
    ## 25             221               439  9.280724
    ## 26             134               665 10.180305
    ## 27               5               156 10.499149
    ## 28             220               370  9.742180
    ## 29             120               369 10.412033
    ## 30               3               238  8.721146
    ## 31             370               648 10.680941
    ## 32               1               358  8.886464
    ## 33               7                50 11.023144
    ## 34             112               553 10.312802
    ## 35               2               124  8.522300
    ## 36               7               313  8.483905
    ## 37               7               112 10.009301
    ## 38               7               360  9.631882
    ## 39               3               436  9.683813
    ## 40             116               605  9.956450
    ## 41               2               123  8.391290
    ## 42               1               197  7.645302
    ## 43               8               134  7.592417
    ## 44             237               364 10.478732
    ## 45               3               239  8.688605
    ## 46               8               385  8.394008
    ## 47              16               295 10.578603
    ## 48              12               382  9.214482
    ## 49              18               353  8.578194
    ## 50              73               646  9.406959
    ## 51              56               614  9.789249
    ## 52               9               165 10.456156
    ## 53               8               216  8.135903
    ## 54              56               643 10.061918
    ## 55              42               557  8.896136
    ## 56             276               597 10.281951
    ## 57              21               344  8.808698
    ## 58              32               122  8.937481
    ## 59              18               147  9.083881
    ## 60              62               559  9.935093
    ## 61             113               459 10.335221
    ## 62              74               404  8.594173
    ## 63             167               586  9.606556
    ## 64              59               477 10.213935
    ## 65               9               188  8.430916
    ## 66             254               571  9.764157
    ## 67               7                80  9.168122
    ## 68               1                28  8.390677
    ## 69               5                72  8.591187
    ## 70               1               258  8.174365
    ## 71               8               279  9.437978
    ## 72               6               134  7.151642
    ## 73             127               541  9.128588
    ## 74               8               257  8.841651
    ## 75             128               273  9.144948
    ## 76               9               349  9.059959
    ## 77              10                28  8.159604
    ## 78             185               634  9.840159
    ## 79              31               106  7.889984
    ## 80             339               600 10.096675
    ## 81              86               219  8.812977
    ## 82               1               114  8.214465
    ## 83              20                74  8.274943
    ## 84             218               463  9.917218
    ## 85               8               274  8.419558
    ## 86               2                49  7.642524
    ## 87               2               169  7.948385
    ## 88               4                55  7.021620
    ## 89              54               275  8.778696
    ## 90               2                30  6.245913
    ## 91               1               380  8.461532
    ## 92               4               152  7.415596
    ## 93              81               382  8.852837
    ## 94               1                11  8.262662
    ## 95              11               211  8.448914
    ## 96               2                82  7.604297
    ## 97              45               488  8.483760
    ## 98              10                44  7.801923
    ## 99               1                36  6.370072
    ## 100            149               301  8.525637
    ## 101              9                86  7.702511
    ## 102              7                17  7.381813
    ## 103             94               577  9.361773
    ## 104              4                86  7.460030
    ## 105              1                20  6.465678
    ## 106             59               504  8.989320
    ## 107              1                 7  6.822088
    ## 108              1               328  7.503675
    ## 109             35               243  8.902129
    ## 110              1                 5  7.309681
    ## 111              7                45  7.855079
    ## 112              1               123  7.372118
    ## 113             45               319  7.667579
    ## 114              1                 6  6.388393
    ## 115              1                17  6.545924
    ## 116              7                39  6.822415
    ## 117              4                45  6.243973
    ## 118              1                43  7.034828
    ## 119              2               173  6.511894
    ## 120             13                51  7.264310
    ## 121             19                57  6.900025
    ## 122             16                56  7.143302

``` r
#Data Anlysis (Linear Regression)

#fitted line
fitH <- lm(HappinessScore ~ GDP_log, data = df)
summary(fitH)
```

    ## 
    ## Call:
    ## lm(formula = HappinessScore ~ GDP_log, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.53981 -0.49211  0.02786  0.41867  1.48769 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.51490    0.38887  -1.324    0.188    
    ## GDP_log      0.66993    0.04262  15.718   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6596 on 120 degrees of freedom
    ## Multiple R-squared:  0.6731, Adjusted R-squared:  0.6703 
    ## F-statistic:   247 on 1 and 120 DF,  p-value: < 2.2e-16

``` r
ggplot(df) +
  geom_point(aes(GDP_log, HappinessScore)) +
  geom_smooth(aes(GDP_log, HappinessScore), method=lm, se=FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](/Users/owlbemi/Documents/RWorkPlace/Final-Project-STAT4355_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#########Exploratory Analysis, ANOVA, and VIF################

#Sum of all alcohol consumption
Total_Consumption <- df$Beer_PerCapita + df$Spirit_PerCapita + df$Wine_PerCapita

df$Total_Consumption <- Total_Consumption

#HappinessScore (y) vs All other continuous variables
df2 <- melt(df[,4:9], id.vars = "HappinessScore")

ggplot(df2) +
  geom_jitter(aes(value, HappinessScore, colour=variable),) +
  geom_smooth(aes(value, HappinessScore, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](/Users/owlbemi/Documents/RWorkPlace/Final-Project-STAT4355_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
# Coefficient Summary
fit1 <- lm(HappinessScore ~ HDI + GDP_PerCapita + Beer_PerCapita + Spirit_PerCapita
          + Wine_PerCapita, data = df)
summary(fit1)
```

    ## 
    ## Call:
    ## lm(formula = HappinessScore ~ HDI + GDP_PerCapita + Beer_PerCapita + 
    ##     Spirit_PerCapita + Wine_PerCapita, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4714 -0.4229  0.0330  0.4038  1.3770 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       1.454e+00  3.997e-01   3.637 0.000413 ***
    ## HDI               5.303e+00  6.659e-01   7.964 1.26e-12 ***
    ## GDP_PerCapita     1.345e-05  3.495e-06   3.848 0.000195 ***
    ## Beer_PerCapita    6.579e-04  7.038e-04   0.935 0.351785    
    ## Spirit_PerCapita -6.719e-04  8.146e-04  -0.825 0.411196    
    ## Wine_PerCapita   -2.124e-03  8.781e-04  -2.419 0.017113 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6295 on 116 degrees of freedom
    ## Multiple R-squared:  0.7121, Adjusted R-squared:  0.6997 
    ## F-statistic: 57.39 on 5 and 116 DF,  p-value: < 2.2e-16

``` r
vif(fit1)
```

    ##              HDI    GDP_PerCapita   Beer_PerCapita Spirit_PerCapita 
    ##         3.040062         2.310697         1.661599         1.343791 
    ##   Wine_PerCapita 
    ##         1.828202

``` r
anova(fit1)
```

    ## Analysis of Variance Table
    ## 
    ## Response: HappinessScore
    ##                   Df  Sum Sq Mean Sq  F value    Pr(>F)    
    ## HDI                1 106.100 106.100 267.7519 < 2.2e-16 ***
    ## GDP_PerCapita      1   5.134   5.134  12.9565 0.0004702 ***
    ## Beer_PerCapita     1   0.044   0.044   0.1112 0.7393494    
    ## Spirit_PerCapita   1   0.108   0.108   0.2719 0.6030674    
    ## Wine_PerCapita     1   2.319   2.319   5.8520 0.0171133 *  
    ## Residuals        116  45.966   0.396                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Beer + Wine + Spirits
fitA <- lm(HappinessScore ~ Beer_PerCapita + Spirit_PerCapita
          + Wine_PerCapita, data = df)
summary(fitA)
```

    ## 
    ## Call:
    ## lm(formula = HappinessScore ~ Beer_PerCapita + Spirit_PerCapita + 
    ##     Wine_PerCapita, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.10876 -0.69790  0.03918  0.66035  2.25924 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      4.650863   0.159928  29.081  < 2e-16 ***
    ## Beer_PerCapita   0.003492   0.001015   3.441 0.000802 ***
    ## Spirit_PerCapita 0.001497   0.001158   1.292 0.198729    
    ## Wine_PerCapita   0.003740   0.001132   3.304 0.001260 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9637 on 118 degrees of freedom
    ## Multiple R-squared:  0.3137, Adjusted R-squared:  0.2962 
    ## F-statistic: 17.98 on 3 and 118 DF,  p-value: 1.129e-09

``` r
vif(fitA)
```

    ##   Beer_PerCapita Spirit_PerCapita   Wine_PerCapita 
    ##         1.473839         1.158650         1.295907

``` r
anova(fitA)
```

    ## Analysis of Variance Table
    ## 
    ## Response: HappinessScore
    ##                   Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## Beer_PerCapita     1  38.866  38.866 41.8486 2.337e-09 ***
    ## Spirit_PerCapita   1   1.075   1.075  1.1574   0.28421    
    ## Wine_PerCapita     1  10.141  10.141 10.9197   0.00126 ** 
    ## Residuals        118 109.589   0.929                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Sum of all Alcohol types
fitT <- lm(HappinessScore ~ Total_Consumption, data = df)
summary(fitT)
```

    ## 
    ## Call:
    ## lm(formula = HappinessScore ~ Total_Consumption, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.15472 -0.71380  0.04325  0.72146  2.23198 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       4.6027432  0.1555400  29.592  < 2e-16 ***
    ## Total_Consumption 0.0030658  0.0004278   7.166 6.79e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9653 on 120 degrees of freedom
    ## Multiple R-squared:  0.2997, Adjusted R-squared:  0.2939 
    ## F-statistic: 51.36 on 1 and 120 DF,  p-value: 6.787e-11

``` r
anova(fitT)
```

    ## Analysis of Variance Table
    ## 
    ## Response: HappinessScore
    ##                    Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## Total_Consumption   1  47.856  47.856  51.358 6.787e-11 ***
    ## Residuals         120 111.815   0.932                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# HDI
fitH <- lm(HappinessScore ~ HDI, data = df)
summary(fitH)
```

    ## 
    ## Call:
    ## lm(formula = HappinessScore ~ HDI, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.73979 -0.48049  0.03845  0.55535  1.37332 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.8950     0.3064   2.921  0.00416 ** 
    ## HDI           6.2491     0.4054  15.416  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6682 on 120 degrees of freedom
    ## Multiple R-squared:  0.6645, Adjusted R-squared:  0.6617 
    ## F-statistic: 237.7 on 1 and 120 DF,  p-value: < 2.2e-16

``` r
anova(fitH)
```

    ## Analysis of Variance Table
    ## 
    ## Response: HappinessScore
    ##            Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## HDI         1 106.100 106.100  237.66 < 2.2e-16 ***
    ## Residuals 120  53.571   0.446                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Income Types
#High Income (~$47,886.8)  >$50K
#Upper-Middle Income (~$10,835.5) >$11k
#Middle Income (~$6,102.0)  >$6K
#Lower-Middle Income (~$2,581.9)  >$2k
#Low Income (~$749.8)  <$2k

#Revised Happiness vs Everything

df3 <- melt(df[,4:11], id.vars="HappinessScore")

ggplot(df3) +
  geom_jitter(aes(value, HappinessScore, colour=variable),) +
  geom_smooth(aes(value, HappinessScore, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](/Users/owlbemi/Documents/RWorkPlace/Final-Project-STAT4355_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
fit2 <- lm(HappinessScore ~ HDI 
           + Total_Consumption, data = df)
summary(fit2)
```

    ## 
    ## Call:
    ## lm(formula = HappinessScore ~ HDI + Total_Consumption, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.65373 -0.53036  0.04141  0.55971  1.35668 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        0.7195452  0.3555156   2.024   0.0452 *  
    ## HDI                6.6534868  0.5805266  11.461   <2e-16 ***
    ## Total_Consumption -0.0004127  0.0004241  -0.973   0.3324    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6683 on 119 degrees of freedom
    ## Multiple R-squared:  0.6671, Adjusted R-squared:  0.6615 
    ## F-statistic: 119.3 on 2 and 119 DF,  p-value: < 2.2e-16

``` r
vif(fit2)
```

    ##               HDI Total_Consumption 
    ##           2.05012           2.05012

``` r
anova(fit2)
```

    ## Analysis of Variance Table
    ## 
    ## Response: HappinessScore
    ##                    Df  Sum Sq Mean Sq  F value Pr(>F)    
    ## HDI                 1 106.100 106.100 237.5598 <2e-16 ***
    ## Total_Consumption   1   0.423   0.423   0.9472 0.3324    
    ## Residuals         119  53.148   0.447                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Scatter plot: HDI vs Happiness Score by Region

hdi_scatter <- ggplot(df, aes(x = HDI, y = HappinessScore, color = Region)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  labs(title = "Human Development Index vs Happiness Score", x = "Human Development Index") +
  facet_wrap(~Region)

#Scatter plot: GDP Per Capita vs Happiness Score by Region

gdp_scatter <- ggplot(df, aes(x = GDP_PerCapita2, y = HappinessScore, color = Region)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "GDP Per Capita vs Happiness Score", x = "GDP Per Capita ($USD)") +
  facet_wrap(~Region)

hdi_scatter + theme_dark() + theme(plot.title = element_text(face = "bold"))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning in qt((1 - level)/2, df): NaN이 생성되었습니다

    ## Warning in max(ids, na.rm = TRUE): max에 전달되는 인자들 중 누락이 있어 -Inf를
    ## 반환합니다

![](/Users/owlbemi/Documents/RWorkPlace/Final-Project-STAT4355_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
gdp_scatter + theme_dark() + theme(plot.title = element_text(face = "bold"))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning in qt((1 - level)/2, df): NaN이 생성되었습니다

    ## Warning in qt((1 - level)/2, df): NaN이 생성되었습니다

    ## Warning in max(ids, na.rm = TRUE): max에 전달되는 인자들 중 누락이 있어 -Inf를
    ## 반환합니다

    ## Warning in max(ids, na.rm = TRUE): max에 전달되는 인자들 중 누락이 있어 -Inf를
    ## 반환합니다

![](/Users/owlbemi/Documents/RWorkPlace/Final-Project-STAT4355_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->
