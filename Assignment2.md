Assignment 2
================
Brandyn Ruiz
9/22/2020

``` r
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
```

# Data Wrangling

``` r
urlfile = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv"

chs_individual <- read_csv(url(urlfile))
chs_individual <- na.omit(chs_individual)

head(chs_individual)
```

    ## # A tibble: 6 x 23
    ##     sid townname  male race  hispanic agepft height weight   bmi asthma
    ##   <dbl> <chr>    <dbl> <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl>
    ## 1     1 Lancast~     1 W            0  10.2     123     54  16.2      0
    ## 2     2 Lancast~     1 W            0  10.5     145     77  16.6      0
    ## 3     6 Lancast~     0 B            0  10.1     145    143  30.9      0
    ## 4     8 Lancast~     0 W            1   9.78    132     61  15.9      0
    ## 5    13 Lancast~     1 O            1  10.2     140     79  18.3      0
    ## 6    16 Lancast~     0 W            0  10.4     141     74  16.9      1
    ## # ... with 13 more variables: active_asthma <dbl>, father_asthma <dbl>,
    ## #   mother_asthma <dbl>, wheeze <dbl>, hayfever <dbl>, allergy <dbl>,
    ## #   educ_parent <dbl>, smoke <dbl>, pets <dbl>, gasstove <dbl>, fev <dbl>,
    ## #   fvc <dbl>, mmef <dbl>

``` r
urlfile = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv"

chs_regional <- read_csv(url(urlfile))
chs_regional <- na.omit(chs_regional)

head(chs_regional)
```

    ## # A tibble: 6 x 27
    ##   townname pm25_mass pm25_so4 pm25_no3 pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc
    ##   <chr>        <dbl>    <dbl>    <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Alpine        8.74     1.73     1.59     0.88    2.54    0.48    3.04    3.25
    ## 2 Lake El~     12.4      1.9      2.98     1.36    3.64    0.62    4.36    4.66
    ## 3 Long Be~     19.1      3.23     6.22     2.57    5.21    1.36    6.25    6.68
    ## 4 Mira Lo~     30.0      2.69    12.2      4.25   11.8     1.25   14.2    15.2 
    ## 5 Riversi~     22.4      2.43     8.66     3.14    5.27    0.94    6.32    6.75
    ## 6 San Dim~     20.5      2.59     7.2      2.71    5.59    1.17    6.71    7.17
    ## # ... with 18 more variables: pm10_ec <dbl>, pm10_tc <dbl>, formic <dbl>,
    ## #   acetic <dbl>, hcl <dbl>, hno3 <dbl>, o3_max <dbl>, o3106 <dbl>,
    ## #   o3_24 <dbl>, no2 <dbl>, pm10 <dbl>, no_24hr <dbl>, pm2_5_fr <dbl>,
    ## #   iacid <dbl>, oacid <dbl>, total_acids <dbl>, lon <dbl>, lat <dbl>

``` r
chs_merge <- merge(chs_individual, chs_regional)

head(chs_merge)
```

    ##   townname sid male race hispanic    agepft height weight      bmi asthma
    ## 1   Alpine 835    0    W        0 10.099932    143     69 15.33749      0
    ## 2   Alpine 839    0    M        1 10.053388    142     86 19.38649      0
    ## 3   Alpine 869    1    W        1  9.629021    141     68 15.54705      0
    ## 4   Alpine 937    0    W        0 10.053388    141     71 16.23295      0
    ## 5   Alpine 938    0    W        0 10.234086    138     85 20.28795      0
    ## 6   Alpine 841    1    W        1 10.548939    150     78 15.75758      0
    ##   active_asthma father_asthma mother_asthma wheeze hayfever allergy educ_parent
    ## 1             0             0             0      0        0       1           3
    ## 2             0             0             1      1        1       1           3
    ## 3             1             0             0      1        0       0           3
    ## 4             0             0             0      0        0       0           3
    ## 5             0             0             0      0        0       0           2
    ## 6             0             0             0      0        0       0           5
    ##   smoke pets gasstove      fev      fvc     mmef pm25_mass pm25_so4 pm25_no3
    ## 1     0    1        0 2529.276 2826.316 3406.579      8.74     1.73     1.59
    ## 2     1    1        0 2121.711 2326.974 2835.197      8.74     1.73     1.59
    ## 3     0    1        0 1856.910 1952.751 3464.236      8.74     1.73     1.59
    ## 4     0    1        1 1853.115 2035.082 2259.344      8.74     1.73     1.59
    ## 5     0    1        0 2180.464 2368.212 3115.232      8.74     1.73     1.59
    ## 6     0    1        0 2251.505 2594.649 2445.151      8.74     1.73     1.59
    ##   pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc pm10_ec pm10_tc formic acetic  hcl
    ## 1     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 2     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 3     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 4     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 5     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ## 6     0.88    2.54    0.48    3.04    3.25    0.49    3.75   1.03   2.49 0.41
    ##   hno3 o3_max o3106 o3_24   no2  pm10 no_24hr pm2_5_fr iacid oacid total_acids
    ## 1 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 2 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 3 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 4 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 5 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ## 6 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28  2.39  3.52         5.5
    ##         lon      lat
    ## 1 -116.7664 32.83505
    ## 2 -116.7664 32.83505
    ## 3 -116.7664 32.83505
    ## 4 -116.7664 32.83505
    ## 5 -116.7664 32.83505
    ## 6 -116.7664 32.83505

``` r
#1) Double checking for duplicates
dim(chs_individual)
```

    ## [1] 782  23

``` r
dim(chs_regional)
```

    ## [1]  8 27

``` r
dim(chs_merge)
```

    ## [1] 519  49

``` r
#2)
chs_merge <- chs_merge%>%
  mutate(obesity_level = case_when(bmi < 14 ~ 'Underweight',
                                   bmi < 23 ~ 'Normal',
                                   bmi < 25 ~ 'Overweight',
                                   bmi > 24 ~ 'Obese'))

chs_merge %>%
  group_by(obesity_level)%>%
  summarise(minBMI = min(bmi), maxBMI = max(bmi), n = n())
```

    ## # A tibble: 4 x 4
    ##   obesity_level minBMI maxBMI     n
    ##   <chr>          <dbl>  <dbl> <int>
    ## 1 Normal          14.0   22.9   448
    ## 2 Obese           25.1   41.3    34
    ## 3 Overweight      23.0   24.9    23
    ## 4 Underweight     12.3   14.0    14

``` r
#3)
chs_merge <- chs_merge%>%
  mutate(smoke_gas_exposure = ifelse(smoke == 1 & gasstove == 0, 'YesSmokeNoStove',
                              ifelse(smoke == 0 & gasstove == 1, 'NoSmokeYesStove',
                              ifelse(smoke == 1 & gasstove == 1, 'YesSmokeYesStove',
                                     'NoSmokeNoStove'))))
```

``` r
#4)
chs_merge %>%
  group_by(townname)%>%
  summarise(AverageFEV = mean(fev), St.devFEV = sd(fev), ProportionAsthma = sum(asthma)/nrow(chs_merge),
            St.devAsthma = sd(asthma))
```

    ## # A tibble: 8 x 5
    ##   townname      AverageFEV St.devFEV ProportionAsthma St.devAsthma
    ##   <chr>              <dbl>     <dbl>            <dbl>        <dbl>
    ## 1 Alpine             2091.      294.           0.0212        0.385
    ## 2 Lake Elsinore      2028.      339.           0.0154        0.340
    ## 3 Long Beach         1981.      329.           0.0212        0.385
    ## 4 Mira Loma          1993.      328.           0.0193        0.366
    ## 5 Riverside          2040.      295.           0.0173        0.353
    ## 6 San Dimas          2051.      317.           0.0212        0.358
    ## 7 Santa Maria        2022.      329.           0.0135        0.324
    ## 8 Upland             2030.      343.           0.0154        0.315

``` r
chs_merge%>%
  group_by(male)%>%
  summarise(AverageFEV = mean(fev), St.devFEV = sd(fev), ProportionAsthma = sum(asthma)/nrow(chs_merge),
            St.devAsthma = sd(asthma))
```

    ## # A tibble: 2 x 5
    ##    male AverageFEV St.devFEV ProportionAsthma St.devAsthma
    ##   <dbl>      <dbl>     <dbl>            <dbl>        <dbl>
    ## 1     0      1967.      331.           0.0617        0.334
    ## 2     1      2089.      302.           0.0829        0.368

``` r
chs_merge%>%
  group_by(obesity_level)%>%
  summarise(AverageFEV = mean(fev), St.devFEV = sd(fev), ProportionAsthma = sum(asthma)/nrow(chs_merge),
            St.devAsthma = sd(asthma))
```

    ## # A tibble: 4 x 5
    ##   obesity_level AverageFEV St.devFEV ProportionAsthma St.devAsthma
    ##   <chr>              <dbl>     <dbl>            <dbl>        <dbl>
    ## 1 Normal             2015.      312.          0.112          0.336
    ## 2 Obese              2270.      250.          0.0173         0.448
    ## 3 Overweight         2197.      330.          0.0135         0.470
    ## 4 Underweight        1658.      284.          0.00193        0.267

``` r
chs_merge%>%
  group_by(smoke_gas_exposure)%>%
  summarise(AverageFEV = mean(fev), St.devFEV = sd(fev), ProportionAsthma = sum(asthma)/nrow(chs_merge),
            St.devAsthma = sd(asthma))
```

    ## # A tibble: 4 x 5
    ##   smoke_gas_exposure AverageFEV St.devFEV ProportionAsthma St.devAsthma
    ##   <chr>                   <dbl>     <dbl>            <dbl>        <dbl>
    ## 1 NoSmokeNoStove          2042.      346.          0.0250         0.320
    ## 2 NoSmokeYesStove         2030.      308.          0.100          0.364
    ## 3 YesSmokeNoStove         2170.      368.          0.00578        0.439
    ## 4 YesSmokeYesStove        1979.      337.          0.0135         0.319

# Looking at the Data (EDA)

``` r
#BMI, smoke and gas exposure, and PM2.5 on FEV
chs_merge%>%
  select(bmi, smoke_gas_exposure, pm25_mass)%>%
  str()
```

    ## 'data.frame':    519 obs. of  3 variables:
    ##  $ bmi               : num  15.3 19.4 15.5 16.2 20.3 ...
    ##  $ smoke_gas_exposure: chr  "NoSmokeNoStove" "YesSmokeNoStove" "NoSmokeNoStove" "NoSmokeYesStove" ...
    ##  $ pm25_mass         : num  8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 ...

``` r
chs_merge%>%
  select(bmi, smoke_gas_exposure, pm25_mass)%>%
  head()
```

    ##        bmi smoke_gas_exposure pm25_mass
    ## 1 15.33749     NoSmokeNoStove      8.74
    ## 2 19.38649    YesSmokeNoStove      8.74
    ## 3 15.54705     NoSmokeNoStove      8.74
    ## 4 16.23295    NoSmokeYesStove      8.74
    ## 5 20.28795     NoSmokeNoStove      8.74
    ## 6 15.75758     NoSmokeNoStove      8.74

``` r
chs_merge%>%
  select(bmi, smoke_gas_exposure, pm25_mass)%>%
  tail()
```

    ##          bmi smoke_gas_exposure pm25_mass
    ## 514 15.14014    NoSmokeYesStove     22.46
    ## 515 19.52934    NoSmokeYesStove     22.46
    ## 516 13.91466   YesSmokeYesStove     22.46
    ## 517 19.90317   YesSmokeYesStove     22.46
    ## 518 15.09766    NoSmokeYesStove     22.46
    ## 519 15.91326    NoSmokeYesStove     22.46

``` r
chs_merge%>%
  select(bmi, smoke_gas_exposure, pm25_mass)%>%
  summary()
```

    ##       bmi        smoke_gas_exposure   pm25_mass    
    ##  Min.   :12.31   Length:519         Min.   : 7.19  
    ##  1st Qu.:15.90   Class :character   1st Qu.:12.35  
    ##  Median :17.48   Mode  :character   Median :20.52  
    ##  Mean   :18.45                      Mean   :18.11  
    ##  3rd Qu.:20.16                      3rd Qu.:22.46  
    ##  Max.   :41.27                      Max.   :29.97

``` r
#1)
ggplot(chs_merge, aes(bmi, fev))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~townname, ncol = 2)+
  labs(title = 'Scatterplot of BMI and FEV by Town name', x = 'BMI', y = 'FEV')
```

![](Assignment2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#2)
ggplot(chs_merge, aes(x = fev, fill = obesity_level))+
  geom_histogram()+
  scale_fill_brewer(palette = 'Paired', direction = -1)+
  # theme_dark()+
  labs(title = 'Staked Histogram of FEV counts by BMI category', x = 'FEV', fill = 'BMI Category')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Assignment2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(chs_merge, aes(x = fev, fill = smoke_gas_exposure))+
  geom_histogram()+
  scale_fill_brewer(palette = 'Purples', direction = -1)+
  # theme_dark()+
  labs(title = 'Staked Histogram of FEV counts by Smoke and Gas Exposure', x = 'FEV', fill = 'Smoke/Gas Exposure')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Assignment2_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
#3)
ggplot(chs_merge, aes(bmi))+
  geom_histogram()+
  facet_wrap(~smoke_gas_exposure)+
  labs(title = 'Histogram of BMI by Smoke and Gas Exposure', x = 'BMI')
```

![](Assignment2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#4)

#FEV by BMI
model <- lm(fev ~ bmi, data = chs_merge)
temp_var <- predict(model, interval = 'prediction')
```

    ## Warning in predict.lm(model, interval = "prediction"): predictions on current data refer to _future_ responses

``` r
test_df <- cbind(chs_merge, temp_var)

ggplot(test_df, aes(bmi, fev))+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_line(aes(y = lwr), color = 'red', linetype = 'dashed')+
  geom_line(aes(y = upr), color = 'red', linetype = 'dashed')+
  labs(title = 'Scatterplot of FEV by BMI with Confidence and Prediction intervals', x = 'BMI', y = 'FEV')
```

![](Assignment2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#FEV by smoke and gas exposure
ggplot(chs_merge, aes(smoke_gas_exposure, fev))+
  geom_boxplot()+
  labs(title = 'Boxplot of FEV by Smoke and Gas Exposure', x = 'Smoke and Gase Exposure',y = 'FEV')
```

![](Assignment2_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
#5)
leaflet(chs_merge)%>%
  addProviderTiles('OpenStreetMap')%>%
  addCircles(lat=~lat, lng=~lon, opacity = 0.2, radius = ~(pm25_mass)^2 * 20, popup = ~townname)
```

<!--html_preserve-->

<div id="htmlwidget-3afc8bedf76e00f53813" class="leaflet html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-3afc8bedf76e00f53813">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751],[-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876],[1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,1527.752,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,3050.45,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,7311.488,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,17964.018,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,10026.242,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,8421.408,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,1033.922,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032,10089.032],null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.2,"fill":true,"fillColor":"#03F","fillOpacity":0.2},["Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Alpine","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Lake Elsinore","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Long Beach","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Mira Loma","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","Riverside","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","San Dimas","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Santa Maria","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland","Upland"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[32.8350521,34.9530337],"lng":[-120.4357191,-116.7664109]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

``` r
#tried different code for opacity but leaflet will not take
```

``` r
#6)
#PM 2.5 mass, FEV
chs_merge$pm25_mass <- as.factor(chs_merge$pm25_mass)

ggplot(chs_merge, aes(pm25_mass, fev, color = pm25_mass))+
  geom_boxplot()+
  labs(title = 'Boxplot of PM 2.5 mass and FEV', x = 'PM 2.5 Mass', y = 'FEV')
```

![](Assignment2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
