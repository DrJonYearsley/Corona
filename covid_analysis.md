COVID analysis
================
Jon Yearsley
4/3/2020

Data downloaded from
<https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports>

``` r
d = read_excel("COVID-19-geographic-disbtribution-worldwide.xlsx")
d$countriesAndTerritories = as.factor(d$countriesAndTerritories)
d$julian = as.numeric(julian(d$dateRep))
d$ID = c(1:nrow(d))

# Order deaths
ind = order(d$julian, d$countriesAndTerritories) 
d = d[ind,]

d$cumdeath = ave(d$deaths, 
                 d$countriesAndTerritories, 
                 FUN=function(x){cumsum(x)})
```

Find day when number of new cases exceeded 3

``` r
dplus = subset(d, cases>100)
startDate = aggregate(julian~countriesAndTerritories + geoId + countryterritoryCode, 
                      data=dplus, 
                      FUN=function(x) {min(x)})

ind = match(dplus$countryterritoryCode,startDate$countryterritoryCode)
dplus$daysIn = dplus$julian - startDate$julian[ind]
```

Select data from some countries

``` r
country = c('Ireland',
            'Italy',
            'Sweden',
            'United_Kingdom',
            'Framce', 
            'Germany')

d2 = subset(dplus, 
            countriesAndTerritories %in% country)
```

Plot the epidemic
![](covid_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## Warning: Transformation introduced infinite values in continuous y-axis
    
    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Removed 14 rows containing non-finite values (stat_smooth).

![](covid_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## Warning: Removed 14 rows containing non-finite values (stat_smooth).

![](covid_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

![](covid_analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
