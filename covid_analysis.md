COVID analysis
================
Jon Yearsley
4/3/2020

Data downloaded from
<https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx>

``` r
d = read_excel("COVID-19-geographic-disbtribution-worldwide.xlsx")
d$countriesAndTerritories = as.factor(d$countriesAndTerritories)
d$julian = as.numeric(julian(d$dateRep))
d$ID = c(1:nrow(d))

# Order deaths by time
ind = order(d$julian, d$countriesAndTerritories) 
d = d[ind,]

d$cumdeaths = ave(d$deaths, 
                  d$countriesAndTerritories, 
                  FUN=function(x){cumsum(x)})
```

Find day when number of new cases exceeded 3

``` r
dplus = subset(d, cases>100)
startDate = aggregate(julian~countriesAndTerritories + 
                        geoId + 
                        countryterritoryCode, 
                      data=dplus, 
                      FUN=function(x) {min(x)})

ind = match(dplus$countryterritoryCode,
            startDate$countryterritoryCode)
dplus$daysIn = dplus$julian - startDate$julian[ind]
```

Select data from some countries

``` r
country = c('Ireland',
            'Italy',
            'Sweden',
            'United_Kingdom',
            'Netherlands', 
            'Germany')

d2 = subset(dplus, 
            countriesAndTerritories %in% country)
```

Calculate 7 day roling averages

``` r
for (i in 1:length(country)) {
  tmp=subset(d2, countriesAndTerritories==country[i])
  tmp$deaths_7day = rollmean(tmp$deaths, k=7, align='right', fill=NA)
  tmp$cumdeaths_7day = rollmean(tmp$cumdeaths, k=7, align='right', fill=NA)
  tmp$cases_7day = rollmean(tmp$cases, k=7, align='right', fill=NA)
  
  if (i==1) {
    d3 = tmp
  } else {
    d3 = rbind(d3, tmp)
  }
}
```

Plot the epidemic

    ## Warning: Removed 36 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 36 rows containing missing values (geom_point).

![](covid_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## Warning: Removed 36 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 36 rows containing missing values (geom_point).

![](covid_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## Warning: Removed 36 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 36 rows containing missing values (geom_point).

![](covid_analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Warning: Removed 36 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 36 rows containing missing values (geom_point).

![](covid_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
