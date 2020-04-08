COVID analysis
================
Jon Yearsley
08 April, 2020

A quick look at the European data on Covid-19.

Data downloaded from
<https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx>

## Process data

``` r
download.file(url='https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx',
              destfile='COVID-19-geographic-disbtribution-worldwide.xlsx')

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

Find day when number of new cases exceeded X (where X is typically set
at 100-300)

``` r
casesThreshold = 100
casesStr = paste0('Days post ',casesThreshold,' cases') # String for graph labels
dplus = subset(d, cases>casesThreshold)
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
            'Sweden',
            'United_Kingdom',
            'France',
            'Denmark',
            'Austria',
            'Spain')

d2 = subset(dplus, 
            countriesAndTerritories %in% country)
```

Calculate rolling average over a window (nominally 7 days). One reason
for doing this is because there’s uncertainty in the dates due to the
time it takes for deaths and cases to be registered, etc.

``` r
dayWindow = 7
rollingStr = paste0('(',dayWindow,' day moving average)')  # String for graph labels
for (i in 1:length(country)) {
  tmp=subset(d2, countriesAndTerritories==country[i])
  tmp$deaths_rolling = rollmean(tmp$deaths, k=dayWindow, align='right', fill=NA)
  tmp$cumdeaths_rolling = rollmean(tmp$cumdeaths, k=dayWindow, align='right', fill=NA)
  tmp$cases_rolling = rollmean(tmp$cases, k=dayWindow, align='right', fill=NA)
  
  if (i==1) {
    d3 = tmp
  } else {
    d3 = rbind(d3, tmp)
  }
}
```

# Visualisations

### Tables of the latest new cases and deaths per country

``` r
d_latest = subset(d3, julian==max(julian))

library(pander)
pander(d_latest[,c(7, 1,14,5,6,13)],
       col.names=c('Country',
                   'Date',
                   paste('Days post\n',casesThreshold,'cases'),
                   'New \nCases',
                   'New \nDeaths',
                   'Total deaths'),
       caption='Table 1: The latest numbers from the European Centre for Diesease Prevention and Control (ECDPC). https://www.ecdc.europa.eu/en',
       keep.line.breaks = TRUE)
```

|     Country     |    Date    | Days post 100 cases | New Cases | New Deaths | Total deaths |
| :-------------: | :--------: | :-----------------: | :-------: | :--------: | :----------: |
|     Ireland     | 2020-04-08 |         19          |    345    |     36     |     210      |
|     Sweden      | 2020-04-08 |         27          |    487    |    114     |     591      |
| United\_Kingdom | 2020-04-08 |         26          |   3634    |    786     |     6159     |
|     France      | 2020-04-08 |         33          |   3777    |    1417    |    10328     |
|     Denmark     | 2020-04-08 |         28          |    390    |     16     |     203      |
|     Austria     | 2020-04-08 |         26          |    343    |     23     |     243      |
|      Spain      | 2020-04-08 |         32          |   5478    |    743     |    13798     |

Table 1: The latest numbers from the European Centre for Diesease
Prevention and Control (ECDPC). <https://www.ecdc.europa.eu/en>

### Temporal trends

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Epidemic indicator

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Fit a linear trend and plot residuals

``` r
m = lm(log10(deaths_rolling)~(1+log10(cumdeaths_rolling))*countriesAndTerritories, 
       data=d3, 
       na.action=na.exclude)
```

Plot residuals from these linear trends

``` r
d3$residuals = residuals(m)
d3$fitted = fitted(m)

p5 = ggplot(data=d3,
       aes(x=fitted, 
           y=residuals, 
           colour=countriesAndTerritories)) +
  geom_abline(intercept=0, slope=0) +
  geom_point() + 
  geom_smooth(method = 'loess', 
              span=1,
              se=FALSE) +
  scale_color_brewer(palette = 'Dark2') +
  theme_bw() + 
  labs(x=paste0('Fitted for log10(number of deaths)'),
       y=paste0('Residuals from linear regression'),
       title='Residuals from cumulative verus current deaths')

#ggplotly(p5)
p5
```

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
