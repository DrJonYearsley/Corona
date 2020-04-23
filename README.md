COVID analysis
================
Jon Yearsley
23 April, 2020

A quick look at the European data on Covid-19.

Data downloaded from two sources

  - European Centre for Diesease Prevention and Control (ECDPC).
    <https://www.ecdc.europa.eu/en>

  - Johns Hopkins University Center for Systems Science and Engineering
    (CSSE), <https://github.com/CSSEGISandData/COVID-19>

## Select countries to visualise

``` r
countryList = c('Ireland',
            'United_Kingdom',
            'United Kingdom',
            'France',
            'Austria',
            'Switzerland',
            'US')
```

## Download data

Data from ECDPC

``` r
download.file(url='https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx',
              destfile='COVID-19-geographic-disbtribution-worldwide.xlsx')

d_ecdpc= read_excel("COVID-19-geographic-disbtribution-worldwide.xlsx")
d_ecdpc = subset(d_ecdpc, countriesAndTerritories%in%countryList)
d_ecdpc$country = as.factor(d_ecdpc$countriesAndTerritories)
d_ecdpc$julian = as.numeric(julian(d_ecdpc$dateRep))
d_ecdpc$ID = c(1:nrow(d_ecdpc))

# Order deaths by time
ind = order(d_ecdpc$julian, d_ecdpc$country) 
d_ecdpc= d_ecdpc[ind,]

d_ecdpc$cumdeaths = ave(d_ecdpc$deaths, 
                        d_ecdpc$country, 
                        FUN=function(x){cumsum(x)})

d_ecdpc$dataSource='ECDPC'
```

Data from CSSE

``` r
url_csse = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'
cases_filename = 'time_series_covid19_confirmed_global.csv'
deaths_filename = 'time_series_covid19_deaths_global.csv'
d_cases = read.csv(url(paste0(url_csse, cases_filename)))
d_deaths = read.csv(url(paste0(url_csse, deaths_filename)))

tmp = droplevels(subset(d_cases, Country.Region%in%countryList))
d1 = pivot_longer(data=tmp, 
                 cols = c(5:ncol(tmp)), 
                 names_to='dateStr', 
                 values_to='cumcases')

tmp = droplevels(subset(d_deaths, Country.Region%in%countryList))
d2 = pivot_longer(data=tmp, 
                 cols = c(5:ncol(tmp)), 
                 names_to='dateStr', 
                 values_to='cumdeaths')

d_csse = aggregate(cbind(cumcases,cumdeaths) ~ dateStr + Country.Region, 
                    data=merge(d1,d2), 
                    FUN=sum, na.rm=TRUE)

d_csse$dateRep = as.POSIXct(strptime(d_csse$dateStr, format='X%m.%d.%y'))
d_csse$julian = as.numeric(julian(d_csse$dateRep))
d_csse$cumcases = as.double(d_csse$cumcases)
d_csse$cumdeaths = as.double(d_csse$cumdeaths)

vars = names(d_csse)
vars[2] = 'country'
names(d_csse) = vars

# Order deaths by time
ind = order(d_csse$julian, d_csse$country) 
d_csse= d_csse[ind,]

# Calculate number of new cases and number of new deaths
for (c in countryList) {
  tmp = subset(d_csse, country==c)
  if (nrow(tmp)>0) {
    tmp$cases = c(tmp$cumcases[1], diff(tmp$cumcases))
    tmp$deaths = c(tmp$cumdeaths[1], diff(tmp$cumdeaths))
    
    if (c==countryList[1]) {
      d_csse_final = tmp
    } else {
      d_csse_final = rbind(d_csse_final, tmp)
    }
  }
}

d_csse_final$dataSource='CSSE'
```

-----

Find day when number of new cases exceeded X (where X is typically set
at 50-300)

``` r
casesThreshold = 50
casesStr = paste0('Days post ',casesThreshold,' cases') # String for graph labels

# Function to calculate days into epidemic
add_daysIn = function(d, threshold) {
  dplus = subset(d, cases>threshold)
  startDate = aggregate(julian~country, 
                        data=dplus, 
                        FUN=function(x) {min(x)})
  
  ind = match(dplus$country,
              startDate$country)
  dplus$daysIn = dplus$julian - startDate$julian[ind]
  return(dplus)
}

varKeep = c('dataSource','dateRep','country','julian',
            'cases','deaths','cumdeaths')


d2 = as.data.frame(rbind(add_daysIn(d_ecdpc[,varKeep], casesThreshold),
           add_daysIn(d_csse_final[,varKeep], casesThreshold)))

d2$dataSource = as.factor(d2$dataSource)
d2$dateRep = as.Date(d2$dateRep)
```

Calculate rolling average over a window (roughly 5- 7 days). One reason
for doing this is because there’s uncertainty in the dates due to the
time it takes for deaths and cases to be registered, etc.

``` r
dayWindow = 5
rollingStr = paste0('(',dayWindow,' day moving average)')  # String for graph labels
for (i in countryList) {
  for (s in c('CSSE','ECDPC')) {
    tmp=subset(d2, country==i & dataSource==s)
    tmp$deaths_rolling = rollmean(tmp$deaths, k=dayWindow, align='right', fill=NA)
    tmp$cumdeaths_rolling = rollmean(tmp$cumdeaths, k=dayWindow, align='right', fill=NA)
    tmp$cases_rolling = rollmean(tmp$cases, k=dayWindow, align='right', fill=NA)

    # # Calculate average 1 week before
    # tmp$cases_rolling = rollapply(tmp$cases, k=dayWindow, align='right', fill=NA)

        
    if (i==countryList[1] & s=='CSSE') {
      d3 = tmp
    } else {
      d3 = rbind(d3, tmp)
    }
  }
}
```

# Visualisations

### Tables of the latest new cases and deaths per country

``` r
library(pander)
ind = aggregate(julian~dataSource+country, 
                data=d3, 
                FUN=max)

pander(d3[d3$julian%in%ind$julian,
          c('dataSource',
                   'country', 
                   'dateRep',
                   'daysIn',
                   'cases',
                   'deaths',
                   'cumdeaths')],
       col.names=c('Database',
                   'Country',
                   'Date',
                   paste('Days post\n',casesThreshold,'cases'),
                   'New \nCases',
                   'New \nDeaths',
                   'Total deaths'),
       caption='Table 1: The latest numbers from the European Centre for Diesease Prevention and Control (ECDPC). https://www.ecdc.europa.eu/en and Johns Hopkins University Center for Systems Science and Engineering (CSSE), https://github.com/CSSEGISandData/COVID-19',
       keep.line.breaks = TRUE,
       row.names=FALSE)
```

| Database |     Country     |    Date    | Days post 50 cases | New Cases | New Deaths | Total deaths |
| :------: | :-------------: | :--------: | :----------------: | :-------: | :--------: | :----------: |
|   CSSE   |     Ireland     | 2020-04-20 |       34.96        |    388    |     43     |     730      |
|   CSSE   |     Ireland     | 2020-04-21 |       35.96        |    631    |     39     |     769      |
|  ECDPC   |     Ireland     | 2020-04-23 |         37         |    631    |     39     |     769      |
|  ECDPC   | United\_Kingdom | 2020-04-23 |         45         |   4451    |    763     |    18100     |
|   CSSE   | United Kingdom  | 2020-04-20 |       43.96        |   4316    |    828     |    17378     |
|   CSSE   | United Kingdom  | 2020-04-21 |       44.96        |   4466    |    773     |    18151     |
|   CSSE   |     France      | 2020-04-20 |       49.96        |   2817    |    537     |    20829     |
|  ECDPC   |     France      | 2020-04-23 |         49         |   1827    |    544     |    21340     |
|   CSSE   |     Austria     | 2020-04-20 |       41.96        |    78     |     21     |     491      |
|   CSSE   |     Austria     | 2020-04-21 |       42.96        |    52     |     19     |     510      |
|  ECDPC   |     Austria     | 2020-04-23 |         43         |    91     |     31     |     494      |
|   CSSE   |   Switzerland   | 2020-04-20 |       45.96        |    119    |     49     |     1478     |
|   CSSE   |   Switzerland   | 2020-04-21 |       46.96        |    205    |     31     |     1509     |
|  ECDPC   |   Switzerland   | 2020-04-23 |         47         |    205    |     30     |     1216     |
|   CSSE   |       US        | 2020-04-20 |       46.96        |   27539   |    2350    |    44444     |
|   CSSE   |       US        | 2020-04-21 |       47.96        |   28486   |    2178    |    46622     |

Table 1: The latest numbers from the European Centre for Diesease
Prevention and Control (ECDPC). <https://www.ecdc.europa.eu/en> and
Johns Hopkins University Center for Systems Science and Engineering
(CSSE), <https://github.com/CSSEGISandData/COVID-19>

### Temporal trends

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Epidemic indicator

Calculate the deaths over 5 days minus the same statistic in the
preceeding window.

``` r
d_plot$death_change = NA
d_plot$death_change[-c(1:dayWindow)] =   d_plot$deaths[-c(1:dayWindow)] 
```

… to be completed

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Fit a linear trend and plot residuals

``` r
d3sub = d_plot
m = lm(log10(deaths_rolling)~(1+log10(cumdeaths_rolling))*country, 
       data=d3sub, 
       na.action=na.exclude)
```

Plot residuals from these linear trends

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
