COVID analysis
================
Jon Yearsley
09 April, 2020

A quick look at the European data on Covid-19.

Data downloaded from two sources

  - European Centre for Diesease Prevention and Control (ECDPC).
    <https://www.ecdc.europa.eu/en>

  - Johns Hopkins University Center for Systems Science and Engineering
    (CSSE), <https://github.com/CSSEGISandData/COVID-19>

## Select countries to visualise

``` r
countryList = c('Ireland',
            'Sweden',
            'United_Kingdom',
            'United Kingdom',
            'France',
            'Italy',
            'Austria',
            'Spain')
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

Calculate rolling average over a window (nominally 7 days). One reason
for doing this is because there’s uncertainty in the dates due to the
time it takes for deaths and cases to be registered, etc.

``` r
dayWindow = 7
rollingStr = paste0('(',dayWindow,' day moving average)')  # String for graph labels
for (i in countryList) {
  for (s in c('CSSE','ECDPC')) {
    tmp=subset(d2, country==i & dataSource==s)
    tmp$deaths_rolling = rollmean(tmp$deaths, k=dayWindow, align='right', fill=NA)
    tmp$cumdeaths_rolling = rollmean(tmp$cumdeaths, k=dayWindow, align='right', fill=NA)
    tmp$cases_rolling = rollmean(tmp$cases, k=dayWindow, align='right', fill=NA)
    
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
       caption='Table 1: The latest numbers from the European Centre for Diesease Prevention and Control (ECDPC). https://www.ecdc.europa.eu/en',
       keep.line.breaks = TRUE)
```

|          | Database |     Country     |    Date    | Days post 50 cases | New Cases | New Deaths |
| :------: | :------: | :-------------: | :--------: | :----------------: | :-------: | :--------: |
| **234**  |   CSSE   |     Ireland     | 2020-04-07 |       21.96        |    365    |     25     |
| **229**  |  ECDPC   |     Ireland     | 2020-04-09 |         23         |    515    |     25     |
| **468**  |   CSSE   |     Sweden      | 2020-04-07 |       33.96        |    726    |     96     |
| **232**  |  ECDPC   |     Sweden      | 2020-04-09 |         33         |    726    |     96     |
| **233**  |  ECDPC   | United\_Kingdom | 2020-04-09 |         31         |   5491    |    938     |
| **546**  |   CSSE   | United Kingdom  | 2020-04-07 |       30.96        |   5525    |    940     |
| **1561** |   CSSE   |     France      | 2020-04-07 |       36.96        |   3894    |    544     |
| **228**  |  ECDPC   |     France      | 2020-04-09 |         35         |   3881    |    541     |
| **312**  |   CSSE   |      Italy      | 2020-04-07 |       44.96        |   3836    |    542     |
| **230**  |  ECDPC   |      Italy      | 2020-04-09 |         46         |   3836    |    540     |
| **781**  |   CSSE   |     Austria     | 2020-04-07 |       28.96        |    303    |     30     |
| **227**  |  ECDPC   |     Austria     | 2020-04-09 |         29         |    329    |     30     |
| **390**  |   CSSE   |      Spain      | 2020-04-07 |       34.96        |   6278    |    747     |
| **231**  |  ECDPC   |      Spain      | 2020-04-09 |         34         |   6180    |    757     |

Table 1: The latest numbers from the European Centre for Diesease
Prevention and Control (ECDPC). <https://www.ecdc.europa.eu/en>
(continued below)

|          | Total deaths |
| :------: | :----------: |
| **234**  |     235      |
| **229**  |     235      |
| **468**  |     687      |
| **232**  |     687      |
| **233**  |     7097     |
| **546**  |     7111     |
| **1561** |    10887     |
| **228**  |    10869     |
| **312**  |    17669     |
| **230**  |    17669     |
| **781**  |     273      |
| **227**  |     273      |
| **390**  |    14792     |
| **231**  |    14555     |

### Temporal trends

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Epidemic indicator

    ## Warning: Removed 42 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 42 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Fit a linear trend and plot residuals

``` r
d3sub = subset(d3,dataSource=='ECDPC')
m = lm(log10(deaths_rolling)~(1+log10(cumdeaths_rolling))*country, 
       data=d3sub, 
       na.action=na.exclude)
```

Plot residuals from these linear trends

``` r
d3sub$residuals = residuals(m)
d3sub$fitted = fitted(m)

p5 = ggplot(data=d3sub,
       aes(x=fitted, 
           y=residuals, 
           colour=country)) +
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

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
