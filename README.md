COVID analysis
================
Jon Yearsley
22 December, 2020

A quick look at the European data on Covid-19.

Data downloaded from two sources

-   European Centre for Disease Prevention and Control (ECDPC).
    <https://www.ecdc.europa.eu/en>

-   Johns Hopkins University Center for Systems Science and Engineering
    (CSSE), <https://github.com/CSSEGISandData/COVID-19>

## Select countries to visualise

``` r
countryList = c('Ireland',
            'United Kingdom',
            'Sweden',
            'France',
            'Switzerland')
```

## Download data

Data from ECDPC

``` r
download.file(url='https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx',
              destfile='COVID-19-geographic-disbtribution-worldwide.xlsx')

d_ecdpc= read_excel("COVID-19-geographic-disbtribution-worldwide.xlsx")
d_ecdpc = subset(d_ecdpc, countriesAndTerritories%in%countryList)
d_ecdpc$country = as.factor(d_ecdpc$countriesAndTerritories)
d_ecdpc$julian = as.numeric(julian(as.Date(d_ecdpc$dateRep)))
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

------------------------------------------------------------------------

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

| Database |    Country     |    Date    | Days post 50 cases | New Cases | New Deaths | Total deaths |
|:--------:|:--------------:|:----------:|:------------------:|:---------:|:----------:|:------------:|
|   CSSE   |    Ireland     | 2020-12-11 |        269         |    304    |     3      |     2120     |
|   CSSE   |    Ireland     | 2020-12-12 |        270         |    249    |     3      |     2123     |
|   CSSE   |    Ireland     | 2020-12-14 |        272         |    264    |     2      |     2126     |
|   CSSE   |    Ireland     | 2020-12-18 |        276         |    576    |     6      |     2149     |
|   CSSE   |    Ireland     | 2020-12-21 |        279         |    725    |     0      |     2158     |
|  ECDPC   |    Ireland     | 2020-12-11 |        269         |    303    |     15     |     2117     |
|  ECDPC   |    Ireland     | 2020-12-12 |        270         |    304    |     3      |     2120     |
|  ECDPC   |    Ireland     | 2020-12-14 |        272         |    429    |     1      |     2124     |
|   CSSE   | United Kingdom | 2020-12-11 |        283         |   21784   |    424     |    63603     |
|   CSSE   | United Kingdom | 2020-12-12 |        284         |   21554   |    520     |    64123     |
|   CSSE   | United Kingdom | 2020-12-14 |        286         |   20377   |    233     |    64500     |
|   CSSE   | United Kingdom | 2020-12-18 |        290         |   28560   |    490     |    66640     |
|   CSSE   | United Kingdom | 2020-12-21 |        293         |   33517   |    215     |    67718     |
|   CSSE   |     Sweden     | 2020-12-11 |        280         |   7370    |    160     |     7514     |
|   CSSE   |     Sweden     | 2020-12-18 |        287         |   9654    |    100     |     7993     |
|  ECDPC   |     Sweden     | 2020-12-11 |        279         |   6051    |     7      |     7514     |
|   CSSE   |     France     | 2020-12-11 |        284         |   13489   |    628     |    57672     |
|   CSSE   |     France     | 2020-12-14 |        287         |   3192    |    376     |    58391     |
|   CSSE   |     France     | 2020-12-18 |        291         |   16005   |    612     |    60345     |
|   CSSE   |     France     | 2020-12-21 |        294         |   5960    |    354     |    61019     |
|  ECDPC   |     France     | 2020-12-11 |        281         |   13750   |    292     |    56940     |
|  ECDPC   |     France     | 2020-12-12 |        282         |   13406   |    627     |    57567     |
|  ECDPC   |     France     | 2020-12-14 |        284         |   11533   |    150     |    57911     |
|   CSSE   |  Switzerland   | 2020-12-11 |        280         |   5136    |    104     |     5928     |
|   CSSE   |  Switzerland   | 2020-12-14 |        283         |   10726   |    169     |     6154     |
|   CSSE   |  Switzerland   | 2020-12-18 |        287         |   4478    |     74     |     6561     |
|   CSSE   |  Switzerland   | 2020-12-21 |        290         |   10002   |    159     |     6781     |
|  ECDPC   |  Switzerland   | 2020-12-11 |        279         |   5038    |     87     |     5273     |
|  ECDPC   |  Switzerland   | 2020-12-12 |        280         |   5111    |    105     |     5378     |

Table 1: The latest numbers from the European Centre for Diesease
Prevention and Control (ECDPC). <https://www.ecdc.europa.eu/en> and
Johns Hopkins University Center for Systems Science and Engineering
(CSSE), <https://github.com/CSSEGISandData/COVID-19>

### Temporal trends

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Warning in self$trans$transform(x): NaNs produced

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning in self$trans$transform(x): NaNs produced

    ## Warning: Transformation introduced infinite values in continuous y-axis

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

    ## Warning in self$trans$transform(x): NaNs produced

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning in self$trans$transform(x): NaNs produced

    ## Warning: Transformation introduced infinite values in continuous y-axis

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
