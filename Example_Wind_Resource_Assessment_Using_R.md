Example Wind Resource Assessment Using R
================
Stephen Lightfoote
2018-05-30

-   [Example Wind Resource Assessment](#example-wind-resource-assessment)
    -   [Packages](#packages)
    -   [Get Wind Data](#get-wind-data)
    -   [Exploratory Analysis](#exploratory-analysis)
    -   [Energy Capture](#energy-capture)
    -   [Results](#results)
-   [Conclusions](#conclusions)

Example Wind Resource Assessment
================================

The goal here is to illustrate how aspects of typical wind resource assessment and energy capture from meteorological data can be accomplished using open source tools, in this case using R. Using publicly available data, I'll walk through some of the typical steps taken in site screening, importing, visualizing and analyzing meteorological data with the goal of modeling the annual energy capture of a wind turbine at a given location. For kicks, as a wink to the NIMBY's out there, let's use my backyard as the example.

Packages
--------

First, load the R packages used to do the analysis.

``` r
library(tidyverse) # r-package ecosystem
library(MASS) # statistical functions
library(knitr) # fancy tables
library(clifro) # windrose plot
library(scales) # percentages
library(elevatr) # elevation data
library(raster) # geospatial
library(leaflet) # mapping
```

Get Wind Data
-------------

Next, we'll download some sample wind resource data from NREL's wind toolkit to see how energetic my location is. The variable arguments in the paste statement below should be replaced with your own information (see <https://developer.nrel.gov/signup/> to get an API key).

``` r
lat=42.2756887
lon=-71.21161789999996
url<-paste('http://developer.nrel.gov/api/wind-toolkit/wind/wtk_download.csv?api_key=',api_key,'&wkt=POINT(',lon,'%20',lat,
')&attributes=wind_speed,wind_direction,power,temperature,pressure&names=2009&full_name=',name,'&email=',email,'&affiliation=',affiliation,
'&reason=Example',sep='')

#download data as a dataframe
df<-read_csv(url,skip=3,col_types = cols())
# tidy names
names(df)<-gsub("[^[:alnum:]]", "", names(df))
# convert dates to timestamp
df$timestamp_utc=as.POSIXct(paste(df$Year,df$Month,df$Day,df$Hour,df$Minute,'00',sep='-'),format='%Y-%m-%d-%H-%M-%S',tz='UTC')
```

Exploratory Analysis
--------------------

Now to the meat of the post, which is to illustrate how to analyze and visualize typical steps for wind resource assessment.

### Data Structure

``` r
head(df)
```

    ## # A tibble: 6 x 11
    ##    Year Month   Day  Hour Minute windspeedat100m… winddirectionat… powerMW
    ##   <int> <int> <int> <int>  <int>            <dbl>            <dbl>   <dbl>
    ## 1  2009     1     1     0      0             13.3              318    16.0
    ## 2  2009     1     1     0      5             13.3              317    16.0
    ## 3  2009     1     1     0     10             13.4              317    16.0
    ## 4  2009     1     1     0     15             13.4              317    16.0
    ## 5  2009     1     1     0     20             13.5              317    16.0
    ## 6  2009     1     1     0     25             13.6              317    16.0
    ## # ... with 3 more variables: airtemperatureat2mK <dbl>,
    ## #   surfaceairpressurePa <dbl>, timestamp_utc <dttm>

### Timeseries

``` r
ggplot(df,aes(timestamp_utc,windspeedat100mms))+geom_line()+theme_minimal()
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/timeseries-1.png" style="display: block; margin: auto;" />

### Monthly Wind Speed Distribution

``` r
ggplot(df,aes(factor(Month),windspeedat100mms))+
  geom_boxplot()+
  theme_minimal()+
  labs(x='Month')
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/monthly winds-1.png" style="display: block; margin: auto;" />

### Wind Rose

``` r
windrose(speed = df$windspeedat100mms,
                 direction = df$winddirectionat100mdeg,
                 n_directions = 12,
                 speed_cuts = seq(0,20,4),
                 ggtheme='minimal',
                 col_pal = 'YlGnBu')
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/wind rose-1.png" style="display: block; margin: auto;" />

### Weibull Fit

``` r
weibull_fit<-fitdistr(df$windspeedat100mms,'weibull')
x<-seq(0,20,.01)
weibull_density<-tibble(x,y=dweibull(x = x,shape = weibull_fit$estimate[1],scale = weibull_fit$estimate[2]))
ggplot(df,aes(windspeedat100mms))+
  geom_histogram(aes(y=..density..),bins=30,color='white')+
  geom_line(data=weibull_density,aes(x=x,y=y),color='red')+
  theme_minimal()
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/weibull-1.png" style="display: block; margin: auto;" />

Energy Capture
--------------

OK, let's model energy capture for this location.

### Power Curve

Let's use the GE 1.5SLE 77m turbine power curve as an example.

``` r
url<-'http://www.wind-power-program.com/Downloads/Databasepowercurves(May2017).zip'
tmp<-tempfile()
download.file(url,tmp)
unzip(tmp,files = 'Databasepowercurves(May2017)/HAWTs/500kw and above/General Electric/GE 1.5SLE 77m 1.5MW (MG).pow',junkpaths = T)
unlink(tmp)
pc<-read_csv('GE 1.5SLE 77m 1.5MW (MG).pow',
             skip=5,col_names = F,col_types='n',n_max = 30)
pc<-tibble(ws=seq(0.5,29.5,1),kw=pc$X1)
ggplot(pc,aes(ws,kw))+geom_point()+geom_line()+theme_minimal()
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/pc-1.png" style="display: block; margin: auto;" />

### Density Adjust Wind Speed

It's important to adjust the raw wind speed by air density as wind power density is a function of air density.

``` r
df<-mutate(df,air_density=(df$surfaceairpressurePa/df$airtemperatureat2mK*287)*.00001,
           dc_ws=windspeedat100mms*(air_density/mean(air_density))^(1/3))
ggplot(df,aes(windspeedat100mms,dc_ws,color=air_density))+
  geom_point()+
  theme_minimal()+
  coord_equal()
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/density adjust-1.png" style="display: block; margin: auto;" />

### Predict Energy

We'll use R's approx function to interpolate the equivalent turbine power for each wind speed in the timeseries. There's lot's of different methods for this of course.

``` r
df$kw<-approx(pc$ws,pc$kw,df$dc_ws)$y
ggplot(df,aes(kw))+
  geom_density(fill='blue')+
  theme_minimal()
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/energy capture-1.png" style="display: block; margin: auto;" />

Results
-------

### Aggregates

``` r
monthly<-df %>%
  group_by(Month) %>%
  summarise(hours=n()/12,
            windspeedat100mms=mean(windspeedat100mms),
            mwh=sum(kw,na.rm=T)/(1000*12)) %>%
  mutate(ncf=percent(mwh/(hours*1.5)))
kable(monthly,digits=1,caption='monthly totals',align='c')
```

| Month | hours | windspeedat100mms |  mwh  |  ncf  |
|:-----:|:-----:|:-----------------:|:-----:|:-----:|
|   1   |  744  |        6.8        | 467.9 | 41.9% |
|   2   |  672  |        8.3        | 584.4 | 58.0% |
|   3   |  744  |        7.2        | 510.5 | 45.7% |
|   4   |  720  |        7.4        | 512.2 | 47.4% |
|   5   |  744  |        6.7        | 420.7 | 37.7% |
|   6   |  720  |        5.2        | 239.8 | 22.2% |
|   7   |  744  |        6.2        | 354.7 | 31.8% |
|   8   |  744  |        5.5        | 285.9 | 25.6% |
|   9   |  720  |        6.4        | 396.3 | 36.7% |
|   10  |  744  |        7.3        | 504.5 | 45.2% |
|   11  |  720  |        7.1        | 471.7 | 43.7% |
|   12  |  744  |        8.7        | 691.6 | 62.0% |

``` r
#annual
annual<-data.frame(mwh=sum(monthly$mwh),ncf=percent(sum(monthly$mwh/(sum(monthly$hours)*1.5))))
kable(annual,digits=1,caption = 'annual totals',align='c')
```

|   mwh  |  ncf  |
|:------:|:-----:|
| 5440.1 | 41.4% |

Nice, a 41.4% NCF for this location is not too shabby. If only we could convince the landowner :)

### Comparison with NREL Model

NREL's dataset comes with an example power measurement based on a 5MW wind turbine. Let's see how our simple model stacks up using the GE 1.5SLE.

``` r
ggplot(df,aes(kw*.001,powerMW))+
  geom_point()+
  theme_minimal()+
  labs(x='GE 1.5sle MW',y='NREL 5MW turbine example MW')
```

<img src="Example_Wind_Resource_Assessment_Using_R_files/figure-markdown_github/nrel-1.png" style="display: block; margin: auto;" />

Conclusions
===========

Hopefully this was a useful tutorial. Let me know what you think!
