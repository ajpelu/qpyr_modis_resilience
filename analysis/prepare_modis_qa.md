``` r
library("tidyverse")
library("knitr")
library("binaryLogic")
source(paste0(di,"/script/R/getComposite.R"))
```

``` r
# Read data
rawdata <- read.csv(file=paste(di, "/data_raw/evi/iv_qp_raw_qa_2017.csv", sep= ""), header = TRUE, sep = ',')
```

Prepare Data
------------

-   Raw data come from GEE script (see [`./script/GEE/get_iv_modis.qp.js`](./script/GEE/get_iv_modis.qp.js)

-   Date:

-   Get date of the image (from hdf title, system.index)
-   Store as date (date format) and create new variable for year

-   Select and rename variables of interest

``` r
raw <- rawdata %>% 
  mutate(
    # Date
    date = as.Date(substr(system.index,1,10), format = "%Y_%m_%d"),
    # date2 = as.Date(substr(system.index,1,10), format = "%Y_%m_%d"),
    year = lubridate::year(date), 
    
    # GEE index
    gee_index = stringr::str_replace(
      substr(system.index,22, nchar(as.character(system.index))),
    pattern = '_', "")) %>% 
    # test = ifelse(date == date2, 0, 1))
  dplyr::select(doy = DayOfYear, evi = EVI, ndvi = NDVI, summQA = SummaryQA, iv_malla_modi_id,
                pop, date, year, gee_index, lat, long, 
                qa_quality, qa_use, qa_aerosol, qa_adj_cloud, 
                qa_atmos, qa_mix_cloud, qa_landwater, qa_snow, qa_shadow)
```

Some metadata of the time series
--------------------------------

### Temporal range of the time series

``` r
# Get temporal range of the data
# Start date
unique(min(as.Date(raw$date)))
```

    ## [1] "2000-02-18"

``` r
# End date 
unique(max(as.Date(raw$date)))
```

    ## [1] "2016-12-18"

Quality assessment
------------------

### Number of images per year

``` r
# See n of images per year and per pixel 
n_images_pixel <- raw %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>%
  summarise(n = n(),
            n_pixel = n()/length(unique(iv_malla_modi_id)))

kable(n_images_pixel)
```

|  year|      n|  n\_pixel|
|-----:|------:|---------:|
|  2000|  17632|        19|
|  2001|  21344|        23|
|  2002|  21344|        23|
|  2003|  21344|        23|
|  2004|  21344|        23|
|  2005|  21344|        23|
|  2006|  21344|        23|
|  2007|  21344|        23|
|  2008|  21344|        23|
|  2009|  21344|        23|
|  2010|  21344|        23|
|  2011|  21344|        23|
|  2012|  21344|        23|
|  2013|  21344|        23|
|  2014|  21344|        23|
|  2015|  20416|        22|
|  2016|  21344|        23|

### Summary table of layer summaryQA

``` r
# Table of QA
raw %>% group_by(summQA) %>% 
  summarise(npixels = n()) %>% 
  mutate(freq = round((npixels / sum(npixels)*100),2),
         QA = plyr::mapvalues(summQA, c(0, 1, 2, 3), c("Good Data", "Marginal data", "Snow/Ice", "Cloudy"))) %>% 
  kable() 
```

|  summQA|  npixels|   freq| QA            |
|-------:|--------:|------:|:--------------|
|       0|   207616|  57.96| Good Data     |
|       1|   115378|  32.21| Marginal data |
|       2|     9268|   2.59| Snow/Ice      |
|       3|    25502|   7.12| Cloudy        |
|      NA|      444|   0.12| NA            |

### Explore temporal distribution of QA

``` r
qadate <- raw %>% 
  group_by(date, summQA) %>% 
  count(date, summQA) %>% 
  mutate(freq = round((n / sum(n))*100, 2),
         QA = plyr::mapvalues(summQA, c(0, 1, 2, 3), c("Good Data", "Marginal data", "Snow/Ice", "Cloudy"))) 


qadate %>% 
  ggplot(aes(x=as.Date(date), y=freq, fill=QA)) + 
  geom_bar(stat = 'identity', position='fill') + 
#  facet_wrap(~QA, ncol=1) + 
  theme_bw() +
  scale_x_date(date_labels = "%Y %d")
```

![](prepare_modis_qa_files/figure-markdown_github/unnamed-chunk-6-1.png)

QA detailed
-----------

-   Only explore the marginal data

``` r
decodeQA <- function(x, nb){
  bit <- intToBits(x)
  paste(tail(rev(as.integer(bit)), nb), collapse="")}

decodeQAv <- Vectorize(decodeQA)


marginal <- raw %>% filter(summQA == 1) 
```

### Quality

``` r
marginal %>% 
  mutate(qa_quality_dec = decodeQAv(qa_quality, nb=2)) %>% 
  group_by(qa_quality_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 2 <U+00D7> 3
    ##   qa_quality_dec     n  freq
    ##            <chr> <int> <dbl>
    ## 1             00 27239 23.61
    ## 2             01 88139 76.39

### Quality usefulness

``` r
# See https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mod13_user_guide.pdf 
raw %>% 
  mutate(qa_use_dec = decodeQAv(qa_use, nb=4)) %>% 
  group_by(qa_use_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 13 <U+00D7> 3
    ##    qa_use_dec      n  freq
    ##         <chr>  <int> <dbl>
    ## 1        0000 190527 53.19
    ## 2        0001  61081 17.05
    ## 3        0010  31599  8.82
    ## 4        0011  27283  7.62
    ## 5        0100  23851  6.66
    ## 6        0101  13003  3.63
    ## 7        0110   6146  1.72
    ## 8        0111   2968  0.83
    ## 9        1000    890  0.25
    ## 10       1001    264  0.07
    ## 11       1010    140  0.04
    ## 12       1011      8  0.00
    ## 13       1111    448  0.13

``` r
# Marginal data 
marginal %>% 
  mutate(qa_use_dec = decodeQAv(qa_use, nb=4)) %>% 
  group_by(qa_use_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 9 <U+00D7> 3
    ##   qa_use_dec     n  freq
    ##        <chr> <int> <dbl>
    ## 1       0001 44230 38.33
    ## 2       0010 27251 23.62
    ## 3       0011 19739 17.11
    ## 4       0100 14742 12.78
    ## 5       0101  6642  5.76
    ## 6       0110  1891  1.64
    ## 7       0111   708  0.61
    ## 8       1000    53  0.05
    ## 9       1111   122  0.11

### Aerosol

``` r
marginal %>% 
  mutate(qa_aerosol_dec = decodeQAv(qa_aerosol, nb=2)) %>% 
  group_by(qa_aerosol_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 4 <U+00D7> 3
    ##   qa_aerosol_dec     n  freq
    ##            <chr> <int> <dbl>
    ## 1             00  1418  1.23
    ## 2             01 27239 23.61
    ## 3             10 78623 68.14
    ## 4             11  8098  7.02

-   Only the 7.02 % of the pixels marked as 'Marginal data' contain high concentration of Aerosols.

### Clouds

``` r
# Adjacents
marginal %>% 
  mutate(qa_adj_cloud_dec = decodeQAv(qa_adj_cloud, nb=1)) %>% 
  group_by(qa_adj_cloud_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 2 <U+00D7> 3
    ##   qa_adj_cloud_dec     n  freq
    ##              <chr> <int> <dbl>
    ## 1                0 80492 69.76
    ## 2                1 34886 30.24

``` r
# Mixed
marginal %>% 
  mutate(qa_mix_cloud_dec = decodeQAv(qa_mix_cloud, nb=1)) %>% 
  group_by(qa_mix_cloud_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 1 <U+00D7> 3
    ##   qa_mix_cloud_dec      n  freq
    ##              <chr>  <int> <dbl>
    ## 1                0 115378   100

-   All the pixel marked as 'Marginal data' do not contain mixed cloud.
-   We have to consider the Adjacent clouds

### Snow

``` r
marginal %>% 
  mutate(qa_snow_dec = decodeQAv(qa_snow, nb=1)) %>% 
  group_by(qa_snow_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 1 <U+00D7> 3
    ##   qa_snow_dec      n  freq
    ##         <chr>  <int> <dbl>
    ## 1           0 115378   100

-   All the pixel marked as 'Marginal data' do not contain snow or ice.

### Shadow

``` r
marginal %>% 
  mutate(qa_shadow_dec = decodeQAv(qa_shadow, nb=1)) %>% 
  group_by(qa_shadow_dec) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum (n)*100,2))
```

    ## # A tibble: 2 <U+00D7> 3
    ##   qa_shadow_dec     n  freq
    ##           <chr> <int> <dbl>
    ## 1             0 77408 67.09
    ## 2             1 37970 32.91

-   We have to consider the shadow

### Filter

We applied customized filter based on Reyes-Díez et al. 2015.

-   Creamos una variable nueva, llamada `filtered`. Aquellos pixeles con *Good Data* le asignamos valor `90`. Los *Snow/Ice* y los *Cloudy* le asignamos valor `99`. Para los *Marginal Data* miramos la calidad. En concreto previamente hemos analizado la cantidad de aerosoles, la de nubes y la de sombras (ver mas arriba). La forma de proceder es la siguiente:
-   Si tiene alta concentración de aerosoles, marcamos el pixel con 1.
-   Si tiene nubes adjacentes, marcamos el pixel con 1.
-   Si tiene sombra, marcamos el pixel con 1.
-   Al final, creamos una variable llamada `f_sum` con la suma de esos tres filtros. Si en alguno de los tres filtros se da la condición 1, es decir `f_sum > = 1`, entonces, le asignamos el valor `99` en la variable filtered.

``` r
rawfilter <- raw %>% 
  mutate(f_aerosol = ifelse(decodeQAv(qa_aerosol, nb=2) == '11', 'Aerosol (high)', 'OK'),
         f_cloud_a = ifelse(decodeQAv(qa_adj_cloud, nb=1) == '1', 'Clouds: adjacent', 'OK'),
         f_shadow = ifelse(decodeQAv(qa_shadow, nb=1) == '1', 'Shadow','OK'),
         # as integer
         f_aerosoli = ifelse(f_aerosol == 'OK', 0, 1),
         f_cloud_ai = ifelse(f_cloud_a == 'OK', 0, 2),
         f_shadowi = ifelse(f_shadow == 'OK', 0, 4)) 

rawfilter <- rawfilter %>% 
  mutate(f_sum = f_aerosoli + f_cloud_ai + f_shadowi,
         filtered = ifelse(summQA == 0, 90, 
                           ifelse(summQA %in% c(2,3), 99, f_sum)), 
         
         filtered_qa = plyr::mapvalues(filtered, 
                                       c(0, 1, 2, 3, 4, 5, 6, 7, 90, 99, NA),
                                       c("Others", "Aerosol",
                                         "Clouds adj", "Aerosol + Clouds adj",
                                         "Shadow", "Aerosol + Shadow", 
                                         "Clouds adj + Shadow", 
                                         "Aerosol + Clouds adj + Shadow",
                                         "Good Data", "Snow / Ice or Cloudy", "NA")),
         mes = lubridate::month(date))

rawfilter %>% group_by(filtered_qa) %>% 
  summarise(npixels = n()) %>% 
  mutate(freq = round((npixels / sum(npixels)*100),2))
```

    ## # A tibble: 11 <U+00D7> 3
    ##                      filtered_qa npixels  freq
    ##                            <chr>   <int> <dbl>
    ## 1                        Aerosol    3246  0.91
    ## 2           Aerosol + Clouds adj    2733  0.76
    ## 3  Aerosol + Clouds adj + Shadow    1392  0.39
    ## 4               Aerosol + Shadow     727  0.20
    ## 5                     Clouds adj   16130  4.50
    ## 6            Clouds adj + Shadow   14631  4.08
    ## 7                      Good Data  207616 57.96
    ## 8                             NA     444  0.12
    ## 9                         Others   55299 15.44
    ## 10                        Shadow   21220  5.92
    ## 11          Snow / Ice or Cloudy   34770  9.71

-   According to Reyes-Díez et al. (2015) we must consider the shadow in the mountain, but we can discard the filter of adjacent clouds. On the other hand, the use of EVI mean is highly stable under the use of any filter (see Reyes-Díez et al. 2015).

``` r
rawfilter <- rawfilter %>% 
  mutate(filtered_2 = ifelse(filtered %in% c(0,1,2,3), 90, filtered),
         filtered_2_qa = plyr::mapvalues(filtered_2, 
                                       c(4, 5, 6, 7, 90, 99, NA),
                                       c("Shadow", "Aerosol + Shadow", 
                                         "Clouds adj + Shadow", 
                                         "Aerosol + Clouds adj + Shadow",
                                         "Good Data", "Snow / Ice or Cloudy", "NA")))


rawfilter %>% group_by(filtered_2_qa) %>% 
  summarise(npixels = n()) %>% 
  mutate(freq = round((npixels / sum(npixels)*100),2))
```

    ## # A tibble: 7 <U+00D7> 3
    ##                   filtered_2_qa npixels  freq
    ##                           <chr>   <int> <dbl>
    ## 1 Aerosol + Clouds adj + Shadow    1392  0.39
    ## 2              Aerosol + Shadow     727  0.20
    ## 3           Clouds adj + Shadow   14631  4.08
    ## 4                     Good Data  285024 79.57
    ## 5                            NA     444  0.12
    ## 6                        Shadow   21220  5.92
    ## 7          Snow / Ice or Cloudy   34770  9.71

We have to explore the temporal distribution of the filtered data (especially for Shadow)

``` r
sombras <- c("Shadow", "Aerosol + Shadow","Clouds adj + Shadow", "Aerosol + Clouds adj + Shadow") 
sombras_n <- c(4, 5, 6, 7)
                                         
# Shadow 
rawfilter %>% filter(filtered_2 %in% sombras_n) %>% group_by(mes) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n  / sum(n )*100),2)) %>% 
  ggplot(aes(x=as.factor(mes), y=n)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label=freq, size=3, vjust=-.25)) + 
  theme_bw() + 
  theme(legend.position = 'none') + 
  xlab('month') + 
  ylab('# Images with shadow')
```

![](prepare_modis_qa_files/figure-markdown_github/unnamed-chunk-16-1.png)

Now for Ice / Cloudy

``` r
# "Snow / Ice or Cloudy" (99)
                                         
# Shadow 
rawfilter %>% filter(filtered_2 == 99) %>% group_by(mes) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n  / sum(n )*100),2)) %>% 
  ggplot(aes(x=as.factor(mes), y=n)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label=freq, size=3, vjust=-.25)) + 
  theme_bw() + 
  theme(legend.position = 'none') + 
  xlab('month') + 
  ylab('# Images with Snow / Ice or Cloudy')
```

![](prepare_modis_qa_files/figure-markdown_github/unnamed-chunk-17-1.png)

We select only data with high Good Quality, because shadow, ice or cloudy are mainly locate at winter months.

``` r
df <- rawfilter %>% 
  filter(filtered_2 == 90) %>% 
  select(doy, evi, ndvi, iv_malla_modi_id, pop, date, year, lat, long, summQA, qa_use, filtered, filtered_2, mes) 

df %>% group_by(mes) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n/sum(n )*100),2)) %>% 
  ggplot(aes(x=as.factor(mes), y=n)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label=freq, size=3, vjust=-.25)) + 
  theme_bw() + 
  theme(legend.position = 'none') + 
  xlab('month') + 
  ylab('# Images with Good Quality')
```

![](prepare_modis_qa_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
ncomposites_year <- df %>% group_by(iv_malla_modi_id, year) %>% summarise(n=n()) 

ncomposites_year %>% ggplot(aes(x=n)) + 
  geom_histogram(stat='count') +
  xlab('# composites by year and pixel') + 
  ylab('# ')+
  theme_bw()
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](prepare_modis_qa_files/figure-markdown_github/unnamed-chunk-19-1.png)

We do not apply a filter treshold, because we selected only the Good Data.

Prepare data
============

### Get the composite of the images and the season

-   See [Testa et al. 2014](https://www.researchgate.net/publication/262566793_Correcting_MODIS_16-day_composite_NDVI_time-series_with_actual_acquisition_dates)
-   Use a [custom function](/R/getComposite.R)

``` r
# Get leap years 
years <- unique(df$year)
ly <- years[lubridate::leap_year(years)]

# Two functions 
rd <- df %>% 
  mutate(m= lubridate::month(date),
         d= lubridate::day(date)) %>% 
  mutate(composite = ifelse(year %in% ly, 
                            getComposite_leap(m,d),
                            getComposite_nonleap(m,d))) %>% 
  mutate(season = ifelse(composite < 6, 'winter',
                   ifelse(composite < 12, 'spring',
                    ifelse(composite < 18, 'summer','autumn'))))
```

### Scale factor of the NDVI and EVI data

``` r
# Apply scale factor https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1 
rd <- rd %>% 
  mutate(evi = evi * 0.0001,
         ndvi = ndvi * 0.0001) 
```

We created several datasets:

###  ***iv\_composite***

iv by pixel and by composite (output as `./data/iv_composite.csv`). It contains the following fields: \* `iv_malla_modi_id`: the identifier of the modis cell \* `year` \* `evi` or `ndvi`: the value of the EVI (or NDVI) for the composite \* `date`: date of adquisition of the image \* `composite`: number of composite (23 by year) \* `long`: longitude coordinates \* `lat`: latitute coordinates \* `pop`: numeric code of the *Q. pyrenaica* population \* `season`: \* `0` annual value \* `1` spring value \* `2` summer value \* `3` autumn value \* `4` winter value \* `seasonF`: the season coded as factor \* `summQA`: the summary Quality Assessment (see MODIS) \* `qa_use`: the usseful index (see QA MODIS)

###  ***evi\_mean***

evi mean by pixel and year (output as `./data/evi_mean.csv`). It contains the following fields:

-   `iv_malla_modi_id`: the identifier of the modis cell
-   `year`
-   `evi`: mean value of evi for pixel *i* at year *j*
-   `long`: longitude coordinates
-   `lat`: latitute coordinates
-   `pop`: numeric code of the *Q. pyrenaica* population
-   `n_composites`: number of composites used to computed the annual mean value of EVI (according to previously filtering)

### ***evi\_seasonal*** and ***ndvi\_seasonal***

-   annual and seasonal evi by pixel (output as `./data/evi_atributes_all.csv`)
-   annual and seasonal ndvi by pixel (output as `./data/ndvi_atributes_all.csv`)

These two dataframes have the following fields:

-   `iv_malla_modi_id`: the identifier of the modis cell
-   `year`
-   `evi` or `ndvi`: the value of the EVI (or NDVI) (cumulative value for each season)
-   `season`: the season of cumulative evi:
-   `0` annual value
-   `1` spring value
-   `2` summer value
-   `3` autumn value
-   `4` winter value
-   `seasonF`: the season coded as factor
-   `long`: longitude coordinates
-   `lat`: latitute coordinates
-   `pop`: numeric code of the *Q. pyrenaica* population

#### Create dataframe with composites

``` r
iv_composite <- rd %>% 
  dplyr::select(iv_malla_modi_id, evi, ndvi, pop, date, year, long, lat, composite, seasonF = season, summQA, qa_use)
```

#### Create EVI mean dataset

``` r
evimean <- rd %>% 
  group_by(iv_malla_modi_id, year) %>%
  summarise(evi = mean(evi[evi >=0]),
            n_composites = length(year)) 

# Add coordinates and pob and other useful info 
aux_rd <- rd %>% dplyr::select(iv_malla_modi_id, long, lat, pop) %>%
  group_by(iv_malla_modi_id) %>% unique()

# Join dataframes 
evimean <- evimean %>% dplyr::inner_join(aux_rd, by="iv_malla_modi_id") 
```

#### Create seasonal dataframes of EVI and NDVI (integrate, EVI and NDVI sum)

-   EVI

``` r
# Create annual evi by pixel 
evi_annual <- rd %>% 
  group_by(iv_malla_modi_id, year) %>%
  summarise(evi = sum(evi[evi >=0])) %>%
  mutate(seasonF='annual', 
         season = 0)

# Create seasonal evi by pixel 
evi_season <- rd %>% 
  group_by(iv_malla_modi_id, year, season) %>%
  summarise(evi = sum(evi[evi >=0])) %>% 
  mutate(seasonF = season) %>% 
  mutate(season = ifelse(season == 'autumn', 3, 
                   ifelse(season == 'winter', 4, 
                    ifelse(season == 'spring', 1,2))))

evidf <- rbind(evi_annual, evi_season)

# Add coordinates and pob 
aux_rd <- rd %>% dplyr::select(iv_malla_modi_id, long, lat, pop) %>%
  group_by(iv_malla_modi_id) %>% unique()


# Join dataframes 
evidf <- evidf %>% dplyr::inner_join(aux_rd, by="iv_malla_modi_id") 
```

-   NDVI

``` r
# Create annual ndvi by pixel 
ndvi_annual <- rd %>% 
  group_by(iv_malla_modi_id, year) %>%
  summarise(ndvi = sum(ndvi[ndvi >=0])) %>%
  mutate(seasonF='annual', 
         season = 0)

# Create seasonal ndvi by pixel 
ndvi_season <- rd %>% 
  group_by(iv_malla_modi_id, year, season) %>%
  summarise(ndvi = sum(ndvi[ndvi >=0])) %>% 
  mutate(seasonF = season) %>% 
  mutate(season = ifelse(season == 'autumn', 3, 
                   ifelse(season == 'winter', 4, 
                    ifelse(season == 'spring', 1,2))))

ndvidf <- rbind(ndvi_annual, ndvi_season)

# Join dataframes 
ndvidf <- ndvidf %>% dplyr::inner_join(aux_rd, by="iv_malla_modi_id")
```

### Export dataframes

``` r
# Export dataframes 
write.csv(evimean, file=paste(di, "/data/evi_mean.csv", sep=""), row.names = FALSE)
write.csv(evidf, file=paste(di, "/data/evi_atributes_all.csv", sep=""), row.names = FALSE)
write.csv(ndvidf, file=paste(di, "/data/ndvi_atributes_all.csv", sep=""), row.names = FALSE)
write.csv(iv_composite, file=paste(di, "/data/iv_composite.csv", sep=""), row.names = FALSE)
```
