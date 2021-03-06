``` r
library("trend")
library('tidyverse')
source(paste0(di, '/script/R/exploreMKTS.r'))
library("rgdal")
library("sp")
library("raster")
library("rasterVis")
```

``` r
# Read data
iv <- read.csv(file=paste(di, "/data/evi_mean.csv", sep=""), header = TRUE, sep = ',')
```

``` r
## ojo filtramos por pop (quitmos 9 pop)
ivtrend <- iv %>% filter(pop != 9) %>% dplyr::select(iv_malla_modi_id, year, evi)
```

``` r
pixel <- unique(ivtrend$iv_malla_modi_id)

out <- data.frame()

for (i in pixel){ 
  m_aux <- c()
  
  # dataframe by pixel
  iv_pixel <- ivtrend %>% filter(iv_malla_modi_id == i)
  
  # MK trend test
  m <- mk.test(iv_pixel$evi, alternative = "two.sided", continuity = TRUE)
  
  # Sen slope 
  s <- sens.slope(iv_pixel$evi, conf.level = 0.95)

  
  p_value = round(m$p.value, 5)
  
  m_aux <- as.data.frame(
    cbind.data.frame(iv_malla_modi_id = i, 
          p_value = round(m$p.value, 6),
          statistic = round(m$statistic, 3),
          t(round(m$estimates, 5)), 
          senslope = round(s$estimates, 5),
          sen_lowci = round(s$conf.int[1],5),
          sen_uppci = round(s$conf.int[2],5),
          sen_levelci = as.character(attributes(s$conf.int))))

  row.names(m_aux) <- NULL 
  
  out <- rbind.data.frame(out, m_aux)
}

iv_att <- iv %>% dplyr::select(-year, -evi, -n_composites) %>% unique()

mk_evi  <- out %>% inner_join(iv_att, by = 'iv_malla_modi_id') 
```

Explore results Trends
----------------------

``` r
mk_per <- exploreMKTS(mk_evi, alpha=0.05) 
mk_per$summary %>% kable()
```

| variable      | n\_pixel | pct\_pixel |
|:--------------|:---------|:-----------|
| tau\_pos      | 720      | 78.95      |
| tau\_pos\_sig | 228      | 31.67      |
| tau\_neg      | 180      | 19.74      |
| tau\_neg\_sig | 10       | 5.56       |
| tau\_0        | 12       | 1.32       |

``` r
mk_evi <- mk_evi %>% mutate(sig = ifelse(p_value < 0.05, 'sig', 'nosig'))
  
mk_evi %>% ggplot(aes(x=tau)) + 
  geom_histogram(stat='bin', bindwidth=.1, fill='blue') +
  facet_wrap(~sig, labeller = as_labeller(c('nosig' = 'Non significant',
                                            'sig' = 'Significant'))) +
  theme_bw() + ylab('npixels') + 
  theme(strip.background = element_rect(colour='black', 
                                        fill='white'))
```

<img src="compute_MannKendall_evimean_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

Maps
====

``` r
# Create a spatial dataframe 
mk_evi_sp <- SpatialPointsDataFrame(coords = mk_evi[,c('long','lat')], 
                                  data = mk_evi, proj4string = CRS("+init=epsg:4326"))

variables <- c('tau', 'senslope', 'p_value')


# Transform 
aux_spatial <- spTransform(mk_evi_sp, CRS("+init=epsg:23030"))

for (i in variables){ 
  
  # raster auxiliar 
  aux_rast <- raster(aux_spatial, resolution=250)
  
  # raster_tau
  mr <- rasterize(aux_spatial, aux_rast, i, fun=mean)
  names(mr) <- paste0(i)
  
  # reprojected raster
  mr_re <- projectRaster(mr, crs=crs("+init=epsg:4326"))
  names(mr_re) <- paste0(i)
  
  # assign 
  name_raster <- paste0('r_', i)
  assign(name_raster, mr)
  
  name_raster_re <- paste0('r_re_', i)
  assign(name_raster_re, mr_re)
  
  writeRaster(mr, file=paste(di, "/data/raster/r_trendEVI_" , i, ".asc", sep=""), overwrite=TRUE)
  writeRaster(mr_re, file=paste(di, "/data/raster/r_trendEVI_re_", i, ".tiff", sep=""), overwrite=TRUE)
  
}


# Create stack of raster 
stack_trends <- stack(r_tau, r_senslope, r_p_value)
stack_trends_re <- stack(r_re_tau, r_re_senslope, r_re_p_value)
```

``` r
# Export stack 
temp <- getwd()
setwd(paste(di, "/data/raster/", sep=""))
writeRaster(stack_trends, filename = 'r_trend_stack', overwrite =TRUE) 
writeRaster(stack_trends_re, filename = 'r_trend_re_stack', overwrite =TRUE) 
setwd(temp)
```

``` r
# Select a palette http://colorbrewer2.org/
mypal <- brewer.pal(11, "RdYlGn")
# Specify the color palette
myTheme=rasterTheme(region=mypal)

lp <- levelplot(stack_trends_re, 
          margin=FALSE,
          layer='tau',
          par.settings=myTheme, 
          #par.settings=RdBuTheme, 
          names.attr="tau",
          pretty=TRUE,
          layout=c(1,1),
          main='tau', xlab=NULL, ylab=NULL) 

print(lp)
```

<img src="compute_MannKendall_evimean_files/figure-markdown_github/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r
tiff(filename=paste0(di, '/images/raster_maps/trends_evi_tau_.tiff'), 
     height = 1600, width = 2500, res=150, pointsize = 20, bg='transparent')
print(lp)
dev.off() 
```

    ## quartz_off_screen 
    ##                 2

``` r
lp <- levelplot(stack_trends_re, 
          margin=FALSE,
          layer='senslope',
          #par.settings=myTheme, 
          par.settings=RdBuTheme, 
          names.attr="senslope",
          pretty=TRUE,
          layout=c(1,1),
          main='Sen Slope', xlab=NULL, ylab=NULL) 

print(lp)
```

<img src="compute_MannKendall_evimean_files/figure-markdown_github/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
tiff(filename=paste0(di, '/images/raster_maps/trends_evi_senslope_.tiff'), 
     height = 1600, width = 2500, res=150, pointsize = 20, bg='transparent')
print(lp)
dev.off() 
```

    ## quartz_off_screen 
    ##                 2
