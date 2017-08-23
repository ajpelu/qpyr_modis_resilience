``` r
library('tidyverse')
```

Read data
=========

Two dataframes: \* EVI mean dataframe \* IV composite

(for another previous analysis see this [repo](https://github.com/ajpelu/qpyr_resilience)) and

``` r
# Read data
# iv <- read.csv(file=paste(di, "/data/iv_composite.csv", sep=""), header = TRUE, sep = ',')
evi <- read.csv(file=paste(di, "/data/evi_mean.csv", sep=""), header = TRUE, sep = ',')
```

Compute anomalies
=================

By evi\_mean
============

``` r
# Set counters
pixels <- unique(evi$iv_malla_modi_id)
# composites <- unique(evi$seasonF)
years <- unique(evi$year)

# Set dataframe to compute anomaly
df <- evi

# Create emtpy datafraje to store output 
anomalos <- data.frame()

for (i in pixels){
  df_aux <- df[df$iv_malla_modi_id == i,]
    
    # Create empty df to store anomlay of composite j of all year 
    aux_anomaly <- data.frame() 
    
    for (y in years){
      # get mean evi for the reference period
      iv_ref <- df_aux %>% 
        filter(year != y) %>% 
        summarise(mean(evi, na.rm=T))
      
      # Get sd ref for standardized anomaly
      sd_ref <- df_aux %>% 
        filter(year != y) %>% 
        summarise(sd(evi, na.rm=T))
      
      # get evi for the year
      iv_year <- df_aux[df_aux$year == y, 'evi']
      # To solve the problems with the year without composites (i.e 2000)
      iv_year <- ifelse(length(iv_year) == 0, 0, iv_year)
      
      # Compute anomaly
      anomaly <- (iv_year - iv_ref)
      names(anomaly) <- 'a'
      
      # Compute normalized anomaly 
      anomaly_nor <- ((iv_year - iv_ref) / (iv_year + iv_ref))*100
      names(anomaly_nor) <- 'nora'

      # Compute standardized anomaly
      anomaly_std <- (iv_year - iv_ref) / sd_ref
      names(anomaly_std) <- 'sa'
      
      # Create dataframe 
      aux <- cbind(y, anomaly, anomaly_nor, anomaly_std)
      aux_anomaly <- rbind(aux_anomaly, aux)
      } 
    
  # Add name of the pixel 
  aux_anomaly <- aux_anomaly %>% mutate(iv_malla_modi_id = i)
  
  # Output 
  anomalos <- rbind(anomalos, aux_anomaly)
}   


# Add pob, lat, long, etc 
evi_aux <- evi %>% dplyr::select(iv_malla_modi_id, long, lat, pop) %>%
  group_by(iv_malla_modi_id) %>% unique()

# Join dataframes 
anomalias_evimean <- anomalos %>% dplyr::inner_join(evi_aux, by="iv_malla_modi_id") 

write.csv(anomalias_evimean, file=paste(di, "/data/anomalies/anomalias_evimean.csv", sep=""), row.names = FALSE)
```

Notas
-----

Las anomalias se calculan para cada pixel del siguiente modo: a = evi\_year - evi\_ref; siendo evi\_year el valor del EVI medio para el año en cuestión y *evi\_ref* la media de todos los valores de EVI medio en todo el periodo de referencia.

En el caso de las anomalías estandarizadas, utilizamos la aproximación de Gao et al. (2016) [doi:10.1038/srep26958](https://dx.doi.org/10.1038/srep26958), donde las anomalias las dividimos por la desviación estandar de los valores de EVI medio para el periodo de referencia. Las anomalías estandarizadas generalmente proporcionan mas información referente a la magnitud de la anomalía ya que las potenciales influencias de la dispersión de los datos han sido eliminadas. Por tanto las anomalías estandarizadas se calculan como: sa = (evi\_year - evi\_ref) / sd\_ref.

Finalmente, computamos las anomalias normalizadas, en las cuales escalamos los valores de las anomalías entre -100 y 100, de acuerdo a la formula: nora = (evi\_year - evi\_ref) / (evi\_year + evi\_ref)\*100. Mas información ver <https://www.researchgate.net/post/how_can_I_normalize_the_range_of_NDVI_anomaly>

By composite (OJO esto aun no lo he computado: 23 / aug)
--------------------------------------------------------

``` r
# Set counters
pixels <- unique(iv$iv_malla_modi_id)
composites <- unique(iv$composite)
years <- unique(iv$year)

# Set dataframe to compute anomaly
df <- iv 

# Create emtpy datafraje to store output 
anomalos <- data.frame()

for (i in pixels){
  df_aux <- df[df$iv_malla_modi_id == i,]
  
  # Create empty auxiliar dataframe to store results
  aux_composite <- data.frame() 
  
  for (j in composites) {
    # Create df_auxiliar by composite
    df_by_composite <- df_aux[df_aux$composite == j,]
    
    # Create empty df to store anomlay of composite j of all year 
    aux_anomaly <- data.frame() 
    
    for (y in years){
      # get mean evi for the reference period
      iv_ref <- df_by_composite %>% 
        filter(year != y) %>% 
        summarise(mean(evi, na.rm=T))
      
      # get evi for the year
      iv_year <- df_by_composite[df_by_composite$year == y, 'evi']
      # To solve the problems with the year without composites (i.e 2000)
      iv_year <- ifelse(length(iv_year) == 0, 0, iv_year)
      
      # Compute standardized anomaly
      anomaly_std <- ((iv_year - iv_ref) / (iv_year + iv_ref))*100
      names(anomaly_std) <- 'anomaly_std'
      
      # Compute anomaly
      anomaly <- (iv_year - iv_ref)
      names(anomaly) <- 'anomaly'
      
      # Create dataframe 
      aux <- cbind(y, anomaly, anomaly_std)
      aux_anomaly <- rbind(aux_anomaly, aux)
      } 
      
    aux_compos <- aux_anomaly %>% mutate(composite = j)
    
    aux_composite <- rbind(aux_composite, aux_compos)
    
  }
    
  # Remove the composites 1,2 an 3 for year 2000 
  aux_composite <- aux_composite[!(aux_composite$y == 2000 & aux_composite$composite %in% c(1:3)), ] 
    
  # Add name of the pixel 
  aux_composite <- aux_composite %>% mutate(iv_malla_modi_id = i)
  
  # Output 
  anomalos <- rbind(anomalos, aux_composite)
}   


# Add pob, lat, long, etc 
iv_aux <- iv %>% dplyr::select(iv_malla_modi_id, long, lat, pop) %>%
  group_by(iv_malla_modi_id) %>% unique()

# Join dataframes 
anomalias_composite <- anomalos %>% dplyr::inner_join(iv_aux, by="iv_malla_modi_id") 

write.csv(anomalias_composite, file=paste(di, "/data/anomalies/anomalias_composite.csv", sep=""), row.names = FALSE)
```
