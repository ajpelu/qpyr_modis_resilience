## Compute Resilience of EVI 

## Resilience metrics: See Lloret et al. 2011 http://dx.doi.org/10.1111/j.1600-0706.2011.19372.x
## df: a dataframe
## event_years: vector with drought years, i.e.: c(1995, 2005)
## window: size of the window to compute previous and post event segments 

## A list with  to df 
## $evi: summary statistics by pixel of evi for each period (prev, dr, post)
## $resilience: resilience metrics for each drougth period 

## Modified from baiResilience (https://github.com/ajpelu/qpyr_dendro)

eviResilience <- function(df, event_years, window){ 
  
  out_evi <- c() 
  out_res <- c() 
  out <- list()
  
  for (i in event_years){
    # Create df previous, during and post event
    df_pre <- df %>% filter(year < i & year >= (i - window)) %>% mutate(disturb = 'pre')
    df_event <- df %>% filter(year == i) %>% mutate(disturb = 'dr')
    df_post <- df %>% filter(year > i & year <= (i + window)) %>% mutate(disturb = 'post')
    
    # Melt data 
    dfmelt <- bind_rows(df_pre, df_event, df_post) %>% 
      gather(variable, value, -disturb, -year, -iv_malla_modi_id)
    
    # Compute mean of Growth (previous, during and posterior)
    evi_df <- dfmelt %>% group_by(disturb, iv_malla_modi_id) %>% 
      summarise(mean_period = mean(value, na.rm=TRUE),
                sd_period = sd(value, na.rm=TRUE),
                se_period = sd_period/sqrt(length(value))) %>% 
      mutate(disturb_year = i) %>% as.data.frame() 
    
    out_evi <- rbind(out_evi, evi_df)
    
    # Compute resilience by pixel 
    aux_resilience <- c() 
    pixels <- unique(df$iv_malla_modi_id)
    
    for (t in pixels){
      # Filter by tree
      df_pixel <- evi_df %>% filter(iv_malla_modi_id == t) 
      
      # Compute resilience metrics by pixel
      aux_df <- df_pixel %>% select(-c(iv_malla_modi_id, sd_period, se_period)) %>% 
        spread(key=disturb, value=mean_period) %>% 
        mutate(rt = dr / pre,
               rc = post / dr,
               rs = post / pre,
               rrs = ((post - dr) / pre),
               iv_malla_modi_id = t) %>% 
        select(rt, rc, rs, rrs, disturb_year, iv_malla_modi_id) %>% as.data.frame() 
      
      aux_resilience <- rbind(aux_resilience, aux_df)
    }
    
    out_res <- rbind(out_res, aux_resilience)
  }
  
  out$evi <- out_evi
  out$resilience <- out_res
  
  return(out)
}

