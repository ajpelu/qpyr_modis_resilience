## Functions to get composite of MODIS images. 
## Based on recomendations of Testa et al. 2014
## European Journal of Remote Sensing 47:285-305 
## DOI: 10.5721/EuJRS20144718

## --- 
# Two functions were created to get the composite (1 to 23) of the MODIS image
# m and d are the month and day (get them using lubridate package)
#
# To get the leap and non-leap years I used lubridate::leap_year
## --- 

## I know this an ugly solution, but it works. Any improvement is welcome!! 

getComposite_nonleap <- function(m,d){
  # m = month 
  # d = day 
  ifelse(m == 1 & d < 17, 1,
  ifelse(m == 1 & d >= 17, 2,
  ifelse(m == 2 & d < 2, 2,      
  ifelse(m == 2 & d < 18, 3, 
  ifelse(m == 2 & d >= 18, 4,
  ifelse(m == 3 & d < 6, 4, 
  ifelse(m == 3 & d < 22, 5, 
  ifelse(m == 3 & d >= 22, 6,
  ifelse(m == 4 & d < 7, 6,        
  ifelse(m == 4 & d < 23, 7, 
  ifelse(m == 4 & d >= 23, 8,
  ifelse(m == 5 & d < 9, 8,       
  ifelse(m == 5 & d < 25, 9, 
  ifelse(m == 5 & d >= 25, 10,
  ifelse(m == 6 & d < 10, 10,
  ifelse(m == 6 & d < 26, 11, 
  ifelse(m == 6 & d >= 26, 12,
  ifelse(m == 7 & d < 12, 12,       
  ifelse(m == 7 & d < 28, 13, 
  ifelse(m == 7 & d >= 28, 14,
  ifelse(m == 8 & d < 13, 14,
  ifelse(m == 8 & d < 29, 15, 
  ifelse(m == 8 & d >= 29, 16,
  ifelse(m == 9 & d < 14, 16,
  ifelse(m == 9 & d < 30, 17, 
  ifelse(m == 9 & d >= 30, 18,
  ifelse(m == 10 & d < 16, 18,
  ifelse(m == 10 & d >= 16, 19,
  ifelse(m == 11 & d < 17, 20,
  ifelse(m == 11 & d >= 17, 21,
  ifelse(m == 12 & d < 3, 21,
  ifelse(m == 12 & d < 19, 22, 23))))))))))))))))))))))))))))))))
}


getComposite_leap <- function(m,d){
  # m = month 
  # d = day 
  ifelse(m == 1 & d < 17, 1,
  ifelse(m == 1 & d >= 17, 2,
  ifelse(m == 2 & d < 2, 2,      
  ifelse(m == 2 & d < 18, 3, 
  ifelse(m == 2 & d >= 18, 4,
  ifelse(m == 3 & d < 5, 4, 
  ifelse(m == 3 & d < 21, 5, 
  ifelse(m == 3 & d >= 21, 6,
  ifelse(m == 4 & d < 6, 6,        
  ifelse(m == 4 & d < 22, 7, 
  ifelse(m == 4 & d >= 22, 8,
  ifelse(m == 5 & d < 8, 8,       
  ifelse(m == 5 & d < 24, 9, 
  ifelse(m == 5 & d >= 24, 10,
  ifelse(m == 6 & d < 9, 10,
  ifelse(m == 6 & d < 25, 11, 
  ifelse(m == 6 & d >= 25, 12,
  ifelse(m == 7 & d < 11, 12,       
  ifelse(m == 7 & d < 27, 13, 
  ifelse(m == 7 & d >= 27, 14,
  ifelse(m == 8 & d < 12, 14,
  ifelse(m == 8 & d < 28, 15, 
  ifelse(m == 8 & d >= 28, 16,
  ifelse(m == 9 & d < 13, 16,
  ifelse(m == 9 & d < 29, 17, 
  ifelse(m == 9 & d >= 29, 18,
  ifelse(m == 10 & d < 15, 18,
  ifelse(m == 10 & d < 31, 19,     
  ifelse(m == 10 & d >= 31, 20,
  ifelse(m == 11 & d < 16, 20,
  ifelse(m == 11 & d >= 16, 21,
  ifelse(m == 12 & d < 2, 21,
  ifelse(m == 12 & d < 18, 22, 23)))))))))))))))))))))))))))))))))
}