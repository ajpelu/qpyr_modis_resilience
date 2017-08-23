## Function to export raster of levelplot (rasterVis) as pdf

exportpdf <- function(mypdf, myplot){
  trellis.device(pdf, file=mypdf,
                 theme = list(fontsize = list(text = 10, points = 6)),
                 height=8, width=15) 
  print(myplot)
  dev.off()
} 