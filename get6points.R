library(changepoint)
get6points = function(data, y, top_3_peaks_indices){
  start = 1
  
  ends = c(top_3_peaks_indices[1], rep(top_3_peaks_indices[2:3], each = 2), nrow(data))
  
  point = 0
  
  mypoints = list()
  for (i in 1:6) {
    end = ends[i]
    subdata = data[start:end, y]
    seg.test = cpt.mean(subdata)
    
    if (length(cpts(seg.test)) == 0) {
      point = end
    } 
    
    else {
      point = cpts(seg.test) + point
    }
    
    mypoints[[i]] = point
    
    start = point
    
    
    #plot(seg.test)
    #text(cpts(seg.test), 1, point, font = 2)
    
  }
  
  mypoints
}
