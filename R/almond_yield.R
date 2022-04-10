#' almond_yield
#'
#' Compute amount of almond yield based on climate data
#' @param year 
#' @param min_temp (degrees C)
#' @param precip (mm)
#' @param constant (default = 0.28)
#' @return yield anomaly (tons/acre)


almond_yield = function(clim, year, min_temp, precip, constant = 0.28) {
  
  #filter(year = year)
  #min_temp for month of february
  #precip for month of january 

  
  result = (-0.015*min_temp) - (0.0046*min_temp^2) 
  - (0.07*precip) + (0.0043*precip^2) + constant 
  
  return(result)
}







