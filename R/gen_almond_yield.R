#' Almond Yield
#'
#' Compute amount of almond yield based on climate data
#' @param temp average minimum temperature in February (degrees C)
#' @param precip sum of precipitation in January (mm)
#' @param a (default = 0.015)
#' @param b (default = 0.0046)
#' @param c (default = 0.07)
#' @param d (default = 0.0043)
#' @param e (default = 0.28)
#' @param year 
#' @return almond_yield (ton/acre)


gen_almond_yield = function(temp, precip, year, a = 0.015, b = 0.0046, c = 0.07, d = 0.0043, e = 0.28){
  
  for (i in 1:length(year)) {
    gen_almond_yield = (-a*temp) - (b*(temp^2)) - (c*precip) + (d*(precip^2)) + e
  }
  
  gen_almond_yield <- as.data.frame(gen_almond_yield) %>% 
    mutate(year = year)
  
  return(gen_almond_yield)
}

