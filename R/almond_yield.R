#' almond_yield
#'
#' Compute amount of almond yield based on climate data
#' @param year 
#' @param min_temp (degrees C)
#' @param precip (mm)
#' @param constant (default = 0.28)
#' @return yield anomaly (tons/acre)


almond_yield = function(year, min_temp, precip, constant = 0.28) {
  
  result = -0.015(min_temp) - 0.0046(min_temp^2) 
  - 0.07(precip) + 0.0043(precip^2) + constant 
  
  return(result)
}

wheat_temperature = function(daily_temp, age, risk_threshold_med = 75, 
                             risk_threshold_high = 82, age_threshold = 8) {
  
  daily_temp = as.data.frame(daily_temp)
  
  risk = 1.8 * daily_temp + 0.5 * (age^2)  
  
  # only high or med if wheat age is greater than 8 months 
  if (any(age >= age_threshold)) {
    risk = case_when(risk < risk_threshold_med ~ "low",
                     risk >= risk_threshold_med &
                       risk < risk_threshold_high ~ "medium", 
                     risk >= risk_threshold_high ~ "high")  
  } else
    risk = "low" 









