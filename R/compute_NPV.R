#' compute_NPV 
#' 
#' compute net present value
#' @param value numeric ($)
#' @param time numeric in the future that cost/value occurs (years)
#' @param discount_rate numeric discount on price ($/ton/acre)
#' @return value in $


compute_NPV = function(value, time, discount_rate = 0.15) {
  result = value / ((1 + discount_rate)^time)
  return(result)
}

