#' Almond Profit
#'
#' Compute amount of almond profit based on yield anomalies 
#' @param gen_almond_yield (ton/acre)
#' @param price ($/ton/acre)
#' @param year
#' @param discount_rate (default = 0.15)
#' @return data frame with estimate of profit anomalies 

compute_almond_profit = function(gen_almond_yield, price, year, discount_rate = 0.15) { 
  
  # make sure values are reasonable
  if (length(gen_almond_yield) < 1)
    return(NA)

  # generate a unique identifier 
  scenario = seq(from = 1, to = length(gen_almond_yield)) 
  yearprofit = data.frame(scenario = scenario, 
                          almond_yield = gen_almond_yield,
                          year = year)
  yearprofit$net = yearprofit$almond_yield * price
  
  yearprofit = yearprofit %>% 
    mutate(npv = compute_NPV(value = net, time = year-year[1], discount_rate = discount_rate))
  
  return(yearprofit)
}
