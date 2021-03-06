#' almond_yield_profit
#'
#' Compute amount of almond yield based on climate data
#' @param df dataframe containing daily climate observations with columns "year", "month" (1-12), "tmin_c" (minimum temperature in ºC), and "precip" (precipitation in mm)
#' @param a numeric coefficient of mean temp min
#' @param b numeric coefficient of mean temp min squared
#' @param c numeric coefficient of precip sum
#' @param d numeric coefficient of precip sum squared
#' @param e numeric constant
#' @param price numeric ($/ton/acre)
#' @param discount_rate numeric discount on price ($/ton/acre)
#' @return list containing 1) yield profit ($/tons/acre) for each year and 2) mean of the net present value (NPV)


almond_yield_profit <- function(df, a=-0.015, b=-0.0046, c=-0.07, d=0.0043, e=0.28,
                         price=1000, discount_rate = 0.15) {
  
  years <- unique(df$year)
  n_years <- length(years)
  yields_df <- data.frame(year = years, yield = numeric(n_years),
                          net = numeric(n_years), npv = numeric(n_years))
  
  for (i in seq_along(years)) {
    
    year_i = years[i]
    
    if (length(df$tmin_c[df$year == year_i & df$month == 2]) != 0 &
        length(df$precip[df$year == year_i & df$month == 1]) != 0) 
    {
      
      min_temp_feb <- mean(df$tmin_c[df$year == year_i & df$month == 2])
      precip_jan <- sum(df$precip[df$year == year_i & df$month == 1])
      
      yield <- (a * min_temp_feb) + (b * min_temp_feb ^ 2) +
        (c * precip_jan) + (d * precip_jan ^ 2) + e
      
      net <- yield * price
      
      npv <- compute_NPV(value = net, time = year_i - years[1], discount_rate = discount_rate)
      
      yields_df[i, "yield"] <- yield
      
      yields_df[i, "net"] <- net
      
      yields_df[i, "npv"] <- npv
      
      
    } 
    
    # else 
    #   
    # {
    #   
    #   yields_df[i, "yield"] <- NA
    #   #warning(paste0("year '", year_i, "' lacks sufficient data to calculate yield anomaly"))
    #   
    # }
    
  } 
  
  mean_npv <- mean(yields_df$npv)
  
  return(list(yields_profit = yields_df, mean_npv = mean_npv))

}





