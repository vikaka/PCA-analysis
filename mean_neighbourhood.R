
#calculating sd value accross numeric sdiables

neighbourhoods_sd <- data.frame(neighbourhoods = unique(rollingsales_manhattan$NEIGHBORHOOD))

for (i in 1:39)
{
  neighbourhoods_sd$sd_sale_price[i] <- sd(rollingsales_manhattan$SALE.PRICE[rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods_sd$neighbourhoods[i]],na.rm = TRUE)
  neighbourhoods_sd$sd_Commercial_units[i] <- sd(rollingsales_manhattan$COMMERCIAL.UNITS[rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods_sd$neighbourhoods[i]],na.rm = TRUE)
  neighbourhoods_sd$sd_Residential_units[i] <- sd(rollingsales_manhattan$RESIDENTIAL.UNITS[rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods_sd$neighbourhoods[i]],na.rm = TRUE)
  neighbourhoods_sd$sd_Land_Square_feet[i] <- sd(rollingsales_manhattan$LAND.SQUARE.FEET[rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods_sd$neighbourhoods[i]],na.rm = TRUE)
  neighbourhoods_sd$sd_built_square_feet[i] <- sd(rollingsales_manhattan$GROSS.SQUARE.FEET[rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods_sd$neighbourhoods[i]],na.rm = TRUE)
  
}

    
dummy_categorical <- data.frame(NEIGHBORHOOD = c(1:23757))

for (i in 1:length(rollingsales_manhattan$NEIGHBORHOOD))
{ 
  if(rollingsales_manhattan$NEIGHBORHOOD[i] == "JAVITS CENTER")
   { dummy_categorical$NEIGHBORHOOD[i] <- 1}
  else {dummy_categorical$NEIGHBORHOOD <- 0}
  }
  
dummy_categorical$NEIGHBORHOOD <- as.factor(dummy_categorical$NEIGHBORHOOD)
