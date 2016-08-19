#comparison

#we first check for the amount of information availble in each column

na.count <- data.frame(c(1:19))
colnames(na.count) <- "column_name"
for(i in 1:19)
{
  na.count$column_name[i] <- colnames(rollingsales_manhattan)[i]
  na.count$count_of_NA[i] <- sum(is.na(rollingsales_manhattan[,i]))  
  na.count$pct_of_missing_values[i] <- ((na.count$count_of_NA[i])/23757)*100
}

# from the table na.count we can see that 80% of the data is NA in land square feet and gross square feet
# 50% of the data is NA in Apartment number

neighbourhoods <- data.frame(neighbourhoods = unique(rollingsales_manhattan$NEIGHBORHOOD))


#median sale Price
for (i in 1:39)
{
  neighbourhoods$median_sale_price[i] <- median(filter(rollingsales_manhattan, rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods[i,1])[,18])
}

#median land square feet

# for (i in 1:39)
# {
#median number of commercial units

for (i in 1:39)
{
  neighbourhoods$median_Commercial_units[i] <- median(filter(rollingsales_manhattan, rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods[i,1])[,11],na.rm = TRUE)
}

#median number of residential units

for (i in 1:39)
{
  neighbourhoods$median_Residential_units[i] <- median(filter(rollingsales_manhattan, rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods[i,1])[,10])
}

#   neighbourhoods$median_Land_Square_feet[i] <- median(filter(rollingsales_manhattan, rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods[i,1])[,13])
# }

#median gross square feet

# for (i in 1:39)
# {
#   neighbourhoods$median_built_square_feet[i] <- median(filter(rollingsales_manhattan, rollingsales_manhattan$NEIGHBORHOOD == neighbourhoods[i,1])[,14])
# }

