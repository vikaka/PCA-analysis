rollingsales_manhattan <- read.csv("~/assignment 3/rollingsales_manhattan.csv",na.strings = "",)

str(rollingsales_manhattan)

# DATA Cleaning

#change date to date format
rollingsales_manhattan$SALE.DATE <- as.Date(rollingsales_manhattan$SALE.DATE, format = "%m/%d/%Y")

#change price to number format
rollingsales_manhattan$SALE.PRICE <- as.numeric(rollingsales_manhattan$SALE.PRICE)


#change address to string
rollingsales_manhattan$ADDRESS <- as.character(rollingsales_manhattan$ADDRESS)

#gross square feet to numeric
rollingsales_manhattan$GROSS.SQUARE.FEET <- as.numeric(rollingsales_manhattan$GROSS.SQUARE.FEET)

# tax class to factors
rollingsales_manhattan$TAX.CLASS.AT.TIME.OF.SALE <- as.factor(rollingsales_manhattan$TAX.CLASS.AT.TIME.OF.SALE)

# since we are using only data from the borough of manhattan we can remove the borough column from the data
rollingsales_manhattan <- rollingsales_manhattan[,-1]

# there are no values in Ease.Ment we can remove that column
summary(rollingsales_manhattan$EASE.MENT)
rollingsales_manhattan <- rollingsales_manhattan[,-6]

# Change land square feet to num
rollingsales_manhattan$LAND.SQUARE.FEET <- as.numeric(rollingsales_manhattan$LAND.SQUARE.FEET)

str(rollingsales_manhattan)

# replace blank cells in apartment number with NA values
str(rollingsales_manhattan$APARTMENT.NUMBER)
rollingsales_manhattan$APARTMENT.NUMBER <- data.frame(gsub("            ",NA,rollingsales_manhattan$APARTMENT.NUMBER, fixed = TRUE))
colnames(rollingsales_manhattan[,8]) <- "APARTMENT.NUMBER"
rollingsales_manhattan$APARTMENT.NUMBER <- as.character(rollingsales_manhattan$APARTMENT.NUMBER)

rollingsales_manhattan$LAND.SQUARE.FEET[rollingsales_manhattan$LAND.SQUARE.FEET == 0] <-NA

rollingsales_manhattan$GROSS.SQUARE.FEET[rollingsales_manhattan$GROSS.SQUARE.FEET == 0] <-NA

rollingsales_manhattan$YEAR.BUILT[rollingsales_manhattan$YEAR.BUILT == 0] <- NA

# we rplace total units with 0 value as NA and only replcae ) value in residential and commercial units corresponding to NA total unit values
rollingsales_manhattan$TOTAL.UNITS[rollingsales_manhattan$TOTAL.UNITS == 0] <- NA

rollingsales_manhattan$RESIDENTIAL.UNITS[rollingsales_manhattan$TOTAL.UNITS == 0] <- NA

rollingsales_manhattan$COMMERCIAL.UNITS[rollingsales_manhattan$TOTAL.UNITS == 0] <- NA
