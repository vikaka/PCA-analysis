# linear regression

lm_train <- lm(SALE.PRICE ~ NEIGHBORHOOD+BUILDING.CLASS.CATEGORY+RESIDENTIAL.UNITS+COMMERCIAL.UNITS+TAX.CLASS.AT.TIME.OF.SALE+BUILDING.CLASS.AT.TIME.OF.SALE, data = rollingsales_manhattan,na.action = na.omit)

neighbourhoods sale class type
built year zipcode

NEIGHBORHOOD+BUILDING.CLASS.CATEGORY+TAX.CLASS.AT.PRESENT+BLOCK+LOT+BUILDING.CLASS.AT.PRESENT+ADDRESS+APARTMENT.NUMBER+ZIP.CODE+RESIDENTIAL.UNITS+COMMERCIAL.UNITS+TOTAL.UNITS+LAND.SQUARE.FEET+GROSS.SQUARE.FEET+YEAR.BUILT+TAX.CLASS.AT.TIME.OF.SALE+BUILDING.CLASS.AT.TIME.OF.SALE+SALE.DATE

lin_reg_1 <- lm(SALE.PRICE ~ NEIGHBORHOOD+BUILDING.CLASS.CATEGORY+TAX.CLASS.AT.PRESENT+BLOCK+LOT+BUILDING.CLASS.AT.PRESENT+ZIP.CODE+RESIDENTIAL.UNITS+COMMERCIAL.UNITS+TOTAL.UNITS+YEAR.BUILT+TAX.CLASS.AT.TIME.OF.SALE+BUILDING.CLASS.AT.TIME.OF.SALE, data = rollingsales_manhattan)


rollingsales_manhattan_csv <- read.csv("~/assignment 3/rollingsales_manhattan_csv.csv", header=TRUE, skip = 4)

lm_train <- lm(rollingsales_manhattan$SALE.PRICE~ dummy_categorical$NEIGHBORHOOD)




library("class")
library("dplyr")
library("knnflex")



  
train_manhattan <- sample_frac(rollingsales_manhattan,0.75)
test_manhattan <- rollingsales_manhattan[-(as.numeric(row.names(train_manhattan))),]

train_manhattan <- train_manhattan[which(train_manhattan$GROSS.SQUARE.FEET > 0 & train_manhattan$LAND.SQUARE.FEET >0),]

test_manhattan <- test_manhattan[which(test_manhattan$GROSS.SQUARE.FEET > 0 & test_manhattan$LAND.SQUARE.FEET >0),]

response_y <- data.frame(train_manhattan$NEIGHBORHOOD)

train_manhattan<- train_manhattan[,-1]
test_response <- data.frame(test_manhattan$NEIGHBORHOOD)
test_manhattan <- test_manhattan[,-1]




knn_predict <- data.frame(knn(train_manhattan[,c(8,13,12,14)],test_manhattan[,c(8,13,12,14)],response_y$train_manhattan.NEIGHBORHOOD,k=10))

a <-confusionMatrix(knn_predict$knn.train_manhattan...c.8..13..12..14....test_manhattan...,test_response$test_manhattan.NEIGHBORHOOD)
a$overall


#knnfit
model_lm <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET+LAND.SQUARE.FEET+ factor(NEIGHBORHOOD)*factor(BUILDING.CLASS.CATEGORY), data = rollingsales_manhattan)

plot(resid(model_lm))


Sales_manhattan <- rollingsales_manhattan[which(rollingsales_manhattan$GROSS.SQUARE.FEET > 0 & rollingsales_manhattan$LAND.SQUARE.FEET >0&rollingsales_manhattan$SALE.PRICE > 0),]


Sales_manhattan_1 <- Sales_manhattan[,c(1,9,14,13,15)]
     
ctrl <- trainControl(method="repeatedcv",repeats = 10)
fit <- train(NEIGHBORHOOD~ ., data = Sales_manhattan_1,trControl = ctrl, method = "knn")                                                                 
plot(fit)


scaled_sales <- scale(Sales_manhattan[,c(13,14,18)], scale = TRUE)

PC = prcomp(~GROSS.SQUARE.FEET+LAND.SQUARE.FEET+SALE.PRICE,data=scaled_sales,na.action = na.omit)
str(PC)
summary(PC)
predict(PC)
plot(PC,type="l")

scr=as.matrix(scaled_sales[,c(1:3)])%*%PC$rot[,1:2]
plot(scr)

scr_1=as.matrix(scaled_sales[,c(1:3)])%*%PC$rot[,c(1,3)]
plot(scr_1)

biplot(PC)
