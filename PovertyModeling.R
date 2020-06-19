install.packages("purr")
install.packages("readxl")
install.packages("tidyverse")
library(tibble)
library(purrr)
library(readxl)
library(tidyverse)

##---- import files and save as a list of tibbles
file <- 'DataDownload.xlsx'
sheets <- excel_sheets(file)
list_df <- lapply(sheets, function(x) read_excel(path = file, sheet = x))
View(list_df)
class(list_df)

##---- combine tibble 5-13 (Excel sheets 5-13) as a dataframe, on the field "FIPS", "State" and "County
data<-as.data.frame(list_df[5:13] %>% reduce(left_join, by = c("FIPS","State","County")))

head(data)

data_33 <- data[ , c('FIPS', 'State', 'County', 'POPLOSS10','POVRATE15','PCT_18YOUNGER10','PCT_65OLDER10','PCT_NHPI10','PCT_NHNA10','PCT_NHASIAN10','PCT_HISP10','PCT_NHBLACK10','PCT_NHWHITE10','RECFACPTH14','PCT_HSPA15','PCT_OBESE_ADULTS13','FARM_TO_SCHOOL13','GHVEG_SQFTPTH12','BERRY_ACRESPTH12','ORCHARD_ACRESPTH12','FRESHVEG_ACRESPTH12','VEG_ACRESPTH12','PCT_FMRKT_OTHERFOOD16','PCT_FMRKT_BAKED16','PCT_FMRKT_ANMLPROD16','PCT_FMRKT_FRVEG16','FMRKTPTH16','PC_DIRSALES12','PCT_LOCLSALE12','PCT_LOCLFARM12','FOOD_TAX14','SODATAX_VENDM14','SODATAX_STORES14','MILK_SODA_PRICE10','SODA_PRICE10','MILK_PRICE10')]

ncol(data_33)
nrow(data_33)

## check number of N/A in each variable
na_count <-sapply(data_33, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

not_use <- c('PCT_HSPA15', 'GHVEG_SQFTPTH12', 'BERRY_ACRESPTH12', 'ORCHARD_ACRESPTH12', 'FRESHVEG_ACRESPTH12', 'VEG_ACRESPTH12', 'PCT_FMRKT_OTHERFOOD16', 'PCT_FMRKT_BAKED16', 'PCT_FMRKT_ANMLPROD16', 'PCT_FMRKT_FRVEG16')

data_23 <- data_33[ , !(names(data_33) %in% not_use)]
ncol(data_23)
nrow(data_23)
names(data_23)


na_count <-sapply(data_23, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

overallavgs <- colMeans(data_23[,4:ncol(data_23)],na.rm=TRUE)

# 1)  replace missing values w/ state average
for (i in 4:ncol(data_23)) {
  for (j in which(is.na(data_23[, i]))) {
    data_23[j, i] <- mean(data_23[data_23[, "State"] == data_23[j, "State"], i],  na.rm = TRUE)
  }
}

#check work
colMeans(is.na(data_23)) #look at remaining missing data


# 2) replace remaining missing values with overall average
for (i in 4:ncol(data_23)) {
  for (j in which(is.na(data_23[, i]))) {
    data_23[j, i] <- overallavgs[[i]]
  }
}

sum(colMeans(is.na(data_23))>0) #no more missing data

## Modeling

library(caTools)

train <- sample.split(data_23$POVRATE15, SplitRatio = 0.6)
data_23.test <- data_23[!train, ]

library(tree)
data_tree3 <- tree(POVRATE15 ~ .-FIPS - State - County, data = data_23, subset = train)
plot(data_tree3)
text(data_tree3, pretty = 0)

# 10-folds cross validation tree.train <-
tree.train2 <- tree(POVRATE15 ~ .-FIPS - State - County, data = data_23, subset = train, 
                   control = tree.control(nobs = nrow(data_23), mindev = 0.005))
plot(tree.train2)
text(tree.train2, pretty = 0)
cv.data2 <- cv.tree(tree.train2, FUN = prune.tree, K = 5)
# tree size (number of terminal nodes) vs deviance (a measure of fit)
plot(cv.data2$size, cv.data2$dev)


prune.data2 <- prune.tree(tree.train2, best = 11)
prune.data2
plot(prune.data2)
text(prune.data2, pretty = 0)

prune.data3 <- prune.tree(tree.train2, best = 8)
prune.data3
plot(prune.data3)
text(prune.data3, pretty = 0)

data_23.test$pred <- NULL
mean((data_23.test$POVRATE15 - predict(tree.train2, newdata = data_23.test))^2)
## [1] 19.49857

hist(data_23$POVRATE15) ## more or less bell curve

## random forest importance
install.packages("randomForest")
library(randomForest)

lm_data <- data_23


lm_data$FIPS <- NULL
lm_data$State <- NULL
lm_data$County <- NULL

train <- sample.split(lm_data$POVRATE15, SplitRatio = 0.6)
lm_data.train <- lm_data[train, ]
lm_data.test <- lm_data[!train, ]

data23_forest <- randomForest(POVRATE15 ~ . , data = lm_data, subset = train, mtry = 9, importance=T)
summary(data23_forest)
importance(data23_forest)


## lm

str(lm_data)

lm_train = lm(POVRATE15 ~ . , data = lm_data, subset = train)
summary(lm_train)
## not great Multiple R-squared:  0.5462,	Adjusted R-squared:  0.5415  
mean((lm_data.test$POVRATE15 - predict(lm_train, lm_data.test))^2)
# [1] 17.02343 - better than tree

## neuralnet

library(neuralnet)
n_data <- lm_data
n_data$POPLOSS10 <- NULL

train <- sample.split(n_data$POVRATE15, SplitRatio = 0.6)
n_data.train <- n_data[train, ]
n_data.test <- n_data[!train, ]

sc_data.train <- scale(n_data.train)
sc_data.test <- scale(n_data.test)

data.net <- neuralnet(POVRATE15 ~ . , data = lm_data.train, hidden = 3, threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net, lm_data.test)
mean((lm_data.test$POVRATE15 - pred)^2)
## [1] no scale 34.68847

data.net1 <- neuralnet(POVRATE15 ~ . , data = lm_data.train, hidden = c(2,2), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net1, lm_data.test)
mean((lm_data.test$POVRATE15 - pred)^2)
## [1] no scale 35.04859

data.net2 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = 5, threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net2, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
##[1] [1] 19.02321
## hid = 5, [1] 15.45141

maxN <- 3
vMSE <- rep(0, maxN)
for (i in 1:maxN) {
  data.net <- neuralnet(POVRATE15 ~ . , data = lm_data.train, hidden = i, threshold = 0.5, stepmax = 1e8)
  pred <- predict(data.net, lm_data.test)
  vMSE[i] <- mean((lm_data.test$POVRATE15 - pred)^2)
}
plot(vMSE)


maxN2 <- 5
vMSE2 <- rep(0, maxN2)
for (i in 1:maxN2) {
  data.net <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = i, threshold = 1.5, stepmax = 1e6)
  pred <- predict(data.net2, sc_data.test)
  vMSE2[i] <- mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
}
plot(vMSE2)
vMSE2

## 2 hidden layers
data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(1,1), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 17.13344

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(2,2), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 15.68508

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(3,3), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 14.67464

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(4,4), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 15.49051

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(5,5), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 16.46956

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(6,6), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 16.09736

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(7,7), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 17.45441

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(3,3,3), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(5,5,5), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)


data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(1,1,1), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)

data.net3 <- neuralnet(POVRATE15 ~ . , data = sc_data.train, hidden = c(4,4,4), threshold = 1.5, stepmax = 1e6)
pred <- predict(data.net3, sc_data.test)
mean((n_data.test$POVRATE15 - mean(n_data.test$POVRATE15) - sd(n_data.test$POVRATE15) * pred)^2)
## 3 hidden layers work worse