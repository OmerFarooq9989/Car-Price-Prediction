  ```{r loadPackages, warning=FALSE, message=FALSE, results='hide'}

if(!require("pacman")) install.packages("pacman")
pacman::p_load(caret,forecast, tidyverse, gplots, GGally, mosaic,scales, mosaic, mapproj, mlbench, data.table, leaps, MASS)
search()
theme_set(theme_classic())
library(data.table)
cars <- read.csv("C:\\Users\\omerf\\Desktop\\R\\carss.csv")
cars.dt <- data.table(cars)
car.dt <- cars.dt[,-c(1,9,10,12,13)]
car.dt

#analysis on numerical variables of the dataset
col_names <- c("price","mileage")
maximum <- cars.dt[,lapply(.SD,max), , .SDcols = col_names]
minimum <- cars.dt[,lapply(.SD,min), , .SDcols = col_names]
mean <- cars.dt[,lapply(.SD,mean), , .SDcols = col_names]
median <- cars.dt[,lapply(.SD,median), , .SDcols = col_names]
sd <- cars.dt[,lapply(.SD,sd), , .SDcols = col_names]

resulting = rbind(maximum,minimum,mean,median,sd)
resulting = cbind(c("MAX","MIN","MEAN","MEDIAN","SD"),resulting)
resulting

box_mileage <- ggplot(cars.dt) + geom_boxplot(aes(,y = mileage),fill = "blue",outlier.color="red")
box_price <- ggplot(cars.dt) + geom_boxplot(aes(,y = price),fill = "blue",outlier.color="red")
box_price
box_mileage

Q <- quantile(cars.dt$price, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(cars.dt$price)
eliminated<- subset(cars.dt, cars.dt$price > (Q[1] - 1.5*iqr) & cars.dt$price < (Q[2]+1.5*iqr))
count(eliminated)

cars <- eliminated
cars.dt <- eliminated

###analysis on classification variables
counts_title <- table(cars$title_status)
barplot(counts_title, main="Car Distribution",xlab="title")

counts_model <- table(cars$model)
barplot(counts_model, main="Car Distribution",xlab="model")

counts_year <- table(cars$year)
barplot(counts_year, main="Car Distribution",xlab="year")

counts_brand <- table(cars$brand)
barplot(counts_brand, main="Car Distribution", xlab="brand")

counts_color <- table(cars$color)
barplot(counts_color, main="Car", xlab="color")

counts_state <- table(cars$state)
barplot(counts_state, main="Car", xlab="state")

counts_mileage <- table(cars$mileage)
barplot(counts_mileage, main="Car", xlab="mileage")


car <- cars[,-c(1,9,10,12,13)]
car1 <- car

car.dt$color <- as.numeric(car.dt$color)
car.dt$state <- as.numeric(car.dt$state)
car.dt$year <- as.numeric(car.dt$year)
car.dt$brand <- as.numeric(car.dt$brand)
car.dt$title_status <- as.numeric(car.dt$title_status)
car.dt$model <- as.numeric(car.dt$model)

car$color <- as.numeric(car$color)
car$state <- as.numeric(car$state)
car$year <- as.numeric(car$year)
car$brand <- as.numeric(car$brand)
car$title_status <- as.numeric(car$title_status)
car$model <- as.numeric(car$model)

brand_frequency <- prop.table(table(cars$brand))
model_frequency <- prop.table(table(cars$model))
year_frequency <- prop.table(table(cars$year))
color_frequency <- prop.table(table(cars$color))
state_frequency <- prop.table(table(cars$state))
title_frequency <-prop.table(table(cars$title_status))

brand_meanprice <- cars %>% group_by(brand) %>% summarise(mean(price))
model_meanprice <- cars %>% group_by(model) %>% summarise(mean(price))
year_meanprice <- cars %>% group_by(year) %>% summarise(mean(price))
color_meanprice <- cars %>% group_by(color) %>% summarise(mean(price))
state_meanprice <- cars %>% group_by(state) %>% summarise(mean(price))
title_meanprice <- cars %>% group_by(title_status) %>% summarise(mean(price))

title_pivottable <- cbind(title_frequency,title_meanprice)
title_pivottable['Freq'] <- title_pivottable['Freq']*100
title_pivottable$Var1<- NULL
title_pivottable <- title_pivottable[c("title_status","Freq","mean(price)")]
names(title_pivottable)[2] <- "Percent Frequency"
title_pivottable

brand_pivottable <- cbind(brand_frequency,brand_meanprice)
brand_pivottable['Freq'] <- brand_pivottable['Freq']*100
brand_pivottable$Var1<- NULL
brand_pivottable <- brand_pivottable[c("brand","Freq","mean(price)")]
names(brand_pivottable)[2] <- "Percent Frequency"
brand_pivottable

model_pivottable <- cbind(model_frequency,model_meanprice)
model_pivottable['Freq'] <- model_pivottable['Freq']*100
model_pivottable$Var1<- NULL
model_pivottable <- model_pivottable[c("model","Freq","mean(price)")]
names(model_pivottable)[2] <- "Percent Frequency"
model_pivottable

year_pivottable <- cbind(year_frequency,year_meanprice)
year_pivottable['Freq'] <- year_pivottable['Freq']*100
year_pivottable$Var1<- NULL
year_pivottable <- year_pivottable[c("year","Freq","mean(price)")]
names(year_pivottable)[2] <- "Percent Frequency"
year_pivottable

color_pivottable <- cbind(color_frequency,color_meanprice)
color_pivottable['Freq'] <- color_pivottable['Freq']*100
color_pivottable$Var1<- NULL
color_pivottable <- color_pivottable[c("color","Freq","mean(price)")]
names(color_pivottable)[2] <- "Percent Frequency"
color_pivottable

state_pivottable <- cbind(state_frequency,state_meanprice)
state_pivottable['Freq'] <- state_pivottable['Freq']*100
state_pivottable$Var1<- NULL
state_pivottable <- state_pivottable[c("state","Freq","mean(price)")]
names(state_pivottable)[2] <- "Percent Frequency"
state_pivottable


cor.mat <- round(cor(car.dt),2) # rounded correlation matrix
cor.mat

heatmap(cor(car.dt[,c("price","title_status","brand","year","model","mileage","color","state")]), Rowv = NA, Colv = NA)


cars.df.plot <- car1
cars.df.plot
round(cor(cars.df.plot),2)
par(mfrow = c(3,3))
for(a in 2:8) {
  plot(cars.df.plot[[a]], cars.df.plot[[1]], main = colnames(cars.df.plot)[[a]], xlab= colnames(cars.df.plot)[[a]], ylab = "price")
}

head(car)
car2 <- car[]
carsss <- car[,-c(7,8)]

head(carsss)
##start linear regression
set.seed(42)
sample_size <- round(0.8 * nrow(carsss))
cars_train <- sample(seq_len(nrow(carsss)), size = sample_size)

train_1 <- car[cars_train,]
train <- carsss[cars_train,]
test <- carsss[-cars_train,]
test_1 <- car[cars_train]

cars.lm.stepwise <- regsubsets(price ~ ., data = train, nbest = 1,nvmax = dim(train)[2],method = "seqrep")
set.seed(42)
sample1_size <- round(0.8 * nrow(carsss))
cars1_train <- sample(seq_len(nrow(carsss)), size = sample_size)

sum1 <- summary(cars.lm.stepwise)
sum1$which
sum1$rsq
sum1$adjr2
sum1$cp

cars1.lm.stepwise <- regsubsets(price ~ ., data = train_1, nbest = 1,nvmax = dim(train)[2],method = "seqrep")
set.seed(42)

sum12 <- summary(cars1.lm.stepwise)
sum12$which
sum12$rsq
sum12$adjr2
sum12$cp



cars.lm <- lm(price ~ mileage + brand + year + title_status+model, data=train)

cars.lm.stepwise.pred <- predict(cars.lm, test)
cars.lm.stepwise.pred
accuracy(cars.lm.stepwise.pred,test$price)

#AIC

cars.lm.bselect <- stepAIC (lm(price ~ ., data = train), direction = "both")
#step_aic func is better
summary(cars.lm.bselect)  # Which variables were dropped?

cars1.lm.bselect <- stepAIC (lm(price ~ ., data = train_1), direction = "both")
#step_aic func is better
summary(cars1.lm.bselect)  # Which variables were dropped?

set.seed(123)
nn2 <- train(price ~ ., data = train, method = "knn",trControl = trainControl("cv", number = 10), preProcess = c("center","scale"),tuneLength = 10)

# Plot model error RMSE vs different values of k
plot(nn2)

nn2$bestTune

predictions <- predict(nn2, test)
RMSE(predictions, test$price)

