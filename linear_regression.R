library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(Hmisc)
library(corrplot)
library(MASS)
almost.final <- read.csv("/Users/alejandrovillanuevalledo/Documents/Groningen/RUG/Thesis/R/almost_final_df_2.csv")

str(almost.final)
summary(almost.final$total_price_usd)
##HISTOGRAMS 
hist(almost.final$total_price_usd)
hist(log(almost.final$total_price_usd))#kkinda normal distribution 
hist(almost.final$quantity)
hist(almost.final$asset.num_sales)
hist(almost.final$Negative)
hist(almost.final$Neutral)
hist(almost.final$Positive)
hist(almost.final$Retweets)
hist(almost.final$Replies)
hist(almost.final$Likes)
hist(almost.final$Followers)
hist(almost.final$Favourites)

hist(log(almost.final$quantity))#not normal - check outliers 
hist(log(almost.final$asset.num_sales))#not normal - check outliers
hist(log(almost.final$Negative))#not normal - check outliers
hist(log(almost.final$Neutral))#not normal - check outliers
hist(log(almost.final$Positive))#not normal - check outliers
hist(log(almost.final$Retweets)) #normal
hist(log(almost.final$Replies))
hist(log(almost.final$Likes))
hist(log(almost.final$Followers))
hist(log(almost.final$Favourites))# not normal - check outliers 

##OUTLIERS 
boxplot(almost.final$total_price_usd)
boxplot(almost.final$quantity)
boxplot(log(almost.final$quantity))#when log only one outlier, get rid of it? 
boxplot(almost.final$asset.num_sales)
boxplot(log(almost.final$asset.num_sales))#too many outliers 

##NEGATIVE TWEETS 
boxplot(almost.final$Negative)
boxplot(log(almost.final$Negative))#when log no outliers, still not normal 
boxplot.stats(almost.final$Negative)$out
summary(almost.final$Negative)#this because most tweets are not negative, but theres few with very negative feelings 
##NEUTRAL TWEETS
boxplot(almost.final$Neutral)
boxplot(log(almost.final$Neutral))#small number of negative cause most tweets are informing 
boxplot.stats(almost.final$Negative)$out#43 outliers

##POSITIVE TWEETS
boxplot(almost.final$Positive)
boxplot.stats(almost.final$Positive)$out

plot(almost.final$asset.num_sales, almost.final$Retweets)
plot(log(almost.final$asset.num_sales), log(almost.final$Retweets))

plot(almost.final$Followers, almost.final$Retweets)#there is one big outlier
plot(log(almost.final$Followers), log(almost.final$Retweets))#there is one big outlier. but looks good 


boxplot(almost.final$Replies)

boxplot(almost.final$Likes)
boxplot(almost.final$Followers)
boxplot(log(almost.final$Followers))
plot(log(almost.final$Followers), log(almost.final$Retweets) )
plot(log(almost.final$Followers), log(almost.final$total_price_usd) )
boxplot(almost.final$Favourites)
boxplot(log(almost.final$Favourites))
boxplot.stats(log(almost.final$Favourites))$out#19

##CORRELATION - 0.3 onwards moderate correlation, larger than 0.5 strong correlation 
cor(almost.final)
rcorr(almost.final$total_price_usd, almost.final$Negative)
rcorr(as.matrix(almost.final), type = c("pearson","spearman"))# p-values below 0.05 indicate correlation coefficients are significant 

corrplot(cor(almost.final))#this is so sad 

#OPENSEA MODEL  
almost.final$ln.quantity <- log(almost.final$quantity +1)
almost.final$ln.num_sales<- log(almost.final$asset.num_sales+1)
almost.final$ln.total_price <- log(almost.final$total_price_usd+1)

plot(ln.total_price ~ ln.quantity, almost.final)

plot(total_price_usd ~ quantity, data = almost.final)
plot(log(total_price_usd) ~ log(asset.num_sales), data = almost.final)
plot(log(total_price_usd) ~ log(quantity), data = almost.final)
plot(log(total_price_usd) ~ log(asset.num_sales), data = almost.final)

summary(lm(ln.total_price ~ ln.quantity + ln.num_sales, almost.final))#this cannot be real 
#they both seem significant, r2 0.34 (good of model explaining y variation), significant f-statistic 

sapply(almost.final, function(x) sum(is.infinite(x)))# no inf values 
m1<-lm(ln.total_price ~ ln.quantity + ln.num_sales, almost.final)
summary(m1)
AIC(m1)#3453.465
BIC(m1)#3472.891


summary(lm(total_price_usd ~ quantity + asset.num_sales, almost.final))

bc <- boxcox(total_price_usd ~ quantity, data = almost.final)#i get an error 
(lambda <- bc$x[which.max(bc$y)])

#TWITTER MODEL 
plot(total_price_usd ~ Retweets, almost.final)
plot(total_price_usd ~ Replies, almost.final)
plot(total_price_usd ~ Likes, almost.final)
plot(total_price_usd ~ Followers, almost.final)
plot(total_price_usd ~ Favourites, almost.final)
#logs
plot(log(total_price_usd) ~ log(Retweets), almost.final)#not looking linear
plot(log(total_price_usd) ~ log(Replies), almost.final)
plot(log(total_price_usd) ~ log(Likes), almost.final)
plot(log(total_price_usd) ~ log(Followers), almost.final)#kinda linear
plot(log(total_price_usd) ~ log(Favourites), almost.final)

m2<-lm(log(total_price_usd+1) ~ log(Followers+1), almost.final)
summary(m2)#low r2, marginally significant 
AIC(m2)#3840.144
BIC(m2)#3854.713

#SENTIMENT MODEL 
plot(total_price_usd ~ Negative, almost.final)
plot(total_price_usd ~ Neutral, almost.final)
plot(total_price_usd ~ Positive, almost.final)

plot(log(total_price_usd) ~ log(Negative), almost.final)
plot(log(total_price_usd) ~ log(Neutral), almost.final)
plot(log(total_price_usd) ~ log(Positive), almost.final)

m3 <- lm(log(total_price_usd+1) ~ log(Negative+1), almost.final)
summary(m3)

#FULL MODEL 
m4 <- lm(log(total_price_usd +1) ~ ln.quantity + ln.num_sales +  log(Followers+1) + log(Negative+1), almost.final )
summary(m4)#r2 just a bit better than m1, still significant
AIC(m4)#3452.506
BIC(m4)#3481.645

#trial model - dont think this means anything 
m5 <- lm(ln.total_price ~ ln.quantity + ln.num_sales + log(Negative+1) + log(Neutral+1)+ log(Positive+1) + log(Retweets+1)+
           log(Replies+1) + log(Likes+1) + log(Followers+1) + log(Favourites+1), almost.final)
summary(m5)

m6 <- lm(asset.num_sales ~ . - total_price - Neutral - Likes, almost.final)
summary(m6)
vif(m6)
#I can only see multicollinearity in neutral and positive

##PREDICTING - if it happens 
## Create a subset 

#Get a 75% estimation sample and 25% validation sample
set.seed(1234)
almost.final$estimation.sample <-rbinom(nrow(almost.final), 1, 0.75)

#Create a new dataframe with only the validation sample
validation.data <- almost.final[almost.final$estimation.sample==0,]
