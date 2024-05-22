library(tidyverse)
library(mlogit)
#im going to set profitability at 30$ 
almost.final$profit[almost.final$total_price_usd > 35] <- "1"
almost.final$profit[almost.final$total_price_usd <= 35] <- "0"

almost.final$Class[almost.final$total_price_usd <= 100] <- "1"
almost.final$Class[almost.final$total_price_usd > 100 & almost.final$total_price_usd <= 1000] <- "2"
almost.final$Class[almost.final$total_price_usd > 1000 & almost.final$total_price_usd <= 10000] <- "3"
almost.final$Class[almost.final$total_price_usd > 10000] <- "4"

almost.final$Class<-as.factor(almost.final$Class)
almost.final$profit<-as.numeric(almost.final$profit)

final.logit <- dfidx(data = almost.final, choice = 'Class', shape = 'wide')

##OPENSEA MODEL ------------------------------------------------
m.opensea <- mlogit(Class ~ 0 | asset.num_sales + quantity, reflevel = "1", data = final.logit)
summary(m.opensea)  #both n sales and quantity seem to have an effect on category 
#this is comparison to our reference level 
#if the total price were 0 most likely the number of sales would be between 30 and 40
#if its positive its more likely than 1 and viceversa
#if its positive it has a positive effect and if its negative it has a negative effect and significance 
#price and quantity both have a positive effect on numer of sales 
#negative it reduces the probability of 2 over 1
#in the summary we can also see if our model is performing significantly better than a null model with pvalue <0.05

#ODDS RATIO 
round(exp(coef(m.opensea)),4)
#if the odds on the intercept are above 1 it means the prob of choosing 1 are higher than nsales 2 
#88 times more likely that choose 1 over 2
#if you change quantity by 1 the odds of nsales 1 over 2 changes by a factor of 0.4619

#MARGINAL EFECTS  
effects(m.opensea, covariate = 'asset.num_sales')
#an increase in the number of sales by 100 can increse tha probability of class 2 or 3 
#point 
#the effect of total price is very small 
effects(m.opensea, covariate = 'quantity')#point 13 percetnage points 
#if quantity increases by 1 the probability of 2 increases by 0.066 percebtage points 

#quantity decreases the probability of selling 1 increases the probs of 2 forward

#validation of the models 
lrtest(null, m.opensea)#you can add models to this to validate models 
# in practice is not that usefull to use the log likelihood test so we wanna see predictive value 

#market share and hitrate - this makes no sense 
m.opensea$fitted = apply(fitted(m.opensea, type = "probabilities"), 2, mean)
rbind(m.opensea$fitted, m.opensea$freq/sum(m.opensea$freq))
#predicted and actual market share are the same so dta might be clusterable 

##HITRATE - this model is not a great predictor 
m.opensea$choice = apply(fitted(m.opensea, type = "probabilities"), 1 , which.max)
m.opensea$choice = factor(m.opensea$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, m.opensea$choice)
(534+166)/950#0.7368421

#TWITTER MODEL ---------------------------------------------------------------------------------------------------
m.twitter <- mlogit(Class ~0 | Likes + Replies + Followers + Favourites, reflevel = "1", data = final.logit)
summary(m.twitter) #nothing seems to be significant 

exp(coef(m.twitter))

#marginal effects 
effects(m.twitter, covariate = 'Likes')
effects(m.twitter, covariate = 'Replies')
effects(m.twitter, covariate = 'Followers')
effects(m.twitter, covariate = 'Favourites')


lrtest(m.twitter, m.opensea)#the model with opensea variables is significantly better than the model with twitter data 

#market share and hitrate - this makes no sense 
m.twitter$fitted = apply(fitted(m.twitter, type = "probabilities"), 2, mean)
rbind(m.twitter$fitted, m.twitter$freq/sum(m.twitter$freq))

##HITRATE - this model is not a great predictor 
m.twitter$choice = apply(fitted(m.twitter, type = "probabilities"), 1 , which.max)
m.twitter$choice = factor(m.twitter$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, m.twitter$choice)
(578+1)/950#0.6094737

#SENTIMENT MODEL -----------------------------------------------------------
m.sentiment <- mlogit(Class ~0 | Positive + Negative, reflevel = "1", data = final.logit)
summary(m.sentiment) #positive has some significance 

exp(coef(m.sentiment))

#marginal effects 
effects(m.sentiment, covariate = 'Positive')
effects(m.sentiment, covariate = 'Negative')

lrtest(m.twitter, m.opensea, m.sentiment)#this does not work, only for nested models 

##HITRATE - awful predictor 
m.sentiment$choice = apply(fitted(m.sentiment, type = "probabilities"), 1 , which.max)
m.sentiment$choice = factor(m.sentiment$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, m.sentiment$choice)
(578+1)/950#0.6094737 - probably does not perform better than a random model 

#FULL MODEL --------------------------------------------------------------------------
m.full <- mlogit(Class ~0 | asset.num_sales + quantity + Positive + Negative + Likes + Replies + Followers + Favourites, reflevel = "1", data = final.logit)
summary(m.full) #nothing seems to be significant 

exp(coef(m.full))

#marginal effects 
effects(m.full, covariate = 'total_price_usd')
effects(m.full, covariate = 'quantity')
effects(m.full, covariate = 'Positive')
effects(m.full, covariate = 'Negative')
effects(m.full, covariate = 'Likes')
effects(m.full, covariate = 'Replies')
effects(m.full, covariate = 'Followers')
effects(m.full, covariate = 'Favourites')

lrtest(m.twitter, m.opensea, m.sentiment, m.full)

##HITRATE - awful predictor 
m.full$choice = apply(fitted(m.full, type = "probabilities"), 1 , which.max)
m.full$choice = factor(m.full$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, m.full$choice)
(535+167)/950 #0.7389474

AIC(m.opensea,m.twitter,m.sentiment,m.full)
BIC(m.opensea,m.twitter,m.sentiment,m.full)

###IIA - Hausman McFadden

m.full.sub <- mlogit(Class ~ 0 | asset.num_sales + quantity,
                     reflevel = "1", alt.subset = c("1","2","3"), data = final.logit)

hmftest(m.opensea, m.full.sub)
#when leaving class 6 out, IIA holds therefore we have to do nested models 

