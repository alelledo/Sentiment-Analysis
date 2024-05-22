library(VGAM)
library(foreign)
library(haven)
library(MASS)
library(pscl)
library(AER)
library(vcd)
mean(almost.final$asset.num_sales)#121.6229
var(almost.final$asset.num_sales)#794300.1
almost.final$asset.num_sales <- as.integer(almost.final$asset.num_sales)
#does non binomial need to follow linear relations or assumptions of normality or heteroskedasticity 

#should I still create different models with my different variables from twitter, opensea and sentiment analysis 
poissonmodel <- glm(asset.num_sales ~  quantity + total_price_usd + Neutral + Negative + Retweets + Replies + Followers + Favourites, data = almost.final, family ='poisson' )
summary(poissonmodel)
round(poissonmodel$coefficients)
dispersiontest(poissonmodel, trafo = 2)
#according to this no overdispersion so we can continue with Poisson 

#OPENSEA MODEL 
poissonmodel.opensea <- glm(asset.num_sales ~ quantity + total_price_usd, data = almost.final, family = 'poisson')
summary(poissonmodel.opensea)

#SENTIMENT MODEL 
poisson.sentiment <- glm(asset.num_sales ~ Negative + Positive, data = almost.final, family = 'poisson')
summary(poisson.sentiment)

#TWITTER MODELS
poisson.followers <- glm(asset.num_sales ~ Followers, data = almost.final, family = 'poisson')
summary(poisson.followers)

poisson.retweets <- glm(asset.num_sales ~ Followers + Retweets, data = almost.final, family = 'poisson')
summary(poisson.retweets)

poisson.replies <-glm(asset.num_sales ~ Followers + Retweets + Replies, data = almost.final, family = 'poisson')
summary(poisson.replies)

poisson.likes <- glm(asset.num_sales ~ Followers + Retweets + Replies + Likes, data = almost.final, family = 'poisson')
summary(poisson.likes)

poisson.fav <- glm(asset.num_sales ~ Followers + Retweets + Replies  + Favourites, data = almost.final, family = 'poisson')
summary(poisson.fav)

#compare between twitter models 
anova(poisson.followers,poisson.retweets,poisson.replies,poisson.fav )


#FULL MODEL 
poisson.full <- glm(asset.num_sales ~ quantity + total_price_usd + Negative + Neutral + Retweets, almost.final, family = 'poisson' )
summary(poisson.full)

#COMPARE MODELS - ANOVA ONLY FOR NESTED MODELS 
poisson.null <- glm(asset.num_sales ~ 1, almost.final, family = 'poisson')
summary(poisson.null)

anova(poisson.null, poissonmodel.opensea, poissonmodel, test = 'Chisq')#model 3 is the best (full) 
#both values are significant, so the second model is better than the first and the third is better than the second 
#test based on likelihood which increases when parameters increase, information criteria takes care of this, likelihood will be better 
#for every parameter, but even if its a random parameter it helps you know if the parameter is good to add. 
#In information criteria you can compare between models that are nos nested unlike log likelihood ratio test. 
anova(poisson.null, poisson.sentiment, poissonmodel, test = 'Chisq')
#third model is significantly better again 

anova(poisson.null,poisson.sentiment, poissonmodel, test = 'Chisq')
#third model significantly better 

anova(poisson.null,poisson.fav, poissonmodel, test = 'Chisq')

#INFORMATION CRITERIA COMPARISON 
AIC(poissonmodel.opensea,poisson.sentiment,poisson.fav, poissonmodel, poisson.null )
#poisson full is the best model in agreement with anova in both AIC and BIC 
BIC(poissonmodel.opensea,poisson.sentiment,poisson.fav, poissonmodel, poisson.null)

