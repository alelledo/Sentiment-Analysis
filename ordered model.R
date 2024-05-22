library(readr)
library(dplyr)
library(caret)
library(glmnet)
library(truncnorm)
library(earth)
library(brant)
almost.final$Class<-ordered(almost.final$Class, levels = c(1,2,3,4))


#NULL MODEL-----------------------------------------------
null.ordered<- polr(data =almost.final, Class~ 1, Hess = TRUE)

#OPENSEA MODEL-----------------------------------------------------------------------------
ordered.opensea <- polr(data = almost.final, Class~ asset.num_sales + quantity, Hess = TRUE)
summary(ordered.opensea) #here we can see that when quantity goes up Class does not go up, num of sales however does have a positive effect                       


brant(ordered.opensea)
#the brant test for parallel assumptions holds 

-1.5960 - (2*0.0337)#-1.6634
-1.5960 + (2*0.0337)# -1.5286

1.2946 - (2*0.1043)#1.086
1.2946 + (2*0.1043)#1.5032

4.1157 - (2*0.3845)#3.3467
4.1157 + (2*0.3845)#4.8847

#Taos dont overlap 

##HITRATE - this model is not a great predictor 
ordered.opensea$choice = apply(fitted(ordered.opensea, type = "probabilities"), 1 , which.max)
ordered.opensea$choice = factor(ordered.opensea$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, ordered.opensea$choice)
(560+2+1)/998#0.5641283


#LRTEST
lrtest(null.ordered, ordered.opensea)

#SENTIMENT MODEL--------------------------------------------------------------------------- 
ordered.sentiment <- polr(data = almost.final, Class~ Negative + Neutral, Hess = TRUE)
summary(ordered.sentiment)

-0.3866 - (2*0.2439    )#-0.8744
-0.3866 + (2*0.2439    )# 0.1012

2.4769 - (2*0.2602     )#1.9565
2.4769 + (2*0.2602     )#2.9973

5.2583   - (2*0.4486    )#4.3611
5.2583   + (2*0.4486    )#6.1555


brant(ordered.sentiment)

##HITRATE - this model is not a great predictor 
ordered.sentiment$choice = apply(fitted(ordered.sentiment, type = "probabilities"), 1 , which.max)
ordered.sentiment$choice = factor(ordered.sentiment$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, ordered.sentiment$choice)
(562)/998#0.5611222

#LRTEST
lrtest(null.ordered, ordered.sentiment)
#not significantly different than the null model 

#TWITTER MODEL--------------------------------------------- 
ordered.twitter <- polr(data = almost.final, Class~ Followers , Hess = TRUE)
summary(ordered.twitter)

-0.6239   - (2*0.0821    )#-0.7881
-0.6239   + (2*0.0821    )# -0.4597

2.2407   - (2*0.1170    )#2.0067
2.2407   + (2*0.1170    )#2.4747

5.0219   - (2*0.3832    )#4.2555
5.0219   + (2*0.3832    )#5.7883

brant(ordered.twitter)

##HITRATE - this model is not a great predictor 
ordered.twitter$choice = apply(fitted(ordered.twitter, type = "probabilities"), 1 , which.max)
ordered.twitter$choice = factor(ordered.twitter$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, ordered.twitter$choice)
(562)/998#0.5631263

#LRTEST
lrtest(null.ordered, ordered.twitter)
#not significantly different than the null model 

#FULL MODEL---------------------------------------------------------
ordered.full <- polr(data = almost.final, Class~ asset.num_sales + quantity +Likes + Replies  + Negative + Neutral , Hess = TRUE)
summary(ordered.full)

-1.2290    - (2*0.1036   )#-1.4362
-1.2290    + (2*0.1036   )# -1.0218

1.6694    - (2*0.1468    )#1.3758
1.6694    + (2*0.1468    )#1.963

4.4944    - (2*0.3988    )#3.6968
4.4944    + (2*0.3988    )#5.292

brant(ordered.full)

##HITRATE - this model is not a great predictor 
ordered.full$choice = apply(fitted(ordered.full, type = "probabilities"), 1 , which.max)
ordered.full$choice = factor(ordered.full$choice, levels = c(1,2,3,4), labels = c("[1,10]","[11,20]","[21,30]","[31,40]"))
table(almost.final$Class, ordered.full$choice)
(563)/998#0.5641283

#LRTEST
lrtest(null.ordered, ordered.full)
#significantly different than the null model 

AIC(ordered.opensea, ordered.sentiment, ordered.twitter, ordered.full, null.ordered)
BIC(ordered.opensea, ordered.sentiment, ordered.twitter, ordered.full)
