install.packages("ROCR")
library(ROCR)
set.seed(1234)

#get a 75% estimation sample and 25% validation sample 
almost.final$estimation.sample <-rbinom(nrow(almost.final), 1, 0.8)

#Create a new dataframe with only the validation sample
validation.final <- almost.final[almost.final$estimation.sample==0,]

#OPENSEA MODEL ---------------------------------------------------------------
#Estimate the model using the estimation sample 
logit.opensea.2 <- glm(profit~ asset.num_sales + quantity,
                     family = binomial(link = "logit"), 
                     data = almost.final, subset = estimation.sample==1)

#get predictions for all observations 
predictions.opensea <- predict(logit.opensea.2, type = "response", newdata = validation.final)

#the fit criteria metrics can be sensitive for unbalanced dataset, so if 90% of the 
#cases is negative it is easy to have a hit ratio of 90%. 
#To overcome this we can set a different cut-off value, a predicted prob of 0.9 is classifed as positive 
#otherwise is negative. Be careful interpreting the validation criteria 

#calculate fit criteria on validation sample 

#HIT RATE 
predicted.opensea <- ifelse(predictions.opensea>.9,1,0)
hitrate.opensea <- table(validation.final$profit, predicted.opensea, dnn = c("Observed", "Predicted"))
hitrate.opensea
(hitrate.opensea[1,1]+hitrate.opensea[2,2])/sum(hitrate.opensea)
#0.1642512

#TDL 
decile.predicted.opensea <- ntile(predictions.opensea,10)

decile.opensea <- table(validation.final$profit, decile.predicted.opensea, dnn = c("Observed", "Predicted"))

decile.opensea

#get TDL 
(decile.opensea[2,10]/(decile.opensea[1,10]+decile.opensea[2,10]))/mean(validation.final$profit)
#1.058523

pred.model.opensea<- prediction(predictions.opensea, validation.final$profit)
perf.opensea <- performance(pred.model.opensea, "tpr", "fpr")
plot(perf.opensea, xlab = "Cumulative % of observations", ylab = "Cumulative % of positive
cases", xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc.opensea <- performance(pred.model.opensea, "auc")

#GINI 
as.numeric(auc.opensea@y.values)*2-1
#0.3660191

#TWITTER MODEL --------------------------------------------------
logit.twitter.2 <- glm(profit~ Likes + Replies + Followers + Favourites,
                       family = binomial(link = "logit"), 
                       data = almost.final, subset = estimation.sample==1)

predictions.twitter <- predict(logit.twitter.2, type = "response", newdata = validation.final)

#HIT RATE 
predicted.twitter <- ifelse(predictions.twitter>.90,1,0)
hitrate.twitter <- table(validation.final$profit, predicted.twitter, dnn = c("Observed", "Predicted"))
hitrate.twitter
(hitrate.twitter[1,1]+hitrate.twitter[2,2])/sum(hitrate.twitter)
#0.2463768

#TDL 
decile.predicted.twitter <- ntile(predictions.twitter,10)

decile.twitter <- table(validation.final$profit, decile.predicted.twitter, dnn = c("Observed", "Predicted"))

decile.twitter

#get TDL 
(decile.twitter[2,10]/(decile.twitter[1,10]+decile.twitter[2,10]))/mean(validation.final$profit)
#1.058523

pred.model.twitter<- prediction(predictions.twitter, validation.final$profit)
perf.twitter <- performance(pred.model.twitter, "tpr", "fpr")
plot(perf.twitter, xlab = "Cumulative % of observations", ylab = "Cumulative % of positive
cases", xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc.twitter <- performance(pred.model.twitter, "auc")

#GINI 
as.numeric(auc.twitter@y.values)*2-1
#0.1625733

#SENTIMENT MODEL ---------------------------------------------------

logit.sentiment.2 <- glm(profit~ Neutral + Negative,
                       family = binomial(link = "logit"), 
                       data = almost.final, subset = estimation.sample==1)

predictions.sentiment <- predict(logit.sentiment.2, type = "response", newdata = validation.final)

#HIT RATE 
predicted.sentiment <- ifelse(predictions.sentiment>.90,1,0)
hitrate.sentiment <- table(validation.final$profit, predicted.sentiment, dnn = c("Observed", "Predicted"))
hitrate.sentiment
(hitrate.sentiment[1,1]+hitrate.sentiment[2,2])/sum(hitrate.sentiment)
#0.1642512 same as opensea 

#TDL 
decile.predicted.sentiment <- ntile(predictions.sentiment,10)

decile.sentiment <- table(validation.final$profit, decile.predicted.sentiment, dnn = c("Observed", "Predicted"))

decile.sentiment

#get TDL 
(decile.sentiment[2,10]/(decile.sentiment[1,10]+decile.sentiment[2,10]))/mean(validation.final$profit)
#1.058523 so far all have same tdl 

pred.model.sentiment<- prediction(predictions.sentiment, validation.final$profit)
perf.sentiment <- performance(pred.model.sentiment, "tpr", "fpr")
plot(perf.sentiment, xlab = "Cumulative % of observations", ylab = "Cumulative % of positive
cases", xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc.sentiment <- performance(pred.model.sentiment, "auc")

#GINI 
as.numeric(auc.sentiment@y.values)*2-1
#0.02291056

#FULL MODEL -------------------------------------------------------

#Estimate the model using the estimation sample 
logit.full.2 <- glm(profit~ asset.num_sales + quantity +Likes + Replies + 
                         Followers + Favourites + Negative + Neutral,
                       family = binomial(link = "logit"), 
                       data = almost.final, subset = estimation.sample==1)

summary(logit.full.2)
#get predictions for all observations 
predictions.full <- predict(logit.full.2, type = "response", newdata = validation.final)

#HIT RATE 
predicted.full <- ifelse(predictions.full>.90,1,0)
hitrate.full <- table(validation.final$profit, predicted.full, dnn = c("Observed", "Predicted"))
hitrate.full
(hitrate.full[1,1]+hitrate.full[2,2])/sum(hitrate.full)
#0.2801932

#TDL 
decile.predicted.full <- ntile(predictions.full,10)

decile.full <- table(validation.final$profit, decile.predicted.full, dnn = c("Observed", "Predicted"))

decile.full

#get TDL 
(decile.full[2,10]/(decile.full[1,10]+decile.full[2,10]))/mean(validation.final$profit)
#0.9997159

pred.model.full<- prediction(predictions.full, validation.final$profit)
perf.full <- performance(pred.model.full, "tpr", "fpr")
plot(perf.full, xlab = "Cumulative % of observations", ylab = "Cumulative % of positive
cases", xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc.full <- performance(pred.model.full, "auc")

#GINI 
as.numeric(auc.full@y.values)*2-1
#0.06561584

#SIGNIFICANT MODEL ---------------------------------------------------

logit.significant.2 <- glm(profit~ quantity +Likes + Favourites,
                    family = binomial(link = "logit"), 
                    data = almost.final, subset = estimation.sample==1)

#get predictions for all observations 
predictions.significant <- predict(logit.significant.2, type = "response", newdata = validation.final)

#HIT RATE 
predicted.significant <- ifelse(predictions.significant>.90,1,0)
hitrate.significant <- table(validation.final$profit, predicted.significant, dnn = c("Observed", "Predicted"))
hitrate.significant
(hitrate.significant[1,1]+hitrate.significant[2,2])/sum(hitrate.significant)
#0.5619835

#TDL 
decile.predicted.significant <- ntile(predictions.significant,10)

decile.significant <- table(validation.final$profit, decile.predicted.significant, dnn = c("Observed", "Predicted"))

decile.significant

#get TDL 
(decile.significant[2,10]/(decile.significant[1,10]+decile.significant[2,10]))/mean(validation.final$profit)
# 0.5159915

pred.model.significant<- prediction(predictions.significant, validation.final$profit)
perf.significant <- performance(pred.model.significant, "tpr", "fpr")
plot(perf.significant, xlab = "Cumulative % of observations", ylab = "Cumulative % of positive
cases", xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc.significant <- performance(pred.model.significant, "auc")

#GINI 
as.numeric(auc.significant@y.values)*2-1
#0.6798825

#ADD SENTIMENT TO EVERYTHING------------------------------------------------

#OPENSEA SENTIMENT --------------------------------------------------------------
logit.opensea.sentiment <- glm(profit~ asset.num_sales + quantity + Positive + Negative,
                       family = binomial(link = "logit"), 
                       data = almost.final, subset = estimation.sample==1)

#get predictions for all observations 
predictions.opensea.sentiment <- predict(logit.opensea.sentiment, type = "response", newdata = validation.final)

#the fit criteria metrics can be sensitive for unbalanced dataset, so if 90% of the 
#cases is negative it is easy to have a hit ratio of 90%. 
#To overcome this we can set a different cut-off value, a predicted prob of 0.9 is classifed as positive 
#otherwise is negative. Be careful interpreting the validation criteria 

#calculate fit criteria on validation sample 

#HIT RATE 
predicted.opensea.sentiment <- ifelse(predictions.opensea.sentiment>.9,1,0)
hitrate.opensea.sentiment <- table(validation.final$profit, predicted.opensea.sentiment, dnn = c("Observed", "Predicted"))
hitrate.opensea.sentiment
(hitrate.opensea.sentiment[1,1]+hitrate.opensea.sentiment[2,2])/sum(hitrate.opensea.sentiment)
#0.1690821

#TDL 
decile.predicted.opensea.sentiment <- ntile(predictions.opensea.sentiment,10)

decile.opensea.sentiment <- table(validation.final$profit, decile.predicted.opensea.sentiment, dnn = c("Observed", "Predicted"))

decile.opensea.sentiment

#get TDL 
(decile.opensea.sentiment[2,10]/(decile.opensea.sentiment[1,10]+decile.opensea.sentiment[2,10]))/mean(validation.final$profit)
#1.058523

pred.model.opensea.sentiment<- prediction(predictions.opensea.sentiment, validation.final$profit)
perf.opensea.sentiment <- performance(pred.model.opensea.sentiment, "tpr", "fpr")
plot(perf.opensea.sentiment, xlab = "Cumulative % of observations", ylab = "Cumulative % of positive
cases", xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc.opensea.sentiment <- performance(pred.model.opensea.sentiment, "auc")

#GINI 
as.numeric(auc.opensea.sentiment@y.values)*2-1
#0.05755132
#TWITTER SENTIMENT-------------------------------------------------------- 
logit.twitter.sentiment <- glm(profit~ Likes + Replies + Followers + Favourites + Positive + Negative ,
                       family = binomial(link = "logit"), 
                       data = almost.final, subset = estimation.sample==1)

predictions.twitter.sentiment <- predict(logit.twitter.sentiment, type = "response", newdata = validation.final)

#HIT RATE 
predicted.twitter.sentiment <- ifelse(predictions.twitter.sentiment>.90,1,0)
hitrate.twitter.sentiment <- table(validation.final$profit, predicted.twitter.sentiment, dnn = c("Observed", "Predicted"))
hitrate.twitter.sentiment
(hitrate.twitter.sentiment[1,1]+hitrate.twitter.sentiment[2,2])/sum(hitrate.twitter.sentiment)
#0.2753623

#TDL 
decile.predicted.twitter.sentiment <- ntile(predictions.twitter.sentiment,10)

decile.twitter.sentiment <- table(validation.final$profit, decile.predicted.twitter.sentiment, dnn = c("Observed", "Predicted"))

decile.twitter.sentiment

#get TDL 
(decile.twitter.sentiment[2,10]/(decile.twitter.sentiment[1,10]+decile.twitter.sentiment[2,10]))/mean(validation.final$profit)
#0.9997159

pred.model.twitter.sentiment<- prediction(predictions.twitter.sentiment, validation.final$profit)
perf.twitter.sentiment <- performance(pred.model.twitter.sentiment, "tpr", "fpr")
plot(perf.twitter.sentiment, xlab = "Cumulative % of observations", ylab = "Cumulative % of positive
cases", xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc.twitter.sentiment <- performance(pred.model.twitter.sentiment, "auc")

#GINI 
as.numeric(auc.twitter.sentiment@y.values)*2-1
#0.142412

AIC(logit.opensea.2,logit.twitter.2,logit.sentiment.2,logit.full.2,logit.opensea.sentiment,logit.twitter.sentiment)
BIC(logit.opensea.2,logit.twitter.2,logit.sentiment.2,logit.full.2,logit.opensea.sentiment,logit.twitter.sentiment)
