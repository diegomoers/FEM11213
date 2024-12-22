################################################################################
################################################################################
############################### SLIDE 1 - 9 ####################################
############################ Basic Regressions #################################
################################################################################
################################################################################
ojdata <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/oj.csv")
summary(ojdata)
#from the slides:

head(ojdata, n=5)
tail(ojdata, n=5)

glm(log(sales) ~ brand + log(price), data=ojdata)

x <- model.matrix(~ brand + log(price), data=ojdata); head(x); tail(x) 
#I understand that because brand is categorical, we assign a combination fo 0 or 1 for each one to signal which brand we're talking about. I (assume) that intercept is the base category, i.e dominicks in this case
#What is not clear to me is why head(x) prints out the same log(price), but tail(x) prints out different log(price) for only dominicks?
#To check this, I wanted to check what the price data looked like for all three brands:
lapply(split(ojdata$price, ojdata$brand), unique)
tapply(ojdata$price, ojdata$brand, mean)
#clearly there is a good variety of pricing for each brand, but the mean price from lowest to highest was dominicks < minute.maid < tropicana. and it is just a coincidence that head(x) returned tropicanas with the same price
#the slides show you can change the base category, but there is no reason to do so since it makes sense to use the cheapest brand as base category

glm(log(sales) ~ log(price)*brand*feat, data=ojdata)
#to interpret, for ex:
  #- a 1% increase in price leads to a 2.77% decrease in sales
  # tropicana is less price sensitive than minutemaid or dominicks
  # being featured is especially good for minutemaid



################################################################################
################################################################################
############################### SLIDE 10 - 16 ##################################
############################ Logistic Regression ###############################
################################################################################
################################################################################
emaildata <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/spam.csv")
summary(emaildata)
dim(emaildata)

spammy <- glm(spam ~ ., data=emaildata, family='binomial')
#what does binomial mean here?
glm(spam ~ ., data=emaildata, family='gaussian')
#this doesn't make sense bc gaussian is for continous variables.
#so basically 'binomial' just means there are only two possible outcomes (spam and not-spam)
#i want the confusion matrix of this table to be able to see how good it actually is at predicting spam/non-spam correctly.
 #i think it is fair to use a 0.5 cutoff value, so:
glm_prob <- predict(spammy, newdata = emaildata, type = "response")
confusion_matrix <- table(Actual = emaildata$spam, Predicted = glm_pred)
glm_pred <- ifelse(glm_prob > 0.5, 1, 0)
print(confusion_matrix)
predict(spammy, newdata = emaildata[c(1,4000),], type="response")


#the glm code is a parametric model (meaning it depends on some assumptions about the data/technique).
#from another class, i know that you can use a rf model to classify (and it requires fewer assumptions).
install.packages("randomForest")
library(randomForest)
emaildata$spam <- as.factor(emaildata$spam)
sample_indices <- sample(nrow(emaildata), size = 0.7 * nrow(emaildata))
train_data <- emaildata[sample_indices, ]
test_data <- emaildata[-sample_indices, ]
rf_model <- randomForest(spam ~ ., data = train_data, ntree = 100, mtry = sqrt(ncol(train_data) - 1), importance = TRUE)
print(rf_model)

# now both models have been tested, and when comparing their confusion matrices:

#RANDOM FOREST MODEL:
print(rf_model)
rf_predictions[, 2]
#note, random forest seems to be dropping some rows... so i'm going to compare the rf model to the glm one in terms of percentage accuracy
  #it reports the out-of-bag error rate if 4.97%
  #it predicts email 1 is spam 100% an email 4000 is spam 2%

#GLM BINOMIAL MODEL:
print(confusion_matrix)
predict(spammy, newdata = emaildata[c(1,4000),], type="response")
  #if you do the math, the accuracy for the GLM model is 93.7%, so it has an error rate of 6.3%, which is higher than the random forest model!!
  #it predicts email 1 is spam 88.4% an email 4000 is spam 15%, so it seems like it is more uncertaint he the rf model...

#interpreting the variables is also very different between the glm and rf model. the glm model has interpretable coefficients:
  coef(spammy)["word_free"]; exp(coef(spammy)["word_free"])
  #thus the the word free increases the odds of email being spam by a factor of 4.68

  #whereas the rf model assigns "importance scores":
  importance_scores <- importance(rf_model)
  importance_scores["word_free", ]
  #because mean decrease accuracy and mean decrease gini are high, this just tells us the word "free" is an important coefficient for predicting spam/non-spam
  #but i don't see an interpretable coeffficient like in glm


  
  
################################################################################
################################################################################
############################### SLIDE 17 - 16 ##################################
########################## Deviance + Likelihood ###############################
################################################################################
################################################################################

#i tried to run my own overfit model (below). very bad idea, computer almost blew up. instead i'm going to work the deviance part with only a subset of the data (first 100 rows)  
  #spenny_overfit <- glm(spam ~ .^2, data = emaildata, family = "binomial")

#subset of data:
emaildata_subset <- emaildata[1:100, ]
  
#overfit model: 
spammy_overfit <- glm(spam ~ .^2, data = emaildata_subset, family = "binomial")
summary(spammy_overfit)
D_of <- summary(spammy_overfit)$deviance
print(D_of)
#as expected, this model has (almost) zero residual deviance

#the fitted model:
spammy <- glm(spam ~ ., data=emaildata, family='binomial')
summary(spammy)
D_fit <- summary(spammy)$deviance
print(D_fit)

#the null model:
spammy_null <- glm(spam ~ 1, data = emaildata[1:100, ], family = "binomial")
summary(spammy_null)
D_null <- summary(spammy_null)$deviance
print(D_null)


#using this i can construct my own R^2 estimate!!
#given that R^2= 1 - (dev(fitted model) - dev(overfit model))/dev(nullmodel)

R2_estimate <- 1 - (D_fit)/(D_null)
print(R2_estimate)
#Because we limited to using only the first 100 rows, the R^2 estimate is terrible (to be expected, it is almost negative infinity)
