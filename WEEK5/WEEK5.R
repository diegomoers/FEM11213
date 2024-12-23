################################################################################
################################################################################
############################### SLIDE 4 - 10 ###################################
########################### K-Nearest Neighbours ###############################
################################################################################
################################################################################
#copied from slides:
library(MASS)
data(fgl)
dim(fgl)
head(fgl, n = 2)
x <- scale(fgl[,1:9]) # column 10 is class label, scale converts to mean 0 sd 1
apply(x,2,sd) # apply function sd to columns of x
library(class) #has knn function 
test <- sample(1:214,10) #draw a random sample of 10 rows 
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
data.frame(fgl$type[test],nearest1,nearest5)

#the slides mention that KNN is very sensitive to the K chosen, I wanted to see this visualised:

test <- sample(1:214, 50)

k1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
k3 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=3)
k5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
k7 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=7)
k11 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=11) 
k15 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=15)
k21 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=21)
k30 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=30)
k100 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=100)

accuracy1 <- mean(k1 == fgl$type[test]) * 100
accuracy3 <- mean(k3 == fgl$type[test]) * 100
accuracy5 <- mean(k5 == fgl$type[test]) * 100
accuracy7 <- mean(k7 == fgl$type[test]) * 100
accuracy11 <- mean(k11 == fgl$type[test]) * 100
accuracy15 <- mean(k15 == fgl$type[test]) * 100
accuracy21 <- mean(k21 == fgl$type[test]) * 100
accuracy30 <- mean(k30 == fgl$type[test]) * 100
accuracy100 <- mean(k100 == fgl$type[test]) * 100
k_values <- c(1,3,5,7,11,15,21,30,100)
accuracies <- c(accuracy1, accuracy3, accuracy5, accuracy7, accuracy11, accuracy15, accuracy21, accuracy30, accuracy100)

png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK5/K-neighbours_sensitivity_K.png")
plot(k_values, accuracies, type="b", 
     xlab="k", ylab="% Correct",
     main="KNN Sentivity to K")
dev.off()
#indeed, as we increase k, it seems to have a negative effect on how often it is correct. it was actually better at predicting with fewer neighbors.
#not much more to add here, I think KNN is rather intuitive

################################################################################
################################################################################
############################## SLIDE 13 - 22 ###################################
######################## Classification + ROC curves ###########################
################################################################################
################################################################################
#from the slides:
library(gamlr)
credit <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/credit.csv")
source("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/naref.R")
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
## a few others
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]
library(gamlr)
source("~/Desktop/EUR/BLOK2/Data and HR Analytics/Data/naref.R")
#had to find the naref.R file on Taddy's github
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)
default <- credit$Default
credscore <- cv.gamlr(credx, default, family="binomial")
par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)

sum(coef(credscore, s="min")!=0) # min
sum(coef(credscore$gamlr)!=0) # AICc
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]

## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))

rule <- 1/5 # move this around to see how these change
sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate at 1/5 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate at 1/5 rule

#first, i want to see what the impact of changing the classification rule is on the rate of false positives and false negatives:
rules <- seq(0, 1.0, by=0.1)
false_pos <- numeric(length(rules))
false_neg <- numeric(length(rules))

for(i in seq_along(rules)) {
  rule <- rules[i]
  false_pos[i] <- sum((pred > rule)[default==0]) / sum(pred > rule)
  false_neg[i] <- sum((pred < rule)[default==1]) / sum(pred < rule)
}

png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK5/classification_rules_FPR_FNR.png")
plot(rules, false_pos, type="l", col="red", 
     ylim=c(0,1),
     xlab="Classification Rule Cutoff", 
     ylab="Error Rate",
     main="Changes in Classification Rule on FPR and FNR")
lines(rules, false_neg, col="blue")
abline(v=seq(0, 1.0, by=0.1), col="gray", lty=3)  # Add vertical gridlines
legend("topright", 
       legend=c("False Positive Rate", "False Negative Rate"),
       col=c("red", "blue"), 
       lty=1)
dev.off()

#as we would expect:
  #with low cutoffs (around 0.2), we have lots of false alerts for defaulting and rarely miss real defaults
    # obviously, for a bank it's probably more dangerous to mislabel defaulters instead of mislabeling good borrowers.
  #at high cutoffs (~0.8): rarely false alert, but miss more real defaults  
  # Thus, if we (as a bank) want to choose the cutoff based on which error hurts most, it would be ideal to have a low cutoff to minimize mislabeling defaulters

sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity

# refit the model using only 1/2 of data
test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
predoos <- predict(credhalf, credx[test,], type="response")
defaultoos <- default[test]

source("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/roc.R")
#ALSO had to find the naref.R file on Taddy's github!! took a lot longer
par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred, y=default, bty="n", main="in-sample")
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[default==0]), 
       y=mean((pred>.2)[default==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[default==0]), 
       y=mean((pred>.5)[default==1]), 
       cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
roc(p=predoos, y=defaultoos, bty="n", main="out-of-sample")
## our 1/5 rule cutoff
points(x= 1-mean((predoos<.2)[defaultoos==0]), 
       y=mean((predoos>.2)[defaultoos==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos<.5)[defaultoos==0]), 
       y=mean((predoos>.5)[defaultoos==1]), 
       cex=1.5, pch=20, col='blue') 

# for my project, i want to  try again using the KNN . I want to see if the ROC curve is different from the one on the slides.
#using Claude AI to convert the code to a KNN model:

library(class)
library(caret)
library(ROCR)

# Verify the data has correct structure per your output
# Numeric variables
numeric_cols <- c("duration", "amount", "installment")

# Categorical variables are already properly factored as shown in your output:
# history: good, poor, terrible
# purpose: newcar, usedcar, goods/repair, edu, biz
# foreign: foreign, german
categorical_cols <- c("history", "purpose", "foreign")

# Create dummy variables for categorical columns
credit_dummy <- model.matrix(~ . - 1, 
                             data=credit[, categorical_cols])

# Scale numeric features
credit_numeric <- scale(credit[, numeric_cols])

# Combine scaled numeric and dummy variables
X <- cbind(credit_numeric, credit_dummy)

# Target variable (Default: 0 or 1)
y <- credit$Default

# Split into training and test sets
set.seed(123)
train_index <- sample(1:nrow(credit), 0.7 * nrow(credit))
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Function to perform KNN and get probabilities
knn_prob <- function(train, test, cl, k) {
  prob <- knn(train, test, cl, k = k, prob = TRUE)
  prob_attr <- attr(prob, "prob")
  ifelse(prob == 1, prob_attr, 1 - prob_attr)
}

# Find optimal k using cross-validation
k_values <- seq(1, 30, by = 2)
cv_errors <- numeric(length(k_values))

set.seed(123)
folds <- createFolds(y_train, k = 5)
for (i in seq_along(k_values)) {
  k <- k_values[i]
  fold_errors <- numeric(length(folds))
  for (j in seq_along(folds)) {
    val_indices <- folds[[j]]
    fold_pred <- knn(X_train[-val_indices,], X_train[val_indices,], 
                     y_train[-val_indices], k = k)
    fold_errors[j] <- mean(fold_pred != y_train[val_indices])
  }
  cv_errors[i] <- mean(fold_errors)
}

optimal_k <- k_values[which.min(cv_errors)]
cat("Optimal k:", optimal_k, "\n")

# Get predictions
train_pred_prob <- knn_prob(X_train, X_train, y_train, k = optimal_k)
test_pred_prob <- knn_prob(X_train, X_test, y_train, k = optimal_k)

# ROC curves
pred_train <- prediction(train_pred_prob, y_train)
perf_train <- performance(pred_train, "tpr", "fpr")
pred_test <- prediction(test_pred_prob, y_test)
perf_test <- performance(pred_test, "tpr", "fpr")

# Plot ROC curves
par(mfrow=c(1,2))

# Training set
png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK5/ROC_KNN_insample.png")
plot(perf_train, main="in-sample", col="blue")
abline(0, 1, lty=2)
points(x = 1-mean((train_pred_prob < 0.2)[y_train == 0]),
       y = mean((train_pred_prob > 0.2)[y_train == 1]),
       col="red", pch=20, cex=1.5)
points(x = 1-mean((train_pred_prob < 0.5)[y_train == 0]),
       y = mean((train_pred_prob > 0.5)[y_train == 1]),
       col="blue", pch=20, cex=1.5)
dev.off()

# Test set
png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK5/ROC_KNN_outsample.png")
plot(perf_test, main="out-of-sample", col="blue")
abline(0, 1, lty=2)
points(x = 1-mean((test_pred_prob < 0.2)[y_test == 0]),
       y = mean((test_pred_prob > 0.2)[y_test == 1]),
       col="red", pch=20, cex=1.5)
points(x = 1-mean((test_pred_prob < 0.5)[y_test == 0]),
       y = mean((test_pred_prob > 0.5)[y_test == 1]),
       col="blue", pch=20, cex=1.5)

legend("bottomright", fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"), bty="n", title="cutoff")
dev.off()

# Calculate error rates
rule <- 1/5
cat("\nFor threshold =", rule, ":\n")
cat("False positive rate:", 
    sum((train_pred_prob > rule)[y_train == 0])/sum(train_pred_prob > rule), "\n")
cat("False negative rate:", 
    sum((train_pred_prob < rule)[y_train == 1])/sum(train_pred_prob < rule), "\n")

#CONCLUSION:
  #With q = 1/5, :
    #gamlr:
        #FPR: 60.60%
        #FNR: 7.74%
    #KNN:
        #FPR: 59.10%
        #FNR: 8.63%
  #Thus, my KNN (K=13) was only marginally worse than the classification model in the slides!