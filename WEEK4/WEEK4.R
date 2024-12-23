################################################################################
################################################################################
############################### SLIDE 1 - 14 ###################################
######################### Cross-Validation (K-folds) ###########################
################################################################################
################################################################################
scdatadata <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/semiconductor.csv")
full <- glm(FAIL ~ ., data=scdata, family=binomial)
1 - full$deviance/full$null.deviance

#ALL FROM SLIDES:
  #define deviance
  deviance <- function(y, pred, family=c("gaussian","binomial")){
    family <- match.arg(family)
    if(family=="gaussian"){
      return( sum( (y-pred)^2 ) )
    }else{
      if(is.factor(y)) y <- as.numeric(y)>1
      return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
    }
  }
  ## get null devaince too, and return R2
  R2 <- function(y, pred, family=c("gaussian","binomial")){
    fam <- match.arg(family)
    if(fam=="binomial"){
      if(is.factor(y)){ y <- as.numeric(y)>1 }
    }
    dev <- deviance(y, pred, family=fam)
    dev0 <- deviance(y, mean(y), family=fam)
    return(1-dev/dev0)
  }
  
    n <- nrow(scdata) 
  K <- 10 
  foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
  Out <- data.frame(full=rep(NA,K)) 
  for(k in 1:K){ 
    train <- which(foldid!=k)
    rfull <- glm(FAIL~., data=scdata, subset=train, family=binomial)
    predfull <- predict(rfull, newdata=scdata[-train,], type="response")
    Out$full[k] <- R2(y=scdata$FAIL[-train], pred=predfull, family="binomial")
    cat(k, " ")
  }
  boxplot(Out, col="plum", ylab="R2")
  colMeans(Out)
#what i noticed is that while the R2 that returns is always negative. it varies each time of course, because every time the code is run a different k-fold is used. 
# i want to plot the distribution of k's on a dsitribution graph rather than a box plot.

png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK4/R2_distribution_10_kfolds.png", width=800, height=600)
  hist(Out$full, 
     col="lightblue", 
      border="white", 
     main="Distribution of R2 Across Folds", 
    xlab="R2", 
    probability=TRUE)
    lines(density(Out$full, na.rm=TRUE), 
    col="darkblue", 
    lwd=2)
dev.off()

n <- nrow(scdata) 
K <- 50
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
Out <- data.frame(full=rep(NA,K)) 
for(k in 1:K){ 
  train <- which(foldid!=k)
  rfull <- glm(FAIL~., data=scdata, subset=train, family=binomial)
  predfull <- predict(rfull, newdata=scdata[-train,], type="response")
  Out$full[k] <- R2(y=scdata$FAIL[-train], pred=predfull, family="binomial")
  cat(k, " ")
}
boxplot(Out, col="plum", ylab="R2")
colMeans(Out)

png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK4/R2_distribution_50_kfolds.png", width=800, height=600)
hist(Out$full, 
     col="lightblue", 
     border="white", 
     main="Distribution of R2 Across Folds", 
     xlab="R2", 
     probability=TRUE)
lines(density(Out$full, na.rm=TRUE), 
      col="darkblue", 
      lwd=2)
dev.off()

#comparing the graph with 10 k-folds to the one with 50 k-folds, it is clear that the R^2 is negative always, and not just because only 10 kfolds were used in the slides.
#however, when i tried to run with 100 k-folds, the code would not run? I will try to troubleshoot why:
#first i thought it was because 100 fodls was too many for the given n, which would result in empty folds. to check this, i ran:
dim(scdata)
#then i wanted to check how many observations were in each of the 50 k-folds i ran:
table(foldid)
  #note: every fold up to 49 has the same nuber of observations (30). however, the last one (50) only has 7... I would think this would have implications for the r^2 calculation because the variance in the 50th k-fold is artifically lower since it has fewer observations
  #chatgpt suggested that i weigh the R^2 based on the size of k folds. I think this should have a very small impact on the final solution, but for safety:
fold_sizes <- table(foldid)
weights <- fold_sizes / sum(fold_sizes)
weighted_R2 <- sum(weights * Out$full, na.rm = TRUE)
print(weighted_R2)
#i get -2.00, which lines up with my graph (but is pretty different from the one on the slides, which suggests the slide should have used more folds)



################################################################################
################################################################################
############################## SLIDE 15 - 18 ###################################
################### Backward/Forward Stepwise Regressions ######################
################################################################################
################################################################################
#the slides mention backwards stepwise regression, but do not actually run one. I want to use it and compare it to forward stepwise regressions.
# I first tried running backwards stepwise regression with all coefficients, but my computer almost exploded again. So I am forced to limit my experiment to the first 100 variables only.
# Create the null and full models, limiting to first 20 predictors

variables <- names(scdata)[1:100]  
formula_str <- paste("FAIL ~", paste(variables, collapse = " + "))
full <- glm(as.formula(formula_str), data=scdata)

# Null model 
null <- glm(FAIL~1, data=scdata)

# Forward stepwise
fwd <- step(null, scope=formula(full), direction="forward")
summary(fwd)

# Backward stepwise 
back <- step(full, direction="backward")
summary(back)

#compare front vs back stepwise:
length(coef(fwd))
length(coef(back))
fwd$deviance
back$deviance
#with 100 variables, both models return an identical number of coefficients, and the exact same deviance
  #however, backwards took much much longer, especially in the beginning when it had to compute through 100 variables (took 65 steps).
  #thus, forward stepwise really is more computationally efficient (only took ≈37 steps).
#to show how much longer it took backward to get to the same spot, i made the following graph using Claude:
png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK4/stepwise_regression_compare.png")
plot(fwd$anova$AIC, type="l", col="blue", 
     ylab="AIC", xlab="Steps", main="Stepwise Regression Comparison",
     xlim=c(1,65))  
lines(back$anova$AIC, col="red")
abline(v=seq(0,65,5), col="gray", lty=3) 
axis(1, at=seq(0,65,5))  
legend("topright", c("Forward", "Backward"), col=c("blue", "red"), lty=1)
dev.off()


################################################################################
################################################################################
############################# SLIDE 30 - 35 ####################################
#################### K-fold Cross Validation  with LASSO #######################
################################################################################
################################################################################

#this part of the code is copied from the slides:
web <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/browser-domains.csv")
## Read in actual website names and relabel site factor
sitenames <- scan("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)

web$id <- factor(web$id, levels=1:length(unique(web$id)))

machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]

xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id),nlevels(web$site)),
  dimnames=list(id=levels(web$id), site=levels(web$site)))

webspend <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/browser-totalspend.csv")
webspend <- as.matrix(webspend) 
webspend_vector <- webspend[, "spend"]
spender <- gamlr(xweb, log(webspend_vector), verb=TRUE); spender
cv.spender <- cv.gamlr(xweb, log(webspend_vector), verb=TRUE); cv.spender

#end of code copied form the slides

#for my code, I want to test what the effect of different penalization functions is on the number of coefficients (as well as the size of coefficients)
#for this, i will compare lasso and ridge penalization
#   -I suspect that lasso will lead a lot of coefficients to 0. I am not entirely sure what the effect will be on the size of coefficients.
#   -In comparison, ridge should be more forgiving against zeroing down coefficients, so I expect the most coefficients here.

#NOTE: I tried using the gamlr package, but it would not work. instead, I used the glmnet package
library(glmnet)

# Convert sparse matrix to regular matrix for glmnet
x_mat <- as.matrix(xweb)

# Fit models with cross-validation
set.seed(123)  # For reproducibility
cv_lasso <- cv.glmnet(x_mat, log(webspend_vector), alpha=1)
cv_ridge <- cv.glmnet(x_mat, log(webspend_vector), alpha=0)

lasso_coef <- coef(cv_lasso, s="lambda.min")
ridge_coef <- coef(cv_ridge, s="lambda.min")

cat("LASSO Results:\n")
cat("Optimal lambda:", cv_lasso$lambda.min, "\n")
cat("Number of non-zero coefficients:", sum(lasso_coef != 0)-1, "\n")  # -1 for intercept
cat("Largest coefficient:", max(abs(lasso_coef[-1])), "\n")
cat("Smallest non-zero coefficient:", min(abs(lasso_coef[lasso_coef != 0 & abs(lasso_coef) > 1e-10])), "\n\n")
#LASSO only has 206 coefficients, and the largest one is 1.14

cat("Ridge Results:\n")
cat("Optimal lambda:", cv_ridge$lambda.min, "\n")
cat("Number of non-zero coefficients:", sum(ridge_coef != 0)-1, "\n")
cat("Largest coefficient:", max(abs(ridge_coef[-1])), "\n")
cat("Smallest non-zero coefficient:", min(abs(ridge_coef[ridge_coef != 0 & abs(ridge_coef) > 1e-10])), "\n\n")
#Ridge retains all 1000 variables, but its largest coefficient is 0.67.
#as was epected, LASSO will reduce variables and set some equal to zero, while Ridge retains them as much as possible, however small.

png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK4/LASSOvRIDGE.png")
par(mfrow=c(2,1))
plot(cv_lasso, main="LASSO")
plot(cv_ridge, main="Ridge")
dev.off()