
################################################################################
################################################################################
############################### SLIDE 1 - 12 ###################################
################## Frequentist vs. Bootstrapping Uncertainty ###################
################################################################################
################################################################################
browserdata <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/web-browsers.csv")
summary(browserdata)
mean(browserdata$spend)
var_browserspend<- var(browserdata$spend)/10000
var_browserspend
#we divide by 1000 bc we have 10k households
sqrt(var_browserspend)
# so in the frequentist approach, we get a fixed standard deviation of 80.3861

#now adopting the bootstrap method:
B <- 1000
mub <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browserdata), replace=TRUE)
  mub <- c(mub, mean(browserdata$spend[samp_b]))
}
sd(mub)
#however if we bootstrap, the standard deviation (and the variance) change each time because the samples are randomly selected each time
sort(samp_b)[1:10]
#this command shows us the first 10 smallest values in the 1000 randomly selected samples (With repetition). this obviously also changes each time we run the bootstrap.

B2 <- 100
mub2 <- c()
for (b2 in 1:100){
  samp_b2 <- sample.int(nrow(browserdata), replace=TRUE)
  mub2 <- c(mub2, mean(browserdata$spend[samp_b2]))
}
sd(mub2)
sort(samp_b2)[1:10]
#same thing, but this time we only bootstrap it 100x. this would expectedly lead to less stable estimates of the std.dev?

B3 <- 10
mub3 <- c()
for (b3 in 1:10){
  samp_b3 <- sample.int(nrow(browserdata), replace=TRUE)
  mub3 <- c(mub3, mean(browserdata$spend[samp_b3]))
}
sd(mub3)
sort(samp_b3)[1:10]
#when we bootstrap it wih even fewer samples, the estimate for st.dev is a lot broader. i got 41.13 once...

#to actually show the distributions, i am going to make distribution plots for 10, 100, and 1000 bootstraps:
png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/bootstrap_distributions.png", width=700, height=500)
plot(density(mub), 
     main="Bootstrap Distribution",
     xlab="Mean",
     ylab="Density",
     col="blue",
     lwd=2)
lines(density(mub2), col="red", lwd=2)
lines(density(mub3), col="green", lwd=2)
legend("topright", 
       legend=c("B=1000", "B=100", "B=10"),
       col=c("blue", "red", "green"),
       lwd=2)
dev.off()

png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/hdist1.png", width=700, height=500)
h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browserdata$spend), sd = sqrt(var(browserdata$spend)/1e4)) 
yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
#QUESTION FROM SLIDES can you explain why we need each term in the last expression? 
#to find out i ran the same code, but removed diff(h$mids[1:2]) or length(mub) to see what happens to the line each time
lines(xfit, yfit, col = "black", lwd = 2)
dev.off()


png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/hdist2.png", width=700, height=500)
h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browserdata$spend), sd = sqrt(var(browserdata$spend)/1e4)) 
yfit <- yfit * length(mub) 
lines(xfit, yfit, col = "black", lwd = 2)
dev.off()


png("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/hdist3.png", width=700, height=500)
h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browserdata$spend), sd = sqrt(var(browserdata$spend)/1e4)) 
yfit <- yfit * diff(h$mids[1:2]) 
lines(xfit, yfit, col = "black", lwd = 2)
dev.off()

#if we remove diff(h$mids[1:2]) or length(mub), the yfit line becomes much flatter. the reason we need both of these is because they scale the yfit line to fit the historgram. 
  #more specifically:
    #1. diff(h$mids[1:2]) calculates the width of each bar to make sure the yfitline aligns with it horizontally and isn't too wide or narrow
    #2. length(mub) makes sure that data fits vertically 

#now, for running the regression on slide 11
B <- 1000
betas <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browserdata), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browserdata[samp_b,])
  betas <- rbind(betas, coef(reg_b))
}; head(betas, n=3)




################################################################################
################################################################################
############################### SLIDE 13 - 27 ##################################
######################### Benjamini-Hochberg Algorithm  ########################
################################################################################
################################################################################

#COPIED FROM SLIDES:
spendy <- glm(log(spend) ~ . -id, data=browserdata)
round(summary(spendy)$coef,2)

pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
pvalrank <- rank(pval)
reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) 
png(file="~/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/BHA1.png",
    width=600, height=350)
plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank, (0.1/9)*pvalrank)
dev.off()
#when q= 0.1, we have a very lenient criterion. in my code, i want to see what happens if we make the q value more strict, and how it shows on the graph
#in the current graph, the line is relatively flat

#when q = 0.05
pval2 <- summary(spendy)$coef[-1, "Pr(>|t|)"]
pvalrank2 <- rank(pval2)
reject <- ifelse(pval2< (0.05/9)*pvalrank2, 2, 1) 
png(file="~/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/BHA2.png",
    width=600, height=350)
plot(pvalrank2, pval2, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank2, (0.05/9)*pvalrank2)
dev.off()
#in this graph, the line is still relitavely flat, but it does not change the number of significant p-values

#when q = 0.01
pval3 <- summary(spendy)$coef[-1, "Pr(>|t|)"]
pvalrank3 <- rank(pval3)
reject <- ifelse(pval3< (0.01/9)*pvalrank3, 2, 1) 
png(file="~/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/BHA3.png",
    width=600, height=350)
plot(pvalrank3, pval3, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank3, (0.01/9)*pvalrank3)
dev.off()
#now when q = 0.01, the 5th p-value is now dropped! the line is significantly flatter than 0.1, too.


#now to try it on the semiconductor data set
#copied from slides
semiConductData <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/semiconductor.csv")
summary(semiConductData)
full <- glm(FAIL ~ ., data=semiConductData, family=binomial)
pval <- summary(full)$coefficients[-1, 4]
n_predictors <- length(pval)
pvalrank <- rank(pval)

#testing when q=0.1
reject <- ifelse(pval < (0.1/n_predictors)*pvalrank, 2, 1)
num_kept <- sum(reject == 1)
png(file="~/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/BHA1_semicond.png",
    width=600, height=350)
plot(pvalrank, pval, 
     ylab="p-value", 
     xlab="p-value rank", 
     pch=16, 
     col=reject,
     main="BH Analysis (for semiconductors)")
lines(pvalrank, (0.1/n_predictors)*pvalrank)
dev.off()
print(num_kept) #when q = 0.1, only 24 hypotheses are rejected.

#testing when q=0.05
reject <- ifelse(pval < (0.05/n_predictors)*pvalrank, 2, 1)
num_kept <- sum(reject == 1)
png(file="~/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/BHA2_semicond.png",
    width=600, height=350)
plot(pvalrank, pval, 
     ylab="p-value", 
     xlab="p-value rank", 
     pch=16, 
     col=reject,
     main="BH Analysis (for semiconductors)")
lines(pvalrank, (0.05/n_predictors)*pvalrank)
dev.off()
print(num_kept) #wwhen q = 0.05, only 24 hypotheses are rejected.

#testing when q=0.01
reject <- ifelse(pval < (0.01/n_predictors)*pvalrank, 2, 1)
num_kept <- sum(reject == 1)
png(file="~/Desktop/EUR/BLOK2/Data and HR Analytics/FEM11213/WEEK2/BHA3_semicond.png",
    width=600, height=350)
plot(pvalrank, pval, 
     ylab="p-value", 
     xlab="p-value rank", 
     pch=16, 
     col=reject,
     main="BH Analysis (for semiconductors)")
lines(pvalrank, (0.01/n_predictors)*pvalrank)
dev.off()
print(num_kept) #wwhen q = 0.01, only 4 hypotheses are rejected.
#it's not immediately clear to me what a good criterion for q is. Especially for the semiconducter dataset, with many coefficients, is it really sensible to choose q= 0.01?