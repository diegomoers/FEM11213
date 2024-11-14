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

h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browserdata$spend), sd = sqrt(var(browserdata$spend)/1e4)) 
yfit <- yfit * diff(h$mids[1:2]) * length(mub) 
#can you explain why we need each term in the last expression? 
#to find out i ran the same code, but removed diff(h$mids[1:2]) or length(mub) to see what happens to the line each time
lines(xfit, yfit, col = "black", lwd = 2)

h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browserdata$spend), sd = sqrt(var(browserdata$spend)/1e4)) 
yfit <- yfit * length(mub) 
lines(xfit, yfit, col = "black", lwd = 2)

h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40) 
yfit <- dnorm(xfit, mean = mean(browserdata$spend), sd = sqrt(var(browserdata$spend)/1e4)) 
yfit <- yfit * diff(h$mids[1:2]) 
lines(xfit, yfit, col = "black", lwd = 2)

#if we remove diff(h$mids[1:2]) or length(mub), the yfit line becomes much flatter. the reason we need both of these is because they scale the yfit line to fit the historgram. 
##more specifically:
#1. diff(h$mids[1:2]) calculates the width of each bar to make sure the yfitline aligns with it horizontally and isn't too wide or narrow
#2. length(mub) makes sure that data fits vertically 

#now, for running the regressions: #slide11
B <- 1000
betas <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browserdata), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browserdata[samp_b,])
  betas <- rbind(betas, coef(reg_b))
}; head(betas, n=3)
