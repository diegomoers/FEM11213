################################################################################
################################################################################
################################## PART 1 ######################################
############################# Abortion Controls ################################
################################################################################
################################################################################

#copied from the slides:
data <- read.table("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),]
data <- data[data$year>84 & data$year<98,]
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state)
controls <- data.frame(data[,c(3,10:17)])
y <- data$y_murd
d <- data$a_murd

summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',]
dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]
exp(dcoef) - 1

cell <- read.csv("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985
par(mai=c(.9,.9,.1,.1))
plot(1985:1997, tapply(d, t, mean), bty="n", xlab="year", ylab="rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
phone <- cellrate[ t + 1 ]
tech <- summary(glm(y ~ phone + t + s +., data=controls))$coef['phone',]
phonecoef <- tech[1]
exp(phonecoef) - 1

t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]

library(gamlr)
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
dim(x)

naive <- cv.gamlr(cbind(d,x),y); head(coef(naive))
coef(naive)["d",]

treat <- cv.gamlr(x,d, lmr=1e-3); head(summary(treat))
predtreat <- predict(treat, x, select="min"); head(predtreat)
dhat <- drop(predtreat); length(dhat)

par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3))

cor(drop(dhat),d)^2
coef(summary( glm( y ~ d + dhat) ))
causal <- cv.gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-3)
coef(causal, select="min")["d",]

#what I want to experiment with is if I include ONLY time and state fixed effects. I want to know how much bias is in the model if we don't account for specific variables, and if that maybe gives a coefficient that is similar to the overfit one.
#first I want to start with a model that only has time and state FE:
# Minimal controls: time and state
minimal_controls <- sparse.model.matrix(~ t + s, data = controls)[, -1]
treat_minimal <- cv.gamlr(minimal_controls, d, lmr = 1e-3)
dhat_minimal <- drop(predict(treat_minimal, minimal_controls, select = "min"))
causal_minimal <- cv.gamlr(cbind(d, dhat_minimal, minimal_controls), y, free = 2, lmr = 1e-3)
coef_minimal <- coef(causal_minimal, select = "min")["d",]
print(coef_minimal)
#it is -0.122, a small effect probably because key variables (like police and pop) are missing, so it is also Likely biased since it can't separate 'd' from unobserved factors.

#Next, the fit of the model with only some (random) controls:
limited_controls <- sparse.model.matrix(~ pop + prison + police + ur, data = controls)[, -1]
treat_limited <- cv.gamlr(limited_controls, d, lmr = 1e-3)
dhat_limited <- drop(predict(treat_limited, limited_controls, select = "min"))
causal_limited <- cv.gamlr(cbind(d, dhat_limited, limited_controls), y, free = 2, lmr = 1e-3)
coef_limited <- coef(causal_limited, select = "min")["d",]
print(coef_limited)
#here, the coefficient is -0.385, which shows that time and fixed effects really are important controls.

################################################################################
################################################################################
################################## PART 2 ######################################
######################## Hetergoenous Treatment Effects ########################
################################################################################
################################################################################

load("/Users/diegomoers/Desktop/EUR/BLOK2/Data and HR Analytics/Data/dominicks-beer.rda")
head(wber)
wber = wber[sample(nrow(wber), 100000), ]
head(upc)
dim(upc)
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"]) #ln price per 12 ounces
coef( margfit <- lm(log(MOVE) ~ lp, data=wber[,]) )
wber$s <- factor(wber$STORE); wber$u <- factor(wber$UPC); wber$w <- factor(wber$WEEK)
xs <- sparse.model.matrix( ~ s-1, data=wber); xu <- sparse.model.matrix( ~ u-1, data=wber); xw <- sparse.model.matrix( ~ w-1, data=wber)
# parse the item description text as a bag o' words
library(tm)
descr <- Corpus(VectorSource(as.character(upc$DESCRIP)))
descr <- DocumentTermMatrix(descr)
descr <- sparseMatrix(i=descr$i,j=descr$j,x=as.numeric(descr$v>0), # convert from stm to Matrix format
                      dims=dim(descr),dimnames=list(rownames(upc),colnames(descr)))
descr[1:5,1:6]
descr[287,descr[287,]!=0]

controls <- cbind(xs, xu, xw, descr[wber$UPC,]) 
dim(controls)

naivefit <- gamlr(x=cbind(lp=wber$lp,controls)[,], y=log(wber$MOVE), free=1, standardize=FALSE)
print( coef(naivefit)[1:2,] )

resids <- orthoLTE( x=controls, d=wber$lp, y=log(wber$MOVE), dreg=dreg, yreg=yreg, nfold=5)
# interact items and text with price
#lpxu <- xu*wber$lp
#colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")
# create our interaction matrix
xhte <- cbind(BASELINE=1,descr[wber$UPC,])
d <- xhte*wber$lp
colnames(d) <- paste("lp",colnames(d),sep=":")
eachbeer <- xhte[match(rownames(upc),wber$UPC),]
rownames(eachbeer) <- rownames(upc)
# fullhte
lnwberMOVE <- log(wber[['MOVE']])
fullhte <- gamlr(x=cbind(d,controls), y=lnwberMOVE, lambda.start=0)
#gamfull <- coef(fullhte)[2:(ncol(lpxu)+1),]
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])
coef(fullhte)

hist(gamfull, main="", xlab="elasticity", col="darkgrey", freq=FALSE)

#for my experiment, I want to divide up beers into groups. Using ChatGPT, I used the following prompt:
print(upc$DESCRIP)
#and asked it to identify premium beers from non-premium beers.
#ChatGPT recommended the following groups:
# Create a vector of premium beer patterns
# Create premium indicators based on our list of premium brands
premium_indicators <- grepl(paste(c(
  # Import/European Premium
  "HEINEKEN", "AMSTEL", "CORONA", "BECK", "GROLSCH", "PILSNER URQUELL",
  "ST PAULI", "WARSTEINER", "MORETTI", "BASS", "GUINNESS", "HARP",
  "RED STRIPE", "DOS EQUIS", "MOOSEHEAD", "LABATTS", "MOLSON",
  
  # Craft/Specialty
  "SAMUEL ADAMS", "SIERRA NEVADA", "PETE'S WICKED", "GOOSE ISLAND",
  "OREGON BREWERY", "STATE STREET", "BADERBRAU", "LEINENKUGEL",
  "KILLIAN'S", "HORNSBY'S",
  
  # Premium Domestic
  "MICHELOB", "LOWENBRAU", "FOSTERS", "ROLLING ROCK",
  
  # Special/Limited Editions
  "BERGHOFF", "AUGSBURGER"
), collapse="|"), upc$DESCRIP)

wber$premium <- premium_indicators[match(wber$UPC, rownames(upc))]
premium_summary <- table(upc$DESCRIP, premium_indicators)

print(premium_summary)
premium_model <- lm(log(MOVE) ~ lp + factor(STORE) + factor(WEEK), 
                    data=wber[wber$premium,])
regular_model <- lm(log(MOVE) ~ lp + factor(STORE) + factor(WEEK), 
                    data=wber[!wber$premium,])

results <- data.frame(
  Beer_Type = c("Premium", "Regular"),
  Price_Elasticity = c(coef(premium_model)["lp"], coef(regular_model)["lp"]))
print(results)
#this code suggests that:
  # Premium beers: -0.89 elasticity (0.89% drop in sales for 1% price increase)
  # Regular beers: -0.47 elasticity (0.47% drop in sales for 1% price increase)
#but this suggests that premium beer buyers more price-sensitive than regular beer buyers, which is counterintuitive to what we discussed in class...
#I would think that this is because there is a lot substitutability potential going from premium brands to cheaper brands,
#and that people who are choosing cheaper beer are less price sensitive. Because of the good (alcohol), I would have thought the pattern would be the other way...
#Thus, premium beer positioning doesn't necessarily mean less price sensitivity.
