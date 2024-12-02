setwd("~/Desktop/EUR/BLOK2/Data and HR Analytics")
ceo_diary <- read.csv("~/Desktop/EUR/BLOK2/Data and HR Analytics/Data/survey_response_data.csv")
#worked with setting wd

View(ceo_diary)
ceo_diary[c(1:5,7),c(1:5,14,15)]
#the c() command lets you specify which columns/rows you want.
##rows go in the first comma, then columns

apply(ceo_diary,2,class)
#tells you the kind of answers in all columns (if you wanted rows, you'd have to put 1), in this case all of them return "character".
apply(ceo_diary,1,class)

nrow(ceo_diary)
#tells you the number of rows
ncol(ceo_diary)
#tells you the number of columns (42)

summary(ceo_diary[1:5])
#gives you the descritpive stats for the first 5 columns, could do all of them through:
summary(ceo_diary)
#can aslo specify the column through name, for example:
summary(ceo_diary["id"])

##png(file="figs/ceotypes.png", width=800, height=300)
##par(mar=c(9, 3 ,1,1))
##barplot(prop.table(table(ceo_diary$type)), las=2)
##dev.off()
#copied from slides but it doesn't work bc I don't have a folder called figs, so I will just output it into my directory:
png(file="FEM11213/WEEK1/ceotypes.png", width=800, height=300)
par(mar=c(9, 3 ,1,1))
barplot(prop.table(table(ceo_diary$type)), las=2)
dev.off()

png(file="FEM11213/WEEK1/ceotypes2.png", width=800, height=300)
par(mar=c(9, 3 ,1,1))
barplot(prop.table(table(ceo_diary$type)), las=1)
dev.off()
#changing the graph, the las command just rotates label on x-axis

png(file="FEM11213/WEEK1/ceotypes3.png", width=800, height=300)
par(mar=c(1,1,1,1))
barplot(prop.table(table(ceo_diary$type)), las=2)
dev.off()
#this got rid of all axis numbers

png(file="FEM11213/WEEK1/ceotypes4.png", width=800, height=300)
par(mar=c(10,10,10,10))
barplot(prop.table(table(ceo_diary$type)), las=2)
dev.off()
#the mar command is margins, dictates how much space you want from the bottom, left, top right (in that order). so in most cases, you'd want margins from the left and bottom, but not really from top and right?

fit <- glm(strategy ~ consultants + politicians, data=ceo_diary); 
summary(fit)
#this just sets a general linear model, with strategy as the DV and consultants and politicians as the IVs.

fit2 <- glm(finance ~ mkting + production + chairman + compliance, data=ceo_diary); 
summary(fit2)
#made my own linear model
##test123