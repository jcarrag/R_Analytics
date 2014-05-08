alldat <- read.csv("~/Desktop/Data.csv")

# Dynamic: 1 = dynamic, 2 = static
# Foregone: 1 = none, 2 = max, 3 = all

# need to check that rounding is the same as in javascript...
alldat$Card1.R <- round(alldat$Card1.R)
alldat$Card2.R <- round(alldat$Card2.R)
alldat$Card3.R <- round(alldat$Card3.R)
alldat$Card4.R <- round(alldat$Card4.R)

alldat$maxVal <- apply(alldat[,paste("Card",1:4,".R",sep="")],1,max)
alldat$loss <- alldat$maxVal - alldat$Selected.Value
alldat$switch <- as.numeric(rbind(0,apply(matrix(alldat$Card.Selection,nrow=200),2,diff)) != 0)
alldat$switch <- as.numeric(alldat$switch)
alldat$hasLoss <- as.numeric(alldat$loss != 0)
alldat$Dynamic <- factor(alldat$Dynamic,labels=c("volatile","static")) 
alldat$Foregone <- factor(alldat$Foregone,labels=c("none","max","all"))
alldat$Seed <- factor(alldat$Seed,labels=c("s72", "s21", "s26", "s135", "s332"))

alldat$block <- cut(alldat$TrialNo.,4)
alldat$block <- factor(alldat$block,labels=paste("block",1:4,sep=""))
alldat$prevLoss <- as.numeric(rbind(NA,matrix(alldat$loss[alldat$Trial. != 1],nrow=199)))

save(alldat,file="~/Dropbox/Dissertation/Working_folder/alldat.RData")

# RM ANOVA on loss (not incl. Seed - saw no significance)
require(car)
require(reshape)
mdat <- melt(alldat[,c("WorkerID","Trial.","block","Dynamic","Foregone","loss")],id=c("WorkerID","Trial.","block","Dynamic","Foregone"),measured="loss")
tdat <- cast(mdat,WorkerID+Dynamic+Foregone ~ block,mean)
idata <- data.frame(block=ordered(c(1,2,3,4)))
#contrasts(idata$hand) <- contr.sum(2)
contrasts(tdat$Dynamic) <- contr.sum(2)
contrasts(tdat$Foregone) <- contr.sum(3)
mod <- lm(cbind(block1,block2,block3,block4) ~ Dynamic*Foregone,data=tdat)
summary(Anova(mod,idata=idata,idesign=~block,type="III"),multivariate=FALSE)

# ANOVA on Cumulative (200 slice)
anova(lm(Selected.Cumulative ~ Foregone,data=alldat,subset=Trial. == 200 & Dynamic == "static"))
# use the one below to replicate SPSS
Anova(lm(Selected.Cumulative ~ Foregone,data=alldat,subset=Trial. == 200 & Dynamic == "static"),type="III")

#tdat <- subset(alldat,Trial. == 200 & Dynamic == "static")
summary(lm(alldat$Selected.Cumulative[seq(200, 25000, by=200)] ~ alldat$Foregone[seq(200, 25000, by=200)]))
summary(lm(alldat$Selected.Cumulative[seq(200, 25000, by=200)] ~ alldat$Dynamic[seq(200, 25000, by=200)]))



# RM ANOVA on loss
require(car)
require(reshape)
mdat <- melt(alldat[,c("WorkerID","Trial.","block","Dynamic","Foregone","loss")],id=c("WorkerID","Trial.","block","Dynamic","Foregone"),measured="loss")
tdat <- cast(mdat,WorkerID+Dynamic+Foregone ~ block,mean)
idata <- data.frame(block=ordered(c(1,2,3,4)))
#contrasts(idata$hand) <- contr.sum(2)
contrasts(tdat$Dynamic) <- contr.sum(2)
contrasts(tdat$Foregone) <- contr.sum(3)
mod <- lm(cbind(block1,block2,block3,block4) ~ Dynamic*Foregone,data=tdat)
summary(Anova(mod,idata=idata,idesign=~block,type="III"),multivariate=FALSE)
# ANOVA on loss (ignoring repeated measure design of block)
mdat <- melt(alldat[,c("WorkerID","Trial.","Dynamic","Foregone","loss")],id=c("WorkerID","Trial.","Dynamic","Foregone"),measured="loss")
tdat <- cast(mdat,WorkerID+Dynamic+Foregone ~ .,mean)
contrasts(tdat$Dynamic) <- contr.sum(2)
contrasts(tdat$Foregone) <- contr.sum(3)
mod <- lm(tdat$"(all)" ~ Dynamic*Foregone,data=tdat)
summary(Anova(mod,type="III"))



# ANOVA on cumulative
require(car)
require(reshape)
mdat <- melt(alldat[,c("WorkerID","Trial.","block","Dynamic","Foregone","Selected.Cumulative")],id=c("WorkerID","Trial.","block","Dynamic","Foregone"),measured="Selected.Cumulative")
tdat <- cast(mdat,WorkerID+Dynamic+Foregone ~ block,mean,subset = Trial.==200)
#idata <- data.frame(block=ordered(c(1,2,3,4)))
#contrasts(idata$hand) <- contr.sum(2)
contrasts(tdat$Dynamic) <- contr.sum(2)
contrasts(tdat$Foregone) <- contr.sum(3)
mod <- lm(block4 ~ Dynamic*Foregone,data=tdat)
summary(Anova(mod,type="III"),multivariate=FALSE)



# RM ANOVA on switches
require(car)
require(reshape)
mdat <- melt(alldat[,c("WorkerID","Trial.","block","Dynamic","Foregone","switch")],id=c("WorkerID","Trial.","block","Dynamic","Foregone"),measured="switch")
## DOUBLE CHECK THE ARC SINE TRANSFORMATION!
tdat <- cast(mdat,WorkerID+Dynamic+Foregone ~ block,function(x) {asin(sqrt(mean(x))) })
write.csv(tdat,file="RMswitch.csv",row.names=FALSE)
idata <- data.frame(block=ordered(c(1,2,3,4)))
#contrasts(idata$hand) <- contr.sum(2)
contrasts(tdat$Dynamic) <- contr.sum(2)
contrasts(tdat$Foregone) <- contr.sum(3)
mod <- lm(cbind(block1,block2,block3,block4) ~ Dynamic*Foregone,data=tdat)
summary(Anova(mod,idata=idata,idesign=~block,type="III"),multivariate=FALSE)



# Logisitc regression for specific conditions, find inflection point
bi=glm(switch ~ prevLoss, family=binomial(link = "logit"), alldat, subset= Foregone == "all" & Dynamic == "static")
(log(0.5/(1-0.5)) - coef(bi)[1]) / coef(bi)[2] # Calculate inflection point (p=0.5)
plot(alldat$switch ~ alldat$loss, subset= alldat$Foregone == "all" & alldat$Dynamic == "static")
curve(predict(bi,data.frame(loss=x),type="resp"),add=TRUE)
abline(h=.5,col="green")
##logi.hist.plot(alldat$loss,alldat$switch,boxp=FALSE,type="hist",col="gray") # Collapse across conditions



# Mixed-effects logistic regression
require(lme4)
tdat <- cast(mdat,WorkerID+Dynamic+Foregone+block ~ .,sum)
colnames(tdat) <- c(colnames(tdat)[-5],"switch")
mod <- glmer(cbind(switch,50) ~ Dynamic*Foregone*block + (1|WorkerID),family=binomial(), data=tdat, contrasts=list(Dynamic="contr.sum",Foregone="contr.sum",block="contr.poly"))
Anova(mod,type=3)



# Maarten's Logit Regression - Find Switch point. Effect of prevLoss on proportions of switches
require(car)
## make a new variable with the previous loss (NA when trial = 1)
alldat$prevLoss <- as.numeric(rbind(NA,matrix(alldat$loss[alldat$Trial. != 1],nrow=199)))
## only use the data from trial > 1 and not Foregone == "none" and dynamic == "static"
mod <- glm(switch~prevLoss*Foregone*Dynamic,family=binomial(),data=alldat,subset=Trial. != 1 & Foregone %in% c("max","all"))
## usual summary function to get the coefficients
summary(mod)
## ANOVA type results
Anova(mod,type=3)
# check model in different conditions as follow-up
summary(glm(switch~prevLoss,family=binomial(),data=alldat,subset=Trial. != 1 & Foregone %in% c("max","all") & Dynamic == "static" & Foregone == "max"))
summary(glm(switch~prevLoss,family=binomial(),data=alldat,subset=Trial. != 1 & Foregone %in% c("max","all") & Dynamic == "static" & Foregone == "all"))
summary(glm(switch~prevLoss,family=binomial(),data=alldat,subset=Trial. != 1 & Foregone %in% c("max","all") & Dynamic == "volatile" & Foregone == "max"))
summary(glm(switch~prevLoss,family=binomial(),data=alldat,subset=Trial. != 1 & Foregone %in% c("max","all") & Dynamic == "volatile" & Foregone == "all"))



# Plots (GGPlot2)
require(ggplot2)
require(grid)
require(RColorBrewer)
require(reshape)
require(lme4)
require(car)
myCols <- brewer.pal(6, "Blues")[-c(1,2)]
bgCol <- rgb(0.8476562,0.8359375,0.7968750)
myTheme <- theme_bw() + theme(legend.direction = "horizontal",legend.position = "top",legend.key=element_rect(colour="white"),strip.background = element_rect(fill = bgCol),plot.margin=unit(c(0,0,0,0),units="mm"),legend.margin=unit(0,"mm"))

p <- ggplot(alldat, aes(x=prevLoss, y=switch))
#p <- p + geom_point(na.rm=TRUE) # Binary values
p <- p + stat_smooth(aes(x=prevLoss, y=switch),  method="glm", family="binomial", na.rm=TRUE)
p <- p + facet_grid(Dynamic ~ Foregone)
p