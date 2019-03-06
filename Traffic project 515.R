#FIRST PLOT
#setwd("/Users/jinusebastin/desktop/RZone")
#traffic <- read.csv(file="TrafficFatality.csv", header=TRUE, row.names=1)
#library("lattice") 
AlcoholCrashes <- c(1713,195,2045,417,317,1567,527,701)
 AlcoholInjuries <- c(1057,140,1275,286,191,1146,303,457)
 AlcoholFatalities <- c(75,12,44,29,12,40,21,29)
 SpeedCrashes <- c(5050,451,7552,1188,1609,4414,1903,2097)
 SpeedInjuries <- c(2740,309,3936,772,1052,2757,927,1109)
 SpeedFatalities <- c(83,13,56,39,26,40,27,32)
 UnrestrainedCrashes <- c(1283,183,899,423,389,824,452,539)
 UnrestrainedInjuries <- c(999,142,692,343,322,627,319,400)
 UnrestrainedFatalities <- c(82,13,38,50,20,39,28,34)
 MotorcyclistCrashes <- c(379,48,544,89,192,170,341,236)
 MotorcyclistInjuries <- c(329,43,485,80,185,150,300,200)
 MotorcyclistFatalities <- c(11,3,9,10,8,7,18,11)
 region <- c("Central","Eastern","Northern","Southside","Southwest","Hampton roads","Valley","West Central")
 reason <- c("AlcoholCrashes","AlcoholInjuries","SpeedCrashes","SpeedInjuries","UnrestrainedCrashes","UnrestrainedInjuries","MotorcyclistCrashes","MotorcyclistInjuries")
 matt <- cbind(AlcoholCrashes,AlcoholInjuries,SpeedCrashes,SpeedInjuries,UnrestrainedCrashes,UnrestrainedInjuries,MotorcyclistCrashes,MotorcyclistInjuries)
 colnames(matt) <- reason
 rownames(matt) <- region
 matt
dotplot(matt,groups=FALSE,
        layout=c(2,4),aspect=.7,
       origin=0,type=c("p","h"),
        main="Virginia traffic fatality",
        xlab="Accidents per 100,000 population",
        scales=list(x=list(tck=0, alternating=FALSE)),
       panel=function(...){
         panel.fill(rgb(.9,.9,.9))
           panel.grid(h=0,v=-1,col="white",lwd=2)
         panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)
        }
)
dotplot(matt[8:1,8:1],groups=FALSE,
        layout=c(2,4),aspect=.7,
       origin=0,type=c("p","h"),
          main="    Virginia traffic crash/injury by region",
         xlab="Incidents per 100,000 population",
          scales=list(x=list(tck=0, alternating=FALSE)),
         panel=function(...){
         panel.fill(rgb(.9,.9,.9))
         panel.grid(h=0,v=-1,col="white",lwd=2)
          panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)+hw
      }
)
#SECOND PLOT
reason <- c("AlcoholFatalities","SpeedFatalities","UnrestrainedFatalities","MotorcyclistFatalities")
matt <- cbind(AlcoholFatalities,SpeedFatalities,UnrestrainedFatalities,MotorcyclistFatalities)
colnames(matt) <- reason
rownames(matt) <- region
matt
dotplot(matt,groups=FALSE,
        layout=c(2,2),aspect=.7,
        origin=0,type=c("p","h"),
        main="Virginia traffic fatality",
        xlab="Accidents per 100,000 population",
        scales=list(x=list(tck=0, alternating=FALSE)),
        panel=function(...){
          panel.fill(rgb(.9,.9,.9))
          panel.grid(h=0,v=-1,col="white",lwd=2)
          panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)
        }
)
dotplot(matt[8:1,4:1],groups=FALSE,
        layout=c(2,2),aspect=.7,
        origin=0,type=c("p","h"),
        main="Virginia traffic fatalities by region",
        xlab="Incidents per 100,000 population",
        scales=list(x=list(tck=0, alternating=FALSE)),
        panel=function(...){
          panel.fill(rgb(.9,.9,.9))
          panel.grid(h=0,v=-1,col="white",lwd=2)
          panel.dotplot(col=rgb(0,.5,1),cex=1.1,...)+hw
        }
)
#THIRD PLOT
setwd("/Users/jinusebastin/desktop/RZone")
virginia <- read.csv("Book3.csv")
ggplot(data=virginia,aes(x=Regions,y=Total))+ geom_boxplot(fill="salmon1")+
  ylab(label = "Total fatal incidents per 100,000 people")+
  xlab("Regions") + labs(title="Boxplot - Total fatalities in Virginia by Region")+hw
#use hw(1).r from RZone
#FOURTH PLOT
traffic <- read.csv(file="TrafficFatality.csv", header=TRUE, row.names=1)
dim(traffic)
colnames(traffic)
set.seed(37)
trafficRf <- randomForest(x = traffic[ , 3:17], y=traffic[, 20],
                          importance=TRUE,  proximity=FALSE, ntree=500,
                          keepForest=TRUE)
imp <- importance(trafficRf)
n <- 15
ordr1 <- order(imp[, 1],decreasing=TRUE)
name1 <- row.names(imp[ordr1, ])[1:n]
ordr2 <- order(imp[, 2],decreasing=TRUE)
name2 <- row.names(imp[ordr2,])[1:n]
varyName <- union(name1,name2)
checkCory <- round( cor(traffic[,varyName],
                        method="spearman"),2)
cor(traffic[,varyName])
M <- cor(traffic[,varyName])
corrplot(M, method = "shade")
#variable selection
tra.lm = lm(TotalFatalities~.,data=traffic)
step(tra.lm,scope=formula(tra.lm),direction = "backward")
#FIFTH PLOT
splom(traffic[,c(20,3:5,7,8,10,14,15)], as.matrix = TRUE,
      xlab = '',main = "Scatterplot matrix - Hexagon binning\n and smoothes ",
      pscale = 0, varname.col = "red",
      varname.cex = 0.56, varname.font = 2,
      axis.text.cex = 0.4, axis.text.col = "red",
      axis.text.font = 2, axis.line.tck = .5,
      panel = function(x,y,...) {
        panel.grid(h = -1,v = -1,...)
        panel.hexbinplot(x,y,xbins = 12,...,
                         border = gray(.7),
                         trans = function(x)x^1)
        panel.loess(x , y, ...,
                    lwd = 2,col = 'purple')
      },
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm = TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d,col = gray(.8),lwd = 2)
        diag.panel.splom(x, ...)
      }
)
#SIXTH PLOT
new1 <- read.csv("central.csv")
plt1 <-  ggplot(new1,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the Central region - Virginia") + hw
plt1
ptLabs1 <- new1 %>% filter(Jurisdiction %in%
                             c('Henrico County','Charles city County'))
ptLabs1
plt1 + ggrepel::geom_label_repel(data = ptLabs1,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
new2 <- read.csv("eastern.csv")
plt2 <-  ggplot(new2,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the Eastern region - Virginia") + hw
plt2
ptLabs2 <- new2 %>% filter(Jurisdiction %in%
                             c('Accomack County','Northumberland County'))
ptLabs2
plt2 + ggrepel::geom_label_repel(data = ptLabs2,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
new3 <- read.csv("northern.csv")
plt3 <-  ggplot(new3,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the Northern region - Virginia") + hw
plt3
ptLabs3 <- new3 %>% filter(Jurisdiction %in%
                             c('Fairfax County','Manassas Park City'))
ptLabs3
plt3 + ggrepel::geom_label_repel(data = ptLabs3,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
new4 <- read.csv("southside.csv")
plt4 <-  ggplot(new4,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the Southside region - Virginia") + hw
plt4
ptLabs4 <- new4 %>% filter(Jurisdiction %in%
                             c('Danville City','Charlotte County'))
ptLabs4
plt4 + ggrepel::geom_label_repel(data = ptLabs4,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
new5 <- read.csv("southwest.csv")
plt5 <-  ggplot(new5,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the Southwest region - Virginia") + hw
plt5
ptLabs5 <- new5 %>% filter(Jurisdiction %in%
                             c('washington County','Norton City'))
ptLabs5
plt5 + ggrepel::geom_label_repel(data = ptLabs5,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
new6 <- read.csv("hampton.csv")
plt6 <-  ggplot(new6,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the Hampton roads region - Virginia") + hw
plt6
ptLabs6 <- new6 %>% filter(Jurisdiction %in%
                             c('Virginia Beach City','Poquoson City'))
ptLabs6
plt6 + ggrepel::geom_label_repel(data = ptLabs6,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
new7 <- read.csv("valley.csv")
plt7 <-  ggplot(new7,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the Valley region - Virginia") + hw
plt7
ptLabs7 <- new7 %>% filter(Jurisdiction %in%
                             c('Frederick County','Buena Vista City'))
ptLabs7
plt7 + ggrepel::geom_label_repel(data = ptLabs7,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
new8 <- read.csv("westcentral.csv")
plt8 <-  ggplot(new8,
                aes(x = TotalCrash, y = TotalFatality)) +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
              size=1.5,color="blue") +
  geom_point(shape=20,size=3,color="black",fill="red") +
  labs(x="Total crashes",
       y="Total death",
       title="Total Crash in the West central region - Virginia") + hw
plt8
ptLabs8 <- new8 %>% filter(Jurisdiction %in%
                             c('Montgomery County','Craig County'))
ptLabs8
plt8 + ggrepel::geom_label_repel(data = ptLabs8,
                                 aes(label = Jurisdiction),
                                 nudge_y = .45)
#SEVENTH PLOT
setwd("/Users/jinusebastin/desktop/RZone")
trafficRan <- read.csv(file="TrafficRFdata.csv", header=TRUE, row.names=1)
dim(trafficRan)
colnames(trafficRan)
set.seed(37)
trafficRf <- randomForest(x = trafficRan[ , 1:10], y=trafficRan[, 11],
                          importance=TRUE,  proximity=FALSE, ntree=500,
                          keepForest=TRUE)
trafficRf
imp <- importance(trafficRf)
imp
varImpPlot(trafficRf,cex=.8)

n <- 10
ordr1 <- order(imp[, 1],decreasing=TRUE)
name1 <- row.names(imp[ordr1, ])[1:n]
name1
ordr2 <- order(imp[, 2],decreasing=TRUE)
name2 <- row.names(imp[ordr2,])[1:n]
name2
varyName <- union(name1,name2)
varyName
checkCory <- round( cor(trafficRan[,varyName],
                        method="spearman"),2)
checkCory
samp <- sample(nrow(trafficRan),0.6*nrow(trafficRan))
train <- trafficRan[samp,]
test <- trafficRan[-samp,]
model <- randomForest(TotalFatalities~.,data=train,mtry=10,importance=TRUE)
model
varImpPlot(model)
pred <- predict(model,newdata = test)
plot(pred,test$TotalFatalities)
abline(0,1)
mean((pred-test$TotalFatalities)^2)
set.seed(37)
rf.traffic <- randomForest(TotalFatalities~.,data=train,mtry=5,importance=TRUE)
rf.traffic
varImpPlot(rf.traffic)
test.rf <- predict(rf.traffic, newdata = test)
plot(test.rf,test$TotalFatalities)
abline(0,1)
mean((test.rf-test$TotalFatalities)^2)
## END ##
trafficRf1 <- randomForest(x = trafficRan[ , 1:10], y=trafficRan[, 11],
                          importance=TRUE,  proximity=FALSE, ntree=25,
                          keepForest=TRUE)
trafficRf1
imp <- importance(trafficRf1)
imp
varImpPlot(trafficRf,cex=.8)

n <- 10
ordr1 <- order(imp[, 1],decreasing=TRUE)
name1 <- row.names(imp[ordr1, ])[1:n]
name1
ordr2 <- order(imp[, 2],decreasing=TRUE)
name2 <- row.names(imp[ordr2,])[1:n]
name2
varyName <- union(name1,name2)
varyName
checkCory <- round( cor(trafficRan[,varyName],
                        method="spearman"),2)
checkCory
samp1 <- sample(nrow(trafficRan),0.6*nrow(trafficRan))
train1 <- trafficRan[samp1,]
test1 <- trafficRan[-samp1,]
model1 <- randomForest(TotalFatalities~.,data=train1,mtry=10,importance=TRUE)
model1
varImpPlot(model1)
pred1 <- predict(model1,newdata = test1)
plot(pred1,test$TotalFatalities)
abline(0,1)
mean((pred1-test$TotalFatalities)^2)
set.seed(1)
rf.traffic1 <- randomForest(TotalFatalities~.,data=train1,mtry=5,importance=TRUE)
rf.traffic1
varImpPlot(rf.traffic1)
test1.rf <- predict(rf.traffic1, newdata = test1)
plot(test1.rf,test$TotalFatalities)
abline(0,1)
mean((test1.rf-test$TotalFatalities)^2)



