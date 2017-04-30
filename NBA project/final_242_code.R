####### IEOR 242 NBA MVP Prediction ##############
# Team 29
# Renee Gu, Nicholas Yuk

#libraries
setwd("~/NBA FUN! MVP prediction")
library(tidyverse)
library(randomForest)
library(ggplot2)
library(ROSE)
library(rpart) #CART
library(rpart.plot) # CART plotting
library(caret)
library(gridExtra)
library(cowplot)
library(ROCR)
library(MASS)
library(dplyr)
library(tidyr)
library(data.table)

#found via basketball reference
#all stats of mvp players from 1980 to 2016 seasons
#####################  MVP DATA SET  ##########################################
mvp1 <- read.csv("nba1980to2016mvppergamestats.csv")
mvp2 <- read.csv("nba1980to2016mvpper100stats.csv")
mvp3 <- read.csv("nba1980to2016mvpadvancedstats.csv")

#remove total stats that are same as mvp 1
head(mvp2)
mvp2 <- mvp2[ -c(3:9, 27:32)]
head(mvp2)

#remove total stats that are same as mvp 1
head(mvp3)
mvp3 <- mvp3[-c(3:9)]
head(mvp3)

#join the three data sets of totals, per100, and advanced
mvpdata <- left_join(mvp1, mvp2, by = ("Rk"))
mvpdata <- inner_join(mvpdata, mvp3, by = ("Rk"))
head(mvpdata)

#remove redundant player names
mvpdata <- subset(mvpdata, select= -c(Player.x, Player.y))

#move names to 2nd column of data set 
refcols <- c("Rk", "Player")
mvpdata <- mvpdata[, c(refcols, setdiff(names(mvpdata), refcols))]
head(mvpdata)

#label that these players won MVP
mvpdata$mvpwin <- rep(1,nrow(mvpdata)) # make new column 
head(mvpdata)

#remove missing values
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

delete.na(mvpdata)
mvpdata[complete.cases(mvpdata),]
mvpdata %>% drop_na() #simplest option to drop NA's

write.csv(mvpdata,"mvpdata.csv")

#####################  MVP DATA SET FINISHED ##################################

##################### ALL Players data Set  ###################################
### Players with  at least: 1600 minutes, 20 PER, 6.5 Win Shares (WS) ################
#this categorization usually gives the top 10-15 players in the league 
#and having contributed a strong amount to the team over the course of the season
#http://www.basketball-reference.com/about/ws.html for more  information about WS

all1 <- read.csv("playerspergame.csv")
all2 <- read.csv("playersper100.csv")
all3 <- read.csv("playersadvanced.csv")

head(all1)
#remove total stats that are same as all1
head(all2)
all2 <- all2[ -c(2:11, 29:35)]
head(all2)

#remove total stats that are same as all 1
head(all3)
all3 <- all3[-c(2:11)]
head(all3)

#join the three data sets of totals, per100, and advanced
alldata <- left_join(all1, all2, by = ("Rk"))
alldata <- left_join(alldata, all3, by = ("Rk"))
head(alldata)

#remove redundant player names
alldata <- subset(alldata, select= -c(Player.x, Player.y))

#move names to 2nd column of data set 
refcols <- c("Rk", "Player")
alldata <- alldata[, c(refcols, setdiff(names(alldata), refcols))]
head(alldata)
write.csv(alldata, "alldata.csv")

mvpwinners <- read.csv("mvpdata.csv", stringsAsFactors = FALSE)
sum(mvpwinners$mvpwin)
head(mvpwinners)
mvpwinners <- subset(mvpwinners, select = c(Player, Season, mvpwin))

test <- merge(alldata, mvpwinners, by = c("Player","Season"), all.y = TRUE, all.x = TRUE)
test[is.na(test)] <- 0
sum(test$mvpwin)

#remove missing values
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

delete.na(alldata)
alldata[complete.cases(alldata),]
test %>% drop_na() #simplest option to drop NA's


write.csv(test,"alldata.csv")

teamdata <- read.csv("team_rankings.csv", stringsAsFactors = FALSE)
head(teamdata)
teamdata[] <- lapply(teamdata, function(x) gsub("[*]", "", x))

teamdata2 <- subset(teamdata, select = c(Season, Tm, W, L, W.L.))
teamdata2$W <- as.numeric(teamdata2$W)
teamdata2$L <- as.numeric(teamdata2$L)
teamdata2$W.L. <- as.numeric(teamdata2$W.L.)
head(teamdata2)

alldata <- read.csv("alldata.csv", stringsAsFactors = FALSE)

df <- merge(teamdata2, alldata, by=c("Tm","Season"), all.y = TRUE)
head(df)
sum(df$mvpwin)

write.csv(df, "mergeddata.csv")

df <- read.csv("mergeddata.csv", stringsAsFactors = FALSE)
df <- subset(df, select = -c(GS))
sum(df$mvpwin)

row.has.na <- apply(df, 1, function(x){any(is.na(x))})
sum(row.has.na)
cleandata <- df[!row.has.na,]
sum(cleandata$mvpwin)

cleandata <- unique(cleandata)
sum(cleandata$mvpwin)

head(cleandata)

cleandata <- subset(cleandata, select = -c(Lg, X, X.1, Rk))

write.csv(cleandata, "cleandata.csv")
######## cleanish set ... just need to maybe remove a few rows ... maybe ###############

##### need to be above 50% win for season ###############

#change Season to a Year
library(zoo)
X4_23_finaldata$Season <- substr(X4_23_finaldata$Season, 1,4)
X4_23_finaldata$Season <- as.Date(X4_23_finaldata$Season, format = "%Y")
X4_23_finaldata$Season <- as.numeric(format(X4_23_finaldata$Season, "%Y"))
write.csv(X4_23_finaldata, "4.24.finaldata.csv")


########## EDA #####################################
data <- read.csv("4.24.finaldata.csv")
head(data)
str(data)
data$mvpwin <- as.factor(data$mvpwin)
summary(data$mvpwin)


#find columns with NAs 
naTF <- sapply(data, anyNA)
amesNaCols <- colnames(data[,naTF])
amesNaCols
#no missing values

#check any outliers?

# more EDA ******************
# Nick u talk abt how u used PER to filter data 
qplot(PTS, PER, colour = mvpwin,data = data) + ggtitle("Points Per Game correlation with PER") + theme(plot.title = element_text(hjust = 0.5)) 
#EDA
#********* no era
p1 <- data %>% ggplot(aes(x = mvpwin, y = WLRatio)) + geom_boxplot() + ggtitle("Win Loss Ratio vs MVP winners") + theme(plot.title = element_text(hjust = 0.5)) 
p1
#************
p1.1 <- data %>% ggplot(aes(x = WLRatio)) + geom_density() + ggtitle("Win Loss Ratio") + theme(plot.title = element_text(hjust = 0.5)) 
p1.1
plot_grid(p1, p1.1, ncol= 2, nrow=1)

# Player Efficiency Rating
p2 <- data %>% ggplot(aes(x = mvpwin, y = PER)) + geom_boxplot()
p2
# Win share (An estimate of the number of wins contributed by a player.)
p3 <- data %>% ggplot(aes(x = mvpwin, y = WS)) + geom_boxplot()
p3
# AST -- Assists Per Game ********** no era
p7 <- data %>% ggplot(aes(x = mvpwin, y = AST)) + geom_boxplot() + ggtitle("MVP winner vs Assists") + theme(plot.title = element_text(hjust = 0.5))
p7
# PTS -- Points Per Game ***********
p10 <- data %>% ggplot(aes(x = mvpwin, y = PTS)) + geom_boxplot() + ggtitle("MVP winner vs Points Per Game") + theme(plot.title = element_text(hjust = 0.5))
p10
plot_grid(p7,p10,ncol = 2, nrow = 1)
# USG% -- Usage Percentage 
p12 <- data %>% ggplot(aes(x = mvpwin, y = USG.)) + geom_boxplot()
p12
# ORtg -- Offensive Rating ******* by era
p13 <- data %>% ggplot(aes(x = mvpwin, y = ORtg, fill= era)) + geom_boxplot() +
  scale_y_continuous(name = "Offensive Rating",
                     breaks = seq(0, 30, 55),
                     limits=c(90, 140)) +
  scale_x_discrete(name = "mvpwin") +
  ggtitle("Offensive Rating vs Winning MVP") +
  theme_bw() +
  theme(
    legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "era")
p13
# DRtg -- Defensive Rating ?******
p14 <- data %>% ggplot(aes(x = mvpwin, y = DRtg, fill = era)) + geom_boxplot() +
  scale_y_continuous(name = "Offensive Rating",
                     breaks = seq(0, 30, 55),
                     limits=c(90, 140)) +
  scale_x_discrete(name = "mvpwin") +
  ggtitle("Defensive Rating vs Winning MVP") +
  theme_bw() +
  theme(
    legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "era")
p14

plot_grid(p13,p14, ncol = 2)

# BPM -- Box Plus/Minus ********* no era
p14.1 <- data %>% ggplot(aes(x = mvpwin, y = BPM)) + geom_boxplot() + ggtitle("Box Plus Minus vs Winning MVP") + theme(plot.title = element_text(hjust = 0.5))
p14.1
# VORP -- Value over Replacement Player *********
p15 <- data %>% ggplot(aes(x = mvpwin, y = VORP)) + geom_boxplot() + ggtitle("Value Over Replacement Player vs Winning MVP") + theme(plot.title = element_text(hjust = 0.5))
p15

plot_grid(p14.1, p15, ncol=2)

########### some correlated variables
### last slide interesting fact
data %>% ggplot(aes(x = Season, y = MP)) + geom_line() +geom_smooth() + ggtitle("Minutes played during a Season")
####combine these two
p16 <- data %>% ggplot(aes(x = Season, y = X3PA)) + geom_line() +geom_smooth() +ggtitle("Number of 3s per Game")
p17 <- data %>% ggplot(aes(x = Season, y = X3P.)) + geom_line() +geom_smooth() +ggtitle("Percentage of 3s made")

plot_grid(p16,p17, nrow= 2)

######################################################################
#split train and test
data$previous.winner <- as.factor(data$previous.winner)
data$Player <- NULL
data$Tm <- NULL
data$era <- NULL
train <- data %>% filter(Season<2006)
test <- data %>% filter(Season>=2006)
train$Season <- NULL
test$Season <- NULL
str(train)
table(train$mvpwin)

#Resample
#over sampling
data_balanced_over <- ovun.sample(mvpwin ~ ., data = train, 
                                  method = "over",N = 724, seed = 1)$data
table(data_balanced_over$mvpwin)
#down sampling
data_balanced_under <- ovun.sample(mvpwin ~ ., data = train, 
                                  method = "under",N = 52,seed = 1)$data
table(data_balanced_under$mvpwin)
#both way sampling
data_balanced_both <- ovun.sample(mvpwin ~ ., data = train, 
                                   method = "both",N = 600,seed = 1)$data
table(data_balanced_both$mvpwin)

#################Logistic regression 
mod0 <- glm(mvpwin ~ Age + WLRatio + MP + PER +G + MPG +TRB + AST + STL + 
              BLK +PTS + TS. + USG. + ORtg + DRtg + WS + BPM + VORP+
              previous.winner, data = train, family = "binomial")
summary(mod0)

pred0 = predict(mod0, newdata=test, type = "response")
summary(pred0)
table(test$mvpwin, pred0 > 0.1)
## FALSE TRUE
##0   168    5
##1     2    8

rocr.log.pred <- prediction(pred0, test$mvpwin)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, col = "blue")
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)
#AUC VALUE : 0.9786127

##########################CART 
## using over sampling data
set.seed(12367)
loss.mat <- cbind(c(0, 1), c(10, 0)) 
mod1 <- rpart(mvpwin ~ .,
             data = data_balanced_over, method="class", 
             parms=list(loss = loss.mat),
             minbucket=5, cp = 0.02)
prp(mod1, digits=3) # plots the tree
pred1 <- predict(mod1, newdata = test, type = "class")
table(test$mvpwin, pred1) #168,5,4,6
table(test$mvpwin)

## with CV
Loss = function(data, lev = NULL, model = NULL, ...) {
  c(AvgLoss = mean(data$weights * (data$obs != data$pred)),
    Accuracy = mean(data$obs == data$pred))
}
trControl = trainControl(method = "cv",
                         number = 10,
                         summaryFunction = Loss)
weights = ifelse(data_balanced_over$mvpwin == 1, 1, 10)
cpVals = data.frame(cp = seq(0,0.04, by=0.002))
mod2 <- train(mvpwin ~ .,data=data_balanced_over,
                 method="rpart",
                 weights = weights,
                 trControl=trControl,
                 tuneGrid=cpVals, 
                 metric="AvgLoss", 
                 maximize=FALSE)
mod2 = mod2$finalModel
prp(mod2, digits=3)
# make predictions
test.mm = as.data.frame(model.matrix(mvpwin~., data=test))
pred2 = predict(mod2, newdata=test.mm, type="class")
table(test$mvpwin, pred2) #169,4,5,5


### using under sampling data
## with CV
weights = ifelse(data_balanced_under$mvpwin == 1, 1, 10)
mod4 <- train(mvpwin ~ .,data=data_balanced_under,
              method="rpart",
              weights = weights,
              trControl=trControl,
              tuneGrid=cpVals, 
              metric="AvgLoss", 
              maximize=FALSE)
mod4 = mod4$finalModel
prp(mod4, digits=3)
# make predictions
test.mm = as.data.frame(model.matrix(mvpwin~., data=test))
pred4 = predict(mod4, newdata=test.mm, type="class")
table(test$mvpwin, pred4) #160,13,3,7

### using both way sampling data
## with CV
weights = ifelse(data_balanced_both$mvpwin == 1, 1, 10)
mod5 <- train(mvpwin ~ .,data=data_balanced_both,
              method="rpart",
              weights = weights,
              trControl=trControl,
              tuneGrid=cpVals, 
              metric="AvgLoss", 
              maximize=FALSE)
mod5 = mod5$finalModel
prp(mod5, digits=3)
# make predictions
test.mm = as.data.frame(model.matrix(mvpwin~., data=test))
pred5 = predict(mod5, newdata=test.mm, type="class")
table(test$mvpwin, pred5) #168,5,3,7

#AUC CART
rocr.CART.pred <- prediction(as.numeric(pred5), as.numeric(test$mvpwin))
CARTPerformance <- performance(rocr.CART.pred, "tpr", "fpr")
plot(CARTPerformance, col = "red", add = TRUE)
abline(0, 1)
as.numeric(performance(rocr.CART.pred, "auc")@y.values)
#0.8355491


#####Random Forests
# over sampling 
set.seed(12367)
mod.rf <- randomForest(mvpwin ~ ., data = data_balanced_over, mtry = 5, nodesize = 5, ntree = 500)
pred.rf <- predict(mod.rf, newdata = test) 
importance(mod.rf)
table(test$mvpwin,pred.rf)
# RF with CV
train.rf <- train(mvpwin ~ .,
                  data = data_balanced_over,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:25),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "Accuracy")
best.rf1 <- train.rf$finalModel
pred.best.rf1 <- predict(best.rf1, newdata = test.mm) # can use same model matrix
table(test$mvpwin,pred.best.rf1)
# pred.best.rf1
#    0   1
#0 171   2
#1  10   0

# under sampling
train.rf2 <- train(mvpwin ~ .,
                  data = data_balanced_under,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:25),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "Accuracy")
best.rf2 <- train.rf2$finalModel
pred.best.rf2 <- predict(best.rf2, newdata = test.mm) # can use same model matrix
table(test$mvpwin,pred.best.rf2)
#pred.best.rf2
#   0   1
#0 150  23
#1   2   8
rocr.rf.pred <- prediction(as.numeric(pred.best.rf2), as.numeric(test$mvpwin))
rfPerformance <- performance(rocr.rf.pred, "tpr", "fpr")
plot(rfPerformance, col = "hot pink", add = TRUE)
abline(0, 1)
as.numeric(performance(rocr.rf.pred, "auc")@y.values)
#0.833526

# both sampling
train.rf3 <- train(mvpwin ~ .,
                   data = data_balanced_both,
                   method = "rf",
                   tuneGrid = data.frame(mtry=1:25),
                   trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                   metric = "Accuracy")
best.rf3 <- train.rf3$finalModel
pred.best.rf3 <- predict(best.rf3, newdata = test.mm) # can use same model matrix
table(test$mvpwin,pred.best.rf3)
#  pred.best.rf3
#    0   1
#0 169   4
#1  10   0

#AUC combined
plot(logPerformance, col = "blue", main = "ROC Curve Comparison Chart")
plot(rfPerformance, col = "dark green", add = TRUE)
plot(CARTPerformance, col = "red", add = TRUE) 
legend("bottomright", legend = c("Logistic Reg", "CART", "Random Forest"), 
                         col = c("blue", "red", "dark green"), lty = 1:1, cex = 0.8)

abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)
#AUC VALUE : 0.9786127
as.numeric(performance(rocr.CART.pred, "auc")@y.values)
#0.8355491
as.numeric(performance(rocr.rf.pred, "auc")@y.values)
#0.833526

#####h2o chug chug chug################
library(h2o)
localH2O <- h2o.init(nthreads = -1)

# Transfer data from R to h2o instance
train1.h2o <- as.h2o(data_balanced_over)
train2.h2o <- as.h2o(data_balanced_under)
train3.h2o <- as.h2o(data_balanced_both)
test.h2o <- as.h2o(test)

# Check column index number
colnames(train1.h2o)
# dependent variable (salary)
y.dep <- 69
# independent variables 
x.indep <- c(1:68,70)

#####Deep learning model
# over sampling
dlearning.model1 <- h2o.deeplearning(y = y.dep,x = x.indep,
                                    training_frame = train1.h2o,
                                    epoch = 60,
                                    hidden = c(100,100),
                                    activation = "Rectifier",
                                    seed = 777)
h2o.performance(dlearning.model1)
predict.dl1 <- as.data.frame(h2o.predict(dlearning.model1, test.h2o))
#170,3,8,2 BY ROW


# under sampling
dlearning.model2 <- h2o.deeplearning(y = y.dep,x = x.indep,
                                     training_frame = train2.h2o,
                                     epoch = 60,
                                     hidden = c(100,100),
                                     activation = "Rectifier",
                                     seed = 777)
h2o.performance(dlearning.model2)
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model2, test.h2o))
#163,10,6,4


# both sampling
dlearning.model3 <- h2o.deeplearning(y = y.dep,x = x.indep,
                                     training_frame = train3.h2o,
                                     epoch = 60,
                                     hidden = c(100,100),
                                     activation = "Rectifier",
                                     seed = 777)
h2o.performance(dlearning.model3)
predict.dl3 <- as.data.frame(h2o.predict(dlearning.model3, test.h2o))
#170,3,6,4

h2o.shutdown(prompt =FALSE)

###########  predict 2017 #############################
test2017$previous.winner=as.factor(test2017$previous.winner)
pred2017 = predict(mod0, newdata=test2017, type = "response")
summary(pred2017)
pred2017/sum(pred2017)

table(test$mvpwin, pred0 > 0.05)
