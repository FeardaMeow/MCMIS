# install.packages("randomForest")
# install.packages("DMwR")
# install.packages("nnet")
#install.packages("e1071")

library(DMwR)


MCMIS <- '/home/liujundi/Desktop/truck_crash/MCMIS/data'
setwd(MCMIS)
crash <- fread('OurCrash.csv', header = T, sep ="auto", sep2 = "auto")

summary(crash)

dotnumbers <- unique(crash$DOT_NUMBER)
injuries <- (crash$INJURIES)
fatalities <- (crash$FATALITIES)
severity <- (crash$SEVERITY_WEIGHT)
date <- (crash$REPORT_DATE)

t <- NULL
for (i in 1:length(date)){
  t <- c(t, as.Date( strptime(date[i], "%m/%d/%Y")))
}

for (i in 1:length(date)){
  t[i] <- t[i] - 16036
}

n <- NULL
label <- NULL
for (dotnumber in dotnumbers) {
  n <- c(n, length(crash$INJURIES[crash$DOT_NUMBER == dotnumber]))
  label <- c(label, dotnumber)
}
l <- data.frame(label,n)
l_order <- l[order(l[,2]),]
s <- 0
class_1 <- NULL
class_2 <- NULL
class_3 <- NULL
class_4 <- NULL
class_5 <- NULL
for (j in 1:90480){
  s <- s + l_order$n[j]
  if (s<=45172){
    class_1 <- c(class_1, l_order$label[j])
  } else if (s>45172 && s<=90345) {
    class_2 <- c(class_2, l_order$label[j])
  } else if (s>90345 && s<=135517) {
    class_3 <- c(class_3, l_order$label[j])
  } else if (s>135517 && s<=180690) {
    class_4 <- c(class_4, l_order$label[j])
  } else if (s>180690 && s<=225863) {
    class_5 <- c(class_5, l_order$label[j])
  }
}

d <- (crash$DOT_NUMBER)
target <- NULL
for (i in 1:length(d)) {
  if (d[i] %in% class_1) {
    target <- c(target, 0)
  } else if (d[i] %in% class_2) {
    target <- c(target, 1)
  } else if (d[i] %in% class_3) {
    target <- c(target, 2)
  } else if (d[i] %in% class_4) {
    target <- c(target, 3)
  } else if (d[i] %in% class_5) {
    target <- c(target, 4)
  }
}

outp <- factor(target)
dot <- data.frame(injuries, fatalities, t, outp)


training = dot
ind <- sample(2, nrow(dot), replace = TRUE, prob = c(0.7, 0.3))
traindata <- training[ind==1,]
testdata <- training[ind==2,]
train <- traindata[sample(1:nrow(traindata), size = 700, replace = FALSE),]
test <- testdata[sample(1:nrow(testdata), size = 300, replace = FALSE),]
# set.seed(71)

# Random Forest
library(randomForest)
dot.rf <- randomForest(outp~., data = traindate, ntree = 10, proximity = TRUE)
dot.pred = predict(dot.rf, testdate)
table(observed=testdate$outp,predicted = dot.pred)
detach(package:randomForest)

# Artificial Neural Network
library(nnet)
model.nnet <- nnet(outp~., lineout = F, size = 100, decay = 0.01, maxit = 10000, trace = F, data = train)
pre.ANN <- predict(model.nnet, test, type = 'class')
table(pre.ANN, test$outp)
detach(package:nnet)

# SVM
library(e1071)

