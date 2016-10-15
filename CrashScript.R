#install.packages("data.table")
library(data.table)

MCMIS <- 'D:\\Program File\\RStudio\\scripts\\RA\\MCMIS'

setwd(MCMIS) #location of data

crash.master <- fread('OurCrash.csv', header = T, sep ="auto", sep2 = "auto")

summary(crash.master)

states <- unique(crash.master$REPORT_STATE)
injuries <- NULL
fatalities <- NULL
severity <- NULL
n <- NULL

for (state in states) {
  injuries <- c(injuries, sum(crash.master$INJURIES[crash.master$REPORT_STATE == state]))
  fatalities <- c(fatalities, sum(crash.master$FATALITIES[crash.master$REPORT_STATE == state]))
  severity <- c(severity, mean(crash.master$SEVERITY_WEIGHT[crash.master$REPORT_STATE == state]))
  n <- c(n,length(crash.master$FATALITIES[crash.master$REPORT_STATE == state]))
}

df <- data.frame(states, injuries, fatalities, severity, n)

hist(df$injuries,nclass=20)
hist(df$fatalities,nclass=20)
hist(df$severity)
hist(df$n,nclass=20)

boxplot(df$injuries, main="injuries")
boxplot(df$fatalities, main="fatalities")
boxplot(df$severity, main="severity")
boxplot(df$n, main="sample size")

df.summary <- summary(df, maxsum=100)
write.table(df.summary,file="summary.csv",append=FALSE)
