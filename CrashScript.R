#install.packages("data.table")
library(data.table)

#Loading in the data
MCMIS <- 'D:\\Program File\\Git\\git_projects\\RA\\MCMIS'

setwd(MCMIS) #location of data

crash.master <- fread('data\\OurCrash.csv', header = T, sep ="auto", sep2 = "auto")
truck.employed <- fread('data\\TruckEmployment.csv', header = T, sep ="auto", sep2 = "auto")

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
df <- merge(df, truck.employed, by="states")
df$sinjuries <- df$injuries/df$employed
df$sfatalities <- df$fatalities/df$employed
df$sn <- df$n/df$employed

hist(df$injuries,nclass=20)
hist(df$fatalities,nclass=20)
hist(df$severity)
hist(df$n)

hist(df$sinjuries)
hist(df$sfatalities)
hist(df$sn)

boxplot(df$injuries, main="injuries")
boxplot(df$fatalities, main="fatalities")
boxplot(df$severity, main="severity")
boxplot(df$n, main="sample size")


