#install.packages("data.table")
library(data.table)

#Loading in the data
#MCMIS <- 'D:\\Program File\\Git\\git_projects\\RA\\MCMIS'
MCMIS <- '/home/liujundi/Desktop/truck_crash/MCMIS/data'

setwd(MCMIS) #location of data

#crash.master <- fread('data\\OurCrash.csv', header = T, sep ="auto", sep2 = "auto")
#truck.employed <- fread('data\\TruckEmployment.csv', header = T, sep ="auto", sep2 = "auto")
crash.master <- fread('OurCrash.csv', header = T, sep ="auto", sep2 = "auto")
truck.employed <- fread('TruckEmployment.csv', header = T, sep ="auto", sep2 = "auto")

summary(crash.master)

states <- unique(crash.master$REPORT_STATE)
injuries <- NULL
fatalities <- NULL
severity <- NULL
n <- NULL

mean_inj <- NULL
median_inj <- NULL
sigma_inj <- NULL
min_inj <-NULL
max_inj <- NULL
mean_fata <- NULL
median_fata <- NULL
sigma_fata <- NULL
min_fata <-NULL
max_fata <- NULL
mean_sever <- NULL
median_sever <- NULL
sigma_sever <- NULL
min_sever <- NULL
max_sever <- NULL

for (state in states) {
  injuries <- c(injuries, sum(crash.master$INJURIES[crash.master$REPORT_STATE == state]))
  fatalities <- c(fatalities, sum(crash.master$FATALITIES[crash.master$REPORT_STATE == state]))
  severity <- c(severity, mean(crash.master$SEVERITY_WEIGHT[crash.master$REPORT_STATE == state]))
  n <- c(n,length(crash.master$FATALITIES[crash.master$REPORT_STATE == state]))
  inj_state <- NULL
  fata_state <- NULL
  sever_state <- NULL
  inj_state <- c(inj_state, crash.master$INJURIES[crash.master$REPORT_STATE == state])
  fata_state <- c(fata_state, crash.master$FATALITIES[crash.master$REPORT_STATE == state])
  sever_state <- c(sever_state, crash.master$SEVERITY_WEIGHT[crash.master$REPORT_STATE == state])
  mean_inj <- c(mean_inj, mean(inj_state))
  median_inj <- c(median_inj, median(inj_state))
  sigma_inj <- c(sigma_inj, sd(inj_state))
  min_inj <- c(min_inj, min(inj_state))
  max_inj <- c(max_inj, max(inj_state))
  mean_fata <- c(mean_fata, mean(fata_state))
  median_fata <- c(median_fata, median(fata_state))
  sigma_fata <- c(sigma_fata, sd(fata_state))
  min_fata <- c(min_fata, min(fata_state))
  max_fata <- c(max_fata, max(fata_state))
  mean_sever <- c(mean_sever, mean(sever_state))
  median_sever <- c(median_sever, median(sever_state))
  sigma_sever <- c(sigma_sever, sd(sever_state))
  min_sever <- c(min_sever, min(sever_state))
  max_sever <- c(max_sever, max(sever_state))
}



tabuler <- data.frame(states,mean_inj, median_inj, sigma_inj, min_inj, max_inj,
                      mean_fata, 
                      median_fata, 
                      sigma_fata, 
                      min_fata,
                      max_fata,
                      mean_sever,
                      median_sever,
                      sigma_sever,
                      min_sever,
                      max_sever)
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



df.summary <- summary(df, maxsum=100)
write.table(df.summary,file="summary.csv",append=FALSE)

mean(injuries)
mean(fatalities)
mean(severity)
median(injuries)
median(fatalities)
median(severity)
range(injuries)
range(fatalities)
range(severity)

