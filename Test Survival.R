library(readxl)
library(ggplot2)
library(dplyr)
library(KMsurv)
library(survival)
library(splines)
library(muhaz)
library(openxlsx)
library(survminer)
library(tidyr)
library(glmnet)

Test_Player <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Test Player.xlsx")

Test = subset(Test_Player,Test_Player$SC > 0.2)


#Dropping NAs in Career Averages

str(Test)
Test$Inns <- as.factor(Test$Inns)
Test$SR <- as.numeric(Test$SR)
Test$`Career Ave` <- as.numeric(Test$`Career Ave`)
Test$`Career SR` <- as.numeric(Test$`Career SR`)

Test <- filter(Test, !is.na(`Career Ave`))


#Test Survival part

Test$`Out ot Not out` <- ifelse(Test$`Out ot Not out` == "Out", 1, 0)

Test1 <- Test %>% filter(`Out ot Not out` == 1)
Test2 <- Test %>% filter(`Total Balls used by the team` == 120) %>% filter(`Out ot Not out` == 0)
Test3 <- rbind(Test1, Test2)

my.surv.Test <- Surv(Test3$Runs, Test3$`Out ot Not out`)
my.surv.Test

coxph.fit.Test <- coxph(my.surv.Test ~ Test3$`Batter Classification` + Test3$Score + Test3$`Total Balls used by the team` + Test3$Wickets)

summary(coxph.fit.Test)

AIC(coxph.fit.Test)

#Test the proportional hazards assumption, Schoenfeld residuals

cox.zph(coxph.fit.Test) 
plot(cox.zph(coxph.fit.Test))

#The examination of influential observations involved scrutinizing Dfbeta residuals

dfbetaresidual.Test <- residuals(coxph.fit.Test, type = "dfbeta")
plot(dfbetaresidual.Test[,1], ylab = "Batter Classification")
plot(dfbetaresidual.Test[,2], ylab = "Exp(Score)")
plot(dfbetaresidual.Test[,3], ylab = "Total Balls used by the team")
plot(dfbetaresidual.Test[,4], ylab = "Wickets")

#To detect nonlinearity in the relationship between the log hazard and the covariates, martingale residuals

martingale_residual.Test <- resid(coxph.fit.Test, type = "martingale")
length(martingale_residual.Test)
length(Test3$`Career Ave`)


plot(Test3$Score[], martingale_residual.Test, ylab = "Martingale Residuals", xlab = "Score")
plot(Test3$`Total Balls used by the team`[], martingale_residual.Test, ylab = "Martingale Residuals", xlab = "Total Balls used by the team")
plot(Test3$Wickets[], martingale_residual.Test, ylab = "Martingale Residuals", xlab = "Wickets")

#Baseline survival function

my.survfit.baseline.Test <- survfit(coxph.fit.Test)
my.survfit.baseline.Test

summary(my.survfit.baseline.Test)

plot(my.survfit.baseline.Test)

# Coefficients of the model Test

unique(T20I$Runs)

coeff.good = summary(coxph.fit.Test)$coefficients[1,1]
coeff.poor = summary(coxph.fit.Test)$coefficients[2,1]
coeff.satisfied = summary(coxph.fit.Test)$coefficients[3,1]
coeff.very.good = summary(coxph.fit.Test)$coefficients[4,1]
coeff.score = summary(coxph.fit.Test)$coefficients[5,1]
coeff.total.balls = summary(coxph.fit.Test)$coefficients[6,1]
coeff.wickets = summary(coxph.fit.Test)$coefficients[7,1]


df1 <- data.frame(my.survfit.baseline.Test$surv, my.survfit.baseline.Test$time) #Creating a dataframe with runs and baseline survival values
colnames(df1) <- c("baseline.surv", "Runs")

Test <- right_join(Test, df1, by = "Runs") #Creating a column with the baseline survival values in Test

Test.notout <- Test %>% filter(`Out ot Not out` == 0) %>% filter(`Total Balls used by the team` != 120)
summary(Test.notout$`Total Balls used by the team`)
Test.notout$Inns <- ifelse(Test.notout$Inns == 1, 0, 1)

df2 <- data.frame(Runs = c(2:max(df1$Runs)))

df2 <- df2 %>% left_join(df1,by="Runs") %>% fill(names(.),.direction = "down")

plot(x=df2$Runs,y=df2$baseline.surv, type = "s")

#Function to calculate the survival values

#Return the c value
Test.survival.estimate <- function(good, poor, satisfied, very.good, score, total.balls, wickets, baseline){
  est <- coeff.good*good + coeff.poor*poor + coeff.satisfied*satisfied + coeff.very.good*very.good + coeff.score*score + coeff.total.balls*total.balls + coeff.wickets*wickets
  exp.est <- exp(est)
  survival <- baseline^(exp.est)
  return(exp.est)
}

#Loop to get all the c values for all the not out values

Not.out.score1 <- c()
c.vec <- c()

for(i in 1:nrow(Test.notout)){
  Not.out.score1 <- c(Not.out.score1, Test.notout$Runs[i])
  c <- Test.survival.estimate(Test.notout$Good[i], Test.notout$Poor[i], Test.notout$Satisfied[i], Test.notout$`Very Good`[i], Test.notout$Score[i], Test.notout$`Total Balls used by the team`[i], Test.notout$Wickets[i], Test.notout$baseline.surv[i])
  c.vec <- c(c.vec, c)
}

#Power values(c values)
df.final.1 <- data.frame("Runs" = Not.out.score1, "C Values" = c.vec)


#Loop to take all the notout values into two vectors

surv.vec.c.sum.all <- c() 
run.all <- c()
denominator <- c()

for(i in 1:nrow(df.final.1)){
  run <- df.final.1$Runs[i]
  c <- df.final.1$C.Values[i]
  surv.vec <- (df2 %>% filter(df2$Runs >= run))$baseline.surv
  surv.vec.c <- surv.vec^c
  denominator <- c(denominator, surv.vec.c[1])
  surv.vec.c.sum <- sum(surv.vec.c)
  surv.vec.c.sum.all <- c(surv.vec.c.sum.all, surv.vec.c.sum)
  run.all <- c(run.all, run)
}


#Numerator and denominator values
df.final.2 <- data.frame("Runs" = run.all, "Survival Estimate" = surv.vec.c.sum.all, "Denominator" = denominator)

#Adding the obtained survival values to the dataframe
Test.notout1 <- cbind(Test.notout, "Numerator" = df.final.2$Survival.Estimate, "Denominator" = df.final.2$Denominator)

#Expected runs
Test.notout2 <- mutate(Test.notout1,"Expected Runs" = Numerator/Denominator)



# Specify the file path where you want to save the Excel file
file_path <- "C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/R Codes/Test Survival Runs1.xlsx"

# Write the data frame to an Excel file
write.xlsx(Test.notout2, file_path)






#Ball Predicting part

str(Test3)

my.surv.Test1 <- Surv(Test3$BF, Test3$`Out ot Not out`)
my.surv.Test1

coxph.fit.Test1 <- coxph(my.surv.Test1 ~ Test3$`Batter Classification` + Test3$Score + Test3$`Total Balls used by the team` + Test3$Wickets + Test3$`Final Runs` + Test3$`Career SR`)

summary(coxph.fit.Test1)

AIC(coxph.fit.Test1)

#Test the proportional hazards assumption, Schoenfeld residuals

cox.zph(coxph.fit.Test1) 
plot(cox.zph(coxph.fit.Test1))

#The examination of influential observations involved scrutinizing Dfbeta residuals

dfbetaresidual.Test1 <- residuals(coxph.fit.Test1, type = "dfbeta")
plot(dfbetaresidual.Test1[,1], ylab = "Batter Classification")
plot(dfbetaresidual.Test1[,2], ylab = "Score")
plot(dfbetaresidual.Test1[,3], ylab = "Total Balls used by the team")
plot(dfbetaresidual.Test1[,4], ylab = "Wickets")
plot(dfbetaresidual.Test1[,5], ylab = "Runs")

#To detect nonlinearity in the relationship between the log hazard and the covariates, martingale residuals

martingale_residual.Test1 <- resid(coxph.fit.Test1, type = "martingale")
length(martingale_residual.Test1)
length(Test3$`Career Ave`)

plot(Test3$Score[], martingale_residual.Test1, ylab = "Martingale Residuals", xlab = "Score")
plot(Test3$`Total Balls used by the team`[], martingale_residual.Test1, ylab = "Martingale Residuals", xlab = "Total Balls used by the team")
plot(Test3$Wickets[], martingale_residual.Test1, ylab = "Martingale Residuals", xlab = "Wickets")
plot(Test3$`Final Runs`[], martingale_residual.Test1, ylab = "Martingale Residuals", xlab = "Runs")

#Baseline survival function

my.survfit.baseline.Test1 <- survfit(coxph.fit.Test1)
my.survfit.baseline.Test1

summary(my.survfit.baseline.Test1)

plot(my.survfit.baseline.Test1)


# Coefficients of the model Test

coeff.good = summary(coxph.fit.Test1)$coefficients[1,1]
coeff.poor = summary(coxph.fit.Test1)$coefficients[2,1]
coeff.satisfied = summary(coxph.fit.Test1)$coefficients[3,1]
coeff.very.good = summary(coxph.fit.Test1)$coefficients[4,1]
coeff.score = summary(coxph.fit.Test1)$coefficients[5,1]
coeff.total.balls = summary(coxph.fit.Test1)$coefficients[6,1]
coeff.wickets = summary(coxph.fit.Test1)$coefficients[7,1]
coeff.final.runs = summary(coxph.fit.Test1)$coefficients[8,1]
coeff.strike.rate = summary(coxph.fit.Test1)$coefficients[9,1]


df3 <- data.frame(my.survfit.baseline.Test1$surv, my.survfit.baseline.Test1$time) #Creating a dataframe with runs and baseline survival values
colnames(df3) <- c("baseline.surv", "BF")

Test4 <- right_join(Test.notout2, df3, by = "BF") #Creating a column with the baseline survival values in ODI

Test5 <- Test4 %>% filter(`Out ot Not out` == 0)
summary(Test5$`Total Balls used by the team`)


df4 <- data.frame(BF = c(5:max(df3$BF)))

df4 <- df4 %>% left_join(df3,by="BF") %>% fill(names(.),.direction = "down")

plot(x=df4$BF,y=df4$baseline.surv, type = "s")

#Function to calculate the survival values

#Return the c value
Test.survival.est <- function(good, poor, satisfied, very.good, score, total.balls, wickets, final.runs, strike.rate, baseline){
  est <- coeff.good*good + coeff.poor*poor + coeff.satisfied*satisfied + coeff.very.good*very.good + coeff.score*score + coeff.total.balls*total.balls + coeff.wickets*wickets + coeff.final.runs*final.runs + coeff.strike.rate*strike.rate
  exp.est <- exp(est)
  survival <- baseline^(exp.est)
  return(exp.est)
}

#Loop to get all the c values for all the not out values

Not.out.balls <- c()
c.vec <- c()

for(i in 1:nrow(Test5)){
  Not.out.balls <- c(Not.out.balls, Test.notout$BF[i])
  c <- Test.survival.est(Test5$Good[i], Test5$Poor[i], Test5$Satisfied[i], Test5$`Very Good`[i], Test5$Score[i], Test5$`Total Balls used by the team`[i], Test5$Wickets[i], Test5$`Final Runs`[i], Test5$`Career SR`[i], Test5$baseline.surv.y[i])
  c.vec <- c(c.vec, c)
}

#Power values(c values)
df.final.3 <- data.frame("BF" = Not.out.balls, "C Values" = c.vec)


#Loop to take all the notout values into two vectors

surv.vec.c.sum.all1 <- c() 
run.all1 <- c()
denominators <- c()

for(i in 1:nrow(df.final.3)){
  ball <- df.final.3$BF[i]
  c1 <- df.final.3$C.Values[i]
  surv.vec1 <- (df4 %>% filter(df4$BF >= ball))$baseline.surv
  surv.vec.c1 <- surv.vec1^c1
  denominators <- c(denominators,surv.vec.c1[1])
  surv.vec.c.sum1 <- sum(surv.vec.c1)
  surv.vec.c.sum.all1 <- c(surv.vec.c.sum.all1, surv.vec.c.sum1)
  run.all1 <- c(run.all1, ball)
}


#Numerator and denominator values
df.final.4 <- data.frame("Balls" = run.all1, "Survival Estimate" = surv.vec.c.sum.all1, "Denominator1" = denominators)

#Adding the obtained survival values to the dataframe
Test.notout3 <- cbind(Test5, "Numerator1" = df.final.4$Survival.Estimate, "Denominator1" = df.final.4$Denominator1)

#Expected balls
Test.notout4 <- mutate(Test.notout3,"Expected Balls" = Numerator1/Denominator1)


# Specify the file path where you want to save the Excel file
file_path <- "C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/R Codes/Test Survival Final.xlsx"

# Write the data frame to an Excel file
write.xlsx(Test.notout4, file_path)
