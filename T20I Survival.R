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

T20I_Player <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/T20I Player.xlsx")

T20I = subset(T20I_Player,T20I_Player$SC > 0.2)


#Dropping NAs in Career Averages

str(T20I)
T20I$Inns <- as.factor(T20I$Inns)
T20I$SR <- as.numeric(T20I$SR)
T20I$`Career Ave` <- as.numeric(T20I$`Career Ave`)
T20I$`Career SR` <- as.numeric(T20I$`Career SR`)

T20I <- filter(T20I, !is.na(`Career Ave`))


#T20I Survival part

T20I$`Out ot Not out` <- ifelse(T20I$`Out ot Not out` == "Out", 1, 0)

T20I1 <- T20I %>% filter(`Out ot Not out` == 1)
T20I2 <- T20I %>% filter(`Total Balls used by the team` == 120) %>% filter(`Out ot Not out` == 0)
T20I3 <- rbind(T20I1, T20I2)

my.surv.T20I <- Surv(T20I3$Runs, T20I3$`Out ot Not out`)
my.surv.T20I

coxph.fit.T20I <- coxph(my.surv.T20I ~ T20I3$`Batter Classification` + T20I3$Score + T20I3$`Remaining Overs` + T20I3$Wickets)

summary(coxph.fit.T20I)

AIC(coxph.fit.T20I)

#Test the proportional hazards assumption, Schoenfeld residuals

cox.zph(coxph.fit.T20I)
plot(cox.zph(coxph.fit.T20I))

#The examination of influential observations involved scrutinizing Dfbeta residuals

dfbetaresidual.T20I <- residuals(coxph.fit.T20I, type = "dfbeta")
plot(dfbetaresidual.T20I[,1], ylab = "Batter Classification")
plot(dfbetaresidual.T20I[,2], ylab = "Score")
plot(dfbetaresidual.T20I[,3], ylab = "Remaining Overs")
plot(dfbetaresidual.T20I[,4], ylab = "Wickets")

#To detect nonlinearity in the relationship between the log hazard and the covariates, martingale residuals

martingale_residual.T20I <- resid(coxph.fit.T20I, type = "martingale")
length(martingale_residual.T20I)
length(T20I3$`Career Ave`)

plot(T20I3$Score[], martingale_residual.T20I, ylab = "Martingale Residuals", xlab = "Score")
plot(T20I3$`Remaining Overs`[], martingale_residual.T20I, ylab = "Martingale Residuals", xlab = "Remaining Overs")
plot(T20I3$Wickets[], martingale_residual.T20I, ylab = "Martingale Residuals", xlab = "Wickets")

#Baseline survival function

my.survfit.baseline.T20I <- survfit(coxph.fit.T20I)
my.survfit.baseline.T20I

summary(my.survfit.baseline.T20I)

plot(my.survfit.baseline.T20I)

# Coefficients of the model ODI

unique(T20I$Runs)

coeff.good = summary(coxph.fit.T20I)$coefficients[1,1]
coeff.poor = summary(coxph.fit.T20I)$coefficients[2,1]
coeff.satisfied = summary(coxph.fit.T20I)$coefficients[3,1]
coeff.very.good = summary(coxph.fit.T20I)$coefficients[4,1]
coeff.exp.score = summary(coxph.fit.T20I)$coefficients[5,1]
coeff.remaining.overs = summary(coxph.fit.T20I)$coefficients[6,1]
coeff.wickets = summary(coxph.fit.T20I)$coefficients[7,1]


df1 <- data.frame(my.survfit.baseline.T20I$surv, my.survfit.baseline.T20I$time) #Creating a dataframe with runs and baseline survival values
colnames(df1) <- c("baseline.surv", "Runs")

T20I <- right_join(T20I, df1, by = "Runs") #Creating a column with the baseline survival values in T20I

T20I.notout <- T20I %>% filter(`Out ot Not out` == 0) %>% filter(`Total Balls used by the team` != 120)
summary(T20I.notout$`Total Balls used by the team`)
T20I.notout$Inns <- ifelse(T20I.notout$Inns == 1, 0, 1)

df2 <- data.frame(Runs = c(2:max(df1$Runs)))

df2 <- df2 %>% left_join(df1,by="Runs") %>% fill(names(.),.direction = "down")

plot(x=df2$Runs,y=df2$baseline.surv, type = "s")

#Function to calculate the survival values

#Return the c value
T20I.survival.estimate <- function(good, poor, satisfied, very.good, exp.score, remain.overs, wickets, baseline){
  est <- coeff.good*good + coeff.poor*poor + coeff.satisfied*satisfied + coeff.very.good*very.good + coeff.exp.score*exp.score + coeff.remaining.overs*remain.overs + coeff.wickets*wickets
  exp.est <- exp(est)
  survival <- baseline^(exp.est)
  return(exp.est)
}

#Loop to get all the c values for all the not out values

Not.out.score1 <- c()
c.vec <- c()

for(i in 1:nrow(T20I.notout)){
  Not.out.score1 <- c(Not.out.score1, T20I.notout$Runs[i])
  c <- T20I.survival.estimate(T20I.notout$Good[i], T20I.notout$Poor[i], T20I.notout$Satisfied[i], T20I.notout$`Very Good`[i], T20I.notout$Score[i], T20I.notout$`Remaining Overs`[i], T20I.notout$Wickets[i], T20I.notout$baseline.surv[i])
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
T20I.notout1 <- cbind(T20I.notout, "Numerator" = df.final.2$Survival.Estimate, "Denominator" = df.final.2$Denominator)

#Expected runs
T20I.notout2 <- mutate(T20I.notout1,"Expected Runs" = Numerator/Denominator)



# Specify the file path where you want to save the Excel file
file_path <- "C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/R Codes/T20I Survival Runs 1.xlsx"

# Write the data frame to an Excel file
write.xlsx(T20I.notout2, file_path)






#Ball Predicting part

str(T20I3)

my.surv.T20I1 <- Surv(T20I3$BF, T20I3$`Out ot Not out`)
my.surv.T20I1

coxph.fit.T20I1 <- coxph(my.surv.T20I1 ~ T20I3$`Batter Classification` + T20I3$Score + T20I3$`Remaining Overs` + T20I3$Wickets + T20I3$`Final Runs` + T20I3$`Career SR`)

summary(coxph.fit.T20I1)

AIC(coxph.fit.T20I1)

#Test the proportional hazards assumption, Schoenfeld residuals

cox.zph(coxph.fit.T20I1)
plot(cox.zph(coxph.fit.T20I1))

#The examination of influential observations involved scrutinizing Dfbeta residuals

dfbetaresidual.T20I1 <- residuals(coxph.fit.T20I1, type = "dfbeta")
plot(dfbetaresidual.T20I1[,1], ylab = "Batter Classification")
plot(dfbetaresidual.T20I1[,2], ylab = "Score")
plot(dfbetaresidual.T20I1[,3], ylab = "Remaining Overs")
plot(dfbetaresidual.T20I1[,4], ylab = "Wickets")
plot(dfbetaresidual.T20I1[,5], ylab = "Runs")
plot(dfbetaresidual.T20I1[,6], ylab = "Career SR")

#To detect nonlinearity in the relationship between the log hazard and the covariates, martingale residuals

martingale_residual.T20I1 <- resid(coxph.fit.T20I1, type = "martingale")
length(martingale_residual.T20I1)
length(T20I3$`Career Ave`)

plot(T20I3$Score[], martingale_residual.T20I1, ylab = "Martingale Residuals", xlab = "Score")
plot(T20I3$`Remaining Overs`[], martingale_residual.T20I1, ylab = "Martingale Residuals", xlab = "Remaining Overs")
plot(T20I3$Wickets[], martingale_residual.T20I1, ylab = "Martingale Residuals", xlab = "Wickets")
plot(T20I3$`Final Runs`[], martingale_residual.T20I1, ylab = "Martingale Residuals", xlab = "Runs")
plot(T20I3$`Career SR`[], martingale_residual.T20I1, ylab = "Martingale Residuals", xlab = "Career SR")

#Baseline survival function

my.survfit.baseline.T20I1 <- survfit(coxph.fit.T20I1)
my.survfit.baseline.T20I1

summary(my.survfit.baseline.T20I1)

plot(my.survfit.baseline.T20I1)


# Coefficients of the model T20I

coeff.good = summary(coxph.fit.T20I1)$coefficients[1,1]
coeff.poor = summary(coxph.fit.T20I1)$coefficients[2,1]
coeff.satisfied = summary(coxph.fit.T20I1)$coefficients[3,1]
coeff.very.good = summary(coxph.fit.T20I1)$coefficients[4,1]
coeff.exp.score = summary(coxph.fit.T20I1)$coefficients[5,1]
coeff.remaining.overs = summary(coxph.fit.T20I1)$coefficients[6,1]
coeff.wickets = summary(coxph.fit.T20I1)$coefficients[7,1]
coeff.final.runs = summary(coxph.fit.T20I1)$coefficients[8,1]
coeff.strike.rate = summary(coxph.fit.T20I1)$coefficients[9,1]


df3 <- data.frame(my.survfit.baseline.T20I1$surv, my.survfit.baseline.T20I1$time) #Creating a dataframe with runs and baseline survival values
colnames(df3) <- c("baseline.surv", "BF")

T20I4 <- right_join(T20I.notout2, df3, by = "BF") #Creating a column with the baseline survival values in ODI

T20I5 <- T20I4 %>% filter(`Out ot Not out` == 0)
summary(T20I5$`Total Balls used by the team`)


df4 <- data.frame(BF = c(5:max(df3$BF)))

df4 <- df4 %>% left_join(df3,by="BF") %>% fill(names(.),.direction = "down")

plot(x=df4$BF,y=df4$baseline.surv, type = "s")

#Function to calculate the survival values

#Return the c value
T20I.survival.est <- function(good, poor, satisfied, very.good, exp.score, remain.overs, wickets, final.runs, strike.rate, baseline){
  est <- coeff.good*good + coeff.poor*poor + coeff.satisfied*satisfied + coeff.very.good*very.good + coeff.exp.score*exp.score + coeff.remaining.overs*remain.overs + coeff.wickets*wickets + coeff.final.runs*final.runs + coeff.strike.rate*strike.rate
  exp.est <- exp(est)
  survival <- baseline^(exp.est)
  return(exp.est)
}

#Loop to get all the c values for all the not out values

Not.out.balls <- c()
c.vec <- c()

for(i in 1:nrow(T20I5)){
  Not.out.balls <- c(Not.out.balls, T20I.notout$BF[i])
  c <- T20I.survival.est(T20I5$Good[i], T20I5$Poor[i], T20I5$Satisfied[i], T20I5$`Very Good`[i], exp(T20I5$Score[i]), T20I5$`Remaining Overs`[i], T20I5$Wickets[i], T20I5$`Final Runs`[i], T20I5$`Career SR`[i], T20I5$baseline.surv.y[i])
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
T20I.notout3 <- cbind(T20I5, "Numerator1" = df.final.4$Survival.Estimate, "Denominator1" = df.final.4$Denominator1)

#Expected balls
T20I.notout4 <- mutate(T20I.notout3,"Expected Balls" = Numerator1/Denominator1)


# Specify the file path where you want to save the Excel file
file_path <- "C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/R Codes/T20I Survival Final.xlsx"

# Write the data frame to an Excel file
write.xlsx(T20I.notout4, file_path)
