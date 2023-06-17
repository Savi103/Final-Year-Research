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

ODI_Player <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/ODI Player.xlsx")

ODI = subset(ODI_Player,ODI_Player$SC > 0.2)


#Dropping NAs in Career Averages

str(ODI)
ODI$Inns <- as.factor(ODI$Inns)
ODI$SR <- as.numeric(ODI$SR)
ODI$`Career Ave` <- as.numeric(ODI$`Career Ave`)
ODI$`Career SR` <- as.numeric(ODI$`Career SR`)

ODI <- filter(ODI, !is.na(`Career Ave`))


#ODI Survival part

ODI$`Out ot Not out` <- ifelse(ODI$`Out ot Not out` == "Out", 1, 0)

ODI1 <- ODI %>% filter(`Out ot Not out` == 1)
ODI2 <- ODI %>% filter(`Total Balls used by the team` == 300) %>% filter(`Out ot Not out` == 0)
ODI3 <- rbind(ODI1, ODI2)

my.surv.ODI <- Surv(ODI3$Runs, ODI3$`Out ot Not out`)
my.surv.ODI

coxph.fit.ODI <- coxph(my.surv.ODI ~  ODI3$`Batter Classification` + ODI3$Score + ODI3$`Remaining Overs` + ODI3$Wickets)

summary(coxph.fit.ODI)

AIC(coxph.fit.ODI)

#Test the proportional hazards assumption, Schoenfeld residuals

cox.zph(coxph.fit.ODI) 
plot(cox.zph(coxph.fit.ODI))

#The examination of influential observations involved scrutinizing Dfbeta residuals

dfbetaresidual.ODI <- residuals(coxph.fit.ODI, type = "dfbeta")
plot(dfbetaresidual.ODI[,1], ylab = "Batter Classification")
plot(dfbetaresidual.ODI[,2], ylab = "Exp(Score)")
plot(dfbetaresidual.ODI[,3], ylab = "Remaining Overs")
plot(dfbetaresidual.ODI[,4], ylab = "Wickets")

#To detect nonlinearity in the relationship between the log hazard and the covariates, martingale residuals

martingale_residual.ODI <- resid(coxph.fit.ODI, type = "martingale")
length(martingale_residual.ODI)
length(ODI3$`Career Ave`)

plot(ODI3$Score[], martingale_residual.ODI, ylab = "Martingale Residuals", xlab = "Score")
plot(ODI3$`Remaining Overs`[], martingale_residual.ODI, ylab = "Martingale Residuals", xlab = "Remaining Overs")
plot(ODI3$Wickets[], martingale_residual.ODI, ylab = "Martingale Residuals", xlab = "Wickets")

#Baseline survival function

my.survfit.baseline.ODI <- survfit(coxph.fit.ODI)
my.survfit.baseline.ODI

summary(my.survfit.baseline.ODI)

plot(my.survfit.baseline.ODI)

# Coefficients of the model ODI

unique(ODI$Runs)

coeff.good = summary(coxph.fit.ODI)$coefficients[1,1]
coeff.poor = summary(coxph.fit.ODI)$coefficients[2,1]
coeff.satisfied = summary(coxph.fit.ODI)$coefficients[3,1]
coeff.very.good = summary(coxph.fit.ODI)$coefficients[4,1]
coeff.exp.score = summary(coxph.fit.ODI)$coefficients[5,1]
coeff.remaining.overs = summary(coxph.fit.ODI)$coefficients[6,1]
coeff.wickets = summary(coxph.fit.ODI)$coefficients[7,1]


df1 <- data.frame(my.survfit.baseline.ODI$surv, my.survfit.baseline.ODI$time) #Creating a dataframe with runs and baseline survival values
colnames(df1) <- c("baseline.surv", "Runs")

ODI <- right_join(ODI, df1, by = "Runs") #Creating a column with the baseline survival values in ODI

ODI.notout <- ODI %>% filter(`Out ot Not out` == 0) %>% filter(`Total Balls used by the team` != 300)
summary(ODI.notout$`Total Balls used by the team`)
ODI.notout$Inns <- ifelse(ODI.notout$Inns == 1, 0, 1)

df2 <- data.frame(Runs = c(2:max(df1$Runs)))

df2 <- df2 %>% left_join(df1,by="Runs") %>% fill(names(.),.direction = "down")

plot(x=df2$Runs,y=df2$baseline.surv, type = "s")

#Function to calculate the survival values

#Return the c value
ODI.survival.estimate <- function(good, poor, satisfied, very.good, exp.score, remain.overs, wickets, baseline){
  est <- coeff.good*good + coeff.poor*poor + coeff.satisfied*satisfied + coeff.very.good*very.good + coeff.exp.score*exp.score + coeff.remaining.overs*remain.overs + coeff.wickets*wickets
  exp.est <- exp(est)
  survival <- baseline^(exp.est)
  return(exp.est)
}

#Loop to get all the c values for all the not out values

Not.out.score1 <- c()
c.vec <- c()

for(i in 1:nrow(ODI.notout)){
  Not.out.score1 <- c(Not.out.score1, ODI.notout$Runs[i])
  c <- ODI.survival.estimate(ODI.notout$Good[i], ODI.notout$Poor[i], ODI.notout$Satisfied[i], ODI.notout$`Very Good`[i], exp(ODI.notout$Score[i]), ODI.notout$`Remaining Overs`[i], ODI.notout$Wickets[i], ODI.notout$baseline.surv[i])
  c.vec <- c(c.vec, c)
}

#Power values(c values)
df.final.1 <- data.frame("Runs" = Not.out.score1, "C Values" = c.vec)


#Loop to take all the not out values into two vectors

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


#Numerator values and denominator values
df.final.2 <- data.frame("Runs" = run.all, "Survival Estimate" = surv.vec.c.sum.all, "Denominator" = denominator)

#Adding the obtained survival values to the dataframe
ODI.notout1 <- cbind(ODI.notout, "Numerator" = df.final.2$Survival.Estimate, "Denominator" = df.final.2$Denominator)

#Expected runs
ODI.notout2 <- mutate(ODI.notout1,"Expected Runs" = Numerator/Denominator)



# Specify the file path where you want to save the Excel file
file_path <- "C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/R Codes/ODI Survival Runs.xlsx"

# Write the data frame to an Excel file
write.xlsx(ODI.notout2, file_path)






#Ball Predicting part

str(ODI3)

my.surv.ODI1 <- Surv(ODI3$BF, ODI3$`Out ot Not out`)
my.surv.ODI1

coxph.fit.ODI1 <- coxph(my.surv.ODI1 ~ ODI3$`Batter Classification` + ODI3$Score + ODI3$`Remaining Overs` + ODI3$Wickets + ODI3$`Final Runs` + ODI3$`Career SR`)

summary(coxph.fit.ODI1)

AIC(coxph.fit.ODI1)

#Test the proportional hazards assumption, Schoenfeld residuals

cox.zph(coxph.fit.ODI1)
plot(cox.zph(coxph.fit.ODI1))

#The examination of influential observations involved scrutinizing Dfbeta residuals

dfbetaresidual.ODI1 <- residuals(coxph.fit.ODI1, type = "dfbeta")
plot(dfbetaresidual.ODI1[,1], ylab = "Batter Classification")
plot(dfbetaresidual.ODI1[,2], ylab = "Score")
plot(dfbetaresidual.ODI1[,3], ylab = "Remaining Overs")
plot(dfbetaresidual.ODI1[,4], ylab = "Wickets")
plot(dfbetaresidual.ODI1[,5], ylab = "Runs")
plot(dfbetaresidual.ODI1[,6], ylab = "Career SR")

#To detect nonlinearity in the relationship between the log hazard and the covariates, martingale residuals

martingale_residual.ODI1 <- resid(coxph.fit.ODI1, type = "martingale")
length(martingale_residual.ODI1)
length(ODI3$`Career Ave`)

plot(ODI3$Score[], martingale_residual.ODI1, ylab = "Martingale Residuals", xlab = "Score")
plot(ODI3$`Remaining Overs`[], martingale_residual.ODI1, ylab = "Martingale Residuals", xlab = "Remaining Overs")
plot(ODI3$Wickets[], martingale_residual.ODI1, ylab = "Martingale Residuals", xlab = "Wickets")
plot(ODI3$`Final Runs`[], martingale_residual.ODI1, ylab = "Martingale Residuals", xlab = "Runs")
plot(ODI3$`Career SR`[], martingale_residual.ODI1, ylab = "Martingale Residuals", xlab = "Career SR")

#Baseline survival function

my.survfit.baseline.ODI1 <- survfit(coxph.fit.ODI1)
my.survfit.baseline.ODI1

summary(my.survfit.baseline.ODI1)

plot(my.survfit.baseline.ODI1)


# Coefficients of the model ODI

coeff.good = summary(coxph.fit.ODI1)$coefficients[1,1]
coeff.poor = summary(coxph.fit.ODI1)$coefficients[2,1]
coeff.satisfied = summary(coxph.fit.ODI1)$coefficients[3,1]
coeff.very.good = summary(coxph.fit.ODI1)$coefficients[4,1]
coeff.exp.score = summary(coxph.fit.ODI1)$coefficients[5,1]
coeff.remaining.overs = summary(coxph.fit.ODI1)$coefficients[6,1]
coeff.wickets = summary(coxph.fit.ODI1)$coefficients[7,1]
coeff.final.runs = summary(coxph.fit.ODI1)$coefficients[8,1]
coeff.strike.rate = summary(coxph.fit.ODI1)$coefficients[9,1]


df3 <- data.frame(my.survfit.baseline.ODI1$surv, my.survfit.baseline.ODI1$time) #Creating a dataframe with runs and baseline survival values
colnames(df3) <- c("baseline.surv", "BF")

ODI4 <- right_join(ODI.notout2, df3, by = "BF") #Creating a column with the baseline survival values in ODI

ODI5 <- ODI4 %>% filter(`Out ot Not out` == 0)
summary(ODI5$`Total Balls used by the team`)


df4 <- data.frame(BF = c(5:max(df3$BF)))

df4 <- df4 %>% left_join(df3,by="BF") %>% fill(names(.),.direction = "down")

plot(x=df4$BF,y=df4$baseline.surv, type = "s")

#Function to calculate the survival values

#Return the c value
ODI.survival.est <- function(good, poor, satisfied, very.good, exp.score, remain.overs, wickets, final.runs, strike.rate, baseline){
  est <- coeff.good*good + coeff.poor*poor + coeff.satisfied*satisfied + coeff.very.good*very.good + coeff.exp.score*exp.score + coeff.remaining.overs*remain.overs + coeff.wickets*wickets + coeff.final.runs*final.runs + coeff.strike.rate*strike.rate
  exp.est <- exp(est)
  survival <- baseline^(exp.est)
  return(exp.est)
}

#Loop to get all the c values for all the not out values

Not.out.balls <- c()
c.vec <- c()

for(i in 1:nrow(ODI5)){
  Not.out.balls <- c(Not.out.balls, ODI.notout$BF[i])
  c <- ODI.survival.est(ODI5$Good[i], ODI5$Poor[i], ODI5$Satisfied[i], ODI5$`Very Good`[i], exp(ODI5$Score[i]), ODI5$`Remaining Overs`[i], ODI5$Wickets[i], ODI5$`Final Runs`[i], ODI5$`Career SR`[i], ODI5$baseline.surv.y[i])
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
ODI.notout3 <- cbind(ODI5, "Numerator1" = df.final.4$Survival.Estimate, "Denominator1" = df.final.4$Denominator1)

#Expected balls
ODI.notout4 <- mutate(ODI.notout3,"Expected Balls" = Numerator1/Denominator1)


# Specify the file path where you want to save the Excel file
file_path <- "C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/R Codes/ODI Survival Final.xlsx"

# Write the data frame to an Excel file
write.xlsx(ODI.notout4, file_path)






#LASSO Regression

# Prepare the data
x <- data.matrix(ODI3[, c('Career Ave', 'Score', 'Wickets', 'Total Balls used by the team')])
y <- Surv(ODI3$Runs, ODI3$`Out ot Not out`)

# Fit Lasso Cox model
lasso.fit <- glmnet(x, y, family = "cox", alpha = 1)

# Perform cross-validation to select the optimal lambda value
cv.lasso <- cv.glmnet(x, y, family = "cox", alpha = 1)

# Get the optimal lambda value
opt_lambda <- cv.lasso$lambda.min

# Fit the final Lasso Cox model using the optimal lambda
final_fit <- glmnet(x, y, family = "cox", alpha = 1, lambda = opt_lambda)

# Extract the coefficients from the final model
coefficients <- coef(final_fit)

# Print the coefficients
print(coefficients)






#Testing part

#GPT Code

# Fit the survival model
surv_model <- survfit(Surv(ODI3$Runs, ODI3$`Out ot Not out`) ~ ODI3$`Batter Classification` + ODI3$Score + ODI3$`Remaining Overs` + ODI3$Wickets, data = ODI3)
plot(surv_model)

# Estimate survival probabilities
surv_prob <- summary(surv_model)$surv

# Compute cumulative hazard function
cumulative_hazard <- -log(surv_prob)

# Calculate maximum cumulative hazard
max_cumulative_hazard <- max(cumulative_hazard)

# Compute mean residual life function
mean_residual_life <- max_cumulative_hazard - cumulative_hazard