library(readxl)
library(ggplot2)
library(dplyr)

#ODI

Opening_Partnership_for_1st_innings_in_ODI <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 1st innings in ODI.xlsx")

Opening_Partnership_for_2nd_innings_in_ODI <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 2nd innings in ODI.xlsx")

ODI = rbind(Opening_Partnership_for_1st_innings_in_ODI,Opening_Partnership_for_2nd_innings_in_ODI)
ODI

ggplot(data = ODI, aes(x = `Start.Date`, y = `NPI`, color=`Inns`)) + geom_point() + geom_line() + labs(title="NPI vs Match Date for Opening Partnership Against the Match Result Colored on Innings in ODI", x="Match Date", y="NPI") + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ Result , ncol=1)
ggplot(data = ODI, aes(x = `Start.Date`, y = `NPI`, color=`Result`)) + geom_point() + geom_line() + labs(title="NPI vs Match Date for Opening Partnership Against the Innings Colored on Match Result in ODI", x="Match Date", y="NPI") + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ Inns , ncol=1)



#T20I

Opening_Partnership_for_1st_innings_in_T20I <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 1st innings in T20I.xlsx")

Opening_Partnership_for_2nd_innings_in_T20I <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 2nd innings in T20I.xlsx")

T20I = rbind(Opening_Partnership_for_1st_innings_in_T20I,Opening_Partnership_for_2nd_innings_in_T20I)
T20I

ggplot(data = T20I, aes(x = `Start.Date`, y = `NPI`, color=`Inns`)) + geom_point() + geom_line() + labs(title="NPI vs Match Date for Opening Partnership Against the Match Result Colored on Innings in T20I", x="Match Date", y="NPI") + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ Result , ncol=1)
ggplot(data = T20I, aes(x = `Start.Date`, y = `NPI`, color=`Result`)) + geom_point() + geom_line() + labs(title="NPI vs Match Date for Opening Partnership Against the Innings Colored on Match Result in T20I", x="Match Date", y="NPI") + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ Inns , ncol=1)




#Test

Opening_Partnership_for_1st_innings_in_Test <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 1st innings in Test.xlsx")

Opening_Partnership_for_2nd_innings_in_Test <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 2nd innings in Test.xlsx")

Opening_Partnership_for_3rd_innings_in_Test <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 3rd innings in Test.xlsx")

Opening_Partnership_for_4th_innings_in_Test <- read_excel("C:/Slides/Slides 4th year 2nd Semester/Final Research/2023.01.18/Opening Partnership Effect/Opening Partnership for 4th innings in Test.xlsx")


Test = rbind(Opening_Partnership_for_1st_innings_in_Test, Opening_Partnership_for_2nd_innings_in_Test, Opening_Partnership_for_3rd_innings_in_Test, Opening_Partnership_for_4th_innings_in_Test)
Test

ggplot(data = Test, aes(x = `Start.Date`, y = `NPI`, color=`Inns`)) + geom_point() + geom_line() + labs(title="NPI vs Match Date for Opening Partnership Against the Match Result Colored on Innings in Test", x="Match Date", y="NPI") + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ Result , ncol=1)
ggplot(data = Test, aes(x = `Start.Date`, y = `NPI`, color=`Result`)) + geom_point() + geom_line() + labs(title="NPI vs Match Date for Opening Partnership Against the Innings Colored on Match Result in Test", x="Match Date", y="NPI") + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ Inns , ncol=1)
