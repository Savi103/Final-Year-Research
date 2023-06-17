library(XML)
library(httr)
library(tidyr)
library(xlsx)
library(openxlsx)

mydata <- htmlParse(rawToChar(GET("https://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;filter=advanced;innings_number=4;orderby=fow_balls_faced;partnership_wicketmax1=1;partnership_wicketval1=partnership_wicket;size=200;spanmax1=31+Dec+2021;spanmin1=01+Jan+2005;spanval1=span;template=results;type=fow;view=innings")$content))
batsman <- readHTMLTable(mydata)
batsman1 <- data.frame(unname(batsman[3]))

pages <- 2:3
pageLinks <- paste("https://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;filter=advanced;innings_number=4;orderby=fow_balls_faced;page=",pages,";partnership_wicketmax1=1;partnership_wicketval1=partnership_wicket;size=200;spanmax1=31+Dec+2021;spanmin1=01+Jan+2005;spanval1=span;template=results;type=fow;view=innings", sep = "")

for (i in 1:length(pages)) {
  mydata <- htmlParse(rawToChar(GET(pageLinks[i])$content))
  byPage <- readHTMLTable(mydata)
  byPage1<-data.frame(unname(byPage[3]))
  
  batsman1 <- rbind(batsman1, byPage1)
  
}

write.csv(batsman1, file = "Opening Partnership for 1st innings in ODI.csv")
write.xlsx(batsman1, file = "Opening Partnership for 4th innings in Test.xlsx")

