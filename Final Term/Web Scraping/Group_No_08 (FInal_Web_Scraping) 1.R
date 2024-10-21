install.packages("rvest")
install.packages("dplyr")
library(rvest)
library(dplyr)
url <- "https://fbref.com/en/comps/Big5/Big-5-European-Leagues-Stats"
page <- read_html(url)
Team <- page %>% html_nodes("img+ a") %>%  html_text()
Team
Country<- page %>% html_nodes(".left+ .left") %>%  html_text()
Country
Rank<- page %>% html_nodes(".right:nth-child(4)") %>%  html_text()
Rank
MatchPlayed<- page %>% html_nodes(".right:nth-child(5)") %>%  html_text()
MatchPlayed
Won<- page %>% html_nodes("tr~ tr+ tr .right:nth-child(6) , tr:nth-child(1) .right:nth-child(6) , tr:nth-child(2) .right:nth-child(5)") %>%  html_text()
Won
Draw<- page %>% html_nodes(".right:nth-child(7)") %>%  html_text()
Draw
Loss<- page %>% html_nodes("tr+ tr .right:nth-child(8) , tr:nth-child(1) .right:nth-child(7)") %>%  html_text()
Loss
GoalScored <- page %>% html_nodes(".right:nth-child(9)") %>%  html_text()
GoalScored
GoalAgainst <- page %>% html_nodes(".right:nth-child(10)") %>%  html_text()
GoalAgainst
GoalDifference<- page %>% html_nodes(".right:nth-child(11)") %>%  html_text()
GoalDifference
Points<- page %>% html_nodes(".right:nth-child(12)") %>%  html_text()
Points
ExpectedGoals<- page %>% html_nodes(".right:nth-child(14)") %>%  html_text()
ExpectedGoals
ExpectedGoalsAgainst<- page %>% html_nodes(".right:nth-child(15)") %>%  html_text()
ExpectedGoalsAgainst
ExpectedGoalsDifference<- page %>% html_nodes(".right:nth-child(16)") %>%  html_text()
ExpectedGoalsDifference
ExpectedGoalsPerMatch<- page %>% html_nodes(".right:nth-child(17)") %>%  html_text()
ExpectedGoalsPerMatch
Attendance<- page %>% html_nodes("td.right~ .left+ .right") %>%  html_text()
Attendance
TopScorer<- page %>% html_nodes(".right:nth-child(20)") %>%  html_text()
TopScorer
ds<-data.frame(Team, Country,Rank,MatchPlayed,Won,Draw,Loss,GoalScored,GoalAgainst,GoalDifference,Points,PointsPerMatch,ExpectedGoals,ExpectedGoalsAgainst,ExpectedGoalsDifference,ExpectedGoalsPerMatch,Attendance,TopScorer,stringsAsFactors = FALSE)
ds
write.csv(ds,"Group_No_08.csv",row.names=FALSE)

