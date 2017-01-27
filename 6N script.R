library(openxlsx)
library(dplyr)
library(tidyr)

six_nations <- readWorkbook("Rugby_results_archive.xlsx", cols = 1:2)
# six_nations <- rename(six_nations, Date = ate)

test_character <- six_nations[1,2]
test_character <- gsub("[A-Z]", "", test_character)
test_character <- gsub("[a-z]", "", test_character)
test_character <- gsub("23 - 21", "", test_character)
test_character <- substr(test_character, 1, 1)

six_nations$Match <- gsub(test_character, " ", six_nations$Match)

six_nations <- separate(six_nations, Date, c("Month", "Day", "Year"), " ")
six_nations <- separate(six_nations, Match, c("Home.Team", "Home.Score", "v", "Away.Score", "Away.Team"), " ")
six_nations <- subset(six_nations, select = -v)
six_nations$Day <- gsub(",","",six_nations$Day)
six_nations$Home.Score <- as.numeric(six_nations$Home.Score)
six_nations$Away.Score <- as.numeric(six_nations$Away.Score)

six_nations$Date <- as.Date(paste0(six_nations$Day, six_nations$Month, six_nations$Year), "%d%b%Y")
six_nations <- six_nations[order(six_nations$Date),]

home_advantage <- mean(six_nations$Home.Score - six_nations$Away.Score)

six_nations <- subset(six_nations, select = -c(Month, Day, Year))
# test <- gather(six_nations, key = Team, value = Venue, c(Home.Team, Away.Team))




# ELO table setup ---------------------------------------------------------

inital_date <-  as.Date(paste0("1", "Jan", "1980"), "%d%b%Y")
teams <- unique(six_nations$Home.Team)
inital_ELO <- 1500


# elo_table <- data.frame(Match.Day=date(),
#                         Team=character(),
#                         ELO=numeric())
elo_table <- cbind(inital_date, teams, inital_ELO)
elo_table <- data.frame(elo_table, stringsAsFactors = F)
elo_table$inital_date <- inital_date
elo_table$inital_ELO <- as.numeric(elo_table$inital_ELO)
elo_table <- rename(elo_table, Date=inital_date)
elo_table <- rename(elo_table, Team=teams)
elo_table <- rename(elo_table, ELO=inital_ELO)

elo_table[elo_table$Team == "Italy",3] <- 700


# ELO function ------------------------------------------------------------

k <- 20

for (i in 1:length(six_nations[,1])) {

current_match <- six_nations[i,]

elo_home <- elo_table[elo_table$Team == current_match$Home.Team,3]
elo_away <- elo_table[elo_table$Team == current_match$Away.Team,3]

elo_home <- elo_home[length(elo_home)]
elo_away <- elo_away[length(elo_away)]

expected_score_home <- 1/(1+10^((elo_away-elo_home)/400))
expected_score_away <- 1/(1+10^((elo_home-elo_away)/400))

expected_score_home <- expected_score_home*2 - home_advantage
expected_score_away <- expected_score_away*2 + home_advantage

actual_net_home_score <- current_match$Home.Score - current_match$Away.Score
actual_net_away_score <- current_match$Away.Score - current_match$Home.Score

new_elo_home <- elo_home + k*(actual_net_home_score-expected_score_home)
new_elo_away <- elo_away + k*(actual_net_away_score-expected_score_away)

new_elo_row <- data.frame(Date=as.Date(current_match$Date), 
                          Team=as.character(current_match$Home.Team), 
                          ELO=as.numeric(new_elo_home))

elo_table <- rbind(elo_table, new_elo_row)

new_elo_row <- data.frame(Date=as.Date(current_match$Date), 
                          Team=as.character(current_match$Away.Team), 
                          ELO=as.numeric(new_elo_away))

elo_table <- rbind(elo_table, new_elo_row)
}