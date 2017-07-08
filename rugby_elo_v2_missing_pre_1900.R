library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)

modern_teams <- getSheetNames("By_Team.xlsx")
# modern_teams <- modern_teams[!(modern_teams %in% home_nations)]

for (i in 1:length(modern_teams)) {
  team <- modern_teams[i]
  
  Team <- readWorkbook("By_Team.xlsx", team)
  colnames(Team) <- Team[1,]
  colnames(Team)[10] <- "Match Date" 
  Team <- Team[Team$Team == team,]
  Team$Date <- as.Date(as.numeric(Team$`Match Date`), origin = "1899-12-30")
  Team <- subset(Team, select = c(Team, Result, For, Aga, Diff, Opposition, Ground, Date))
  if (i == 1) {all_teams <- Team} else {all_teams <- rbind(all_teams, Team)}
  
}

all_teams <- all_teams[order(all_teams$Date),]
all_teams$Opposition <- gsub("v ", "", all_teams$Opposition)
all_teams <- all_teams[all_teams$Opposition %in% modern_teams,]
all_teams <- all_teams[!duplicated(all_teams[c("Date", "Ground")]),]
all_teams <- all_teams[!is.na(all_teams$Date),]
all_teams$Diff <- gsub("G", "", all_teams$Diff)
all_teams$Diff <- as.numeric(all_teams$Diff) 

# ELO table setup ---------------------------------------------------------

inital_date <-  as.Date(paste0("1", "Jan", "1880"), "%d%b%Y")
inital_ELO <- 1500

elo_table <- cbind(inital_date, modern_teams, inital_ELO)
elo_table <- data.frame(elo_table, stringsAsFactors = F)
elo_table$inital_date <- inital_date
elo_table$inital_ELO <- as.numeric(elo_table$inital_ELO)
elo_table <- rename(elo_table, Date=inital_date)
elo_table <- rename(elo_table, Team=modern_teams)
elo_table <- rename(elo_table, ELO=inital_ELO)
elo_table$Expected_win <- ""
elo_table$Result <- ""
elo_table$Opposition <- ""
# elo_table$Competition <- ""

# ELO function ------------------------------------------------------------

k <- 25

# run_elo <- function() {


for (i in 1:length(all_teams[,1])) {
  
  current_match <- all_teams[i,]
  
  elo_home <- elo_table[elo_table$Team == current_match$Team,3]
  elo_away <- elo_table[elo_table$Team == current_match$Opposition,3]
  
  elo_home <- elo_home[length(elo_home)]
  elo_away <- elo_away[length(elo_away)]
  
  home_q <- 10^(elo_home/400)
  away_q <- 10^(elo_away/400)
  
  total_q <- home_q + away_q
  
  expected_score_home <- home_q/total_q
  expected_score_away <- away_q/total_q
  
  # expected_score_home <- 1/(1+10^((elo_away-elo_home)/400))
  # expected_score_away <- 1/(1+10^((elo_home-elo_away)/400))
  
  if (current_match$Diff > 0) {
    actual_net_home_score <- 1
    actual_net_away_score <- 0
    winning_team_elo <- elo_home
    losing_team_elo <- elo_away
    margin_of_victory <- log(abs(current_match$Diff+1))*(2.2/((winning_team_elo-losing_team_elo)*.001+2.2))
  } else if (current_match$Diff < 0) {
    actual_net_home_score <- 0
    actual_net_away_score <- 1
    winning_team_elo <- elo_away
    losing_team_elo <- elo_home
    margin_of_victory <- log(abs(current_match$Diff+1))*(2.2/((winning_team_elo-losing_team_elo)*.001+2.2))
   } else {
    actual_net_home_score <- 0.5
    actual_net_away_score <- 0.5
    winning_team_elo <- 0
    losing_team_elo <- 0
    margin_of_victory <- 1
   }
  
  if (is.infinite(margin_of_victory)) {margin_of_victory <- 1}
  
  new_elo_home <- elo_home + k*(actual_net_home_score-expected_score_home)*margin_of_victory
  new_elo_away <- elo_away + k*(actual_net_away_score-expected_score_away)*margin_of_victory
  
  # new_elo_home <- elo_home + k*(actual_net_home_score-expected_score_home)
  # new_elo_away <- elo_away + k*(actual_net_away_score-expected_score_away)
  
  new_elo_row <- data.frame(Date=as.Date(current_match$Date), 
                            Team=as.character(current_match$Team), 
                            ELO=as.numeric(new_elo_home),
                            Expected_win=as.numeric(expected_score_home),
                            Result=as.numeric(actual_net_home_score),
                            Opposition=as.character(current_match$Opposition))
  
  elo_table <- rbind(elo_table, new_elo_row)
  
  new_elo_row <- data.frame(Date=as.Date(current_match$Date), 
                            Team=as.character(current_match$Opposition), 
                            ELO=as.numeric(new_elo_away),
                            Expected_win=as.numeric(expected_score_away),
                            Result=as.numeric(actual_net_away_score),
                            Opposition=as.character(current_match$Team))
  
  elo_table <- rbind(elo_table, new_elo_row)
}
# }
plot_table <- elo_table
plot_table$Team <- as.factor(plot_table$Team)

# qplot(Date, ELO, data = plot_table, color = Team, geom = "line")

qplot(Date, ELO, data = plot_table[plot_table$Date > "1980-01-01",], color = Team, geom = "step")

new_six_nations <- plot_table[(plot_table$Team == "New Zealand" &# | plot_table$Team == "Wales" |
                              # plot_table$Team == "Ireland" | plot_table$Team == "France" |
                              # plot_table$Team == "Scotland" | plot_table$Team == "Italy") #&
                               plot_table$Date > "1980-01-01"),]

qplot(Date, ELO, data = new_six_nations, color = Team, geom = "step")