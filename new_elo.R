
# Library -----------------------------------------------------------------
library(openxlsx)
library(dplyr)

new_country <- function(){
  current_teams <- unique(elo_table$Team)
  elo <- c()
  for (i in 1:length(current_teams)) {
    team <- current_teams[i]
    all_elo <- elo_table[elo_table$Team == team,3]
    all_elo <- all_elo[length(all_elo)]
    elo <- c(elo,all_elo)
    elo_out <- min(elo)
  }
  elo_out <- min(elo)
  return(elo_out)
}


# Import ------------------------------------------------------------------
file <- "Rugby_db.xlsx"
tests_cleaned <- readWorkbook(file,"Tests_cleaned", cols = 1:10)
grounds <- readWorkbook(file, "Grounds")
teams <- readWorkbook(file, "Teams")

# Cleaning ----------------------------------------------------------------

tests_cleaned$Match.Date <- as.Date(as.numeric(tests_cleaned$Match.Date), origin = "1900-01-01")
tests_cleaned <- tests_cleaned[!is.na(tests_cleaned$Match.Date),]
tests_cleaned$Opposition <- sub("v ", "", tests_cleaned$Opposition)

teams$Team.out[is.na(teams$Team.out)] <- teams$Team.in[is.na(teams$Team.out)]
tests_cleaned <- left_join(tests_cleaned, teams, by = c("Team" = "Team.in"))
tests_cleaned <- select(tests_cleaned, -c(Team, `Non-country`))
tests_cleaned <- rename(tests_cleaned, Team = Team.out)

tests_cleaned <- left_join(tests_cleaned, teams, by = c("Opposition" = "Team.in"))
tests_cleaned <- select(tests_cleaned, -c(Opposition, `Non-country`))
tests_cleaned <- rename(tests_cleaned, Opposition = Team.out)

tests_cleaned <- left_join(tests_cleaned, grounds)

tests_cleaned <- tests_cleaned[!is.na(tests_cleaned$Home.Team),]
tests_cleaned <- tests_cleaned[!is.na(tests_cleaned$Opposition),]

tests_cleaned$Home_Team <- NA
tests_cleaned$Away_Team <- NA

tests_cleaned$Home_Team[tests_cleaned$Team == tests_cleaned$Home.Team] <- tests_cleaned$Team[tests_cleaned$Team == tests_cleaned$Home.Team]
tests_cleaned$Home_Team[tests_cleaned$Opposition == tests_cleaned$Home.Team] <- tests_cleaned$Opposition[tests_cleaned$Opposition == tests_cleaned$Home.Team]
tests_cleaned$Home_Team[is.na(tests_cleaned$Home_Team)] <- "Neutral"
tests_cleaned$Away_Team[tests_cleaned$Home_Team == "Neutral"] <- "Neutral"

tests_cleaned$Away_Team[tests_cleaned$Team == tests_cleaned$Home_Team] <- tests_cleaned$Opposition[tests_cleaned$Team == tests_cleaned$Home_Team]
tests_cleaned$Away_Team[is.na(tests_cleaned$Away_Team)] <- tests_cleaned$Team[is.na(tests_cleaned$Away_Team)]

tests_cleaned$Team_A <- NA
tests_cleaned$Team_B <- NA

tests_cleaned$Team_A[tests_cleaned$Home_Team != "Neutral"] <- tests_cleaned$Home_Team[tests_cleaned$Home_Team != "Neutral"]
tests_cleaned$Team_B[tests_cleaned$Away_Team != "Neutral"] <- tests_cleaned$Away_Team[tests_cleaned$Away_Team != "Neutral"]
tests_cleaned$Team_A[is.na(tests_cleaned$Team_A)] <- tests_cleaned$Team[is.na(tests_cleaned$Team_A)]
tests_cleaned$Team_B[is.na(tests_cleaned$Team_B)] <- tests_cleaned$Opposition[is.na(tests_cleaned$Team_B)]

tests_cleaned$A_points <- NA
tests_cleaned$B_points <- NA

tests_cleaned$A_points[tests_cleaned$Team == tests_cleaned$Team_A] <- tests_cleaned$For[tests_cleaned$Team == tests_cleaned$Team_A]
tests_cleaned$B_points[tests_cleaned$Team == tests_cleaned$Team_A] <- tests_cleaned$Aga[tests_cleaned$Team == tests_cleaned$Team_A]

tests_cleaned$A_points[tests_cleaned$Opposition == tests_cleaned$Team_A] <- tests_cleaned$Aga[tests_cleaned$Opposition == tests_cleaned$Team_A]
tests_cleaned$B_points[tests_cleaned$Opposition == tests_cleaned$Team_A] <- tests_cleaned$For[tests_cleaned$Opposition == tests_cleaned$Team_A]

tests_out <- c()
for (i in 2:length(tests_cleaned[,1])) {
  if(paste0(tests_cleaned$Team_A[i],tests_cleaned$Team_B[i],tests_cleaned$Match.Date[i]) == 
     paste0(tests_cleaned$Team_A[i-1],tests_cleaned$Team_B[i-1],tests_cleaned$Match.Date[i-1])) {
    tests_out <- rbind(tests_out,tests_cleaned[i,])
  }
  if(paste0(tests_cleaned$Team_A[i],tests_cleaned$Team_B[i],tests_cleaned$Match.Date[i]) == 
     paste0(tests_cleaned$Team_B[i-1],tests_cleaned$Team_A[i-1],tests_cleaned$Match.Date[i-1])) {
    tests_out <- rbind(tests_out,tests_cleaned[i,])
  }
}

tests_out$A_points <- as.numeric(tests_out$A_points)
tests_out$B_points <- as.numeric(tests_out$B_points)
tests_out$Diff <- tests_out$A_points - tests_out$B_points
tests_out <- tests_out[!is.na(tests_out$Diff),]

tests_out <- select(tests_out, c(Team_A, Team_B, A_points, B_points, Diff, Ground, Home.Team, Match.Date))

# ELO table setup ---------------------------------------------------------

inital_date <-  as.Date(paste0("1", "Jan", "1900"), "%d%b%Y")
inital_ELO <- 1500
modern_teams <- c("England", "Wales")

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

k <- 20

# run_elo <- function() {


for (i in 1:length(tests_out[,1])) {
  
  current_match <- tests_out[i,]
  
  if(current_match$Team_A %in% unique(elo_table$Team)) {
    elo_home <- elo_table[elo_table$Team == current_match$Team_A,3]
    elo_home <- elo_home[length(elo_home)]
  } else {
    # elo_home <- new_country()
    elo_home <- 1500
  }
  
  if(current_match$Team_B %in% unique(elo_table$Team)) {
    elo_away <- elo_table[elo_table$Team == current_match$Team_B,3]
    elo_away <- elo_away[length(elo_away)]
  } else {
    # elo_away <- new_country()
    elo_away <- 1500
  }
  
  home_q <- 1/(1+10^(elo_home/400))
  away_q <- 1/(1+10^(elo_away/400))
  
  total_q <- home_q + away_q
  
  # expected_score_home <- home_q/total_q
  # expected_score_away <- away_q/total_q
  
  expected_score_home <- 1/(1+10^((elo_away-elo_home)/400))
  expected_score_away <- 1/(1+10^((elo_home-elo_away)/400))
  
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
  
  new_elo_row <- data.frame(Date=as.Date(current_match$Match.Date), 
                            Team=as.character(current_match$Team_A), 
                            ELO=as.numeric(new_elo_home),
                            Expected_win=as.numeric(expected_score_home),
                            Result=as.numeric(actual_net_home_score),
                            Opposition=as.character(current_match$Team_B))
  
  elo_table <- rbind(elo_table, new_elo_row)
  
  new_elo_row <- data.frame(Date=as.Date(current_match$Match.Date), 
                            Team=as.character(current_match$Team_B), 
                            ELO=as.numeric(new_elo_away),
                            Expected_win=as.numeric(expected_score_away),
                            Result=as.numeric(actual_net_away_score),
                            Opposition=as.character(current_match$Team_A))
  
  elo_table <- rbind(elo_table, new_elo_row)
}
# }

plot_table <- elo_table
plot_table$Team <- as.factor(plot_table$Team)

# qplot(Date, ELO, data = plot_table, color = Team, geom = "line")

qplot(Date, ELO, data = plot_table[plot_table$Team == "Georgia" | plot_table$Team == "Italy",], color = Team, geom = "step")

new_six_nations <- plot_table[((plot_table$Team == "New Zealand" | plot_table$Team == "Wales" |
                                  plot_table$Team == "Ireland" | plot_table$Team == "France" |
                                  plot_table$Team == "Scotland" | plot_table$Team == "Italy") &
                                 plot_table$Date > "1980-01-01"),]

qplot(Date, ELO, data = new_six_nations, color = Team, geom = "step")

new_six_nations <- plot_table[(plot_table$Team %in% classifications$Six_Nations | plot_table$Team %in% classifications$Tri_Nations) &
                                 plot_table$Date > "2000-01-01",]

qplot(Date, ELO, data = new_six_nations, color = Team, geom = "step")