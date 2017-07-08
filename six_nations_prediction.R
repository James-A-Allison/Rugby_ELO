elo_Ireland <- elo_table[elo_table$Team == "Ireland",3]
elo_Ireland <- elo_Ireland[length(elo_Ireland)]

elo_Wales <- elo_table[elo_table$Team == "Wales",3]
elo_Wales <- elo_Wales[length(elo_Wales)]

elo_Scotland <- elo_table[elo_table$Team == "Scotland",3]
elo_Scotland <- elo_Scotland[length(elo_Scotland)]

elo_France <- elo_table[elo_table$Team == "France",3]
elo_France <- elo_France[length(elo_France)]

elo_Italy <- elo_table[elo_table$Team == "Italy",3]
elo_Italy <- elo_Italy[length(elo_Italy)]

elo_England <- elo_table[elo_table$Team == "England",3]
elo_England <- elo_England[length(elo_England)]



home_team <- c("Scotland",
               "England",
               "Italy",
               "Italy",
               "Wales",
               "France",
               "Scotland",
               "Ireland",
               "England",
               "Wales",
               "Italy",
               "England",
               "Scotland",
               "France",
               "Ireland")

away_team <- c("Ireland",
               "France",
               "Wales",
               "Ireland",
               "England",
               "Scotland",
               "Wales",
               "France",
               "Italy",
               "Ireland",
               "France",
               "Scotland",
               "Italy",
               "Wales",
               "England")


fixtures_2017 <- data.frame(Team=home_team,
                            Opposition=away_team,
                            expected_score_away=as.character(),
                            expected_score_home=as.character(),
                            stringsAsFactors = F)

for (i in 1:length(fixtures_2017[,1])) {
  
  current_match <- fixtures_2017[i,]
  
  elo_home <- elo_table[elo_table$Team == current_match$Team,3]
  elo_away <- elo_table[elo_table$Team == current_match$Opposition,3]
  
  elo_home <- elo_home[length(elo_home)]
  elo_away <- elo_away[length(elo_away)]
  
  home_q <- 10^(elo_home/400)
  away_q <- 10^(elo_away/400)
  
  total_q <- home_q + away_q
  s
  fixtures_2017$expected_score_home[i] <- home_q/total_q
  fixtures_2017$expected_score_away[i] <- away_q/total_q
  

}

home <- fixtures_2017[,c(1,3)]
away <- fixtures_2017[,c(2,4)]

colnames(home) <- c("Team", "Expected.Score")
colnames(away) <- c("Team", "Expected.Score")

predictions_2017 <- rbind(home, away)

library(data.table)

wins_2017 <- data.table(predictions_2017)
wins_2017 <- wins_2017[,list(win_totals=sum(Expected.Score)),by="Team"]