current_ratings <- function(elo_table_input = elo_table, teams_in = NA) {
  elo_table <- elo_table_input
  output_ratings <- c()
  teams <- unique(elo_table$Team)
  if (!is.na(teams_in)) {
    teams <- teams[teams %in% teams_in]
  }
  for (i in 1:length(teams)) {
    team <- teams[i]
    elo <- elo_table[elo_table$Team == team,3]
    elo <- elo[length(elo)]
    row <- data.frame(Team = as.character(team), 
                      Elo = as.numeric(elo))
    output_ratings <- rbind(output_ratings, row)
  }
  output_ratings <-  output_ratings[order(output_ratings$Elo, decreasing = TRUE),]
  row.names(output_ratings) <- NULL 
  return(output_ratings)
}

average_rating <- function(date_range = NA, teams_in = NA, elo_table_input = elo_table) {
  dates <- unique(elo_table_input$Date)
  output_table <- c()
  for (i in 1:length(dates)) {
    date <- dates[i]
    working_elo_table <- elo_table_input[elo_table_input$Date <= date,]
    mean_elo <- mean(current_ratings(elo_table_input = working_elo_table, teams_in = teams_in)[,2])
    output_row <- data.frame(Date=as.Date(date),
                             Average_Elo=as.numeric(mean_elo))
    output_table <- rbind(output_table, output_row)
  }
  return(output_table)
}

optimise_elo <- function(k = 20) {
  k <- k
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
  
  for (i in 1:length(tests_out[,1])) {
    
    current_match <- tests_out[i,]
    
    if(current_match$Team_A %in% unique(elo_table$Team)) {
      elo_home <- elo_table[elo_table$Team == current_match$Team_A,3]
      elo_home <- elo_home[length(elo_home)]
    } else {
      elo_home <- 1500
    }
    
    if(current_match$Team_B %in% unique(elo_table$Team)) {
      elo_away <- elo_table[elo_table$Team == current_match$Team_B,3]
      elo_away <- elo_away[length(elo_away)]
    } else {
      elo_away <- 1500
    }
    
    home_q <- 1/(1+10^(elo_home/400))
    away_q <- 1/(1+10^(elo_away/400))
    
    total_q <- home_q + away_q
    
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
  elo_table <- elo_table[is.numeric(elo_table$Result),]
  elo_table$sum_of_sq_diff <- (elo_table$Expected_win - elo_table$Result)^2
  return(sum(elo_table$sum_of_sq_diff))
}

generate_fixtures <- function(teams = NA, times_vs_opp = NA) {
  matches <- data.frame(combn(teams, 2))
  matches$Slot <- c("Team_A","Team_B")
  suppressWarnings(matches <- gather(matches, key = Slot))
  colnames(matches)[2] <- "Match_number"
 suppressWarnings(matches <- spread(matches, key = Slot, value = value))  
  if(times_vs_opp == 1) {return(matches)}
  else if (times_vs_opp == 2) {return(rbind(matches, matches[,c(1,3,2)]))} #switching columns round not working yet
  else {print("Not ready yet")}
}

sim_tournament <- function(fixtures = NA){
  teams <- unique(c(fixtures$Team_A, fixtures$Team_B))
  league_table <- data.frame(Team = as.character(teams),
                             Points = as.numeric(rep(0,length(teams))))
  
  for (i in 1:length(fixtures[,1])) {
    match <- fixtures[i,]
    team_A <- match$Team_A
    team_B <- match$Team_B
    
    elo_team_A <- current_ratings(teams_in = team_A)[,2]
    elo_team_B <- current_ratings(teams_in = team_B)[,2]
    
    expected_score_home <- 1/(1+10^((elo_team_B-elo_team_A)/400))
    expected_score_away <- 1/(1+10^((elo_team_A-elo_team_B)/400))
    
    winner <- sample(c(team_A, team_B),
                     size = 1,
                     replace = TRUE,
                     prob = c(expected_score_home, 1-expected_score_home))
    if (winner == team_A) {
      league_table$Points[league_table$Team == team_A] <- league_table$Points[league_table$Team == team_A] + 3
    } else {
      league_table$Points[league_table$Team == team_B] <- league_table$Points[league_table$Team == team_B] + 3
    }
  }
  league_table <- league_table[order(league_table$Points, decreasing = T),]
  league_table$Team <- as.character(league_table$Team)
  return(league_table)
}

sim_game <- function(teams = NA){
  
    team_A <- teams[1]
    team_B <- teams[2]
    
    elo_team_A <- current_ratings(teams_in = team_A)[,2]
    elo_team_B <- current_ratings(teams_in = team_B)[,2]
    
    expected_score_home <- 1/(1+10^((elo_team_B-elo_team_A)/400))
    expected_score_away <- 1/(1+10^((elo_team_A-elo_team_B)/400))
    
    winner <- sample(c(team_A, team_B),
                     size = 1,
                     replace = TRUE,
                     prob = c(expected_score_home, 1-expected_score_home))

  return(winner)
}
