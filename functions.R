current_ratings <- function() {
  output_ratings <- c()
  teams <- unique(elo_table$Team)
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

average_elo_out <- c()
unique_match_dates <- unique(elo_table$Date)
for (i in 1:length(unique_match_dates)) {
  date <- unique_match_dates[i]
  small_table <- elo_table[elo_table$Date <= date,]
  teams <- unique(small_table$Team)
  output_ratings <- c()
  for (j in 1:length(teams)) {
    team <- teams[j]
    elo <- small_table[small_table$Team == team,3]
    elo <- elo[length(elo)]
    row <- data.frame(Team = as.character(team), 
                      Elo = as.numeric(elo))
    output_ratings <- rbind(output_ratings, row)
  }
  ave_elo <- mean(output_ratings$Elo)
  row <- data.frame(Date = as.Date(date),
                    Average.Elo = as.numeric(ave_elo))
  average_elo_out <- rbind(average_elo_out, row)
}