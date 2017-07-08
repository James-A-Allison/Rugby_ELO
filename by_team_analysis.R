analysis_table <- elo_table[elo_table$Date > "1995-08-25",]
analysis_table$Expected_win <- as.numeric(analysis_table$Expected_win)
analysis_table$Result <- as.numeric(analysis_table$Result)
analysis_table <- data.table(analysis_table)
analysis_table <- analysis_table[,list(Expected_wins=sum(Expected_win), Wins=sum(Result)),by=c("Team", "Opposition")]
analysis_table$Differnce <- analysis_table$Wins - analysis_table$Expected_wins

analysis_table_2 <- elo_table[elo_table$Date > "1995-08-25",]
analysis_table_2$Expected_win <- as.numeric(analysis_table_2$Expected_win)
analysis_table_2$Result <- as.numeric(analysis_table_2$Result)
analysis_table_2 <- data.table(analysis_table_2)
analysis_table_2 <- analysis_table_2[,list(Expected_wins=sum(Expected_win), Wins=sum(Result)),by=c("Team")]
analysis_table_2$Differnce <- analysis_table_2$Wins - analysis_table_2$Expected_wins