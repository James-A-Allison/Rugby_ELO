Ireland_home_advantage <- mean(six_nations$Home.Score[six_nations$Home.Team == "Ireland"] - six_nations$Away.Score[six_nations$Home.Team == "Ireland"])
England_home_advantage <- mean(six_nations$Home.Score[six_nations$Home.Team == "England"] - six_nations$Away.Score[six_nations$Home.Team == "England"])
Wales_home_advantage <- mean(six_nations$Home.Score[six_nations$Home.Team == "Wales"] - six_nations$Away.Score[six_nations$Home.Team == "Wales"])
Scotland_home_advantage <- mean(six_nations$Home.Score[six_nations$Home.Team == "Scotland"] - six_nations$Away.Score[six_nations$Home.Team == "Scotland"])
France_home_advantage <- mean(six_nations$Home.Score[six_nations$Home.Team == "France"] - six_nations$Away.Score[six_nations$Home.Team == "France"])
Italy_home_advantage <- mean(six_nations$Home.Score[six_nations$Home.Team == "Italy"] - six_nations$Away.Score[six_nations$Home.Team == "Italy"])

Ireland_home_advantage_ex <- mean(six_nations$Home.Score[six_nations$Home.Team == "Ireland" & six_nations$Away.Team != "Italy"] - 
                                    six_nations$Away.Score[six_nations$Home.Team == "Ireland" & six_nations$Away.Team != "Italy"])
England_home_advantage_ex <- mean(six_nations$Home.Score[six_nations$Home.Team == "England" & six_nations$Away.Team != "Italy"] - 
                                    six_nations$Away.Score[six_nations$Home.Team == "England" & six_nations$Away.Team != "Italy"])
Wales_home_advantage_ex <- mean(six_nations$Home.Score[six_nations$Home.Team == "Wales" & six_nations$Away.Team != "Italy"] - 
                                    six_nations$Away.Score[six_nations$Home.Team == "Wales" & six_nations$Away.Team != "Italy"])
France_home_advantage_ex <- mean(six_nations$Home.Score[six_nations$Home.Team == "France" & six_nations$Away.Team != "Italy"] - 
                                    six_nations$Away.Score[six_nations$Home.Team == "France" & six_nations$Away.Team != "Italy"])
Scotland_home_advantage_ex <- mean(six_nations$Home.Score[six_nations$Home.Team == "Scotland" & six_nations$Away.Team != "Italy"] - 
                                    six_nations$Away.Score[six_nations$Home.Team == "Scotland" & six_nations$Away.Team != "Italy"])





print(paste0("England home advantage ", round(England_home_advantage,1), " points (including Italy), ", round(England_home_advantage_ex,1), " points (excluding Italy)"))
print(paste0("France home advantage ", round(France_home_advantage,1), " points (including Italy), ", round(France_home_advantage_ex,1), " points (excluding Italy)"))
print(paste0("Ireland home advantage ", round(Ireland_home_advantage,1), " points (including Italy), ", round(Ireland_home_advantage_ex,1), " points (excluding Italy)"))
print(paste0("Wales home advantage ", round(Wales_home_advantage,1), " points (including Italy), ", round(Wales_home_advantage_ex,1), " points (excluding Italy)"))
print(paste0("Scotland home advantage ", round(Scotland_home_advantage,1), " points (including Italy), ", round(Scotland_home_advantage_ex,1), " points (excluding Italy)"))
print(paste0("Italy home advantage ", round(Italy_home_advantage,1), " points"))