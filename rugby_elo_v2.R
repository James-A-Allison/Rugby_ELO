library(openxlsx)
library(dplyr)
library(tidyr)


# England -----------------------------------------------------------------


England <- readWorkbook("By_Team.xlsx", "England")
colnames(England) <- England[1,]
England <- England[England$Team == "England",]

pre_1900_England <- England[1:66,]
post_1990_England <- England[67:705,]

pre_1900_England$Date <- as.Date(pre_1900_England$`Match DateAscending`, format = "%d %B %Y")
post_1990_England$Date <- as.Date(as.numeric(post_1990_England$`Match DateAscending`), origin = "1899-12-30")
England <- rbind(pre_1900_England, post_1990_England)


# Wales -------------------------------------------------------------------

Wales <- readWorkbook("By_Team.xlsx", "Wales")
colnames(Wales) <- Wales[1,]
Wales <- Wales[Wales$Team == "Wales",]

pre_1900_Wales <- Wales[1:47,]
post_1990_Wales <- Wales[48:694,]

pre_1900_Wales$Date <- as.Date(pre_1900_Wales$`Match DateAscending`, format = "%d %B %Y")
post_1990_Wales$Date <- as.Date(as.numeric(post_1990_Wales$`Match DateAscending`), origin = "1899-12-30")
Wales <- rbind(pre_1900_Wales, post_1990_Wales)


# Ireland -----------------------------------------------------------------

Ireland <- readWorkbook("By_Team.xlsx", "Ireland")
colnames(Ireland) <- Ireland[1,]
Ireland <- Ireland[Ireland$Team == "Ireland",]

pre_1900_Ireland <- Ireland[1:64,]
post_1990_Ireland <- Ireland[65:667,]

pre_1900_Ireland$Date <- as.Date(pre_1900_Ireland$`Match DateAscending`, format = "%d %B %Y")
post_1990_Ireland$Date <- as.Date(as.numeric(post_1990_Ireland$`Match DateAscending`), origin = "1899-12-30")
Ireland <- rbind(pre_1900_Ireland, post_1990_Ireland)


# Scotland ----------------------------------------------------------------

Scotland <- readWorkbook("By_Team.xlsx", "Scotland")
colnames(Scotland) <- Scotland[1,]
Scotland <- Scotland[Scotland$Team == "Scotland",]

pre_1900_Scotland <- Scotland[1:67,]
post_1990_Scotland <- Scotland[68:664,]

pre_1900_Scotland$Date <- as.Date(pre_1900_Scotland$`Match DateAscending`, format = "%d %B %Y")
post_1990_Scotland$Date <- as.Date(as.numeric(post_1990_Scotland$`Match DateAscending`), origin = "1899-12-30")
Scotland <- rbind(pre_1900_Scotland, post_1990_Scotland)

