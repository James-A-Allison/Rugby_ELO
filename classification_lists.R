classifications <- list(Six_Nations = c("England", "Wales", "Scotland", "Ireland", "France", "Italy"),
                        Tri_Nations = c("New Zealand", "Australia", "South Africa", "Argentina"),
                       
                        Tier2 = c("Fiji", "Japan", "Samoa", "Tonga",
                                  "Georgia", "Portugal", "Romania", "Russia", "Spain",
                                  "Canada", "United States of America", "Uruguay",
                                  "Namibia")
                        )

classifications$Tier1 = c(classifications$Six_Nations, classifications$Tri_Nations)