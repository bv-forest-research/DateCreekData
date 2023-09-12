
Unit_Plot_Labels <- data.table::fread("D:/Github/DateCreekData/data-raw/Unit_Plot_Labels.csv")

usethis::use_data(Unit_Plot_Labels, overwrite = TRUE)

Deciduous_retention <- data.table::fread("D:/Github/DateCreekData/data-raw/Harvests_Plants/Decid_Harvest_Intensity_CC.csv")

usethis::use_data(Deciduous_retention, overwrite = TRUE)

Treatments <- data.table::data.table(Unit = c("A4", "B1", "C1", "D3",#No harvest
                                "A2", "B5", "C3", "D5",# light removal (30% BasalArea)
                                "B2", "B3", "C2", "D4",#heavy removal (60% BasalArea)
                                "A1", "A3", "B4", "D2"),#clearcut
                       Treatment = c(rep("NH",4),rep("LR",4),
                                     rep("HR",4),rep("CC",4)))

usethis::use_data(Treatments,overwrite=TRUE)
