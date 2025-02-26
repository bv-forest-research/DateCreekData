

#' Title
#'
#' @param cruise_data
#'
#' @return
#' @export
#'
#' @examples
get_all_plots_1992 <- function(cruise_data = "./data-raw/Trees/1992data.csv"){
  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  dat.1992.cruise <- read.csv(cruise_data, stringsAsFactors = FALSE)
  dat.1992.cruise <- subset(dat.1992.cruise, dat.1992.cruise$PLOT_TYPE == "M")
  # Create unique plot names
  dat.1992.cruise <- dat.1992.cruise %>%
    dplyr::mutate(PlotNum = paste0(STRIP, ".", PLOT))

  # Rename columns to match other years
  names(dat.1992.cruise )[names(dat.1992.cruise ) == "BLOCK"] <- "Unit"

  # Rename units to match other years
  dat.1992.cruise <- dat.1992.cruise %>%
    dplyr::mutate(Unit = replace(Unit, Unit == "0A1", "A1"),
                  Unit = replace(Unit, Unit == "0A2", "A2"),
                  Unit = replace(Unit, Unit == "0A3", "A3"),
                  Unit = replace(Unit, Unit == "0A4", "A4"),
                  Unit = replace(Unit, Unit == "0B1", "B1"),
                  Unit = replace(Unit, Unit == "0B2", "B2"),
                  Unit = replace(Unit, Unit == "0B3", "B3"),
                  Unit = replace(Unit, Unit == "0B4", "B4"),
                  Unit = replace(Unit, Unit == "0B5", "B5"),
                  Unit = replace(Unit, Unit == "0C1", "C1"),
                  Unit = replace(Unit, Unit == "0C2", "C2"),
                  Unit = replace(Unit, Unit == "0C3", "C3"),
                  Unit = replace(Unit, Unit == "0D2", "D2"),
                  Unit = replace(Unit, Unit == "0D3", "D3"),
                  Unit = replace(Unit, Unit == "0D4", "D4"),
                  Unit = replace(Unit, Unit == "0D5", "D5"))

  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  labels1992 <- dat.1992.cruise %>%
    select(Unit, PlotNum, DBH) %>%
    group_by(Unit, PlotNum) %>%
    summarise(count = n()) %>%
    select(-count) %>%
    mutate(Year = 1992)

  return(labels1992)
}


#' Make plot labels for 1993
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#'
#'
get_all_plots_1993 <- function(data = "./data-raw/Trees/SS93forR.csv"){
  # Read in the data for both fixed radius and cruise plots
  dat.1993 <- read.csv(data, stringsAsFactors = FALSE)

  # Rename columns to match other years
  names(dat.1993)[names(dat.1993) == "Plot_ID"] <- "Unit"
  names(dat.1993)[names(dat.1993) == "SPP"] <- "Spp"
  names(dat.1993)[names(dat.1993) == "TC"] <- "Tree.Class"


  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  dat.1993$count <- rep(1, length(dat.1993$Unit ))
  #Plot_in_Units #stands should have 23 or 30 plots (30 for 40% retention treatments)
  Trees_in_Plots <- dat.1993 %>%
    select(Unit, PlotNum, count) %>%
    group_by(Unit, PlotNum) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit ))

  Plot_in_Units <- Trees_in_Plots %>%
    select(Unit, count) %>%
    group_by(Unit) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  labels1993 <- Trees_in_Plots %>%
    select(Unit, PlotNum)%>%
    mutate(Year = 1993)

  return(labels1993)
}

#' Title
#'
#' @param lrg_trees
#' @param cc_trees
#' @param small_trees
#' @param all_plots FALSE = trees > dbh only, TRUE = all plots
#'
#' @return
#' @export
#'
#' @examples

get_all_plots_2010 <- function(lrg_trees = "./data-raw/Trees/Data Creek 2010 Data large trees.csv",
                               cc_trees = "./data-raw/Trees/Trees 10cm and above in clearcut 2010.csv",
                               small_trees = "./data-raw/Trees/Date Creek 2010 Trees less than 10 cm tallies.csv",
                               all_plots = TRUE){

  # Read in the data for large trees (10+cm DBH) - No Clear Cut plots
  dat.2010 <- read.csv(lrg_trees, stringsAsFactors = FALSE, na.strings = "n/a")
  dat.2010.CC <- read.csv(cc_trees, stringsAsFactors = FALSE)
  dat.2010.sm <- read.csv(small_trees) #tallied trees with dbh <4

  # Rename columns/ variables to match other years
  dat.2010$Unit <- as.factor(dat.2010$Unit)
  names(dat.2010)[names(dat.2010) == "Tree.No."] <- "Tree.No" #Rename
  names(dat.2010)[names(dat.2010) == "DBH.2010"] <- "DBH" #Rename
  dat.2010$Spp[which(dat.2010$Spp == "Sw")] <- "Sx"

  dat.2010 <- corr_trees_2010(dat.2010)
  dat.2010 <- remove_plots_trees(dat.2010)

  dat1 <- dat.2010 %>% dplyr::select(Unit, Gd.Pt) %>% distinct(Unit, Gd.Pt)
  dat2 <- dat.2010.CC %>% dplyr::select(Unit, Gd.Pt) %>% distinct(Unit, Gd.Pt)
  dat3 <- dat.2010.sm %>% dplyr::select(Unit, Gd.Pt) %>% distinct(Unit, Gd.Pt)

  if(all_plots){
    print("all plots included (large, cc, & small trees (<4cm dbh)")
    dat4 <- rbind(dat1, dat2, dat3)
    dat5 <- tibble(dat4 %>% distinct(Unit, Gd.Pt)) %>%
      mutate(Year = 2010) %>%
      rename(PlotNum = Gd.Pt)
  }else{
    print("")
  }

  #this is old code, and it's not clear
  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  dat.2010$count <- rep(1, length(dat.2010$Unit))
  Trees_in_Plots <- dat.2010 %>%
    select(Unit, Gd.Pt, Plot.Size, count) %>%
    group_by(Unit, Gd.Pt, Plot.Size) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit ))
  Plot_in_Units <- Trees_in_Plots %>%
    select(Unit, count) %>%
    group_by(Unit) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments)
  # C2 is down 1 plot; B2, B5 and C3 down 1 plot each
  #no clear-cut large tree plots

  labels2010 <- Trees_in_Plots %>%
    select(Unit, Gd.Pt) %>%
    rename(PlotNum = Gd.Pt)%>%
    mutate(Year = 2010)

  return(dat5)
}



#' Title
#'
#' @param data_file
#' @param data_2018
#' @param data_2019
#'
#' @return
#' @export
#'
#' @examples
get_all_plots_201x <- function(data_file = "./data-raw/Trees/Date Creek 2018 Data large trees_re-entered.xlsx",
                               data_2018 = "DataCk re-entry 2018 largeTrees",
                               data_2019 = "./data-raw/Trees/Data Creek 2019 Data large trees.csv"){

  dat.2018 <- openxlsx::read.xlsx(data_file,
                                  sheet = data_2018,
                                  na.strings = "n/a")
  dat.2019 <- read.csv(data_2019)

  #run Erica's error corrections
  dat.2018 <- corr_trees_2018(dat.2018)
  dat.2019 <- corr_trees_2019(dat.2019)

  # Half the sites were done in 2018 and the other half in 2019, but we are considering them all 2018
  dat.201x <- rbind(dat.2018, dat.2019)
  # get rid of 999 trees that have a different plot radius and will not be included
  dat.201x <- subset(dat.201x, dat.201x$Tree.No != "999")
  # get rid of trees class 5 trees which were dead fallen
  dat.201x <- subset(dat.201x, dat.201x$Tree.Class != "5")

  #drop problematic plots
  dat.201x <- remove_plots_trees(dat.201x)
  #remove C2 J300 which could not be found in 2018 or 2019
  dat.201x <- subset(dat.201x, dat.201x$Unit != "C2" | dat.201x$Gd.Pt != "J300")

  #Eliminate Extra Large trees from expanded plots (Tree no. 999) that have 7.98 m radius
  dat.201x <- subset(dat.201x, dat.201x$Tree.No != "999")

  label_dat <- tibble(dat.201x %>% distinct(Unit, Gd.Pt)) %>%
    mutate(Year = 2018)%>%
    rename(PlotNum = Gd.Pt)


  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  #dat.201x$count <- 1 #rep(1, length(dat.2018x$Unit))
  #Trees_in_Plots <- dat.201x %>%
  #  group_by(Unit, Gd.Pt, Plot.Size) %>%
  #  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  #Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))

  #Plot_in_Units <- Trees_in_Plots %>%
  #  group_by(Unit) %>%
  #  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  #Plot_in_Units  #A3 missing the 2 problematic plots, C2 missing 2 plots that couldn't be found and B2 and B5 and C3 had plots removed from all years
  #save plot labels to merge into dataset after summarizing
  #labels201x <- Trees_in_Plots %>%
  #  select(Unit, Gd.Pt) %>%
   # rename(PlotNum = Gd.Pt)%>%
  #  mutate(Year = 2018)

  return(label_dat)
}



#' Title
#'
#' @param data_file
#' @param large_trees
#'
#' @return
#' @export
#'
#' @examples
get_all_plots_22 <- function(data_file = "./data-raw/Trees/StandStructureData_Nov2022_Final.xlsx",
                             large_trees = "Large",
                             inter_trees = "Inter",
                             small_trees = "Small"){

  dat.2022 <- openxlsx::read.xlsx(data_file,
                                  sheet = "Large", na.strings = "n/a")
  dat.2022.int <- openxlsx::read.xlsx(data_file,
                                      sheet = "Inter", na.strings = "NA")
  dat.2022.sm <- openxlsx::read.xlsx(data_file,
                                     sheet = "Small", na.strings = "NA")

  dat1 <- dat.2022 %>% dplyr::select(Unit, Gd.Pt) %>% distinct(Unit, Gd.Pt)
  dat2 <- dat.2022.int %>% dplyr::select(Unit, Gd.Pt) %>% distinct(Unit, Gd.Pt)
  dat3 <- dat.2022.sm %>% dplyr::select(Unit, Gd.Pt) %>% distinct(Unit, Gd.Pt)

  dat4 <- rbind(dat1, dat2, dat3)
  label_dat <- tibble(dat4 %>% distinct(Unit, Gd.Pt)) %>%
    mutate(Year = 2022)%>%
    rename(PlotNum = Gd.Pt)




  dat.2022$Plot.Size <- dat.2022$radius
  #check how many plots there are for each treatment unit
  dat.2022$count <- rep(1, length(dat.2022$Unit))

  Trees_in_Plots <- dat.2022 %>%
    group_by(Unit, Gd.Pt, Plot.Size) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))

  Plot_in_Units <- Trees_in_Plots %>%
    group_by(Unit) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments),
  #except stands were plots were deleted, C2, B5, B2, C1

  #save plot labels to merge into dataset after summarizing
  labels2022 <- Trees_in_Plots %>%
    select(Unit, Gd.Pt) %>%
    rename(PlotNum = Gd.Pt) %>%
    mutate(Year = 2022)

  return(label_dat)

}

