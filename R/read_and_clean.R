

#' read and clean 1992 Date Creek data
#'
#' @param cruise_data
#' @param fixed_data
#'
#' @return
#' @export
#'
#' @examples
read_92_trees <- function(cruise_data, fixed_data){

  ###################
  ### Cruise plots ###
  ###################

  # Read in the data for the cruise plots
  cruise_92 <- read.csv(cruise_data, stringsAsFactors = FALSE)

  #Data includes count (C) and measure (M) plots from cruise together coded by PLOT_TYPE
  # Removing C plots for C pool calculation - based on Erica Lilles' data prep script
  cruise_92 <- subset(cruise_92, cruise_92$PLOT_TYPE == "M")

  # Rename columns to match other years
  names(cruise_92 )[names(cruise_92 ) == "BLOCK"] <- "Unit"
  names(cruise_92 )[names(cruise_92 ) == "MAIN_BAF"] <- "PrismBands"
  names(cruise_92 )[names(cruise_92 ) == "SPECIES"] <- "Spp"
  names(cruise_92 )[names(cruise_92 ) == "TREE_CLASS"] <- "Tree.Class"

  # Create unique plot names
  cruise_92 <- cruise_92 %>%
    dplyr::mutate(PlotNum = paste0(STRIP, ".", PLOT))
  # Create STUBYN column and find snags of class 7 or 8 but don't include ones that don't seem like stub equation should be used (too tall)
  cruise_92$StubYN <- ifelse(cruise_92$SnagCode >= 7 & cruise_92$CRUISED_HEIGHT <= 10,
                                   "Y", "N" )

  # make sure live trees are not coded as Stubs
  cruise_92$StubYN <- ifelse(cruise_92$Tree.Class <3 , "N", cruise_92$StubYN )

  # Eliminate unwanted columns
  cruise_92 <- cruise_92 %>%
    dplyr::select(Unit, PlotNum, PrismBands, Spp, DBH, Tree.Class, CRUISED_HEIGHT, StubYN)

  # Rename species to match other years and functions
  cruise_92 <- cruise_92 %>%
    dplyr::mutate(Spp = replace(Spp, Spp == "AC", "Ac"),
                  Spp = replace(Spp, Spp == "CT", "Ac"),
                  Spp = replace(Spp, Spp == "AT", "At"),
                  Spp = replace(Spp, Spp == "BA", "Ba"),
                  Spp = replace(Spp, Spp == "BL", "Bl"),
                  Spp = replace(Spp, Spp == "CW", "Cw"),
                  Spp = replace(Spp, Spp == "EP", "Ep"),
                  Spp = replace(Spp, Spp == "HW", "Hw"),
                  Spp = replace(Spp, Spp == "PL", "Pl"),
                  Spp = replace(Spp, Spp == "SX", "Sx"),
                  Spp = replace(Spp, Spp == "S", "Sx"),
                  Spp = replace(Spp, Spp == "H", "Hw"),
                  Spp = replace(Spp, Spp == "B", "Ba"),
                  Spp = replace(Spp, Spp == "C", "Cw"))

  # Rename units to match other years
  cruise_92 <- cruise_92 %>%
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


  #length(unique(paste(cruise_92$Unit,cruise_92$PlotNum))) #there should be 251 plots


  # Eliminate  <7.5 measures (should not have been included in cruise)
  # This will remove plot with no trees, but later it will come back in with the labels merge
  cruise_92 <- subset(cruise_92, cruise_92$DBH >= 7.5 | cruise_92$DBH == 0)

  cruise_92 <- cruise_92 %>%
    mutate(BA = pi*(DBH/200)^2) %>%
    mutate(SPH = PrismBands/BA)

  cruise_92 <- as.data.table(cruise_92)
  cruise_92[,`:=`(PlotType = "V")]

  ###################
  ### Fixed plots ###
  ###################

  # Read in the data for the fixed radius plots
  fixed_92 <- fread(fixed_data, stringsAsFactors = FALSE)
  fixed_92[,V1:=NULL]
  data.table::setnames(fixed_92,  c("Init.Dens.1.3Ht", "Init.Dens.2.5",
                                          "Init.Dens.5", "Init.Dens.5-7.5"),
                       c("1","2.5","5","7.5"))
  measure_vars <- c("1", "2.5", "5", "7.5")
  fixed_92[, (measure_vars) := lapply(.SD, as.numeric), .SDcols = measure_vars]
  fixed_92 <- data.table::melt(fixed_92, id.vars =c("Unit","Spp"),
                                     variable.name = "DiamClass",
                                     value.name = "SPH")
  # get the median diameter for each class
  fixed_92[, DBH := ifelse(DiamClass == "1", NA,
                                 ifelse(DiamClass == "2.5", median(c(1,2.5)),
                                        ifelse(DiamClass == "5", median(c(2.5,5)),
                                               median(c(5,7.5)))))]
  fixed_92[,`:=`(Tree.Class = 1,
                 BA = pi*(DBH/200)^2,
                 PlotNum = 1,
                 PlotType = "F")]

  fixed_92[ ,CRUISED_HEIGHT := NA]
  fixed_92[ ,StubYN := "N"]


  ####################
  ### Combine     ###
  ####################
  dat.1992_all <- rbind(cruise_92[,.(Unit,PlotNum, Spp, Tree.Class,DBH, BA,
                                           SPH, CRUISED_HEIGHT, StubYN, PlotType)],
                        fixed_92[,.(Unit,PlotNum, Spp, Tree.Class,DBH,BA,
                                          SPH, CRUISED_HEIGHT, StubYN, PlotType)])
  setnames(dat.1992_all, "CRUISED_HEIGHT", "cruise_hgt")

  return(dat.1992_all)

}



