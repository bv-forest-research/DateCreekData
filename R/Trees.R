
clean_trees_all_yrs <- function(){
  trees92 <- trees_1992(calc_height = TRUE)
  trees93 <- trees_1993(calc_height = TRUE)
  trees10 <- trees_2010(calc_height = TRUE)
  trees18 <- trees_201x(calc_height = TRUE)
  trees22 <- trees_2022(calc_height = TRUE)

  trees_all <- rbind(trees92, trees93, trees10, trees18, trees22,fill=TRUE)
}


#' Clean 1992 tree data need to change all ddply to get rid of plyr (replaced by dplyr)
#'
#' @param cruise_data
#' @param fixed_data
#' @param calc_height description
#'
#' @return
#' @export
#' @importFrom plyr ddply
#' @description
#' cleans tree data and calculates heights (if height = TRUE)
#' can't get this working because of ddply - needs to be updated to either dplyr or data.table
#'
#'
#' @examples
trees_1992 <- function(cruise_data = "./data-raw/Trees/1.1992data.csv",
                       fixed_data = "./data-raw/Trees/2.1992fixed_radius_data_fromTable20_DateCkHandbook.csv",
                       calc_height = TRUE){
  # Read in the data for the cruise plots
  dat.1992.cruise <- read.csv(cruise_data, stringsAsFactors = FALSE)

  #Data includes count (C) and measure (M) plots from cruise together coded by PLOT_TYPE
  # Removing C plots for C pool calculation - based on Erica Lilles' data prep script
  dat.1992.cruise <- subset(dat.1992.cruise, dat.1992.cruise$PLOT_TYPE == "M")

  # Rename columns to match other years
  names(dat.1992.cruise )[names(dat.1992.cruise ) == "BLOCK"] <- "Unit"
  names(dat.1992.cruise )[names(dat.1992.cruise ) == "MAIN_BAF"] <- "PrismBands"
  names(dat.1992.cruise )[names(dat.1992.cruise ) == "SPECIES"] <- "Spp"
  names(dat.1992.cruise )[names(dat.1992.cruise ) == "TREE_CLASS"] <- "Tree.Class"

  # Create unique plot names
  dat.1992.cruise <- dat.1992.cruise %>%
    dplyr::mutate(PlotNum = paste0(STRIP, ".", PLOT))

  # Eliminate unwanted columns
  dat.1992.cruise <- dat.1992.cruise %>%
    dplyr::select(Unit, PlotNum, PrismBands, Spp, DBH, Tree.Class)

  # Rename species to match other years and functions
  dat.1992.cruise <- dat.1992.cruise %>%
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


  length(unique(paste(dat.1992.cruise$Unit,dat.1992.cruise$PlotNum))) #there should be 251 plots

  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  labels1992.cruise <- ddply(dat.1992.cruise[c("Unit", "PlotNum",  "DBH")], .(Unit, PlotNum), numcolwise(length))
  labels1992.cruise$DBH <- NULL # this is really a count of the trees in each plot which we don't need in the labels
  test.labels <- labels1992.cruise
  test.labels$count <- rep(1, length(labels1992.cruise$Unit))
  Plot_in_Units92 <- ddply(test.labels, .(Unit), numcolwise(sum))

  # Eliminate  <7.5 measures (should not have been included in cruise)
  # This will remove plot with no trees, but later it will come back in with the  labels merge
  dat.1992.cruise <- subset(dat.1992.cruise, dat.1992.cruise$DBH >= 7.5 | dat.1992.cruise$DBH == 0)

  ###################
  ### Fixed plots ###
  ###################

  # Read in the data for the fixed radius plots
  dat.1992.fixed <- fread(fixed_data, stringsAsFactors = FALSE)
  dat.1992.fixed[,V1:=NULL]
  data.table::setnames(dat.1992.fixed,  c("Init.Dens.1.3Ht", "Init.Dens.2.5",
                                       "Init.Dens.5", "Init.Dens.0-7.5"),
                       c("1","2.5","5","7.5"))
  dat.1992.fixed <- data.table::melt(dat.1992.fixed, id.vars =c("Unit","Spp"),
                                    variable.name = "DiamClass",
                                    value.name = "PHF")
  # get the median diameter for each class
  dat.1992.fixed[, DBH := ifelse(DiamClass == "1", NA,
                          ifelse(DiamClass == "2.5", median(c(1,2.5)),
                            ifelse(DiamClass == "5", median(c(2.5,5)),
                                   median(c(5,7.5)))))]
  dat.1992.fixed[,`:=`(Tree.Class = 1,
                       BA = pi*(DBH/200)^2,
                       PlotNum = 1)]

  ####################
  ### Cruise Plots ###
  ####################
  dat.1992.cruise <- dat.1992.cruise %>%
    mutate(BA = pi*(DBH/200)^2) %>%
    mutate(PHF = PrismBands/BA)

  # Calculate stems per hectare for the cruise plots
  #dat.1992.cruise$SPH <- calculateSPH(pBands = dat.1992.cruise$PrismBands,
   #                                     DBH = dat.1992.cruise$DBH)

  dat.1992.cruise <- as.data.table(dat.1992.cruise)

  dat.1992_all <- rbind(dat.1992.cruise[,.(Unit,PlotNum, Spp, Tree.Class,DBH, BA, PHF)],
                        dat.1992.fixed[,.(Unit,PlotNum, Spp, Tree.Class,DBH,BA,PHF)])

  # Calculate tree height
  dat.1992_all[, Height := treeCalcs::DiamHgtFN(Spp, DBH, BECzone = "ICH"),
               by = seq_len(nrow(dat.1992_all))]

  dat.1992_all <- dat.1992_all[,.(Unit,Year=1992, PlotNum, Spp, Tree.Class, DBH, Height, BA, PHF)]
  return(dat.1992_all)
}



#' Clean 1993 tree data
#'
#' @param data
#' @param calc_height
#'
#' @return
#' @export
#'
#' @examples
trees_1993 <- function(data = "./data-raw/Trees/3.SS93forR.csv",
                       calc_height = TRUE){
  # Read in the data for both fixed radius and cruise plots
  dat.1993 <- read.csv(data, stringsAsFactors = FALSE)

  # Rename columns to match other years
  names(dat.1993)[names(dat.1993) == "Plot_ID"] <- "Unit"
  names(dat.1993)[names(dat.1993) == "SPP"] <- "Spp"
  names(dat.1993)[names(dat.1993) == "TC"] <- "Tree.Class"


  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  dat.1993$count <- rep(1, length(dat.1993$Unit ))
  Trees_in_Plots <- ddply(dat.1993[c("Unit", "PlotNum", "count")], .(Unit, PlotNum), numcolwise(sum))
  Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit ))
  Plot_in_Units <- ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
  #Plot_in_Units #stands should have 23 or 30 plots (30 for 40% retention treatments)

  # Create Labels for all plot numbers within treatments to include plots with no trees later,
  #after putting zeros
  labels1993 <- Trees_in_Plots[c("Unit", "PlotNum")]

  # Eliminate unwanted columns
  dat.1993 <- dat.1993 %>%
    dplyr::select(Unit, PlotNum, PrismBands, FixedRad, Spp, DBH, Tree.Class)

  dat.1993 <- subset(dat.1993, dat.1993$DBH != ".")

  dat.1993 <- dat.1993 %>%
    dplyr::mutate(DBH = as.numeric(DBH))

  # Rename species to match other years and the funtions
  dat.1993 <- dat.1993 %>%
    dplyr::mutate(Spp = replace(Spp, Spp == "AC", "Ac"),
                  Spp = replace(Spp, Spp == "CT", "Ac"),
                  Spp = replace(Spp, Spp == "AT", "At"),
                  Spp = replace(Spp, Spp == "BA", "Ba"),
                  Spp = replace(Spp, Spp == "BL", "Bl"),
                  Spp = replace(Spp, Spp == "CW", "Cw"),
                  Spp = replace(Spp, Spp == "EP", "Ep"),
                  Spp = replace(Spp, Spp == "HW", "Hw"),
                  Spp = replace(Spp, Spp == "PL", "Pl"),
                  Spp = replace(Spp, Spp == "SX", "Sx"))


  # Subset fixed radius plots
  dat.1993.fixed <- subset(dat.1993, dat.1993$FixedRad != ".")
  dat.1993.fixed$PrismBands <- NULL

  # Subset cruise plots
  dat.1993.cruise <- subset(dat.1993, dat.1993$FixedRad == ".")
  dat.1993.cruise$FixedRad <- NULL

  ###################
  ### Fixed Plots ###
  ###################

  dat.1993.fixed <- as.data.table(dat.1993.fixed)

  # Add in plot size
  # Fixed radius plots were 5.64 m or 100 m^2
  # see page 16 of "The Date Creek Silvicultural Systems Study in the Interior Cedar-Hemlock
  # Forests of Northwestern British Columbia: Overview and Treatment Summaries (1997)"
  dat.1993.fixed <- dat.1993.fixed %>%
    #dplyr::mutate(Plot.Factor = 0.01, PHF = 1/0.01)
    dplyr::mutate(PHF = 1/0.01) %>%
    mutate(BA = pi*(DBH/200)^2)



  ####################
  ### Cruise Plots ###
  ####################

  # Calculate stems per hectare for the cruise plots
  dat.1993.cruise <- dat.1993.cruise %>%
    mutate(PrismBands = as.numeric(PrismBands)) %>%
    mutate(BA = pi*(DBH/200)^2) %>%
    mutate(PHF = PrismBands/BA)

  dat.1993.cruise <- as.data.table(dat.1993.cruise)
  #dat.1993.cruise$SPH <- calculateSPH(pBands = dat.1993.cruise$PrismBands,
   #                                     DBH = dat.1993.cruise$DBH)


  ############
  ### Both ###
  ############

  # Combine the data from the fixed and cruise plots
  dat.1993_all <- rbind(dat.1993.fixed[,.(Unit, PlotNum,Spp,Tree.Class,DBH, BA, PHF)],
                        dat.1993.cruise[,.(Unit, PlotNum,Spp,Tree.Class,DBH, BA, PHF)])
  # Calculate tree height
  dat.1993_all[, Height := treeCalcs::DiamHgtFN(Spp, DBH, BECzone = "ICH"),
               by = seq_len(nrow(dat.1993_all))]

  dat.1993_all <- dat.1993_all[,.(Unit, Year=1993,PlotNum, Spp, Tree.Class, DBH, Height, BA, PHF)]

  return(dat.1993_all)

}

#' Title
#'
#' @param data
#' @param calc_height
#'
#' @return
#' @export
#'
#' @examples
trees_2010 <- function(lrg_trees = "./data-raw/Trees/4.Data Creek 2010 Data large trees.csv",
                       cc_trees = "./data-raw/Trees/5.Trees 10cm and above in clearcut 2010.csv",
                       small_trees = "./data-raw/Trees/6.Date Creek 2010 Trees less than 10 cm tallies.csv",
                       calc_height = TRUE){

  #############################
  ### LARGE TREES (No CC) ###
  #############################

  # Read in the data for large trees (10+cm DBH) - No Clear Cut plots
  dat.2010 <- read.csv(lrg_trees, stringsAsFactors = FALSE, na.strings = "n/a")

  # Rename columns/ variables to match other years
  dat.2010$Unit <- as.factor(dat.2010$Unit)
  names(dat.2010)[names(dat.2010) == "Tree.No."] <- "Tree.No" #Rename
  names(dat.2010)[names(dat.2010) == "DBH.2010"] <- "DBH" #Rename
  dat.2010$Spp[which(dat.2010$Spp == "Sw")] <- "Sx"

  dat.2010 <- corr_trees_2010(dat.2010)
  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  dat.2010$count<-rep(1, length(dat.2010$Unit ))
  Trees_in_Plots<-ddply(dat.2010[c("Unit", "Gd.Pt","Plot.Size", "count")], .(Unit, Gd.Pt,Plot.Size), numcolwise(sum))
  Trees_in_Plots$count<-rep(1, length(Trees_in_Plots$Unit ))
  Plot_in_Units<-ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
  Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments)
  # C2 is down 2 plots
  #no clear-cut large tree plots

  # Eliminate unwanted columns
  #dat.2010 <- dat.2010 %>%
   # dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class)


  dat.2010 <- as.data.table(dat.2010)
  dat.2010[, PHF := 1/(round((pi*Plot.Size^2)/10000,4)), by = seq_len(nrow(dat.2010))]

  #############################
  ### LARGE TREES (CC only) ###
  #############################

  # For large trees (10+cm DBH) - ONLY in Clear Cut plots
  # No dead large trees in the clear cut plots
  dat.2010.CC<- read.csv(cc_trees, stringsAsFactors = FALSE)

  # Rename columns to match other years
  names(dat.2010.CC)[names(dat.2010.CC) == "Species"] <- "Spp" #Rename
  names(dat.2010.CC)[names(dat.2010.CC) == "Dave"] <- "DBH" #Rename

  # Add tree class
  # no dead trees found > 10 cm in clear-cuts
  dat.2010.CC$Tree.Class <- rep(1, length(dat.2010.CC$Unit))

  # Add plots per unit for clear cut units
  Plot_in_Units.CC <- data.frame(Unit = c("A1", "A3", "B4", "D2"), count = rep(20, 4))

  Plot_in_Units <- rbind(Plot_in_Units, Plot_in_Units.CC)

  # Eliminate unwanted columns
  #dat.2010.CC <- dat.2010.CC %>%
   # dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class)

  dat.2010.CC <- as.data.table(dat.2010.CC)
  dat.2010.CC[, PHF := 1/(round((pi*Plot.Size^2)/10000,4)),by = seq_len(nrow(dat.2010.CC))]

  ###################
  ### SMALL TREES ###
  ###################

  # For small trees (5.1-10cm) - All plots
  dat.2010.sm <- read.csv(small_trees) #tallied trees with dbh <4

  #Eliminate plots C2 J050 and J300, which could not be found
  #dat.2010.sm <- subset(dat.2010.sm, dat.2010.sm$Unit != "C2"| dat.2010.sm$Gd.Pt != "J050")
  #dat.2010.sm <- subset(dat.2010.sm, dat.2010.sm$Unit != "C2"| dat.2010.sm$Gd.Pt != "J300")

  #dat.2010.sm <- subset(dat.2010.sm, dat.2010.sm$Size.Cl == "5.1-10cm")

  # Eliminate unwanted columns
  #dat.2010.sm <- dat.2010.sm %>%
   # dplyr::select(Unit, Gd.Pt, Plot.Size, Hw, Cw, Sx, Pl, Bl, Ba, Ep, At, Ac)

  # Pivot table from wide to long - both live and dead
  dat.2010.sm <- as.data.table(dat.2010.sm)
  dat.2010.sm_l <- melt(dat.2010.sm,
                      id.vars = c("Unit", "Gd.Pt", "Plot.Size", "Size.Cl"),
                      measure.vars = c("Hw", "Cw", "Sx", "Pl", "Bl", "Ba", "Ep", "At", "Ac"),
                      variable.name = "Spp",
                      value.name = "Tally")
  dat.2010.sm_l[,Tree.Class := 1]

  dat.2010.sm_d <- melt(dat.2010.sm,
                        id.vars = c("Unit", "Gd.Pt", "Plot.Size", "Size.Cl"),
                        measure.vars = c("dead.Hw", "deadCw", "deadSx", "deadPl",
                                         "deadBl", "deadBa", "deadEp", "deadAt", "deadAc"),
                        variable.name = "DSpp",
                        value.name = "Tally")
  dat.2010.sm_d[, c("Tree.Class", "Spp") := tstrsplit(DSpp, "ad", fixed=TRUE)]
  dat.2010.sm_d$Spp <- gsub(".Hw","Hw", dat.2010.sm_d$Spp)
  dat.2010.sm_d[, Tree.Class:= 3][,DSpp:=NULL]

  #bring them together
  dat.2010.sm <- rbind(dat.2010.sm_l,dat.2010.sm_d)
  dat.2010.sm <- dat.2010.sm[Tally >0]

  # Duplicate rows for the number of tallies that it has
  #dup.times <- dat.2010.sm$Tally
  #idx <- rep(1:nrow(dat.2010.sm), dup.times)
  #dat.2010.sm <- dat.2010.sm[idx,]
  #dat.2010.sm$tally <- NULL

  # Add average DBH for each size class
  dat.2010.sm[, DBH := ifelse(Size.Cl == "0-5cm",median(c(0,5)),
                        ifelse(Size.Cl == "5.1-10cm", median(c(5.1,10)),
                         0))]

  #multiply the per hectare factor by the tally - when dividing by the number of plots,
  #should get SPH --- ERICA CAN YOU CHECK THIS MAKES SENSE ---
  dat.2010.sm[, PHF := (1/(round((pi*Plot.Size^2)/10000,4)))*Tally,
              by = seq_len(nrow(dat.2010.sm))]

  # Combine Large trees, CC large trees, and small trees
  dat.2010_all <- rbind(dat.2010[,.(Unit, Gd.Pt,Spp,Tree.Class, DBH, PHF)],
                        dat.2010.CC[,.(Unit, Gd.Pt,Spp,Tree.Class, DBH, PHF)],
                        dat.2010.sm[,.(Unit,Gd.Pt,Spp,Tree.Class, DBH, PHF)])

  setnames(dat.2010_all, c("Gd.Pt"),c("PlotNum"))

  dat.2010_all[, BA :=  pi*(DBH/200)^2]

  # use "U" for n/a species in large tree data
  dat.2010_all[is.na(Spp), Spp := "U"]

  # Calculate tree height
  dat.2010_all[, Height := treeCalcs::DiamHgtFN(Spp, DBH, BECzone = "ICH"),
               by = seq_len(nrow(dat.2010_all))]

  #Eliminate plots C2 J050 and J300, which could not be found in 2018-19
  #This step is optional and is only done to match 2010 and 2018 data more closely
  dat.2010_all <- subset(dat.2010_all, dat.2010_all$Unit != "C2"| dat.2010_all$PlotNum != "J050")
  dat.2010_all <- subset(dat.2010_all, dat.2010_all$Unit != "C2"| dat.2010_all$PlotNum != "J300")

  dat.2010_all <- dat.2010_all[,.(Unit,Year=2010, PlotNum, Spp, Tree.Class, DBH, Height, BA, PHF)]

  return(dat.2010_all)

}

#' Title
#'
#' @param data
#' @param calc_height
#'
#' @return
#' @export
#'
#' @examples
trees_201x <- function(data_2018 = "./data-raw/Trees/7.Data Creek 2018 Data large trees.csv",
                       data_2019 = "./data-raw/Trees/8.Data Creek 2019 Data large trees.csv",
                       small_trees = "./data-raw/Trees/9.2018-19intermediatetrees.csv",
                       calc_height = TRUE){

  ###################
  ### LARGE TREES ###
  ###################

  # Read in the data for large trees (10+cm DBH)
  dat.2018 <- read.csv(data_2018, na.strings = "n/a")
  dat.2019 <- read.csv(data_2019, na.strings = "n/a")

  #run Erica's error corrections
  dat.2018 <- corr_trees_2018(dat.2018)
  dat.2019 <- corr_trees_2019(dat.2019)

  # Half the sites were done in 2018 and the other half in 2019, but we are considering them all 2018
  dat.201x <- rbind(dat.2018, dat.2019)


  # Count how many plots there are for each treatment unit
  # so when averaging carbon/unit later, we can take into account the plots that had zero C
  dat.201x$count <- 1 #rep(1, length(dat.2018x$Unit))
  Trees_in_Plots <- plyr::ddply(dat.201x[c("Unit", "Gd.Pt","Plot.Size", "count")],
                                .(Unit, Gd.Pt,Plot.Size), numcolwise(sum))
  Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))
  Plot_in_Units <- plyr:: ddply(Trees_in_Plots[c("Unit", "count")],
                                .(Unit), numcolwise(sum))
  Plot_in_Units

  # Eliminate unwanted columns
  dat.201x <- dat.201x %>%
    dplyr::select(Unit, Gd.Pt, Tree.No, Plot.Size, Spp, DBH, Tree.Class)

  ###################
  ### SMALL TREES ###
  ###################

  # Read in the data for small trees
  dat.201x.sm <- read.csv(small_trees, stringsAsFactors = FALSE)

  # Eliminate unwanted columns
  dat.201x.sm <- dat.201x.sm %>%
    dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class) %>%
    dplyr::mutate(Tree.No = NA)

  # Select the trees 5cm+ - not sure why we needed to remove small trees?
  #dat.201x.sm <- subset(dat.201x.sm, dat.201x.sm$DBH > 5)

  # Assign tree class for 2018 measurements with no tree class
  # We are assuming they are all living
  dat.201x.sm  <- dat.201x.sm %>%
    dplyr::mutate(Tree.Class = ifelse(is.na(Tree.Class), 1, Tree.Class))

  # Combine large and small trees
  dat.201x_all <- rbind(dat.201x, dat.201x.sm)

  # run this fucntion to eliminate plots and trees that were problematic. See ?remove_plots_trees
  # for details - might need to run earlier or figure out how to drpo these for plot labels merge
  dat.201x_all <- data.table(remove_plots_trees(dat.201x_all))

  dat.201x_all[is.na(Spp)]

  # Calculate tree height (NA if no trees in plot)
  dat.201x_all[, Height := ifelse(!is.na(Spp),
                              treeCalcs::DiamHgtFN(Spp, DBH, BECzone = "ICH"),
                              NA),
               by = seq_len(nrow(dat.201x_all))]

  #calculate BA and PHF
  dat.201x_all[,`:=`(BA =  pi*(DBH/200)^2,
                     PHF = 1/(round((pi*Plot.Size^2)/10000,4))),
                        by = seq_len(nrow(dat.201x_all))]

  setnames(dat.201x_all, "Gd.Pt", "PlotNum")
  dat.201x_all <- dat.201x_all[,.(Unit,Year=2018,PlotNum, Spp, Tree.Class, DBH, Height, BA, PHF)]

}

#' 2022 data
#'
#' @param data
#' @param calc_height
#'
#' @return
#' @export
#'
#' @examples
trees_2022 <- function(large_trees = "./data-raw/Trees/10.2022 large trees.csv",
                       inter_trees = "./data-raw/Trees/11.2022 intermediate trees.csv",
                       small_trees = "./data-raw/Trees/12.2022 small trees.csv",
                       calc_height = TRUE){

  #LARGE TREES
  dat.2022 <- read.csv(large_trees)

  dat.2022$DBH.22 <- as.numeric(dat.2022$DBH.22)
  dat.2022$Unit <- as.factor(dat.2022$Unit)
  dat.2022$PlotNumber <-
    paste(dat.2022$Unit, dat.2022$Gd.Pt, sep = ".")
  dat.2022$TreeSpp <- dat.2022$Spp
  dat.2022$Plot.Size <- dat.2022$radius

  # Rename columns to match other years
  names(dat.2022)[names(dat.2022) == "CLASS.22"] <- "Tree.Class"
  names(dat.2022)[names(dat.2022) == "DBH.22"] <- "DBH"
  names(dat.2022)[names(dat.2022) == "HT.22"] <- "Meas.HT"

  #check species... and clean
  unique(dat.2022$TreeSpp)
  dat.2022$TreeSpp[which(dat.2022$TreeSpp == "sx")] <- "Sx"
  dat.2022$TreeSpp[which(dat.2022$TreeSpp == "hW")] <- "Hw"
  dat.2022$TreeSpp[which(dat.2022$TreeSpp == "cw")] <- "Cw"
  dat.2022$TreeSpp[which(dat.2022$TreeSpp == "bl")] <- "Bl"
  dat.2022$TreeSpp[which(dat.2022$TreeSpp == "PL")] <- "Pl"
  dat.2022$TreeSpp[which(dat.2022$TreeSpp == "n/a")] <- NA
  dat.2022$Spp <- dat.2022$TreeSpp

  # Eliminate unwanted columns
  dat.2022 <- dat.2022 %>%
    dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class, Meas.HT)

  #check how many plots there are for each treatment unit
  dat.2022$count <- rep(1, length(dat.2022$Unit))
  Trees_in_Plots <-
    ddply(dat.2022[c("Unit", "Gd.Pt", "Plot.Size", "count")], .(Unit, Gd.Pt, Plot.Size), numcolwise(sum))
  Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))
  Plot_in_Units <-
    ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
  Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments),
  #except stands were plots were deleted, C2, B5, B2, C1

  #save plot labels to merge into dataset after summarizing
  labels2022 <- Trees_in_Plots[c("Unit", "Gd.Pt")]

  #update decay classes
  #2022 tree class codes: 1	Clean, 2	Suspect, 3	Dead Potential, 4	Dead Useless, 5	Veteran, DF	Dead Fallen
  unique(dat.2022$Tree.Class)
  dat.2022$Tree.Class[which(dat.2022$Tree.Class == "1.0")] <- "1"
  dat.2022$Tree.Class[which(dat.2022$Tree.Class == "4.0")] <- "4"
  dat.2022$Tree.Class[which(dat.2022$Tree.Class == "df")] <- "DF"
  dat.2022$Tree.Class[which(dat.2022$Tree.Class == " ")] <- ""

  #remove trees that have fallen (Tree class = DF or df), or were space holders (0, NA, blank)
  good.T.codes <- c("1", "2", "3", "4", "5")
  dat.2022 <- subset(dat.2022, dat.2022$Tree.Class %in% good.T.codes)
  #change code 5 for veteran to code 1 for live healthy for carbon purposes
  dat.2022$Tree.Class[which(dat.2022$Tree.Class == "5")] <- "1"
  dat.2022$Tree.Class <- as.numeric((dat.2022$Tree.Class))


  # run this fucntion to eliminate plots and trees that were problematic. See ?remove_plots_trees
  # for details
  dat.2022 <- data.table(remove_plots_trees(dat.2022))

  ##########################
  ### intermediate TREES ###
  ##########################
  dat.2022.int <- read.csv(inter_trees)

  dat.2022.int$DBH <- as.numeric(dat.2022.int$DBH.22)
  dat.2022.int$Unit <- as.factor(dat.2022.int$Unit)
  dat.2022.int$PlotNumber <-
    paste(dat.2022.int$Unit, dat.2022.int$Gd.Pt, sep = ".")
  dat.2022.int$Plot.Size <- dat.2022.int$radius

  #check tree class... remove trees
  unique(dat.2022.int$CLASS.22)
  good.T.codes <- c("1", "2", "3", "4", "5")
  dat.2022.int <- subset(dat.2022.int, dat.2022.int$CLASS.22 %in% good.T.codes)

  #plot elimination at this point must match the large tree data set where the labels come from
  dat.2022.int <- data.table(remove_plots_trees(dat.2022.int))

  #check species...
  unique(dat.2022.int$Spp)
  #put Hw for the tree with no species, all other species in that plot are Hw and it is the most common species
  dat.2022.int[Spp =="", Spp := "Hw"]

  # Get rid of old DBH and Rename columns to match other years
  dat.2022.int[,DBH:=NULL]
  setnames(dat.2022.int, c("CLASS.22","DBH.22"),
           c("Tree.Class","DBH"))

  # Eliminate unwanted columns
  dat.2022.int <- dat.2022.int %>%
    dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class)


  #########################
  ### SMALL TREES #########
  #########################

  dat.2022.sm <- read.csv(small_trees)
  dat.2022.sm <- data.table(remove_plots_trees(dat.2022.sm))

  dat.2022.sm_l <- melt(dat.2022.sm,
                        id.vars = c("Unit", "Gd.Pt", "Plot.Size", "Size.Cl"),
                        measure.vars = c("Hw", "Cw", "Sx", "Pl", "Bl", "Ba", "Ep", "At", "Ac"),
                        value.factor = FALSE,
                        variable.name = "Spp",
                        variable.factor = FALSE,
                        value.name = "Tally")
  dat.2022.sm_l[,Tree.Class := 1]

  dat.2022.sm_d <- melt(dat.2022.sm,
                        id.vars = c("Unit", "Gd.Pt", "Plot.Size", "Size.Cl"),
                        measure.vars = c("dead.Hw", "deadCw", "deadSx", "deadPl",
                                         "deadBl", "deadBa", "deadEp", "deadAt", "deadAc"),
                        value.factor = FALSE,
                        variable.name = "DSpp",
                        variable.factor = FALSE,
                        value.name = "Tally")
  dat.2022.sm_d[, c("Tree.Class", "Spp") := tstrsplit(DSpp, "ad", fixed=TRUE)]
  dat.2022.sm_d$Spp <- gsub(".Hw","Hw", dat.2022.sm_d$Spp)
  dat.2022.sm_d[, Tree.Class:= 3][,DSpp:=NULL]

  #bring them together
  dat.2022.sm <- rbind(dat.2022.sm_l,dat.2022.sm_d)
  dat.2022.sm <- dat.2022.sm[Tally >0]

  # Duplicate rows for the number of tallies that it has
  #dup.times <- dat.2022.sm$Tally
  #idx <- rep(1:nrow(dat.2022.sm), dup.times)
  #dat.2022.sm <- dat.2022.sm[idx,]
  #dat.2022.sm$tally <- NULL

  # Add average DBH for each size class
  dat.2022.sm[, DBH := ifelse(Size.Cl == "0-5cm",median(c(0,5)),
                              ifelse(Size.Cl == "5.1-10cm", median(c(5.1,10)),
                                     0))]

  #multiply the per hectare factor by the tally - when dividing by the number of plots,
  #should get SPH --- ERICA CAN YOU CHECK THIS MAKES SENSE ---
  dat.2022.sm[, PHF := (1/(round((pi*Plot.Size^2)/10000,4)))*Tally,
              by = seq_len(nrow(dat.2022.sm))]


  ################
  ### COMBINE ####
  ################
  dat.2022_all <- rbind(dat.2022[,.(Unit,Gd.Pt,Plot.Size,Spp,Tree.Class,DBH,Meas.HT)],
                          dat.2022.int[,.(Unit,Gd.Pt,Plot.Size,Spp,Tree.Class,DBH)],
                          dat.2022.sm[,.(Unit,Gd.Pt,Plot.Size,Spp,Tree.Class,DBH)], fill=TRUE)

  setnames(dat.2022_all,"Gd.Pt","PlotNum")

  dat.2022_all[,`:=`(BA =  pi*(DBH/200)^2,
                       PHF = 1/(round((pi*Plot.Size^2)/10000,4)),
                       Height = ifelse(!is.na(Spp),
                                  treeCalcs::DiamHgtFN(Spp, DBH, BECzone = "ICH"),
                                  NA)),
               by = seq_len(nrow(dat.2022_all))]
  dat.2022_all <- dat.2022_all[,.(Unit,Year=2022, PlotNum, Spp, Tree.Class, DBH, Meas.HT, Height, BA, PHF)]

  return(dat.2022_all)

}

#' summarize live or dead trees by unit
#' @author Kiri Daust, Erica Lilles, Jonathan VanElslander and Alana Clason worked on this function
#' @description summarize the stems/ha by species and diameter class (and tree class if snags) by unit in
#' the Date Creek Silvicultural Trial. This tree list from the 1992 pre-treatment data can be used to initiate forest
#' models such as SORTIE. Use
#' @param diam_sizeClass
#' @param dateCreek92_data
#' @param dateCreek_92_tallies
#' @param liveTrees
#'
#'
#' @export
#'
#'
calc_sph_92data <- function(diam_sizeClass = 2.5, dateCreek92_data, dateCreek_92_tallies, liveTrees = TRUE){
  #### read in the data
  dat92 <- data.table::fread(dateCreek92_data)
  dat92_tally <- data.table::fread(dateCreek_92_tallies)

  #### tree data:
  #add plot numbers
  dat92[,PlotNum := paste(STRIP, PLOT, sep = ".")]
  #Data includes count (C) and measure (M) plots from cruise together coded by PLOT_TYPE. Removing C plots for C pool calculation
  dat92 <- subset(dat92, dat92$PLOT_TYPE == "M")

  #keep only needed columns and rename to match 2018 data
  dat92 <- dat92[,.(Unit = BLOCK, PrismBands = MAIN_BAF, Ht = CRUISED_HEIGHT,
                    Spp = SPECIES,DBH, TC = TREE_CLASS,PlotNum)]

  #cleaning Spp codes to match 2018 data
  dat92$Spp[which(dat92$Spp == "AC")] <- "Ac"
  dat92$Spp[which(dat92$Spp == "CT")] <- "Ac"
  dat92$Spp[which(dat92$Spp == "AT")] <- "At"
  dat92$Spp[which(dat92$Spp == "BA")] <- "Ba"
  dat92$Spp[which(dat92$Spp == "BL")] <- "Bl"
  dat92$Spp[which(dat92$Spp == "CW")] <- "Cw"
  dat92$Spp[which(dat92$Spp == "EP")] <- "Ep"
  dat92$Spp[which(dat92$Spp == "HW")] <- "Hw"
  dat92$Spp[which(dat92$Spp == "PL")] <- "Pl"
  dat92$Spp[which(dat92$Spp == "SX")] <- "Sx"
  dat92$Spp[which(dat92$Spp == "C")] <- "Cw"
  dat92$Spp[which(dat92$Spp == "H")] <- "Hw"
  dat92$Spp[which(dat92$Spp == "E")] <- "Ep"
  dat92$Spp[which(dat92$Spp == "S")] <- "Sx"
  #Guessing that plain "B" entries by lazy cruiser in A and B units are Ba
  dat92$Spp[which(dat92$Spp == "B")] <- "Ba"

  #cleaning unit codes to match 2018 data
  dat92$Unit[which(dat92$Unit == "0A1")]<-"A1"
  dat92$Unit[which(dat92$Unit == "0A2")]<-"A2"
  dat92$Unit[which(dat92$Unit == "0A3")]<-"A3"
  dat92$Unit[which(dat92$Unit == "0A4")]<-"A4"
  dat92$Unit[which(dat92$Unit == "0B1")]<-"B1"
  dat92$Unit[which(dat92$Unit == "0B2")]<-"B2"
  dat92$Unit[which(dat92$Unit == "0B3")]<-"B3"
  dat92$Unit[which(dat92$Unit == "0B4")]<-"B4"
  dat92$Unit[which(dat92$Unit == "0B5")]<-"B5"
  dat92$Unit[which(dat92$Unit == "0C1")]<-"C1"
  dat92$Unit[which(dat92$Unit == "0C2")]<-"C2"
  dat92$Unit[which(dat92$Unit == "0C3")]<-"C3"
  dat92$Unit[which(dat92$Unit == "0D2")]<-"D2"
  dat92$Unit[which(dat92$Unit == "0D3")]<-"D3"
  dat92$Unit[which(dat92$Unit == "0D4")]<-"D4"
  dat92$Unit[which(dat92$Unit == "0D5")]<-"D5"

  #Create Labels for all plot numbers within treatments to include plots with no trees later, after putting zeros
  #for 1992 data, dbh classes from 8-10, 10-12, etc will work. 7.5 cm trees and below were measured in fixed radius plots
  sizeClasses <- seq(10,262.5, by = diam_sizeClass)
  labels92 <- unique(dat92[,.(Unit,PlotNum)])
  #write.csv(labels92, "./data-raw/Unit_Plot_Labels.csv", row.names = FALSE)
  #add species to every plot
  Spp_labels <- rep(unique(dat92$Spp)[!is.na(unique(dat92$Spp))], nrow(labels92))
  labels92 <- labels92[rep(seq(.N), length(unique(dat92$Spp)[!is.na(unique(dat92$Spp))]))]
  labels92[, Spp:= Spp_labels]

  #add diameter class to every species in every plot
  diam_labels <- rep(sizeClasses, nrow(labels92))
  labels92 <- labels92[rep(seq(.N), length(sizeClasses))]
  data.table::setorder(labels92, Unit, PlotNum, Spp)
  labels92[, DiamClass:= diam_labels]
  labels92[, DiamClass := as.factor(DiamClass)]

  #This will remove plot with no trees, and the 2.1 DBH tree from plot C1, which should not have been included in prism sweep
  # the empty plot will come back in with the labels merge. All trees > or = to 7.5 included in the prism data
  dat92 <- dat92[!is.na(Spp) & DBH >= 7.5]
  #zero height checking
  #zero <- dat92[Ht ==0] #one plot in A4 had half heights missed, and one dead useless in C1.
  #can't tell if C1 should be removed but leaving in because it isn't clear

  ### Are we summarizing the live trees or snags?
  # if snags, we want to keep the Tree class, if live, we don't
  if(liveTrees == TRUE){
    ### LIVE TREES
    dat92 <- dat92[TC <= 2]
    ### Summarize prism plots (trees > 7.5cm)
    #Calculate SPH for each treatment unit and dbh class
    #the prism bands number is the BAF for metric prisms

    dat92[,SPH:= calculateSPH(dat92$PrismBands, dat92$DBH)]

    #assign the diameter class
    data.table::setDT(dat92)[,"DiamClass":= cut(DBH, breaks = c(0,sizeClasses),
                                                labels = sizeClasses, right = FALSE)]

    # sum by species by diameter class within each plot within each unit
    dat92_plot <- dat92[,sum(SPH), by=.(Unit,PlotNum,Spp, DiamClass)]
    data.table::setnames(dat92_plot, "V1", "SPH")

    ## merge with labels, all species in all diam classes to get plots with 0s:
    # add all species to every plot - empty plot is added back here
    dat92_plot <- merge(labels92, dat92_plot, by =c("Unit","PlotNum","Spp","DiamClass"), all.x=TRUE)
    dat92_plot[is.na(SPH), SPH := 0]

    # Now take the mean of SPH by species and diameter class across plots to get a unit-level summary
    dat92_unit <- dat92_plot[,mean(SPH), by=.(Unit, Spp, DiamClass)]
    data.table::setnames(dat92_unit, "V1", "SPH")

    #output does not match data from table 16 in the Date Ck handbook but that is OK because erica checked through
    #cruise comp reports from 1992 and there were data entry mistakes in them, so our numbers might be better
    #also the treatment unit totals were adjusted for the double sampling ratio which doesn't make sense to do
    #for the SORTIE output
    ### Finally got it to work without plyr/ddply!


    ### Summarize fixed area tallies (trees < 7.5cm)
    #now we will add the fixed area tallies: the tallies in fixed radius plots were from Date Ck Handbook table 20
    dat92_tally[,V1:=NULL]
    data.table::setnames(dat92_tally,  c("Init.Dens.1.3Ht", "Init.Dens.2.5", "Init.Dens.5", "Init.Dens.0-7.5"),
                         c("1","2.5","5","7.5"))
    dat92_tally_m <- data.table::melt(dat92_tally, id=c("Unit","Spp"),
                                      variable.name = "DiamClass",
                                      value.name = "SPH")

    # outputs
    dat92_sph_diamClass <- rbind(dat92_tally_m, dat92_unit)
  }else{
    ### SNAGS
    dat92 <- dat92[TC > 2]
    ### Summarize prism plots (trees > 7.5cm)
    #Calculate SPH for each treatment unit and dbh class
    #the prism bands number is the BAF for metric prisms

    dat92[,SPH:= calculateSPH(dat92$PrismBands, dat92$DBH)]

    #assign the diameter class
    data.table::setDT(dat92)[,"DiamClass":= cut(DBH, breaks = c(0,sizeClasses),
                                                labels = sizeClasses, right = FALSE)]

    # sum by species by diameter class within each plot within each unit
    dat92_plot <- dat92[,sum(SPH), by=.(Unit,PlotNum,Spp, TC,DiamClass)]
    data.table::setnames(dat92_plot, "V1", "SPH")

    ## merge with labels, all species in all diam classes in all decay classes to get plots with 0s:
    # add all species to every plot - empty plot is added back here
    TC_labels <- rep(seq(1,5), nrow(labels92))
    labels92 <- labels92[rep(seq(.N), length(seq(1,5)))]
    data.table::setorder(labels92, Unit, PlotNum, Spp, DiamClass)
    labels92[, TC := TC_labels]

    dat92_plot <- merge(labels92, dat92_plot, by =c("Unit","PlotNum","Spp","DiamClass","TC"), all.x=TRUE)
    dat92_plot[is.na(SPH), SPH := 0]

    # Now take the mean of SPH by species and diameter class across plots to get a unit-level summary
    dat92_sph_diamClass <- dat92_plot[,mean(SPH), by=.(Unit, Spp, DiamClass,TC)]
    data.table::setnames(dat92_sph_diamClass, "V1", "SPH")

    #there are no dead trees in the tally plots, so assume it is zero
    #final table needs total SPH in each size class by species, and then the proportion in each decay class
  }


  return(dat92_sph_diamClass)

}

