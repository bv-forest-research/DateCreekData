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
  dat92_plot <- dat92[,.(SPH = sum(SPH)), by=.(Unit,PlotNum,Spp, DiamClass)]

  ## merge with labels, all species in all diam classes to get plots with 0s:
  # add all species to every plot - empty plot is added back here
  dat92_plot <- merge(labels92, dat92_plot, by =c("Unit","PlotNum","Spp","DiamClass"), all.x=TRUE)
  dat92_plot[is.na(SPH), SPH := 0]

  # Now take the mean of SPH by species and diameter class across plots to get a unit-level summary
  dat92_unit <- dat92_plot[,.(SPH = mean(SPH)), by=.(Unit, Spp, DiamClass)]


  #output does not match data from table 16 in the Date Ck handbook but that is OK because erica checked through
  #cruise comp reports from 1992 and there were data entry mistakes in them, so our numbers might be better
  #also the treatment unit totals were adjusted for the double sampling ratio which doesn't make sense to do
  #for the SORTIE output
  ### Finally got it to work without plyr/ddply!


  ### Summarize fixed area tallies (trees < 7.5cm)
  #now we will add the fixed area tallies: the tallies in fixed radius plots were from Date Ck Handbook table 20
  dat92_tally[,V1:=NULL]
  data.table::setnames(dat92_tally,  c("Init.Dens.1.3Ht", "Init.Dens.2.5", "Init.Dens.5", "Init.Dens.5-7.5"),
                       c("1","2.5","5","7.5"))
  dat92_tally_m <- data.table::melt(dat92_tally, id.vars =c("Unit","Spp"),
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
  dat92_plot <- dat92[,.(SPH = sum(SPH)), by=.(Unit,PlotNum,Spp, TC,DiamClass)]

  ## merge with labels, all species in all diam classes in all decay classes to get plots with 0s:
  # add all species to every plot - empty plot is added back here
  TC_labels <- rep(seq(1,5), nrow(labels92))
  labels92 <- labels92[rep(seq(.N), length(seq(1,5)))]
  data.table::setorder(labels92, Unit, PlotNum, Spp, DiamClass)
  labels92[, TC := TC_labels]

  dat92_plot <- merge(labels92, dat92_plot, by =c("Unit","PlotNum","Spp","DiamClass","TC"), all.x=TRUE)
  dat92_plot[is.na(SPH), SPH := 0]

  # Now take the mean of SPH by species and diameter class across plots to get a unit-level summary
  dat92_sph_diamClass <- dat92_plot[,.(SPH = mean(SPH)), by=.(Unit, Spp, DiamClass,TC)]

  #there are no dead trees in the tally plots, so assume it is zero
  #final table needs total SPH in each size class by species, and then the proportion in each decay class
  sizeClasses <- 7.5 #can't have snags <5-7.5cm diameter class
  labels92 <- Unit_Plot_Labels
  labels_unit <- unique(labels92[,.(Unit)])
  #add species to every plot
  Spp_labels <- rep(unique(Spp_labels)[!is.na(unique(Spp_labels))], nrow(labels_unit))
  labels_unit <- labels_unit[rep(seq(.N), length(unique(Spp_labels)[!is.na(unique(Spp_labels))]))]
  labels_unit[, Spp:= Spp_labels]

  #add diameter class to every species in every plot
  diam_labels <- rep(sizeClasses, nrow(labels_unit))
  labels_unit <- labels_unit[rep(seq(.N), length(sizeClasses))]
  data.table::setorder(labels_unit, Unit, Spp)
  labels_unit[, DiamClass:= diam_labels]
  labels_unit[, DiamClass := as.factor(DiamClass)]

  TC_labels <- rep(seq(1,5), nrow(labels_unit))
  labels_unit <- labels_unit[rep(seq(.N), length(seq(1,5)))]
  data.table::setorder(labels_unit, Unit, Spp, DiamClass)
  labels_unit[, TC := TC_labels]

  DC_dat <- merge(labels_unit, dat92_sph_diamClass, by =c("Unit","Spp","DiamClass","TC"), all =TRUE)
  DC_dat[is.na(SPH), SPH := 0]
}
