

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
