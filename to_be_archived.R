#'subplot_outputs
#'
#'
#' @export
#'
#'
#' @param out_path
#' @param run_name
#' @param Units_path
#' @param yrs
#' @param Units_to_output a vector of character names for which units to include for subplotting outputs
#' @param dist_edge [numeric()] how far from unit boundary to allow subplots (in m)
#' @param num_subplots [numeric()] how many subplots
#' @param size_subplot [numeric()] radius of plot (standard is 7.98m)
#' @param plotting TRUE/FALSE - whether or not to display plots with the unit and subplot location
#'
#'
subplot_outputs <- function(out_path, run_name, Units_path, yrs, subplot_type = multiple , Units_to_output = "all",
                            dist_edge = 20, num_subplots = 30, size_subplot = 7.98, plotting = TRUE){

  # Reading in unit boundaries and creating shapefiles group by removal class

  #to add: run through the output names and see which years are missing
  #to add: instead of passing years as a parameter, get it from the file names (would solve the above)

  spatialBlocks <- ReadSpatialBounds(Units_path)

  if(any(Units_to_output == "all")){
    Blocks <- spatialBlocks$Unit

  }else{
    Blocks <- Units_to_output
  }

  for(ij in 1:length(Blocks)){
    dt_table <- data.table()
    #restrict trees to within the treatment
    Unit_i <- Blocks[ij]
    TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                        ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                               ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
    #Forest size
    if(TreatType=="NH"){
      NameEnd <- paste0(run_name,"_nh_det_")
    }else{
      NameEnd <- paste0(run_name,"_det_")
    }

    #create sample points here so they are the same for every year
    bb <- st_bbox(st_buffer(spatialBlocks %>% filter(Unit==Unit_i), dist = 10))
    Unit_b <- spatialBlocks %>%
      dplyr::filter(Unit==Unit_i)


    if(num_subplots == 1){
      size_subplot <- 56.41897 #1ha central plot
      xp <-unname(bb$xmin + (bb$xmax - bb$xmin)/2)
      yp <- unname(bb$ymin +(bb$ymax - bb$ymin)/2)
      sampPtsSF <- sf::st_as_sf(data.table(pt=1,x = xp, y = yp), coords = c("x","y"),
                                crs = crs(Unit_b))
      sampPtsSFP <- st_buffer(sampPtsSF, dist=size_subplot)

    }else{
      Unit_b_edge <- st_buffer(Unit_b, dist=-dist_edge) #20m from the edge
      sampPts <- sp::spsample(as_Spatial(Unit_b_edge),n=num_subplots,type="regular")
      sampPtsSF <- st_as_sf(sampPts)
      sampPtsSFP <- st_buffer(sampPtsSF, dist=size_subplot)
    }

    if(plotting == TRUE){
      plot(Unit_b$geometry)
      #plot(Unit_b_edge$geometry, add=TRUE)
      #plot(sampPts,add=TRUE)
      plot(sampPtsSF$geometry,add=TRUE)
      plot(sampPtsSFP$geometry,add=TRUE)
    }

    #add subplot label
    sampPtsDT <- as.data.table(sampPtsSFP)
    sampPtsDT[,SubPlot:=seq(1:nrow(sampPtsSF))]
    sampPts <- st_as_sf(sampPtsDT)

    for(i in 1:length(yrs)){

      file_to_read <- paste0(out_path,"ext_ICH-",TreatType,"-",Unit_i,NameEnd,yrs[i])

      if(file.exists(file_to_read)){
        dt <- fread(file_to_read,sep="\t", header=T,na.strings = "--", skip=1)
        dt[, ':='(timestep = yrs[i],Treatment = TreatType, Unit=Unit_i)]
        dt[, ':='(x_utm =bb[1]+X ,y_utm=bb[2]+Y)] #put the SORTIE outputs in the coordinates of sf
        TreeXY <- st_as_sf(dt, coords = c("x_utm","y_utm"), crs=crs(spatialBlocks))
        plot_trees <- st_intersection(TreeXY,sampPts) #this is the slow part
        print(paste(TreatType,Unit_i,"year",yrs[i],"sampled"))

        if(nrow(plot_trees)==0){ #if no trees (i.e in a clearcut before a plant)
          plot_trees <- as.data.table(plot_trees)
          plot_trees <- rbind(data.table(Species="NA"),plot_trees,fill=TRUE)
          plot_trees[, ':='(timestep = yrs[i],Treatment = TreatType, Unit=Unit_i)]
          plot_trees[,geometry:=NULL]
          print(paste(TreatType,Unit_i,"year",yrs[i],"No trees"))
        }else{
          plot_trees <- as.data.table(plot_trees)
          subPl_area <- length(unique(plot_trees$SubPlot))*(pi*size_subplot^2)
          print(paste(TreatType,Unit_i,"year",yrs[i],"Subplot BA =",
                      round(sum(plot_trees[!is.na(DBH),pi*(DBH/2)^2])/subPl_area,0)))

          plot_trees[,geometry:=NULL]
        }
        dt_table <- rbind(dt_table,plot_trees)

      }else{
        print(paste(file_to_read,"does not exist"))
        dt_table <- dt_table
      }

    }
    write.csv(dt_table,
              paste0(out_path,TreatType,"-",Unit_i,run_name,".csv"), row.names = FALSE)
  }
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



#' Title
#'
#' @param data
#'
#' @return
#' @export
#' @details
#' starts a number list of trees thought to be planted
#'
#'
#' @examples
PlantedTrees_2010 <- function(data = "./data-raw/Trees/Data Creek 2010 Data large trees.csv"){

  # Read in the data for large trees (10+cm DBH) - No Clear Cut plots
  dat.2010 <- fread(data)

  # Rename columns/ variables to match other years
  dat.2010[, Unit := as.factor(Unit)]
  setnames(dat.2010, c("Tree No.","DBH 2010","Gd Pt","Tree Class"),
           c("Tree.No","DBH","PlotNum","Tree.Class"))

  dat.2010$Spp[which(dat.2010$Spp == "Sw")] <- "Sx"
  dat.2010 <- subset(dat.2010, dat.2010$Spp != "n/a")

  dat.2010 <- corr_trees_2010(dat.2010)

  #calculate heights to help find planted trees
  dat.2010[, Height := treeCalcs::height_dbh_Residuals(Species=Spp, DBH=DBH, BECzone = "ICH"),
           by = seq_len(nrow(dat.2010))]

  #Differentiate planted and residual trees as best as possible from existing data
  clearcuts <- c("A3", "A1", "B4", "D2")
  heavyunits <- c("B2", "B3", "C2", "D4")
  dat.2010[, PlantedYN := ifelse(Unit %in% clearcuts, "Y", "N")]
  dat.2010[, PlantedYN := ifelse(Height < 20 & Spp ==  "Sx", "Y", PlantedYN)]
  dat.2010[, PlantedYN := ifelse(DBH < 20 & Unit %in% heavyunits, "Y", PlantedYN)]

  #B3 H050 and J050 are gap plots with fast growing planted spruce
  exclude_B3 <- c("H050", "J050")
  #B2 G150 is gap plots with fast growing planted spruce
  exclude_B2 <- c("G150")
  #also C2 F300 and I450 and E150 and B050
  exclude_C2 <- c("F300", "I450", "E150", "B050")
  #also D4 C350, E300, F350 maybe E200
  exclude_D4 <- c("C350", "E300", "F350", "E200")

  dat.2010[, PlantedYN := ifelse(Unit == "B3" & PlotNum %in% exclude_B3,  "Y", PlantedYN)]
  dat.2010[, PlantedYN := ifelse(Unit == "C2" & PlotNum %in% exclude_C2,  "Y", PlantedYN)]
  dat.2010[, PlantedYN := ifelse(Unit == "D4" & PlotNum %in% exclude_D4,  "Y", PlantedYN)]
  dat.2010[, PlantedYN := ifelse(Unit == "B2" & PlotNum %in% exclude_B2,  "Y", PlantedYN)]
  dat.2010[, Unique.Tree.No := paste(Unit, Tree.No, sep= ".")]

  PlantedTrees <- dat.2010$Unique.Tree.No[which(dat.2010$PlantedYN == "Y" & is.na(dat.2010$Tree.No) == FALSE)]

  return(PlantedTrees)
}



#' Title
#'
#' @param dat
#' @param lrg_trees_2010
#' @param planted_hgt
#' @param sp_plant
#'
#' @return
#' @export
#'
#' @examples
heights_2010 <- function(dat,
                         lrg_trees_2010 = "./data-raw/Trees/Data Creek 2010 Data large trees.csv",
                         planted_hgt = 20, sp_plant = "Sx",
                         id_gap_trees = TRUE){

  clean_201x <- dat
  # Calculate BA and tree height... using residual height function for all trees
  #then plantation height function for clear-cut trees and planted trees in other treatments
  #as best as possible with available data
  dat.2010_all[, Height := treeCalcs::height_dbh_Residuals(Species=Spp,
                                                           DBH=DBH,
                                                           BECzone = "ICH"),
               by = seq_len(nrow(dat.2010_all))]

  dat.2010_all[, PlantationHeight := treeCalcs::height_dbh_plantations(Species=Spp,
                                                                       DBH=DBH,
                                                                       BECzone = "ICH"),
               by = seq_len(nrow(dat.2010_all))]

  #Differentiate planted and residual trees as best as possible from existing data
  #this might
  dat.2010_all[, Unique.Tree.No := paste(Unit, Tree.No, sep= ".")]
  clearcuts <- c("A3", "A1", "B4", "D2")
  heavyunits <- c("B2", "B3", "C2", "D4")
  dat.2010_all[,PlantedYN := ifelse(Unit %in% clearcuts, "Y", "N")]
  dat.2010_all[, PlantedYN := ifelse(Height < 20 & Spp ==  "Sx", "Y", PlantedYN)]
  dat.2010_all[, PlantedYN := ifelse(DBH < 20 & Unit %in%  heavyunits, "Y", PlantedYN)]

  #get the trees that were likely planted
  PlantedList <- PlantedTrees_2010(lrg_trees)

  dat.2010_all[, PlantedYN := ifelse(Unique.Tree.No %in% PlantedList, "Y", PlantedYN)]

  dat.2010_all$Height <-ifelse(dat.2010_all$PlantedYN == "Y",
                               dat.2010_all$PlantationHeight,
                               dat.2010_all$Height)




  clean_201x[, Height :=  treeCalcs::height_dbh_Residuals(Spp, DBH, BECzone = "ICH"),
             by = seq_len(nrow(clean_201x))]
  clean_201x[, PlantationHeight := treeCalcs::height_dbh_plantations(Species=Spp, DBH=DBH, BECzone = "ICH"),
             by = seq_len(nrow(clean_201x))]
  clean_201x[, BA :=  pi*(DBH/200)^2]

  setnames(clean_201x, "Gd.Pt", "PlotNum")

  # Differentiate planted and residual trees as best as possible from existing data
  clean_201x[, Unique.Tree.No := paste(Unit, Tree.No, sep= ".")]
  clearcuts <- c("A3", "A1", "B4", "D2")
  heavyunits <- c("B2", "B3", "C2", "D4")
  clean_201x[, PlantedYN := ifelse(Unit %in% clearcuts, "Y", "N")]
  clean_201x[, PlantedYN := ifelse(Height < planted_hgt &
                                     Spp ==  sp_plant, "Y",
                                   PlantedYN)]
  clean_201x[, PlantedYN := ifelse(DBH < 20 & Unit %in%  heavyunits, "Y", PlantedYN)]
  PlantedList <- PlantedTrees_2010(lrg_trees_2010)
  clean_201x[, PlantedYN := ifelse(Unique.Tree.No %in% PlantedList, "Y", PlantedYN)]


  #this is unique to 201x:
  if(id_gap_trees){
    #this is all hard-coded and possibly better ways to do this
    #B3 H050 and J050 are gap plots with fast growing planted spruce
    exclude_B3 <- c("H050", "J050")
    #B2 G150 is gap plots with fast growing planted spruce
    exclude_B2 <- c("G150")
    #also C2 F300 and I450 and E150 and B050
    exclude_C2 <- c("F300", "I450", "E150", "B050")
    #also D4 C350, E300, F350 maybe E200
    exclude_D4 <- c("C350", "E300", "F350", "E200")
    clean_201x[, PlantedYN := ifelse(Unit == "B3" & PlotNum %in% exclude_B3,
                                     "Y", PlantedYN)]
    clean_201x[, PlantedYN := ifelse(Unit == "C2" & PlotNum %in% exclude_C2,  "Y", PlantedYN)]
    clean_201x[, PlantedYN := ifelse(Unit == "D4" & PlotNum %in% exclude_D4,  "Y", PlantedYN)]
    clean_201x[, PlantedYN := ifelse(Unit == "B2" & PlotNum %in% exclude_B2,  "Y", PlantedYN)]
  }


  clean_201x[, Height := ifelse(PlantedYN == "Y", PlantationHeight, Height)]
  clean_201x <- clean_201x[,.(Unit, PlotNum, Tree.No, Plot.Size, Spp, DBH,Height, Tree.Class,
                              StubYN, PHF)]

  return(clean_201x)

}


#' Title
#'
#' @param dat
#' @param planted_hgt #what height should these trees
#' @param sp_plant
#'
#' @return
#' @export
#'
#' @details
#' the decisions around which plots are "gaps"/fast growing trees is still imbedded, but I've created
#' a parameter to specify the height and species that we look for to id whether it's planted or not
#'
#' @examples
heights_201x <- function(dat,
                         lrg_trees_2010 = "./data-raw/Trees/Data Creek 2010 Data large trees.csv",
                         planted_hgt = 20, sp_plant = "Sx"){

  clean_201x <- dat
  # Calculate BA and tree height... using residual height function for all trees
  #then plantation height function for clear-cut trees and planted trees in other treatments
  #as best as possible with available data
  clean_201x[, Height :=  treeCalcs::height_dbh_Residuals(Spp, DBH, BECzone = "ICH"),
             by = seq_len(nrow(clean_201x))]
  clean_201x[, PlantationHeight := treeCalcs::height_dbh_plantations(Species=Spp, DBH=DBH, BECzone = "ICH"),
             by = seq_len(nrow(clean_201x))]
  clean_201x[, BA :=  pi*(DBH/200)^2]

  setnames(clean_201x, "Gd.Pt", "PlotNum")

  # Differentiate planted and residual trees as best as possible from existing data
  clean_201x[, Unique.Tree.No := paste(Unit, Tree.No, sep= ".")]
  clearcuts <- c("A3", "A1", "B4", "D2")
  heavyunits <- c("B2", "B3", "C2", "D4")
  clean_201x[, PlantedYN := ifelse(Unit %in% clearcuts, "Y", "N")]
  clean_201x[, PlantedYN := ifelse(Height < planted_hgt &
                                     Spp ==  sp_plant, "Y",
                                   PlantedYN)]
  clean_201x[, PlantedYN := ifelse(DBH < 20 & Unit %in%  heavyunits, "Y", PlantedYN)]
  PlantedList <- PlantedTrees_2010(lrg_trees_2010)
  clean_201x[, PlantedYN := ifelse(Unique.Tree.No %in% PlantedList, "Y", PlantedYN)]

  #B3 H050 and J050 are gap plots with fast growing planted spruce
  exclude_B3 <- c("H050", "J050")
  #B2 G150 is gap plots with fast growing planted spruce
  exclude_B2 <- c("G150")
  #also C2 F300 and I450 and E150 and B050
  exclude_C2 <- c("F300", "I450", "E150", "B050")
  #also D4 C350, E300, F350 maybe E200
  exclude_D4 <- c("C350", "E300", "F350", "E200")
  clean_201x[, PlantedYN := ifelse(Unit == "B3" & PlotNum %in% exclude_B3,
                                   "Y", PlantedYN)]
  clean_201x[, PlantedYN := ifelse(Unit == "C2" & PlotNum %in% exclude_C2,  "Y", PlantedYN)]
  clean_201x[, PlantedYN := ifelse(Unit == "D4" & PlotNum %in% exclude_D4,  "Y", PlantedYN)]
  clean_201x[, PlantedYN := ifelse(Unit == "B2" & PlotNum %in% exclude_B2,  "Y", PlantedYN)]

  clean_201x[, Height := ifelse(PlantedYN == "Y", PlantationHeight, Height)]
  clean_201x <- clean_201x[,.(Unit, PlotNum, Tree.No, Plot.Size, Spp, DBH,Height, Tree.Class,
                              StubYN, PHF)]

  return(clean_201x)

}

#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
corr_trees_2018 <- function(dat){
  dat.2018 <- dat

  #data corrections based on 2022 remeasurement
  #plot radius corrections
  dat.2018$Plot.Size[which(dat.2018$Unit == "A3" &
                             dat.2018$Gd.Pt == "A300")] <- 7.98

  #DBH corrections... for trees more than 20 cm dbh out. many more corrections needed after
  #looking at raw dataforms these are guesss to the correct dbh based on common errors
  #(14 sounds like 40, etc.)
  dat.2018$DBH[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 576)] <- 14.5
  dat.2018$DBH[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 577)] <- 15.2
  dat.2018$DBH[which(dat.2018$Unit == "B2" &
                       dat.2018$Tree.No == 382)] <- 27.1
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 367)] <- 17.9
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 368)] <- 18.7
  dat.2018$DBH[which(dat.2018$Unit == "B2" &
                       dat.2018$Tree.No == 382)] <- 18.7
  dat.2018$DBH[which(dat.2018$Unit == "B4" &
                       dat.2018$Tree.No == 572)] <- 14.1
  dat.2018$DBH[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 318)] <- 14.2
  dat.2018$DBH[which(dat.2018$Unit == "B2" &
                       dat.2018$Tree.No == 367)] <- 23.2

  #correction based on looking at subplot growth,
  dat.2018$DBH[which(dat.2018$Unit == "C1" &
                       dat.2018$Tree.No == 188)] <-
    36.3 #assuming typo wrote 26.3 instead of 36.3
  dat.2018$DBH[which(dat.2018$Unit == "D3" &
                       dat.2018$Tree.No == 233)] <-
    76.3 #assuming dbh between 2010 and 2022 measurements
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 481)] <-
    74.9 #assuming typo wrote 47.9 instead of 74.9

  #tree class corrections
  #had a height in 2022 so couldn't be class 5 for dead fallen in 2018
  dat.2018$Tree.Class[which(dat.2018$Unit == "A2" &
                              dat.2018$Tree.No == 123)] <-
    "1" #checked in field and tree is healthy - class 1
  dat.2018$DBH[which(dat.2018$Unit == "A2" &
                       dat.2018$Tree.No == 123)] <-
    13.7 #this tree also needs a dbh, entering something in between other two years
  dat.2018$Tree.Class[which(dat.2018$Unit == "A2" &
                              dat.2018$Tree.No == 131)] <-
    "1" #checked in field and tree is healthy - class 1, it is a veteran
  dat.2018$DBH[which(dat.2018$Unit == "A2" &
                       dat.2018$Tree.No == 131)] <-
    28.5 #this tree also needs a dbh, entering something in between other two years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 251)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D3" &
                       dat.2018$Tree.No == 251)] <-
    34 #this tree also needs a dbh, entering something in between other two years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 521)] <- "2"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 521)] <-
    32.6 #this tree also needs a dbh, entering something in between other two years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 523)] <- "4"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 523)] <-
    44.5 #this tree also needs a dbh, entering something in between other two years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 633)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 633)] <-
    57.6 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "A4" &
                              dat.2018$Tree.No == 463)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "A4" &
                       dat.2018$Tree.No == 463)] <-
    25 #this tree also needs a dbh, entering year 22 number. 25 in 2022, 37 in 2010. 2010 dbh changed
  dat.2018$Tree.Class[which(dat.2018$Unit == "B5" &
                              dat.2018$Tree.No == 227)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 227)] <-
    18.4 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "B5" &
                              dat.2018$Tree.No == 346)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 346)] <-
    19 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 634)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 634)] <-
    33 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 635)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 635)] <-
    12 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 638)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 638)] <-
    14 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 598)] <- "2"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 598)] <-
    13 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "B3" &
                              dat.2018$Gd.Pt == "I200" & dat.2018$Tree.No == 173)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "B3" &
                       dat.2018$Gd.Pt == "I200" &
                       dat.2018$Tree.No == 173)] <-
    15 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "A4" &
                              dat.2018$Tree.No == 484)] <- "3"
  dat.2018$DBH[which(dat.2018$Unit == "A4" &
                       dat.2018$Tree.No == 484)] <-
    22.4 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "B5" &
                              dat.2018$Tree.No == 118)] <- "3"
  dat.2018$DBH[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 118)] <-
    39 #entering 39 which seems plausible. changing 2010 dbh from 29 to 39
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 577)] <- "3"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 577)] <-
    37.7 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "B1" &
                              dat.2018$Tree.No == 616)] <- "4"
  dat.2018$DBH[which(dat.2018$Unit == "B1" &
                       dat.2018$Tree.No == 616)] <-
    33 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 506)] <- "4"
  dat.2018$DBH[which(dat.2018$Unit == "C3" &
                       dat.2018$Tree.No == 506)] <-
    16.9 #entering something in between other years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 561)] <- "4"
  dat.2018$DBH[which(dat.2018$Unit == "D3" &
                       dat.2018$Tree.No == 561)] <-
    22 #entering something in between other years


  #recorded as class 1 in 2022, so could not have been class 4 in  2018
  dat.2018$Tree.Class[which(dat.2018$Unit == "B1" &
                              dat.2018$Tree.No == 646)] <- "1"
  #recorded as dead in 2010 and 2022, so could not have been live in  2018
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 96)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 100)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 580)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 243)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 616)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "A2" &
                              dat.2018$Tree.No == 171)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 363)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 621)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 626)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B1" &
                              dat.2018$Tree.No == 644)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B1" &
                              dat.2018$Tree.No == 649)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B3" &
                              dat.2018$Tree.No == 36)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B3" &
                              dat.2018$Tree.No == 108)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B5" &
                              dat.2018$Tree.No == 179)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 110)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 121)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C2" &
                              dat.2018$Tree.No == 331)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 509)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 511)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 694)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 668)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 618)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 625)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 592)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D5" &
                              dat.2018$Tree.No == 381)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D5" &
                              dat.2018$Tree.No == 469)] <- "4"

  #more corrections... these were not dead in 2018
  dat.2018$Tree.Class[which(dat.2018$Unit == "B1" &
                              dat.2018$Tree.No == 436)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B3" &
                              dat.2018$Tree.No == 109)] <- "1"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 61)] <- "1"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 507)] <- "1"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 510)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 515)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 522)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "A2" &
                              dat.2018$Tree.No == 106)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 607)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D5" &
                              dat.2018$Tree.No == 428)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "A4" &
                              dat.2018$Tree.No == 508)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 672)] <- "2"

  #Species corrections
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 76)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 91)] <- "Pl"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 560)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 562)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 563)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 564)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 565)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 568)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 571)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "A1" &
                       dat.2018$Tree.No == 574)] <- "Ac"

  dat.2018$Spp[which(dat.2018$Unit == "A3" &
                       dat.2018$Tree.No == 341)] <- "Ep"
  dat.2018$Spp[which(dat.2018$Unit == "A3" &
                       dat.2018$Tree.No == 115)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "A3" &
                       dat.2018$Tree.No == 123)] <- "Hw"

  dat.2018$Spp[which(dat.2018$Unit == "A4" &
                       dat.2018$Tree.No == 536)] <- "Ac"

  dat.2018$Spp[which(dat.2018$Unit == "B3" &
                       dat.2018$Tree.No == 108)] <- "Bl"
  dat.2018$Spp[which(dat.2018$Unit == "B3" &
                       dat.2018$Tree.No == 200)] <- "Sx"

  dat.2018$Spp[which(dat.2018$Unit == "B4" &
                       dat.2018$Tree.No == 599)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "B4" &
                       dat.2018$Tree.No == 179)] <- "Ba"

  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 131)] <- "Ac"

  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 16)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 17)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 312)] <- "At"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 2)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 210)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 102)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 103)] <- "Cw"

  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 115)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 116)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 114)] <- "Ba"

  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 105)] <- "Ba"

  #these trees were outside of 5.64 m plot radius and should excluded from data
  dat.2018 <-
    subset(dat.2018, dat.2018$Unit != "D2" | dat.2018$Tree.No != 102)
  dat.2018 <-
    subset(dat.2018, dat.2018$Unit != "D2" | dat.2018$Tree.No != 103)

  #these corrections also needed for 2010 data, continuance of the original error
  dat.2018$Spp[which(dat.2018$Unit == "A2" &
                       dat.2018$Tree.No == 625)] <- "Ba"
  dat.2018$Spp[which(dat.2018$Unit == "A2" &
                       dat.2018$Tree.No == 628)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "A4" &
                       dat.2018$Tree.No == 471)] <-
    "Ep" #conifer to decid mistake - could check in field
  dat.2018$Spp[which(dat.2018$Unit == "B3" &
                       dat.2018$Tree.No == 94)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B3" &
                       dat.2018$Tree.No == 139)] <- "At"
  dat.2018$Spp[which(dat.2018$Unit == "B3" &
                       dat.2018$Tree.No == 200)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 124)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 227)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 259)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 342)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 312)] <-
    "At" #conifer to decid mistake - could check in field
  dat.2018$Spp[which(dat.2018$Unit == "C3" &
                       dat.2018$Tree.No == 484)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 648)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 631)] <- "Ba"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 632)] <- "Ba"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 606)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 587)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 570)] <- "At"
  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 413)] <- "Ba"
  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 414)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 488)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 527)] <- "Bl"

  dat.2018$Spp[which(dat.2018$Unit == "B5" & dat.2018$Tree.No ==124)]<-"Hw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" & dat.2018$Tree.No ==227)]<-"Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" & dat.2018$Tree.No ==259)]<-"Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" & dat.2018$Tree.No ==342)]<-"Cw"

  return(dat.2018)

}





##########################
### INTERMEDIATE TREES ###
##########################

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

###################
### SMALL TREES ###
###################
