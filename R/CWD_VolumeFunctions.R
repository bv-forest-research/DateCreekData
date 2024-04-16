
#' Date Creek CWD - all years
#'
#' @param dat_loc what is the directory where all data is stored
#'
#' @details we expect the directory to contain 6 files: CWD_1992.csv, CWD_1993.csv, CWD_2011.csv, CWD_2018.csv and CWD_2019.csv
#' @return
#' @export
#'
#' @examples
CWD_vol_calc <- function(dat_loc, incl_sp_decay = FALSE){

  #calculate volume for all years
  dc_cwd_92 <- CWD_1992_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_1992.csv"),
                                 out_carbon_comp = incl_sp_decay)
  #1993
  dc_cwd_93 <- CWD_1993_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_1993.csv"),
                                 out_carbon_comp = incl_sp_decay)
  #2011
  dc_cwd_11 <- CWD_2011_Vol_calC(CWD_dat = paste0(dat_loc,"CWD_2011.csv"),
                                 Horiz_dat = paste0(dat_loc,"CWD_horizontal_dist.csv"),
                                 out_carbon_comp = incl_sp_decay)
  #2018 - 2018 transect data only occurred in CC and is in the 2019 data. 2018 csv contains plot data, not suitable for
  #time series analysis
  #dc_cwd_18 <- CWD_2018_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_2018.csv"),
   #                              out_carbon_comp = incl_sp_decay)
  #2019
  dc_cwd_19 <- CWD_2019_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_2019.csv"),
                                 Horiz_dat = paste0(dat_loc,"CWD_horizontal_dist.csv"),
                                 out_carbon_comp = incl_sp_decay)

  if(incl_sp_decay == FALSE){
    cd_cwd_allyears <- rbind(dc_cwd_92[,.(Year,Yrs_Post = 0,Unit, Unique_plot, VolumeHa)],
                             dc_cwd_93[,.(Year,Yrs_Post = 1,Unit = Unit,Unique_plot, VolumeHa)],
                             dc_cwd_11[,.(Year = 2011, Yrs_Post = 19,Unit,Unique_plot, VolumeHa)],
                             dc_cwd_19[,.(Year = 2019, Yrs_Post = 27,Unit, Unique_plot, VolumeHa)])

  }else{
    cd_cwd_allyears <- rbind(dc_cwd_92[,.(Year,Yrs_Post = 0,Unit,Unique_plot, Sp, Decay, VolumeHa)],
                             dc_cwd_93[,.(Year,Yrs_Post = 1,Unit = Unit,Unique_plot,
                                          Sp, Decay, VolumeHa)],
                             dc_cwd_11[,.(Year = 2011, Yrs_Post = 19,Unit,Unique_plot,
                                          Sp, Decay, VolumeHa)],
                             dc_cwd_19[,.(Year = 2019, Yrs_Post = 27,Unit,Unique_plot,
                                          Sp, Decay = Decay_2019, VolumeHa)])

    #clean the species columns
    cd_cwd_allyears[, Sp := ifelse(Sp=="u","U",
                                   ifelse(Sp == "", "U",
                                          ifelse(Sp == "ep", "Ep",
                                                 ifelse(Sp == "Act","Ac",Sp))))]
  }




  return(cd_cwd_allyears)

}








#' 1992 CWD volume calculation
#'
#' @param CWD_dat
#' @param size_group description
#' @param out_carbon_comp description
#' @return
#' @export
#' @details Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#'  CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]
#'   Where: 	L = length of total transect (horizontal distance (HD) in m)
#'                                         HD = SD / Square root of [1 + (% slope / 100)2]
#' slope was not measured so assume total transect length to be 90m
#' D = diameter of each piece of CWD (cm)
#' A = tilt angle from horizontal for each piece (degrees)
#' Because tilt angle was not measured in 1992 and 1993, it was assumed to be zero,
#' so that cos (A) = 1 for all pieces in those years.
#'
#' @examples
CWD_1992_Vol_calc <- function(CWD_dat, out_carbon_comp = FALSE, size_group = FALSE){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1992
  CWD.1992 <- fread(CWD_dat)

  # Square diameter
  CWD.1992[, D2_cosA:= Diam_cm^2, by = seq_len(nrow(CWD.1992))]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  if(out_carbon_comp == FALSE){
    if(size_group){
      # Category for small and large logs
      CWD.1992[, sizeGr:= ifelse(Diam_cm < 15, "small", "large")]
      CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Unit", "Block", "Treatment", "Unique_plot",
                                      "sizeGr")]
    }else {
      CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Unit", "Block", "Treatment", "Unique_plot")]
    }

  }else{
    #if volume will be used for carbon, need to keep species and decay class columns
    if(size_group){
      # Category for small and large logs
      CWD.1992[, sizeGr:= ifelse(Diam_cm < 15, "small", "large")]
      CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Unit","Sp", "Decay","Block", "Treatment",
                                      "Unique_plot","sizeGr")]

    }else {
      CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Unit","Sp", "Decay","Block", "Treatment",
                                      "Unique_plot")]

    }
      }

  # Volume (m3/ha) calculation (includes transect length(90m))
  CWD.1992_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA]

  return(CWD.1992_plot)

}

#' Calculate mean diameters by size class for 1992 CWD by Unit
#'
#' @param CWD_dat
#'
#' @return
#'
#' @export
#'
#' @examples
CWD_1992_diams <- function(CWD_dat){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1992
  CWD.1992<- fread(CWD_dat)

  #### Average diameters

  #### Average diameters

  # Category for small and large logs
  CWD.1992[, sizeGr:= ifelse(Diam_cm < 15, "small", "large")]

  #Species groups
  Group1 <- c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3 <- c("At","Ac","Ep")
  CWD.1992$SpGrp <- ifelse(CWD.1992$Sp %in% Group1, 1,
                          ifelse(CWD.1992$Sp %in% Group3, 3, 2 ))

  CWD.1992_mnLogs <- CWD.1992[, .(mean_diam = mean(Diam_cm)),
                              by = .(Unit, sizeGr, SpGrp, Decay)]

  return(CWD.1992_mnLogs)

}

#' Title
#'
#' @param CWD_dat
#'
#' @return
#' @export
#'
#' @details
#' LePage et al. 2000 substrate covers for log proportions 1.2 and 10.8 are percent cover and 0.2
#' is rotten wood for fresh log and moss covered log substrates in full canopy sites at Date Creek
#'
#' from SORTIE manual, formula to calculate log volume in m2/ha from percent log area (PLA)
#' and mean diameter class (MDBH)LV = 1/3 * π * 100 * PLA * (MDBH / 2)
#'
#'
#' @examples
CWD_1992_props <- function(dat_loc){

  # Import 1992 data and calc volume
  CWD.1992 <- CWD_1992_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_1992.csv"),
                                out_carbon_comp = TRUE,
                                size_group  = TRUE)

  # Species groups -----
  Group1 <- c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3 <- c("At","Ac","Ep")
  CWD.1992$SpGrp <- ifelse(CWD.1992$Sp %in% Group1, 1,
                           ifelse(CWD.1992$Sp %in% Group3, 3, 2 ))

  #for now assuming all Units have 10 plots
  cwd92_Unit <- CWD.1992[,.(VolumeHa = sum(VolumeHa)/10),
                         by = .(Unit, SpGrp, sizeGr, Decay)]
  #check calculations
  #cwd92_Unit[,.(VolumeHa = sum(VolumeHa)), by = .(Unit)]

  #get mean diameters
  cwd92_d <- CWD_1992_diams(CWD_dat = paste0(dat_loc,"CWD_1992.csv"))

  cwd92_Unit <- merge(cwd92_Unit, cwd92_d,
                      by = c("Unit", "sizeGr", "SpGrp", "Decay"),
                      all.x = TRUE)

  # calculate proportion log area
  cwd92_Unit[, PLA := VolumeHa /(1/3 * pi * (mean_diam/ 2))]

  # Percent area logs by whole unit...using mean diameters for every combination
  # these seem realistic compared to LePage et al. 2000
  cwd92_Unit[,.(PLA = sum(PLA)), by = .(Unit)]

  sp_decay_size <- CJ(Unit = DateCreekData::Treatments$Unit,
                      SpGrp = c(1,2,3),
                      sizeGr = c("small","large"),
                      Decay = c(1,2,3,4,5))


  cwd92_Unit_all <- merge(cwd92_Unit,
                          sp_decay_size,
                          by = c("Unit","SpGrp","sizeGr","Decay"), all = TRUE)

  setnafill(cwd92_Unit_all, cols = c("VolumeHa", "mean_diam", "PLA"), fill = 0)

  cwd92_Unit_all[,propLA := PLA/100]

  return(cwd92_Unit_all)

}



#' 1993 CWD volume calculation
#'
#' @param CWD_dat
#' @param size_group description
#'
#' @return
#' @export
#' @details Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#' CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]
#'  Where: 	L = length of total transect (horizontal distance (HD) in m)
#'                                         HD = SD / Square root of [1 + (% slope / 100)2]
#' slope was not measured so assume total transect length to be 90m
#' D = diameter of each piece of CWD (cm)
#' A = tilt angle from horizontal for each piece (degrees)
#' Because tilt angle was not measured in 1992 and 1993, it was assumed to be zero,
#' so that cos (A) = 1 for all pieces in those years.
#'
#'
#' @examples
CWD_1993_Vol_calc <- function(CWD_dat, out_carbon_comp = FALSE, size_group = FALSE){

  CWD.1993 <- fread(CWD_dat)

  # Square diameter
  CWD.1993[, D2_cosA:= Diam_cm^2, by = seq_len(nrow(CWD.1993))]

  CWD.1993[is.na(Decay)]
  CWD.1993[is.na(Decay), Decay := round(mean(na.omit(CWD.1993$Decay)))]
  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  if(out_carbon_comp == FALSE){
    if(size_group){
      # Category for small and large logs
      CWD.1993[, sizeGr:= ifelse(Diam_cm < 15, "small", "large")]
      CWD.1993_plot <- CWD.1993[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Stand", "Block", "Treatment", "Unique_plot",
                                      "sizeGr")]
    }else {
      CWD.1993_plot <- CWD.1993[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Stand", "Block", "Treatment", "Unique_plot")]
    }

  }else{
    #if volume will be used for carbon, need to keep species and decay class columns
    if(size_group){
      # Category for small and large logs
      CWD.1993[, sizeGr:= ifelse(Diam_cm < 15, "small", "large")]
      CWD.1993_plot <- CWD.1993[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Stand","Sp", "Decay","Block", "Treatment",
                                      "Unique_plot","sizeGr")]

    }else {
      CWD.1993_plot <- CWD.1993[, .(D2cosA = sum(D2_cosA)),
                                by =c("Year", "Stand","Sp", "Decay","Block", "Treatment",
                                      "Unique_plot")]

    }
  }

  # Volume (m3/ha) calculation
  CWD.1993_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA]

  setnames(CWD.1993_plot, "Stand", "Unit")
  return(CWD.1993_plot)

}

#' 1993 cwd diams
#'
#' @param CWD_dat
#'
#' @return
#' @export
#'
#' @examples
CWD_1993_diams <- function(CWD_dat){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1993
  CWD.1993<- fread(CWD_dat)

  #### Average diameters

  #### Average diameters

  # Category for small and large logs
  CWD.1993[, sizeGr:= ifelse(Diam_cm < 15, "small", "large")]

  #there's one tree with unknown Decay
  CWD.1993[is.na(Decay)]
  #use average decay class?
  CWD.1993[is.na(Decay), Decay := round(mean(na.omit(CWD.1993$Decay)))]

  #Species groups
  Group1 <- c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3 <- c("At","Ac","Ep")
  CWD.1993$SpGrp <- ifelse(CWD.1993$Sp %in% Group1, 1,
                           ifelse(CWD.1993$Sp %in% Group3, 3, 2 ))

  CWD.1993_mnLogs <- CWD.1993[, .(mean_diam = mean(Diam_cm)),
                              by = .(Stand, sizeGr, SpGrp, Decay)]
  setnames(CWD.1993_mnLogs, "Stand", "Unit")
  return(CWD.1993_mnLogs)

}

#' Calculate CWD proportions after harvest from Date Creek study
#'
#' @param dat_loc
#'
#' @return
#' @export
#'
#' @examples
CWD_1993_props <- function(dat_loc){
  #-----------------------Prepare data -----------------------------------------#

  # Import 1992 data and calc volume
  CWD.1993 <- CWD_1993_Vol_calc(CWD_dat = paste0(dat_loc,"CWD_1993.csv"),
                                out_carbon_comp = TRUE,
                                size_group  = TRUE)

  # Species groups -----
  Group1 <- c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3 <- c("At","Ac","Ep")
  CWD.1993$SpGrp <- ifelse(CWD.1993$Sp %in% Group1, 1,
                           ifelse(CWD.1993$Sp %in% Group3, 3, 2 ))


  #for now assuming all Units have 10 plots
  cwd93_Unit <- CWD.1993[,.(VolumeHa = sum(VolumeHa)/10),
                         by = .(Unit, SpGrp, sizeGr, Decay)]
  #check calculations
  #cwd92_Unit[,.(VolumeHa = sum(VolumeHa)), by = .(Unit)]

  #get mean diameters
  cwd93_d <- CWD_1993_diams(CWD_dat = paste0(dat_loc,"CWD_1993.csv"))

  cwd93_Unit <- merge(cwd93_Unit, cwd93_d,
                      by = c("Unit", "sizeGr", "SpGrp", "Decay"),
                      all.x = TRUE)

  # calculate proportion log area
  cwd93_Unit[, PLA := VolumeHa /(1/3 * pi * (mean_diam/ 2))]

  # Percent area logs by whole unit...using mean diameters for every combination
  # these seem realistic compared to LePage et al. 2000
  cwd93_Unit[,.(PLA = sum(PLA)), by = .(Unit)]

  sp_decay_size <- CJ(Unit = DateCreekData::Treatments$Unit,
                      SpGrp = c(1,2,3),
                      sizeGr = c("small","large"),
                      Decay = c(1,2,3,4,5))

  cwd93_Unit_all <- merge(cwd93_Unit,
                          sp_decay_size,
                          by = c("Unit","SpGrp","sizeGr","Decay"), all = TRUE)

  setnafill(cwd93_Unit_all, cols = c("VolumeHa", "mean_diam", "PLA"), fill = 0)

  cwd93_Unit_all[,propLA := PLA/100]

  return(cwd93_Unit_all)

}

#' 2011 CWD volume calculation
#'
#' @param CWD_dat
#'
#' @return
#' @export
#' @details Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#' CWD volume (m3/ha) = pi^2/8L  *  sum[D^2/cos (A)]
#'  Where: 	L = length of total transect (horizontal distance (HD) in m)
#'                                       HD = SD / Square root of [1 + (% slope / 100)2]
#'                                        D = diameter of each piece of CWD (cm)
#'                                         A = tilt angle from horizontal for each piece (degrees)
#' @details There is only one horizontal distance file - works for both 2011 and 2018?
#' @examples
CWD_2011_Vol_calC <- function(CWD_dat, Horiz_dat, out_carbon_comp = FALSE){
  # Import Data
  CWD.2011<- fread(CWD_dat, na.strings = "x") #AC: trying ot make x na so that the str makes more sense at import

  # Remove rows that are place-holders for transects with no CWD by subsetting data (but they will be accounted for later)
  # Only include pieces >= 10 cm diameter
  CWD.2011 <- CWD.2011[!is.na(Diam_cm) & !is.na(Dist_m) & Diam_cm >= 10]

  #CWD.2011 <- CWD.2011[Diam_cm != "x",]
  #CWD.2011[, Diam_cm:= as.numeric(Diam_cm)] #If switch backt to x, need to do this
  #CWD.2011$Dist_m[which(CWD.2011$Dist_m == "x")]
  #CWD.2011 <- CWD.2011[Dist_m != "x"]


  # Import horizontal transect csv file then convert individual lines to plot summary
  transect <- fread(Horiz_dat)
  transectPlot <- transect[, .(HorDist = sum(Horizontal_distance)), by =c("Unit", "Unique_plot")]

  #-------------------------Calculate volume ------------------------------------#
  # diameter square/cos(A)
  # Convert deg to radians -- if you don't you will get a negative value
  CWD.2011[, Tilt.radians:= pi/180*Tilt_deg]
  CWD.2011[, D2_cosA:= Diam_cm^2 /cos(Tilt.radians)]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  if(out_carbon_comp == FALSE){
    CWD.2011_plot <- CWD.2011[, .(D2cosA = sum(D2_cosA)),
                              by =c("Unit", "Block", "Treatment", "Unique_plot")]

  }else{
    CWD.2011_plot <- CWD.2011[, .(D2cosA = sum(D2_cosA)),
                              by =c("Year", "Unit","Sp", "Decay","Block", "Treatment", "Unique_plot")]
  }

  # Merge horizontal distances with all CWD at the plot level
  CWD.2011_plot <- merge(CWD.2011_plot, transectPlot, by = c("Unit", "Unique_plot"))

  # Volume (m3/ha) calculation
  CWD.2011_plot[, VolumeHa:= pi^2/(8* HorDist) * D2cosA]

  ## NOTE CLEARCUTS WERE ACTUALLY MEASURED IN 2018 ##
  return(CWD.2011_plot)

}



#' 2018 CWD function
#'
#' @description `CWD_2018_Vol_calc()`
#' @author Ingrid Farnell
#' @details 2018 Date Creek coarse woody SQUARE PLOTS (not transects)
#' @details Alternative volume calculation (not used - but here just in case want to play with)
#' Volume calculation - taper volume equation (used in Gove paper)Using the taper equation results in the most similar volume values as the volume from transects
#' Functions for calculating gove volume
#'
#' Using conic-parabloid equation from Fraver et al. 2013
#' length and diameter in meters
#' Bole volume equations
#'
#' Assign relevant data to the equation components
#' du <- CWD.2018$small_diam #small diameter
#' db <- CWD.2018$large_diam #large diameter
#' L  <- CWD.2018$Lngth2end #total length
#' il <- L/2 #intermediate log length
#' r  <- 3 #paraboloid
#'
#'  Calculate volumes of the truncated boles
#' taper <- taper_volume(du,db,L,r,il)
#'
#' @param CWD_dat
#'
#' @export
#'
#' @return data.table with total volume of CWD by plot and Unit
#'
#'
CWD_2018_Vol_calc <- function(CWD_dat, out_carbon_comp = FALSE){
  #Import Data

  CWD.2018 <- fread(CWD_dat)
  CWD.2018[,Plot:=as.factor(Plot)]#changing plot to be a factor

  # For time series calculation, use only pieces with at least one diameter > 10 cm to match with previous years data
  CWD.2018 <- CWD.2018[Diam1m >= 0.10| Diam2m >= 0.10]
  #CWD.2018<- subset(CWD.2018, CWD.2018$Diam1m >= 0.10 | CWD.2018$Diam2m >= 0.10)

  CWD.2018[Decay ==0, Decay := 1]# <- 1 #Putting pieces that were decay 0 into decay 1
  #min(CWD.2018$Decay)

  # check all plots for CWD pieces
  #table(CWD.2018$Plot, CWD.2018$TreatmentUnitType) #B4 only has 4 plots, empty plot needs to be accounted for later... counts the number of CWD in each plot

  #--------------------- Calculate volume / piece -------------------------------#

  Conic_paraboloid <- function(L,du,db){
    vol_conicparab <- (L/12)*((5*((pi/4)*db^2))+(5*((pi/4)*du^2))+2*(sqrt(((pi/4)*db^2)*((pi/4)*du^2))))
    return(vol_conicparab)
  }

  # Assign relevant data to the equation components
  # du = small diameter
  #db = large diameter
  # L = total length

  t <- vector()
  for(i in 1:nrow(CWD.2018)){
    t[i] <- Conic_paraboloid(du = CWD.2018[i,small_diam], db= CWD.2018[i,large_diam],
                             L= CWD.2018[i,Lngth2end])
  }
  CWD.2018[, PieceVolume:= t]

  #-- Sausage method of inclusion zone
  # Inclusion zone

  # Formula: as= s*(s+4*l/pi) from Peter Ott
  # s = side length of the sqaure plot = 10 m
  # l = length of CWD piece
  CWD.2018[, a_s:= (10*(10+4*Lngth2end/pi))]

  # Add count
  # Volume with Inclusion probability - individual piece
  CWD.2018[, Volume_inclus_ind:= PieceVolume/a_s]


  #------------------ Convert volume to plot level ------------------------------#
  # Convert individual piece to plot summary
  if(out_carbon_comp == FALSE){
    CWD.2018_plot <- CWD.2018[, .(volume = sum(Volume_inclus_ind)),
                              by= c("Block", "Treatment", "TreatmentType", "TreatmentUnitType","Unit")]

  }else{
    CWD.2018_plot <- CWD.2018[, .(volume = sum(Volume_inclus_ind)),
                              by= c("Block", "Treatment", "TreatmentType","Sp", "Decay","TreatmentUnitType","Unit")]
  }



  # Add row for plot without CWD
  #table(CWD.2018_plot$Treatment, CWD.2018_plot$Block) # missing a clearcut mesic mature plot = B4-0

  # Create and add B4-0
  `B4-0` <- data.table("Block" = "mesic mature", "Treatment" = "clearcut", "TreatmentType" = "B4-0",
                       "TreatmentUnitType" = "B4", "Unit" = "B4-0", "volume" = 0)
  CWD.2018_plot <- rbind(CWD.2018_plot, `B4-0`, fill=TRUE)

  # Convert volume into volume/ha (unweighted by treatment)
  CWD.2018_plot[, UnW_VolHa:= volume* 10000] # I think this conversion is correct - I think the a_s makes it /m2 ** check with Erica**
  #CWD.2018_plot[, volumeHa:= volume/100* 10000] #each plot is 100 m2 and there are 10,000 m2 per ha


  #------ Need account for the two different areas sampled in the heavy removal treatments
  #Weight heavy treatments volumes by area sampled in gaps vs. matrix forest

  TreatmentWeightFN <- function(TreatmentUnitType, volumeHa){
    if(is.na(TreatmentUnitType)){
      print(paste("Unit not found"))
      VolumeHa <- NA
    } else if (TreatmentUnitType == "B3 M"){
      VolumeHa <- volumeHa*0.6628
    } else if (TreatmentUnitType == "B3 O"){
      VolumeHa <- volumeHa*0.3372
    } else if (TreatmentUnitType == "C2 M"){
      VolumeHa <- volumeHa*0.7628
    } else if (TreatmentUnitType == "C2 O"){
      VolumeHa <- volumeHa*0.2372
    } else if (TreatmentUnitType == "B2 M"){
      VolumeHa <- volumeHa*0.7356
    } else if (TreatmentUnitType == "B2 O"){
      VolumeHa <- volumeHa*0.2644
    } else if (TreatmentUnitType == "D4 M"){
      VolumeHa <- volumeHa*0.7263
    } else if (TreatmentUnitType == "D4 O"){
      VolumeHa <- volumeHa*0.2737
    } else {
      VolumeHa <- volumeHa*1
    }
    return(VolumeHa)
  }

  v <- vector()
  for(i in 1:nrow(CWD.2018_plot)){
    v[i] <- TreatmentWeightFN(TreatmentUnitType = CWD.2018_plot[i,TreatmentUnitType],
                              volumeHa= CWD.2018_plot[i,UnW_VolHa])
  }

  #update the volume by treatment weight
  CWD.2018_plot[, VolumeHa:= v]

  # Create new rows that will be the gap and the matrix added
  if(out_carbon_comp == FALSE){
    CWD.2018_Plot <- CWD.2018_plot[, .(VolumeHa = sum(VolumeHa)), by=c("Block", "Treatment", "TreatmentType")]

  }else{
    CWD.2018_Plot <- CWD.2018_plot[, .(VolumeHa = sum(VolumeHa)), by=c("Block", "Treatment", "TreatmentType",
                                                                       "Sp","Decay")]
  }

  #add Unit to match other years
  CWD.2018_Plot[, c("Unit") := tstrsplit(TreatmentType, "-", fixed=TRUE, keep=1L)]

  return(CWD.2018_Plot)

}



#' 2019 function:
#' @description `CWD_2019_Vol_calc()`
#' @author Ingrid Farnell
#' @details 2019 Date Creek coarse woody transects - repeat measure from 2011
#' @details Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#' CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]
#' Where: 	L = length of total transect (horizontal distance (HD) in m) HD = SD / Square root of [1 + (% slope / 100)2]
#' slope was not measured so assume total transect length to be 90m
#' D = diameter of each piece of CWD (cm)
#' A = tilt angle from horizontal for each piece (degrees)
#' Because tilt angle was not measured in 2019, it was assumed to be zero, so that cos (A) = 1 for all pieces in those years.
#'
#' @param CWD_dat
#' @param Horiz_dat
#'
#' @return data.table with total volume of CWD by plot and Unit
#'
#' @export
#'
CWD_2019_Vol_calc <- function(CWD_dat, Horiz_dat, out_carbon_comp = FALSE){

  # Import data
  CWD.2019<- fread(CWD_dat)
  # Square diameter
  CWD.2019[, D2_cosA:= Diam_2019_cm^2]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  if(out_carbon_comp == FALSE){
    CWD2019_plot <- CWD.2019[, .(D2cosA = sum(D2_cosA)),
                             by =c("Unit", "Block", "Treatment", "Unique_plot")]
  }else{
    CWD2019_plot <- CWD.2019[, .(D2cosA = sum(D2_cosA)),
                             by =c("Unit", "Block", "Treatment", "Unique_plot",
                                                               "Sp", "Decay_2019")]
  }


  # Import horizontal transect csv file then convert individual lines to plot summary
  transect<- fread(Horiz_dat)
  transectPlot <- transect[, .(HorDist = sum(Horizontal_distance)), by =c("Unit", "Unique_plot")]

  # Merge horizontal distances with all CWD at the plot level
  CWD2019_plot <- merge(CWD2019_plot, transectPlot, by = c("Unit", "Unique_plot"))

  # Volume (m3/ha) calculation
  CWD2019_plot[, VolumeHa:= pi^2/(8*HorDist) * D2cosA]

  # Check number of plots
  #table(CWD2019_plot$Treatment, CWD2019_plot$Block)
  return(CWD2019_plot)

}

