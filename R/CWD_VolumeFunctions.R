
#' 1992 CWD volume calculation
#'
#' @param CWD_dat
#'
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
CWD_1992_Vol_calc <- function(CWD_dat){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1992
  CWD.1992<- fread(CWD_dat)

  # Square diameter
  CWD.1992[, D2_cosA:= Diam_cm^2]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  CWD.1992_plot<- CWD.1992[, .(D2cosA = sum(D2_cosA)), by =c("Year", "Unit", "Block", "Treatment", "Unique_plot")]


  # Volume (m3/ha) calculation
  CWD.1992_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA]

  return(CWD.1992_plot)

}




#' 1993 CWD volume calculation
#'
#' @param CWD_dat
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
CWD_1993_Vol_calc <- function(CWD_dat){

  CWD.1993 <- fread(CWD_dat)


  # Square diameter
  CWD.1993[, D2_cosA:= Diam_cm^2]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  CWD.1993_plot<- CWD.1993[, .(D2cosA = sum(D2_cosA)), by =c("Year", "Unit", "Block", "Treatment", "Unique_plot")]

  # Volume (m3/ha) calculation
  CWD.1993_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA]

  return(CWD.1993_plot)

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
CWD_2011_Vol_cal <- function(CWD_dat, Horiz_dat){
  # Import Data
  CWD.2011<- fread(CWD_dat)

  # Remove rows that are place-holders for transects with no CWD by subsetting data (but they will be accounted for later)
  CWD.2011 <- CWD.2011[Diam_cm != "x",]
  CWD.2011[, Diam_cm:= as.numeric(Diam_cm)]
  #CWD.2011$Dist_m[which(CWD.2011$Dist_m == "x")]
  CWD.2011 <- CWD.2011[Dist_m != "x"]

  # Only include pieces >= 10 cm diameter
  CWD.2011 <- CWD.2011[Diam_cm >= 10]

  #-------------------------Calculate volume ------------------------------------#

  # diameter square/cos(A)
  CWD.2011[, Tilt_deg:= as.numeric(Tilt_deg)]
  CWD.2011[, D2_cosA:= Diam_cm^2 /cos(Tilt_deg)]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  CWD.2011_plot <- CWD.2011[, .(D2cosA = sum(D2_cosA)), by =c("Unit", "Block", "Treatment", "Unique_plot")]

  # Import horizontal transect csv file then convert individual lines to plot summary
  transect <- fread(Horiz_dat)
  transectPlot <- transect[, .(HorDist = sum(Horizontal_distance)), by =c("Unit", "Unique_plot")]

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
#' @details 2018 Date Creek coarse woody transects
#' @details Alternative volume calculation (not used - but here just in case want to play with)
#' Volume calculation - taper volume equation (used in Gove paper)Using the taper equation results in the most similar volume values as the volume from transects
#' Functions for calculating gove volume
#' taper_volume <- function (du,db,L,r,il){
#'   p1 <- (du^2)*il + L*((db-du)^2)*(r/(r+4))*(1-(1-(il/L)^((r+4)/r)))
#'   p2 <- 2*L*du*(db-du)*(r/(r+2))*(1-(1-(1-(il/L)^((r+2)/r))))
#'   v  <- (pi/4)*(p1+p2)
#'   return(v)
#' }
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
#'
#'
#' @return data.table with total volume of CWD by plot and Unit
#'
#'
CWD_2018_Vol_calc <- function(CWD_dat){
  #Import Data

  CWD.2018 <- fread(CWD_dat)
  CWD.2018[,Plot:=as.factor(Plot)]#changing plot to be a factor

  # For time series calculation, use only pieces with at least one diameter > 10 cm to match with previous years data
  CWD.2018<- subset(CWD.2018, CWD.2018$Diam1m >= 0.10 | CWD.2018$Diam2m >= 0.10)

  CWD.2018[Decay ==0] <- 1 #Putting pieces that were decay 0 into decay 1
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
  CWD.2018_plot <- CWD.2018[, .(volume = sum(Volume_inclus_ind)), by= c("Block", "Treatment", "TreatmentType", "TreatmentUnitType","Unit")]


  # Add row for plot without CWD
  #table(CWD.2018_plot$Treatment, CWD.2018_plot$Block) # missing a clearcut mesic mature plot = B4-0

  # Create and add B4-0
  `B4-0` <- data.table("Block" = "mesic mature", "Treatment" = "clearcut", "TreatmentType" = "B4-0", "TreatmentUnitType" = "B4", "Unit" = "B4-0", "volume" = 0)
  CWD.2018_plot <- rbindlist(list(CWD.2018_plot, `B4-0`))

  # Convert volume into volume/ha
  CWD.2018_plot[, volumeHa:= volume* 10000] # I think this conversion is correct - I think the a_s makes it /m2 ** check with Erica**
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
                              volumeHa= CWD.2018_plot[i,volumeHa])
  }

  CWD.2018_plot[, VolumeHa:= v]


  # Create new rows that will be the gap and the matrix added
  CWD.2018_Plot <- CWD.2018_plot[, .(VolumeHa = sum(VolumeHa)), by=c("Block", "Treatment", "TreatmentType")]

  return(CWD.2018_plot)

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
#'
CWD_2019_Vol_calc <- function(CWD_dat, Horiz_dat){

  # Import data
  CWD.2019<- fread(CWD_dat)
  # Square diameter
  CWD.2019[, D2_cosA:= Diam_2019_cm^2]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  CWD2019_plot <- CWD.2019[, .(D2cosA = sum(D2_cosA)), by =c("Unit", "Block", "Treatment", "Unique_plot")]


  # Import horizontal transect csv file then convert individual lines to plot summary
  transect<- fread(Horiz_dat)
  transectPlot <- transect[, .(HorDist = sum(Horizontal_distance)), by =c("Unit", "Unique_plot")]

  # Merge horizontal distances with all CWD at the plot level
  CWD2019_plot <-merge(CWD2019_plot, transectPlot, by = c("Unit", "Unique_plot"))

  # Volume (m3/ha) calculation
  CWD2019_plot[, VolumeHa:= pi^2/(8*HorDist) * D2cosA]

  # Check number of plots
  #table(CWD2019_plot$Treatment, CWD2019_plot$Block)
  return(CWD2019_plot)

}

