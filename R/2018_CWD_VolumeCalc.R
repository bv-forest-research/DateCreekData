# Ingrid Farnell
# 2018 Date Creek coarse woody debris square plots

# This script uses previously cleaned data, calculates volume from 10 m x 10 m square plots established in 2018

rm(list=ls())   #cleans the workspace so all previous objects are deleted

setwd("C:/Users/Farne/Documents/Masters/Thesis/Chapter_1/PublishedManusrcipt/Data")


#------------------------ Load libraries---------------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("nlme")) # analysis

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#--------------- Prepare data into summaries for each treatment unit-----------#
#Import Data

CWD.2018 <- fread("CWD_2018.csv") 
str(CWD.2018)
CWD.2018$Plot<-as.factor(CWD.2018$Plot) #changing plot to be a factor

# For time series calculation, use only pieces with at least one diameter > 10 cm to match with previous years data
CWD.2018<- subset(CWD.2018, CWD.2018$Diam1m >= 0.10 | CWD.2018$Diam2m >= 0.10)

CWD.2018[Decay ==0] <-1 #Putting pieces that were decay 0 into decay 1
min(CWD.2018$Decay)

# check all plots for CWD pieces
table(CWD.2018$Plot, CWD.2018$TreatmentUnitType) #B4 only has 4 plots, empty plot needs to be accounted for later... counts the number of CWD in each plot

# data summaries
summary(CWD.2018$Lngth2end)
summary(CWD.2018$small_diam)
summary(CWD.2018$large_diam)
summary(CWD.2018$Elev1)
summary(CWD.2018$Elev2)
unique(CWD.2018$Treatment)
unique(CWD.2018$Block)


#--------------------- Calculate volume / piece -------------------------------#
# Using conic-parabloid equation from Fraver et al. 2013
# length and diameter in meters

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

##########################
# Alternative volume calculation (not used - but here just in case want to play with)
##########################

# #Volume calculation - taper volume equation (used in Gove paper)
# # Using the taper equation results in the most similar volume values as the volume from transects
# 
# # Functions for calculating gove volume
# taper_volume <- function (du,db,L,r,il){
#   p1 <- (du^2)*il + L*((db-du)^2)*(r/(r+4))*(1-(1-(il/L)^((r+4)/r)))
#   p2 <- 2*L*du*(db-du)*(r/(r+2))*(1-(1-(1-(il/L)^((r+2)/r))))
#   v  <- (pi/4)*(p1+p2)
#   return(v)
# }
# 
# 
# # Bole volume equations
# 
# # Assign relevant data to the equation components
# du <- CWD.2018$small_diam #small diameter
# db <- CWD.2018$large_diam #large diameter
# L  <- CWD.2018$Lngth2end #total length
# il <- L/2 #intermediate log length
# r  <- 3 #paraboloid
# 
# # Calculate volumes of the truncated boles
# taper <- taper_volume(du,db,L,r,il)
# 
# # Add grove_volume as a new column
# CWD.2018$Volume <- taper
####################


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
table(CWD.2018_plot$Treatment, CWD.2018_plot$Block) # missing a clearcut mesic mature plot = B4-0

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
str(CWD.2018_plot)

CWD.2018_Plot <- CWD.2018_plot[, .(VolumeHa = sum(VolumeHa)), by=c("Block", "Treatment", "TreatmentType")]


#Data check
table(CWD.2018_Plot$Treatment, CWD.2018_Plot$Block) #should be 5 in all treatments

