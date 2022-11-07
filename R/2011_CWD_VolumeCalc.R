# Ingrid Farnell
# 2011 Date Creek coarse woody transects

# This script calculates volume


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

#--------------------Prepare the data into treatment summaries-----------------#
# Import Data
CWD.2011<- fread("CWD_2011.csv") 
str(CWD.2011) 


# Remove rows that are place-holders for transects with no CWD by subsetting data (but they will be accounted for later)
CWD.2011 <- CWD.2011[Diam_cm != "x",]
CWD.2011[, Diam_cm:= as.numeric(Diam_cm)]
unique(CWD.2011$Dist_m) 
CWD.2011$Dist_m[which(CWD.2011$Dist_m == "x")]
CWD.2011<-CWD.2011[Dist_m != "x"]


# Only include pieces >= 10 cm diameter

CWD.2011 <- CWD.2011[Diam_cm >= 10]

#-------------------------Calculate volume ------------------------------------#
# Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula

# CWD volume (m3/ha) = pi^2/8L  *  sum[D^2/cos (A)]          

# Where: 	L = length of total transect (horizontal distance (HD) in m)
#                                       HD = SD / Square root of [1 + (% slope / 100)2]
# D = diameter of each piece of CWD (cm)
# A = tilt angle from horizontal for each piece (degrees)

# diameter square/cos(A)
CWD.2011[, Tilt_deg:= as.numeric(Tilt_deg)]
CWD.2011[, D2_cosA:= Diam_cm^2 /cos(Tilt_deg)]

# Convert individual piece to plot summary for L (length of total transect horizontal distance)
CWD.2011_plot<- CWD.2011[, .(D2cosA = sum(D2_cosA)), by =c("Unit", "Block", "Treatment", "Unique_plot")]

# Import horizontal transect csv file then convert individual lines to plot summary
transect<- fread("CWD_horizontal_dist.csv")
transectPlot <- transect[, .(HorDist = sum(Horizontal_distance)), by =c("Unit", "Unique_plot")]

# Merge horizontal distances with all CWD at the plot level
CWD.2011_plot <-merge(CWD.2011_plot, transectPlot, by = c("Unit", "Unique_plot"))

# Volume (m3/ha) calculation
CWD.2011_plot[, VolumeHa:= pi^2/(8* HorDist) * D2cosA] 

## NOTE CLEARCUTS WERE ACTUALLY MEASURED IN 2018 ##
