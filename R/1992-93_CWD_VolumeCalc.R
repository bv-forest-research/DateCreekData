# Ingrid Farnell
# 1992 Date Creek coarse woody transects

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

#########
# 1992
#########
#-----------------------Prepare data -----------------------------------------#
# Import 1992
CWD.1992<- fread("CWD_1992.csv")

str(CWD.1992)


#----------------------- Calcuate volume---------------------------------------#
# Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula

# CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]          

# Where: 	L = length of total transect (horizontal distance (HD) in m)
#                                       HD = SD / Square root of [1 + (% slope / 100)2]
# slope was not measured so assume total transect length to be 90m
# D = diameter of each piece of CWD (cm)
# A = tilt angle from horizontal for each piece (degrees)
# Because tilt angle was not measured in 1992 and 1993, it was assumed to be zero, so that cos (A) = 1 for all pieces in those years.

# Square diameter
CWD.1992[, D2_cosA:= Diam_cm^2]

# Convert individual piece to plot summary for L (length of total transect horizontal distance)
CWD.1992_plot<- CWD.2011[, .(D2cosA = sum(D2_cosA)), by =c("Year", "Unit", "Block", "Treatment", "Unique_plot")]


# Volume (m3/ha) calculation
CWD.1992_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA]


##############################################
# 1993
##############################################
#---------------Prepare data---------------------------------------------------#
# Import 1993 data
CWD.1993 <- fread("CWD_1993.csv")


#--------------------- Calculate volume----------------------------------------#
# Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula

# CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]          

# Where: 	L = length of total transect (horizontal distance (HD) in m)
#                                       HD = SD / Square root of [1 + (% slope / 100)2]
# slope was not measured so assume total transect length to be 90m
# D = diameter of each piece of CWD (cm)
# A = tilt angle from horizontal for each piece (degrees)
# Because tilt angle was not measured in 1993 and 1993, it was assumed to be zero, so that cos (A) = 1 for all pieces in those years.

# Square diameter
CWD.1993[, D2_cosA:= Diam_cm^2]

# Convert individual piece to plot summary for L (length of total transect horizontal distance)
CWD.1993_plot<- CWD.2011[, .(D2cosA = sum(D2_cosA)), by =c("Year", "Unit", "Block", "Treatment", "Unique_plot")]

# Volume (m3/ha) calculation
CWD.1993_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA] 
