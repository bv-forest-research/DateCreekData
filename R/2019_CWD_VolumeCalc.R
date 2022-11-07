# Ingrid Farnell
# 2019 Date Creek coarse woody transects - repeat measure from 2011

# This script uses previously cleaned data to calculate volume


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


#--------------------Prepare data----------------------------------------------#
# Import data
CWD.2019<- fread("CWD_2019.csv") # importing only the 2019 data


#-----------------------Calculate volume---------------------------------------#

# Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
# CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]          

# Where: 	L = length of total transect (horizontal distance (HD) in m)
#                                       HD = SD / Square root of [1 + (% slope / 100)2]
# slope was not measured so assume total transect length to be 90m
# D = diameter of each piece of CWD (cm)
# A = tilt angle from horizontal for each piece (degrees)
# Because tilt angle was not measured in 2019, it was assumed to be zero, so that cos (A) = 1 for all pieces in those years.

# Square diameter
CWD.2019[, D2_cosA:= Diam_2019_cm^2]

# Convert individual piece to plot summary for L (length of total transect horizontal distance)
CWD.2019_plot <- CWD.2019[, .(D2cosA = sum(D2_cosA)), by =c("Unit", "Block", "Treatment", "Unique_plot")]


# Import horizontal transect csv file then convert individual lines to plot summary
transect<- fread("CWD_horizontal_dist.csv")
transectPlot <- transect[, .(HorDist = sum(Horizontal_distance)), by =c("Unit", "Unique_plot")]

# Merge horizontal distances with all CWD at the plot level
CWD.2019_plot <-merge(CWD.2019_plot, transectPlot, by = c("Unit", "Unique_plot"))

# Volume (m3/ha) calculation
CWD.2019_plot[, VolumeHa:= pi^2/(8*HorDist) * D2cosA]


# Check number of plots
table(CWD.2019_plot$Treatment, CWD.2019_plot$Block)


