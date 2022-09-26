
#random choice made here:
#Eliminate plots C2 J050 and J300, which could not be found in 2018-19
#This step is optional and is only done to match 2010 and 2018 data more closely
dat.2010 <- subset(dat.2010, dat.2010$Unit != "C2"| dat.2010$Gd.Pt != "J050")
dat.2010 <- subset(dat.2010, dat.2010$Unit != "C2"| dat.2010$Gd.Pt != "J300")





#1992

# Read in the data for the cruise plots
dat.1992.cruise <- read.csv("./data-raw/1. 1992data.csv", stringsAsFactors = FALSE)

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


# Count how many plots there are for each treatment unit
# so when averaging SPH or carbon/unit later, we can take into account the plots that had zero trees
labels1992.cruise <- ddply(dat.1992.cruise[c("Unit","PlotNum",  "DBH")], .(Unit, PlotNum), numcolwise(length))
labels1992.cruise$DBH <- NULL # this is really a count of the trees in each plot which we don't need in the labels
test.labels <- labels1992.cruise
test.labels$count <- rep(1, length(labels1992.cruise$Unit))
Plot_in_Units92 <- ddply(test.labels, .(Unit), numcolwise(sum))

# Subset for LIVE trees only
dat.1992.cruise_L <- subset(dat.1992.cruise, dat.1992.cruise$Tree.Class <= 2)

# Eliminate  <7.5 measures (should not have been included in cruise)
# This will remove plot with no trees, but later it will come back in with the  labels merge
dat.1992.cruise_L <- subset(dat.1992.cruise_L, dat.1992.cruise_L$DBH >= 7.5 | dat.1992.cruise_L$DBH == 0)
###### ends here and then becomes a script to make inputs for SORTIE


###################
### Fixed plots ###
###################

# Read in the data for the fixed radius plots
dat.1992.fixed <- read.csv("./FromErica/1992fixed_radius_data_fromTable20_DateCkHandbook.csv", stringsAsFactors = FALSE)

# Eliminate unwanted columns
# Only need the 5.1-7.5cm DBH column
dat.1992.fixed <- dat.1992.fixed %>%
  dplyr::select(Unit, Spp, Init.Dens.0.7.5)

# The inital density tallies are really a count of stems per hectare (SPH)
names(dat.1992.fixed)[names(dat.1992.fixed) == "Init.Dens.0.7.5"] <- "SPH"

# Add in the average DBH for this size class and add tree class 1 because all are living
dat.1992.fixed <- dat.1992.fixed %>%
  dplyr::mutate(DBH = 6.3, Tree.Class = 1)





#calculate heights

# Calculate stems per hectare for the cruise plots
dat.1992.cruise_L$SPH <- calculateSPH(pBands = dat.1992.cruise_L$PrismBands,
                                      DBH = dat.1992.cruise_L$DBH)

# Calculate tree height
dat.1992.cruise_L <- as.data.table(dat.1992.cruise_L)

HT <- vector()

for(i in 1:nrow(dat.1992.cruise_L)){
  HT[i] <- DiamHgtFN_ICH(Species = dat.1992.cruise_L[i, Spp], DBH = dat.1992.cruise_L[i, DBH])
}
# Calculate tree height
dat.1992.fixed <- as.data.table(dat.1992.fixed)

HT <- vector()

for(i in 1:nrow(dat.1992.fixed)){
  HT[i] <- DiamHgtFN_ICH(Species = dat.1992.fixed[i, Spp], DBH = dat.1992.fixed[i, DBH])
}








####1993

# Read in the data for both fixed radius and cruise plots
dat.1993 <- read.csv("./data-raw/3. SS93forR.csv", stringsAsFactors = FALSE)

# Rename columns to match other years
names(dat.1993)[names(dat.1993) == "Plot_ID"] <- "Unit"
names(dat.1993)[names(dat.1993) == "SPP"] <- "Spp"
names(dat.1993)[names(dat.1993) == "TC"] <- "Tree.Class"


# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.1993$count<-rep(1, length(dat.1993$Unit ))
Trees_in_Plots<-ddply(dat.1993[c("Unit", "PlotNum", "count")], .(Unit, PlotNum), numcolwise(sum))
Trees_in_Plots$count<-rep(1, length(Trees_in_Plots$Unit ))
Plot_in_Units<-ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units #stands should have 23 or 30 plots (30 for 40% retention treatments)

# Create Labels for all plot numbers within treatments to include plots with no trees later, after putting zeros
labels1993<- Trees_in_Plots[c("Unit", "PlotNum")]

# Eliminate unwanted columns
dat.1993 <- dat.1993 %>%
  dplyr::select(Unit, PlotNum, PrismBands, FixedRad, Spp, DBH, Tree.Class)

dat.1993 <- subset(dat.1993, dat.1993$DBH != ".")

dat.1993 <- dat.1993 %>%
  dplyr::mutate(DBH = as.numeric(DBH))

# Subset the data to our 5cm DBH limit
dat.1993 <- subset(dat.1993, dat.1993$DBH > 5)

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

# Subset LIVE trees only
dat.1993_L <- subset(dat.1993, dat.1993$Tree.Class <= 2)

# Subset fixed radius plots
dat.1993.fixed_L <- subset(dat.1993_L, dat.1993_L$FixedRad != ".")
dat.1993.fixed_L$PrismBands <- NULL

# Subset cruise plots
dat.1993.cruise_L <- subset(dat.1993_L, dat.1993_L$FixedRad == ".")
dat.1993.cruise_L$FixedRad <- NULL











#2010
# Read in the data for large trees (10+cm DBH) - No Clear Cut plots
dat.2010 <- read.csv("./data-raw/4. Data Creek 2010 Data large trees.csv", stringsAsFactors = FALSE)

# Rename columns/ variables to match other years
dat.2010$Unit <- as.factor(dat.2010$Unit)
names(dat.2010)[names(dat.2010) == "Tree.No."] <- "Tree.No" #Rename
names(dat.2010)[names(dat.2010) == "DBH.2010"] <- "DBH" #Rename
dat.2010$Spp[which(dat.2010$Spp == "Sw")] <- "Sx"

#Eliminate plots C2 J050 and J300, which could not be found in 2018-19
#This step is optional and is only done to match 2010 and 2018 data more closely
dat.2010 <- subset(dat.2010, dat.2010$Unit != "C2"| dat.2010$Gd.Pt != "J050")
dat.2010 <- subset(dat.2010, dat.2010$Unit != "C2"| dat.2010$Gd.Pt != "J300")

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
dat.2010 <- dat.2010 %>%
  dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class)

# Subset for LIVE trees only
dat.2010_L <- subset(dat.2010, dat.2010$Tree.Class <= 2)


#############################
### LARGE TREES (CC only) ###
#############################

# For large trees (10+cm DBH) - ONLY in Clear Cut plots
# No dead large trees in the clear cut plots
dat.2010.CC<- read.csv("./FromErica/Trees 10cm and above in clearcut 2010.csv", stringsAsFactors = FALSE)

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
dat.2010.CC <- dat.2010.CC %>%
  dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class)


###################
### SMALL TREES ###
###################

# For small trees (5.1-10cm) - All plots
dat.2010.sm <- read.csv("./FromErica/Date Creek 2010 Trees less than 10 cm tallies.csv") #tallied trees with dbh <4

#Eliminate plots C2 J050 and J300, which could not be found
dat.2010.sm <- subset(dat.2010.sm, dat.2010.sm$Unit != "C2"| dat.2010.sm$Gd.Pt != "J050")
dat.2010.sm <- subset(dat.2010.sm, dat.2010.sm$Unit != "C2"| dat.2010.sm$Gd.Pt != "J300")

dat.2010.sm <- subset(dat.2010.sm, dat.2010.sm$Size.Cl == "5.1-10cm")

# Eliminate unwanted columns
dat.2010.sm_L <- dat.2010.sm %>%
  dplyr::select(Unit, Gd.Pt, Plot.Size, Hw, Cw, Sx, Pl, Bl, Ba, Ep, At, Ac)

# Pivot table from wide to long
dat.2010.sm_L <- as.data.table(dat.2010.sm_L)
dat.2010.sm_L <- melt(dat.2010.sm_L, id.vars = c("Unit", "Gd.Pt", "Plot.Size"),
                      measure.vars = c("Hw", "Cw", "Sx", "Pl", "Bl", "Ba", "Ep", "At", "Ac"))

# Rename columns
names(dat.2010.sm_L)[names(dat.2010.sm_L) == "variable"] <- "Spp"
names(dat.2010.sm_L)[names(dat.2010.sm_L) == "value"] <- "tally"

dat.2010.sm_L <-subset(dat.2010.sm_L, dat.2010.sm_L$tally > 0)

# Duplicate rows for the number of tallies that it has
dup.times <- dat.2010.sm_L$tally
idx <- rep(1:nrow(dat.2010.sm_L), dup.times)
dat.2010.sm_L <- dat.2010.sm_L[idx,]
dat.2010.sm_L$tally <- NULL

# Add average DBH for this size class and tree class of 1 (all living)
dat.2010.sm_L <- dat.2010.sm_L %>%
  mutate(DBH = 7.55, Tree.Class = 1)







####2018
# Read in the data for large trees (10+cm DBH)
dat.2018 <- read.csv("./data-raw/7. Data Creek 2018 Data large trees.csv")
dat.2019 <- read.csv("./data-raw/8. Data Creek 2019 Data large trees.csv")

# Half the sites were done in 2018 and the other half in 2019, but we are considering them all 2018
dat.2018x <- rbind(dat.2018, dat.2019)

# Rename columns to match other years
names(dat.2018x)[names(dat.2018x) == "Gd Pt"] <- "Gd.Pt"
names(dat.2018x)[names(dat.2018x) == "Plot Size"] <- "Plot.Size"
names(dat.2018x)[names(dat.2018x) == "Tree Class"] <- "Tree.Class"

#Eliminate plots C2 J050 and J300, which could not be found
dat.2018x <- subset(dat.2018x, dat.2018x$Unit != "C2"| dat.2018x$Gd.Pt != "J050")
dat.2018x <- subset(dat.2018x, dat.2018x$Unit != "C2"| dat.2018x$Gd.Pt != "J300")

#check for duplicates (999 duplicate is OK)
uniqueID <- paste(dat.2018x$Unit, dat.2018x$Gd.Pt, dat.2018x$Tree.No, dat.2018x$Spp, dat.2018x$DBH, sep = ".")
uniqueID[duplicated(uniqueID)]

#Eliminate Extra Large trees from expanded plots (Tree no. 999) that have 7.98 m radius
dat.2018x <- dat.2018x[dat.2018x$Tree.No != "999", ]
dat.2018x <- subset(dat.2018x, is.na(dat.2018x$Unit) == FALSE)

# Count how many plots there are for each treatment unit
# so when averaging carbon/unit later, we can take into account the plots that had zero C
dat.2018x$count <- rep(1, length(dat.2018x$Unit))
Trees_in_Plots <- ddply(dat.2018x[c("Unit", "Gd.Pt","Plot.Size", "count")], .(Unit, Gd.Pt,Plot.Size), numcolwise(sum))
Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))
Plot_in_Units <- ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
Plot_in_Units

# Eliminate unwanted columns
dat.2018x <- dat.2018x %>%
  dplyr::select(Unit, Gd.Pt, Plot.Size, Spp, DBH, Tree.Class)

# Subset for LIVE trees only
dat.2018_L <- subset(dat.2018, dat.2018$Tree.Class <= 2)





