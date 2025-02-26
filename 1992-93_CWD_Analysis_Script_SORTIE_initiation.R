#Ingrid's CWD analysis script for Date Creek 1992 and 1993 data
rm(list=ls())   #cleans the workspace so all previous objects are deleted

setwd("//sfp.idir.bcgov/U164/ELILLES$/Date Creek CWD/Ingrid Final CWD Analysis scripts")
#######################################################################################################################
#######################################################################################################################

library(vegan)
library(plyr)
library(reshape2)
library(nlme)
library(reshape)
library(Rmisc)

########################Prepare data into summaries for each treatment unit
##  
# Import Data
##########################
all.CWD<- read.csv(file="Date Creek CWD 1992-3.csv",stringsAsFactors = T) #we will also tell R which variables should be recognized as a factor
str(all.CWD)
all.CWD$Year<-as.factor(all.CWD$Year) #changing year to be a factor
all.CWD$Plot<-as.factor(all.CWD$Plot) #changing plot to be a factor
names(all.CWD)[names(all.CWD) == "Stand"] <- "Unit"
all.CWD$Class <- as.character(all.CWD$Class) #changing decay class to be a character

##########
#########      Data checking
#########

#unique (all.CWD$Spec.) 
#unique (all.CWD$Diam.)
#unique (all.CWD$Unit)
#unique (all.CWD$Plot)
#unique (all.CWD$Ht.)
#unique (all.CWD$Class) #there is a decay class "WI" I'm not sure what it stands for..

min(all.CWD$Diam.) #make sure minimum diameter is 10cm


##########
#########      calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#########

# CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]          

# Where: 	L = length of total transect (horizontal distance (HD) in m)
#                                       HD = SD / Square root of [1 + (% slope / 100)2]
# slope was not measured so assume total transect length to be 90m
# D = diameter of each piece of CWD (cm)
# A = tilt angle from horizontal for each piece (degrees)
# Because tilt angle was not measured in 1992 and 1993, it was assumed to be zero, so that cos (A) = 1 for all pieces in those years.

# Square diameter
all.CWD$D2_cosA<- all.CWD$Diam.^2

# Need to convert individual piece to plot summary for L (length of total transect horizontal distance)
all.CWD_PlotVolume<- ddply(all.CWD[c("Year", "Unit", "Treatment", "Block", "Plot", "D2_cosA")], .(Year, Unit, Treatment, Block, Plot), numcolwise(sum))
str(all.CWD_PlotVolume)

# Volume (m3/ha) calculation
all.CWD_PlotVolume$Volume_Ha<- (pi^2/(8*90)) * all.CWD_PlotVolume$D2_cosA 


# Could play around with data here and look at frequency distributions, etc.
hist(all.CWD_PlotVolume$Volume_Ha)
table(all.CWD_PlotVolume$Year, all.CWD_PlotVolume$Treatment)

# Need to average plots to a treatment unit summary ... ok to use mean for now because there are no zero plots, but this may change with class12 volume
all.CWD_byUnit<- ddply(all.CWD_PlotVolume[c("Year","Unit", "Block", "Treatment",  "Volume_Ha")], .(Year, Unit, Block, Treatment), numcolwise(mean))
str(all.CWD_byUnit)

# Final dataset
all.CWD_byTreatmentUnit<-all.CWD_byUnit
all.CWD_byTreatmentUnit

# subset dataset into 1992 and 1993 datasets
all.CWD_byTreatmentUnit_1992<-subset(all.CWD_byTreatmentUnit, Year == 1992) # 1992 dataset
all.CWD_byTreatmentUnit_1993<-subset(all.CWD_byTreatmentUnit, Year == 1993) # 1993 dataset

############
########## separating CWD volume by species and decay class to parameterize SORTIE starting conditions
###########

all.CWD$SppGroup<-ifelse(all.CWD$Spec.== "CW"|all.CWD$Spec.== "Cw"|all.CWD$Spec.== "c", "cedar",
                  ifelse(all.CWD$Spec.== "Ac"|all.CWD$Spec.== "At"| all.CWD$Spec.== "Ep"| all.CWD$Spec.== "Ukd"| all.CWD$Spec.== "Uka"| all.CWD$Spec.== "ALDER", "decid", "othercon"))
all.CWD$SizeClass<-ifelse(all.CWD$Diam. <20, "Small", "Large")

# Need to convert individual piece to plot summary for L (length of total transect horizontal distance)
all.CWD_PlotVolume.spp<- ddply(all.CWD[c("Year", "Unit", "Treatment", "Block", "Plot", "Class", "SppGroup", "SizeClass", "D2_cosA")], .(Year, Unit, Treatment, Block, Plot, Class, SppGroup, SizeClass), numcolwise(sum))
str(all.CWD_PlotVolume.spp)

# Volume (m3/ha) calculation
all.CWD_PlotVolume.spp$Volume_Ha<- (pi^2/(8*90)) * all.CWD_PlotVolume.spp$D2_cosA 

#plots with species covered... not a whole set so in next step, divide plots sum by 10
table(all.CWD_PlotVolume.spp$SppGroup, all.CWD_PlotVolume.spp$Unit)

# Need to average plots to a treatment unit summary ... need to calculates sum and divide by 10 plots per TU
all.CWD_byUnit.spp<- ddply(all.CWD_PlotVolume.spp[c("Year", "Unit",  "Block", "Treatment", "Class", "SppGroup", "SizeClass", "Volume_Ha")], .(Year, Unit, Block, Treatment, Class, SppGroup, SizeClass), numcolwise(sum))
all.CWD_byUnit.spp$Volume_Ha<-all.CWD_byUnit.spp$Volume_Ha/10


#check that species groups add up to volume totals... yes correct
test<-ddply(all.CWD_byUnit.spp[c("Year", "Unit",  "Block", "Treatment",  "Volume_Ha")], .(Year, Unit, Block, Treatment), numcolwise(sum))
plot(test$Volume_Ha, all.CWD_byUnit$Volume_Ha)

all.CWD_byUnit.spp.1992<-subset(all.CWD_byUnit.spp, all.CWD_byUnit.spp$Year == "1992")
all.CWD_byUnit.spp.1993<-subset(all.CWD_byUnit.spp, all.CWD_byUnit.spp$Year == "1993")
xyplot(data=all.CWD_byUnit.spp.1992, Volume_Ha~ Treatment| SppGroup)
xyplot(data=all.CWD_byUnit.spp.1993, Volume_Ha~ Treatment| SppGroup)

#################################################################
#Proportion in each species group pre-harvest in each decay class
#################################################################

#othercon
othercon_small<-subset(all.CWD_byUnit.spp.1992, all.CWD_byUnit.spp.1992$SppGroup == "othercon" & all.CWD_byUnit.spp.1992$SizeClass == "Small")
wide.othercon_small<-reshape(othercon_small, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.othercon_small<-merge(wide.othercon_small, all.CWD_byTreatmentUnit_1992[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.othercon_small[is.na(wide.othercon_small)]<-0
wide.othercon_small$pro_DClass1<-wide.othercon_small$Volume_Ha.1/wide.othercon_small$Volume_Ha
wide.othercon_small$pro_DClass2<-wide.othercon_small$Volume_Ha.2/wide.othercon_small$Volume_Ha
wide.othercon_small$pro_DClass3<-wide.othercon_small$Volume_Ha.3/wide.othercon_small$Volume_Ha
wide.othercon_small$pro_DClass4<-wide.othercon_small$Volume_Ha.4/wide.othercon_small$Volume_Ha
wide.othercon_small

othercon_large<-subset(all.CWD_byUnit.spp.1992, all.CWD_byUnit.spp.1992$SppGroup == "othercon" & all.CWD_byUnit.spp.1992$SizeClass == "Large")
wide.othercon_large<-reshape(othercon_large, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.othercon_large<-merge(wide.othercon_large, all.CWD_byTreatmentUnit_1992[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.othercon_large[is.na(wide.othercon_large)]<-0
wide.othercon_large$pro_DClass1<-wide.othercon_large$Volume_Ha.1/wide.othercon_large$Volume_Ha
wide.othercon_large$pro_DClass2<-wide.othercon_large$Volume_Ha.2/wide.othercon_large$Volume_Ha
wide.othercon_large$pro_DClass3<-wide.othercon_large$Volume_Ha.3/wide.othercon_large$Volume_Ha
wide.othercon_large$pro_DClass4<-wide.othercon_large$Volume_Ha.4/wide.othercon_large$Volume_Ha
wide.othercon_large


#decid
decid_small<-subset(all.CWD_byUnit.spp.1992, all.CWD_byUnit.spp.1992$SppGroup == "decid"& all.CWD_byUnit.spp.1992$SizeClass == "Small")
wide.decid_small<-reshape(decid_small, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup", "SizeClass"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.decid_small<-merge(wide.decid_small, all.CWD_byTreatmentUnit_1992[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.decid_small[is.na(wide.decid_small)]<-0
wide.decid_small$pro_DClass1<-wide.decid_small$Volume_Ha.1/wide.decid_small$Volume_Ha
wide.decid_small$pro_DClass2<-wide.decid_small$Volume_Ha.2/wide.decid_small$Volume_Ha
wide.decid_small$pro_DClass3<-wide.decid_small$Volume_Ha.3/wide.decid_small$Volume_Ha
wide.decid_small$pro_DClass4<-wide.decid_small$Volume_Ha.4/wide.decid_small$Volume_Ha
wide.decid_small

decid_large<-subset(all.CWD_byUnit.spp.1992, all.CWD_byUnit.spp.1992$SppGroup == "decid"& all.CWD_byUnit.spp.1992$SizeClass == "Large")
wide.decid_large<-reshape(decid_large, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup", "SizeClass"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.decid_large<-merge(wide.decid_large, all.CWD_byTreatmentUnit_1992[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.decid_large[is.na(wide.decid_large)]<-0
wide.decid_large$pro_DClass1<-wide.decid_large$Volume_Ha.1/wide.decid_large$Volume_Ha
wide.decid_large$pro_DClass2<-wide.decid_large$Volume_Ha.2/wide.decid_large$Volume_Ha
wide.decid_large$pro_DClass3<-wide.decid_large$Volume_Ha.3/wide.decid_large$Volume_Ha
wide.decid_large$pro_DClass4<-wide.decid_large$Volume_Ha.4/wide.decid_large$Volume_Ha
wide.decid_large


#cedar
cedar_small<-subset(all.CWD_byUnit.spp.1992, all.CWD_byUnit.spp.1992$SppGroup == "cedar" & all.CWD_byUnit.spp.1992$SizeClass == "Small")
wide.cedar_small<-reshape(cedar_small, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.cedar_small<-merge(wide.cedar_small, all.CWD_byTreatmentUnit_1992[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.cedar_small[is.na(wide.cedar_small)]<-0
wide.cedar_small$pro_DClass1<-wide.cedar_small$Volume_Ha.1/wide.cedar_small$Volume_Ha
wide.cedar_small$pro_DClass2<-wide.cedar_small$Volume_Ha.2/wide.cedar_small$Volume_Ha
wide.cedar_small$pro_DClass3<-wide.cedar_small$Volume_Ha.3/wide.cedar_small$Volume_Ha
wide.cedar_small$pro_DClass4<-wide.cedar_small$Volume_Ha.4/wide.cedar_small$Volume_Ha
wide.cedar_small

cedar_large<-subset(all.CWD_byUnit.spp.1992, all.CWD_byUnit.spp.1992$SppGroup == "cedar" & all.CWD_byUnit.spp.1992$SizeClass == "Large")
wide.cedar_large<-reshape(cedar_large, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.cedar_large<-merge(wide.cedar_large, all.CWD_byTreatmentUnit_1992[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.cedar_large[is.na(wide.cedar_large)]<-0
wide.cedar_large$pro_DClass1<-wide.cedar_large$Volume_Ha.1/wide.cedar_large$Volume_Ha
wide.cedar_large$pro_DClass2<-wide.cedar_large$Volume_Ha.2/wide.cedar_large$Volume_Ha
wide.cedar_large$pro_DClass3<-wide.cedar_large$Volume_Ha.3/wide.cedar_large$Volume_Ha
wide.cedar_large$pro_DClass4<-wide.cedar_large$Volume_Ha.4/wide.cedar_large$Volume_Ha
wide.cedar_large


#bind treatment unit values together and write to file for input into SORTIE initiation with detailed substrate
initial_substrate<-rbind(wide.othercon_small, wide.othercon_large, wide.decid_small, wide.decid_large, wide.cedar_small, wide.cedar_large)
table(initial_substrate$Unit, initial_substrate$Treatment) #not all size and species groups present in all units

#check that proportions add to 1 for every treatment unit
initial_substrate$pro_sppXgroup<-initial_substrate$pro_DClass1+initial_substrate$pro_DClass2+initial_substrate$pro_DClass3+initial_substrate$pro_DClass4
ddply(initial_substrate[c( "Unit", "pro_sppXgroup")], .( Unit), numcolwise(sum))

write.csv(initial_substrate, "Pre-harvest substrate proportions for SORTIE.csv", row.names = FALSE)

#just initial substrate in mature for generic SORTIE parameter file for ICH
initial_substrate_mature<-subset(initial_substrate, initial_substrate$Block != 4)

ddply(initial_substrate_mature[c("Year", "SppGroup", "SizeClass", "pro_DClass1", "pro_DClass2",  "pro_DClass3",  "pro_DClass4")], .(Year, SppGroup, SizeClass), numcolwise(mean))

#################################################################
#Proportion in each species group pre-harvest in each decay class
#################################################################

#othercon
othercon_small<-subset(all.CWD_byUnit.spp.1993, all.CWD_byUnit.spp.1993$SppGroup == "othercon" & all.CWD_byUnit.spp.1993$SizeClass == "Small")
wide.othercon_small<-reshape(othercon_small, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.othercon_small<-merge(wide.othercon_small, all.CWD_byTreatmentUnit_1993[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.othercon_small[is.na(wide.othercon_small)]<-0
wide.othercon_small$pro_DClass1<-wide.othercon_small$Volume_Ha.1/wide.othercon_small$Volume_Ha
wide.othercon_small$pro_DClass2<-wide.othercon_small$Volume_Ha.2/wide.othercon_small$Volume_Ha
wide.othercon_small$pro_DClass3<-wide.othercon_small$Volume_Ha.3/wide.othercon_small$Volume_Ha
wide.othercon_small$pro_DClass4<-wide.othercon_small$Volume_Ha.4/wide.othercon_small$Volume_Ha
wide.othercon_small

othercon_large<-subset(all.CWD_byUnit.spp.1993, all.CWD_byUnit.spp.1993$SppGroup == "othercon" & all.CWD_byUnit.spp.1993$SizeClass == "Large")
wide.othercon_large<-reshape(othercon_large, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.othercon_large<-merge(wide.othercon_large, all.CWD_byTreatmentUnit_1993[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.othercon_large[is.na(wide.othercon_large)]<-0
wide.othercon_large$pro_DClass1<-wide.othercon_large$Volume_Ha.1/wide.othercon_large$Volume_Ha
wide.othercon_large$pro_DClass2<-wide.othercon_large$Volume_Ha.2/wide.othercon_large$Volume_Ha
wide.othercon_large$pro_DClass3<-wide.othercon_large$Volume_Ha.3/wide.othercon_large$Volume_Ha
wide.othercon_large$pro_DClass4<-wide.othercon_large$Volume_Ha.4/wide.othercon_large$Volume_Ha
wide.othercon_large


#decid
decid_small<-subset(all.CWD_byUnit.spp.1993, all.CWD_byUnit.spp.1993$SppGroup == "decid"& all.CWD_byUnit.spp.1993$SizeClass == "Small")
wide.decid_small<-reshape(decid_small, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup", "SizeClass"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.decid_small<-merge(wide.decid_small, all.CWD_byTreatmentUnit_1993[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.decid_small[is.na(wide.decid_small)]<-0
wide.decid_small$pro_DClass1<-wide.decid_small$Volume_Ha.1/wide.decid_small$Volume_Ha
wide.decid_small$pro_DClass2<-wide.decid_small$Volume_Ha.2/wide.decid_small$Volume_Ha
wide.decid_small$pro_DClass3<-wide.decid_small$Volume_Ha.3/wide.decid_small$Volume_Ha
wide.decid_small$pro_DClass4<-wide.decid_small$Volume_Ha.4/wide.decid_small$Volume_Ha
wide.decid_small

decid_large<-subset(all.CWD_byUnit.spp.1993, all.CWD_byUnit.spp.1993$SppGroup == "decid"& all.CWD_byUnit.spp.1993$SizeClass == "Large")
wide.decid_large<-reshape(decid_large, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup", "SizeClass"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.decid_large<-merge(wide.decid_large, all.CWD_byTreatmentUnit_1993[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.decid_large[is.na(wide.decid_large)]<-0
wide.decid_large$pro_DClass1<-wide.decid_large$Volume_Ha.1/wide.decid_large$Volume_Ha
wide.decid_large$pro_DClass2<-wide.decid_large$Volume_Ha.2/wide.decid_large$Volume_Ha
wide.decid_large$pro_DClass3<-wide.decid_large$Volume_Ha.3/wide.decid_large$Volume_Ha
wide.decid_large$pro_DClass4<-wide.decid_large$Volume_Ha.4/wide.decid_large$Volume_Ha
wide.decid_large


#cedar
cedar_small<-subset(all.CWD_byUnit.spp.1993, all.CWD_byUnit.spp.1993$SppGroup == "cedar" & all.CWD_byUnit.spp.1993$SizeClass == "Small")
wide.cedar_small<-reshape(cedar_small, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.cedar_small<-merge(wide.cedar_small, all.CWD_byTreatmentUnit_1993[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.cedar_small[is.na(wide.cedar_small)]<-0
wide.cedar_small$pro_DClass1<-wide.cedar_small$Volume_Ha.1/wide.cedar_small$Volume_Ha
wide.cedar_small$pro_DClass2<-wide.cedar_small$Volume_Ha.2/wide.cedar_small$Volume_Ha
wide.cedar_small$pro_DClass3<-wide.cedar_small$Volume_Ha.3/wide.cedar_small$Volume_Ha
wide.cedar_small$pro_DClass4<-wide.cedar_small$Volume_Ha.4/wide.cedar_small$Volume_Ha
wide.cedar_small

cedar_large<-subset(all.CWD_byUnit.spp.1993, all.CWD_byUnit.spp.1993$SppGroup == "cedar" & all.CWD_byUnit.spp.1993$SizeClass == "Large")
wide.cedar_large<-reshape(cedar_large, idvar = c("Treatment", "Unit", "Block", "Year", "SppGroup"), v.names = "Volume_Ha", timevar = c("Class"), direction="wide")
wide.cedar_large<-merge(wide.cedar_large, all.CWD_byTreatmentUnit_1993[c("Unit", "Volume_Ha")], by = c("Unit"))
wide.cedar_large[is.na(wide.cedar_large)]<-0
wide.cedar_large$pro_DClass1<-wide.cedar_large$Volume_Ha.1/wide.cedar_large$Volume_Ha
wide.cedar_large$pro_DClass2<-wide.cedar_large$Volume_Ha.2/wide.cedar_large$Volume_Ha
wide.cedar_large$pro_DClass3<-wide.cedar_large$Volume_Ha.3/wide.cedar_large$Volume_Ha
wide.cedar_large$pro_DClass4<-wide.cedar_large$Volume_Ha.4/wide.cedar_large$Volume_Ha
wide.cedar_large


#bind treatment unit values together and write to file for input into SORTIE initiation with detailed substrate
initial_substrate<-rbind(wide.othercon_small, wide.othercon_large, wide.decid_small, wide.decid_large, wide.cedar_small, wide.cedar_large)
table(initial_substrate$Unit, initial_substrate$Treatment) #not all size and species groups present in all units

#check that proportions add to 1 for every treatment unit
initial_substrate$pro_sppXgroup<-initial_substrate$pro_DClass1+initial_substrate$pro_DClass2+initial_substrate$pro_DClass3+initial_substrate$pro_DClass4
ddply(initial_substrate[c( "Unit", "pro_sppXgroup")], .( Unit), numcolwise(sum))

write.csv(initial_substrate, "Post-harvest substrate proportions for SORTIE.csv", row.names = FALSE)

#just initial substrate in mature for generic SORTIE parameter file for ICH
initial_substrate_mature<-subset(initial_substrate, initial_substrate$Block != 4)
initial_substrate_mature$Treatment_group <-ifelse (initial_substrate_mature$Treatment == 1, "CC", 
                                                   ifelse(initial_substrate_mature$Treatment == 4, "NH", "PH"))
ddply(initial_substrate_mature[c("Treatment_group", "SppGroup", "SizeClass", "pro_DClass1", "pro_DClass2",  "pro_DClass3",  "pro_DClass4")], .(Treatment_group, SppGroup, SizeClass), numcolwise(mean))


             ##############################################
#################### mean diameters for clearcuts and partial cuts  ################
               ###########################################
head(all.CWD)
all.CWD.1992.small<-subset(all.CWD, all.CWD$Year == "1992" & all.CWD$Diam. <20)
all.CWD.1992.large<-subset(all.CWD, all.CWD$Year == "1992"& all.CWD$Diam. >=20)
#simple mean of diameters, not weighted by volume or frequency to intiate SORTIE
summarySE(all.CWD.1992.small, measurevar="Diam.", groupvars=c("Block"))
summarySE(all.CWD.1992.large, measurevar="Diam.", groupvars=c("Block"))

all.CWD.1993.small<-subset(all.CWD, all.CWD$Year == "1993" & all.CWD$Diam. <20)
all.CWD.1993.large<-subset(all.CWD, all.CWD$Year == "1993"& all.CWD$Diam. >=20)
#simple mean of diameters, not weighted by volume or frequency to intiate SORTIE
summarySE(all.CWD.1993.small, measurevar="Diam.", groupvars=c("Treatment"))
summarySE(all.CWD.1993.large, measurevar="Diam.", groupvars=c("Treatment"))


             ##############################################
#################### mean diameters for all units to intialize ################
               ###########################################
pre_small<-summarySE(all.CWD.1992.small, measurevar="Diam.", groupvars=c("Unit"))
pre_large<-summarySE(all.CWD.1992.large, measurevar="Diam.", groupvars=c("Unit"))
post_small<-summarySE(all.CWD.1993.small, measurevar="Diam.", groupvars=c("Unit"))
post_large<-summarySE(all.CWD.1993.large, measurevar="Diam.", groupvars=c("Unit"))

pre_small$variable <-rep("pre_small", length(pre_small$Unit))
pre_large$variable <-rep("pre_large", length(pre_small$Unit))
post_small$variable <-rep("post_small", length(pre_small$Unit))
post_large$variable <-rep("post_large", length(pre_small$Unit))
mean_diams<-rbind(pre_small, pre_large, post_small, post_large)

write.csv(mean_diams, "DateCreek mean diameters SORTIE substrate.csv", row.names = FALSE)
