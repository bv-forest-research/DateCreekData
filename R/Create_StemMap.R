rm(list=ls())   #cleans the workspace so all previous objects are deleted
setwd("C:/Users/elilles/Dropbox/Date Ck Timber/Stand Structure Data/Stand Structure Data/Jonathan VanElslander Scripts and Data/FinalDateCkStandStructureScripts")

######load.packages
library(sp)
library(sf)
library(sfheaders)


#################
#Import full tree list#
################

#B1 full treatment unit
setwd("C:/Users/elilles/Dropbox/SORTIEruns/First_NoNat_runs")
NH_B1_SO_Full<-read.csv(file = "NH-B1_NoNat_Full.csv")
str(NH_B1_SO_Full)
NH_B1_SO_Full$X.1<-NULL
plot(NH_B1_SO_Full$X, NH_B1_SO_Full$Y, pch = ".")

setwd("C:/Users/elilles/Dropbox/SORTIEruns/Competitive mortality runs no natural regen")


#######################################
#    extract just 1992 data         ##########
# and only trees > 5 cm dbh (adult trees) ########
#######################################
NH_B1_SO_Full_92<-subset(NH_B1_SO_Full, NH_B1_SO_Full$timestep == 0)
NH_B1_SO_Full_92a<-subset(NH_B1_SO_Full_92, NH_B1_SO_Full_92$Type== "Adult")
NH_B1_SO_Full_92j<-subset(NH_B1_SO_Full_92, NH_B1_SO_Full_92$Type!= "Adult")
plot(NH_B1_SO_Full_92a$X, NH_B1_SO_Full_92a$Y, pch = ".")
length(NH_B1_SO_Full_92a$Species)

#make a polygon around all the trees
B1_poly <- as(NH_B1_SO_Full_92a %>%
  st_as_sf(coords = c("X", "Y")) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_convex_hull(), Class = "Spatial")
plot(B1_poly)

#create coordinates within the polygons with different sampling methods

#test<-spsample(x=B1_poly, n = length(NH_B1_SO_Full_92a$Species), type = "regular")
#head(test)
#str(test)
#length(test$x1)
#plot(test$x1, test$x2, pch = ".", cex = .1)
#plot(test$x1, test$x2, pch = ".", cex = .1, xlim = c(200, 300), ylim = c(200, 300))

#test2<-spsample(x=B1_poly, n = length(NH_B1_SO_Full_92a$Species), type = "hexagonal")
#head(test2)
#str(test2)
#length(test2$x)
#plot(test2$x, test2$y, pch = ".", cex = .1)
#plot(test2$x, test2$y, pch = ".", cex = .1, xlim = c(200, 300), ylim = c(200, 300))

#test3<-spsample(x=B1_poly, n = length(NH_B1_SO_Full_92a$Species), type = "stratified")
#head(test3)
#str(test3)
#length(test3$x1)
#plot(test3$x1, test3$x2, pch = ".", cex = .1)
#plot(test3$x1, test3$x2, pch = ".", cex = .1, xlim = c(200, 300), ylim = c(200, 300))

#non-aligned looks like the best to me with very few trees right next to each other
test4<-spsample(x=B1_poly, n = length(NH_B1_SO_Full_92a$Species), type = "nonaligned")
head(test4)
str(test4)
length(test4$x1)
plot(test4$x1, test4$x2, pch = ".", cex = .1)
plot(test4$x1, test4$x2, pch = ".", cex = .1, xlim = c(200, 300), ylim = c(200, 300))

#swap out the old random coordinates for the new non-aligned coordinates
NH_B1_SO_Full_92a<-NH_B1_SO_Full_92a[1: length(test4$x1),]
NH_B1_SO_Full_92a$X <-test4$x1
NH_B1_SO_Full_92a$Y <-test4$x2
plot(NH_B1_SO_Full_92a$X, NH_B1_SO_Full_92a$Y, pch = ".")
#reconnect juvenile and adult trees
NH_B1_SO_Full_92_New<-rbind(NH_B1_SO_Full_92a, NH_B1_SO_Full_92j)

#create tree map dataframe in the correct format for SORTIE input
NH_B1_SO_Full_92_New$Diam<-ifelse(NH_B1_SO_Full_92_New$Type == "Seedling", NH_B1_SO_Full_92_New$Diam10, NH_B1_SO_Full_92_New$DBH)
NH_B1_SO_TreeMap<- NH_B1_SO_Full_92_New[c("X", "Y", "Species", "Type", "Diam", "Height")]
