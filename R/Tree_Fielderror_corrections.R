

corr_trees_2010 <- function(dat){

  #correct species calls based on later measurements
  dat$Spp[which(dat$Unit == "B5" &
                       dat$Tree.No == 351)] <- "Cw"
  #below errors are also errors in 2018, caught in 2022
  dat$Spp[which(dat$Unit == "A2" &
                       dat$Tree.No == 625)] <- "Ba"
  dat$Spp[which(dat$Unit == "A2" &
                       dat$Tree.No == 628)] <- "Sx"
  dat$Spp[which(dat$Unit == "A4" &
                       dat$Tree.No == 471)] <-
    "Ep" #conifer to decid mistake - could check in field
  dat$Spp[which(dat$Unit == "B3" &
                       dat$Tree.No == 94)] <- "Cw"
  dat$Spp[which(dat$Unit == "B3" &
                       dat$Tree.No == 139)] <- "At"
  dat$Spp[which(dat$Unit == "B3" &
                       dat$Tree.No == 200)] <- "Sx"
  dat$Spp[which(dat$Unit == "B5" &
                       dat$Tree.No == 124)] <- "Hw"
  dat$Spp[which(dat$Unit == "B5" &
                       dat$Tree.No == 227)] <- "Cw"
  dat$Spp[which(dat$Unit == "B5" &
                       dat$Tree.No == 259)] <- "Cw"
  dat$Spp[which(dat$Unit == "B5" &
                       dat$Tree.No == 342)] <- "Cw"
  dat$Spp[which(dat$Unit == "C2" &
                       dat$Tree.No == 312)] <-
    "At" #conifer to decid mistake - could check in field
  dat$Spp[which(dat$Unit == "C3" &
                       dat$Tree.No == 484)] <- "Sx"
  dat$Spp[which(dat$Unit == "D4" &
                       dat$Tree.No == 648)] <- "Sx"
  dat$Spp[which(dat$Unit == "D4" &
                       dat$Tree.No == 631)] <- "Ba"
  dat$Spp[which(dat$Unit == "D4" &
                       dat$Tree.No == 632)] <- "Ba"
  dat$Spp[which(dat$Unit == "D4" &
                       dat$Tree.No == 606)] <- "Hw"
  dat$Spp[which(dat$Unit == "D4" &
                       dat$Tree.No == 587)] <- "Hw"
  dat$Spp[which(dat$Unit == "D4" &
                       dat$Tree.No == 570)] <- "At"
  dat$Spp[which(dat$Unit == "D5" &
                       dat$Tree.No == 413)] <- "Ba"
  dat$Spp[which(dat$Unit == "D5" &
                       dat$Tree.No == 414)] <- "Hw"
  dat$Spp[which(dat$Unit == "D5" &
                       dat$Tree.No == 488)] <- "Hw"
  dat$Spp[which(dat$Unit == "D5" &
                       dat$Tree.No == 527)] <- "Bl"

  #correct tree class mistake
  dat$Tree.Class[which(dat$Unit == "D3" &
                              dat$Tree.No == 655)] <- "4" #4 m high stub was marked as class 1
  #more corrections... marked dead in 2010  but not dead later on
  dat$Tree.Class[which(dat$Unit == "A2" &
                              dat$Tree.No == 625)] <- "1"
  dat$Tree.Class[which(dat$Unit == "A2" &
                              dat$Tree.No == 628)] <- "1"
  dat$Tree.Class[which(dat$Unit == "C2" &
                              dat$Tree.No == 312)] <- "1"
  dat$Tree.Class[which(dat$Unit == "C3" &
                              dat$Tree.No == 529)] <- "1"
  dat$Tree.Class[which(dat$Unit == "D5" &
                              dat$Tree.No == 486)] <- "2"
  dat$Tree.Class[which(dat$Unit == "A4" &
                              dat$Tree.No == 471)] <- "1"
  dat$Tree.Class[which(dat$Unit == "B1" &
                              dat$Tree.No == 651)] <- "1"
  dat$Tree.Class[which(dat$Unit == "C2" &
                              dat$Tree.No == 265)] <- "1"

  #correct dbh mistake
  dat$DBH[which(dat$Unit == "D5" &
                       dat$Tree.No == 449)] <- 62.8 #82.8 typo for 62.8
  dat$DBH[which(dat$Unit == "B2" &
                       dat$Tree.No == 2)] <-
    106.3 #130.8 must be mistake. giving it same dbh as later years
  dat$DBH[which(dat$Unit == "A4" &
                       dat$Tree.No == 463)] <-
    25#was 37 in 2010, then DF in 2018, then 25 in 2022 with "d est". changing to 25
  dat$DBH[which(dat$Unit == "B5" &
                       dat$Tree.No == 118)] <-
    39 #changing 2010 dbh from 29 to 39 to match 2022 diameter. will trust 2022

  #corrections of typo or data entry mistakes
  #tree 105 and 106 are entered in backwards on dbh - switched in data
  subset(dat, dat$Unit == "B3" & dat$Tree.No == 105)
  subset(dat, dat$Unit == "B3" & dat$Tree.No == 106)
  dat$DBH[which(dat$Unit == "B3" &
                       dat$Tree.No == 105)] <- 39.8
  dat$DBH[which(dat$Unit == "B3" &
                       dat$Tree.No == 106)] <- 13.6

  subset(dat, dat$Unit == "B3" & dat$Tree.No == 125)
  subset(dat, dat$Unit == "B3" & dat$Tree.No == 126)
  dat$DBH[which(dat$Unit == "B3" &
                       dat$Tree.No == 125)] <- 34
  dat$DBH[which(dat$Unit == "B3" &
                       dat$Tree.No == 126)] <- 11.2

  #check species codes
  unique(dat$Spp)
  #change Sw to Sx
  dat$Spp[which(dat$Spp == "Sw")] <- "Sx"
  unique(dat$Spp)
  hist(dat$DBH)
  test.DBH <- subset(dat$DBH, dat$DBH > 0)
  min(test.DBH) #no diameters less than 10 in dataset

  #check for duplicates (999 duplicate is OK)
  uniqueID <-
    paste(dat$Unit,
          dat$Gd.Pt,
          dat$Tree.No,
          dat$Spp,
          dat$DBH,
          sep = ".")
  uniqueID[duplicated(uniqueID)]

  #Eliminate plot B2 I000, which changed location between 2010 (zero trees) and 2018
  dat <-
    subset(dat, dat$Unit != "B2" | dat$Gd.Pt != "I000")
  #Eliminate plot C2 J050, which could not be found
  dat <-
    subset(dat, dat$Unit != "C2" | dat$Gd.Pt != "J050")
  #Eliminate plots B5 A200 which had 4 trees cut down
  dat <-
    subset(dat, dat$Unit != "B5" | dat$Gd.Pt != "A200")
  #Eliminate plot B2 I000, which changed location between 2010 (zero trees) and 2018
  dat <-
    subset(dat, dat$Unit != "B2" | dat$Gd.Pt != "I000")
  #For not don't Eliminate plot B5 A200, which had a tree felled by chainsaw in 2019
  #dat<-subset(dat, dat$Unit != "B5"| dat$Gd.Pt != "A200")
  #Eliminate plot C1 G050, which had a big CW forked below dbh that was not measured correctly in all years
  dat <-
    subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "G050")
  #Eliminate plot C1 K050, which had a tree missed in 2010 and 2018
  dat <-
    subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "K050")
  #Eliminate plot C1 D100, which had a tree missed in 2010 and 2018 and a forked tree measured incorrectly
  dat <-
    subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "D100")
  #Eliminate plot C3 F350, which changed location between 2010 and 2018
  dat <-
    subset(dat, dat$Unit != "C3" | dat$Gd.Pt != "F350")


  #write cleaned 2010 to file
  #write.csv(dat, "FinalDateCkStandStructureScripts/Cleaned 2010 data.csv")

  #check how many plots there are for each treatment unit
  dat$count <- rep(1, length(dat$Unit))
  Trees_in_Plots <-
    ddply(dat[c("Unit", "Gd.Pt", "Plot.Size", "count")], .(Unit, Gd.Pt, Plot.Size), numcolwise(sum))
  Trees_in_Plots$count <- rep(1, length(Trees_in_Plots$Unit))
  Plot_in_Units <-
    ddply(Trees_in_Plots[c("Unit", "count")], .(Unit), numcolwise(sum))
  Plot_in_Units #stands should have 20 or 30 plots (30 for 40% and 70% retention treatments), plots were removed above to match other years
  #no clear-cut large tree plots
  #save plot labels to merge into dataset after summarizing
  labels2010 <- Trees_in_Plots[c("Unit", "Gd.Pt")]

  return(dat)
}


#' Internal function to correct 2018 tree data
#'
#' @param dat
#'
#' @return
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




#' Internal function to correct 2019 tree data
#'
#' @param dat
#'
#' @return
#'
#' @examples
corr_trees_2019 <- function(dat){
  #data corrections based on 2022 remeasurement
  #tree class corrections
  dat.2019$Tree.Class[which(dat.2019$Unit == "B5" & dat.2019$Tree.No ==341)]<-"1"

  #Species corrections
  dat.2019$Spp[which(dat.2019$Unit == "B5" & dat.2019$Tree.No ==351)]<-"Cw" #call correct this year but missed on data entry

  dat.2019$Spp[which(dat.2019$Unit == "C3" & dat.2019$Tree.No ==484)]<-"Sx" #previous two years called this Bl, perhaps check next year

  dat.2019$Spp[which(dat.2019$Unit == "D2" & dat.2019$Tree.No ==369)]<-"Hw"
  dat.2019$Spp[which(dat.2019$Unit == "D2" & dat.2019$Tree.No ==377)]<-"Sx"
  dat.2019$Spp[which(dat.2019$Unit == "D2" & dat.2019$Tree.No ==110)]<-"Ba"

  return(dat.2019)

}

#' Remove trees or plots from 2018/19 data
#'
#' @param dat
#'
#' @return
#' @details
#' eliminates trees or plots from 2018/19 data to match missing plots from other years
#'
#' @examples
remove_plots_trees <- function(dat){

  #Eliminate plots C2 J050 and J300, which could not be found
  dat <-
    subset(dat, dat$Unit != "C2" | dat$Gd.Pt != "J050")
  dat <-
    subset(dat, dat$Unit != "C2" | dat$Gd.Pt != "J300")

  #match elimination for other plots to match 2010 and 201x
  #Eliminate plots B5 A200 which had 4 trees cut down in 2022
  dat <-
    subset(dat, dat$Unit != "B5" | dat$Gd.Pt != "A200")
  #Eliminate plot B2 I000, which changed location between 2010 (zero trees) and 2018
  dat <-
    subset(dat, dat$Unit != "B2" | dat$Gd.Pt != "I000")
  #For not don't Eliminate plot B5 A200, which had a tree felled by chainsaw in 2019
  #dat<-subset(dat, dat$Unit != "B5"| dat$Gd.Pt != "A200")
  #Eliminate plots C1 G050, which had a big CW forked below dbh that was not measured correctly in all years
  dat <-
    subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "G050")
  #Eliminate plot C1 K050, which had a tree missed in 2010 and 2018
  dat <-
    subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "K050")
  #Eliminate plot C1 D100, which had a tree missed in 2010 and 2018 and a forked tree measured incorrectly
  dat <-
    subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "D100")
  #Eliminate plot C3 F350, which changed location between 2010 and 2018
  dat <-
    subset(dat, dat$Unit != "C3" | dat$Gd.Pt != "F350")


  #For now eliminate plot in A3 which did not have tagged trees and 2 crews in 2022 could not make sense of
  dat <-
    subset(dat, dat$Unit != "A3" | dat$Gd.Pt != "F100")
  #For now eliminate plot in A3 which had many messed up diameters
  dat <-
    subset(dat, dat$Unit != "A3" | dat$Gd.Pt != "A300")


  #from 2022 small trees - it should work to just run the above
  ##Eliminate plot C2 J050, which could not be found
  #dat.2022.sm<-subset(dat.2022.sm, dat.2022.sm$Unit != "C2"| dat.2022.sm$Gd.Pt != "J050")
  #Eliminate plots B3 H050, which does not have heights measured yet
  #dat.2022.sm<-subset(dat.2022.sm, dat.2022.sm$Unit != "B3"| dat.2022.sm$Gd.Pt != "H050")
  #Eliminate plots B5 A200 which had 4 trees cut down
  #dat.2022.sm<-subset(dat.2022.sm, dat.2022.sm$Unit != "B5"| dat.2022.sm$Gd.Pt != "A200")
  #Eliminate plot B2 G150, which 2022.sm contractors did not measure all the trees in
  #dat.2022.sm<-subset(dat.2022.sm, dat.2022.sm$Unit != "B2"| dat.2022.sm$Gd.Pt != "G150")
  #For not don't Eliminate plot B5 A200, which had a tree felled by chainsaw in 2019
  #This plot was eliminated above - so shoule be here
  #dat.2022.sm<-subset(dat.2022.sm, dat.2022.sm$Unit != "B5"| dat.2022.sm$Gd.Pt != "A200")


  #check for duplicates (999 duplicate is OK)
  uniqueID <-
    paste(dat$Unit,
          dat$Gd.Pt,
          dat$Tree.No,
          dat$Spp,
          dat$DBH,
          sep = ".")
  uniqueID[duplicated(uniqueID)]

  #Eliminate Extra Large trees from expanded plots (Tree no. 999) that have 7.98 m radius
  #dat <- subset(dat, dat$Tree.No != "999")

  return(dat)
}



