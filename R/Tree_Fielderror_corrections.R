
#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
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
  dat$DBH[which(dat$Unit == "B3" &
                  dat$Tree.No == 80)] <- 12.3 #33.4 must be mistake. giving it same dbh as 2018
  dat$DBH[which(dat$Unit == "C1" &
                  dat$Tree.No == 177)] <- 11.6 #14.3 must be mistake. giving it same dbh as 2018
  dat$DBH[which(dat$Unit == "D3" &
                  dat$Tree.No == 639)] <- 69.9 #59.9 must be mistake. guessing one digit was off
  dat$DBH[which(dat$Unit == "D3" &
                  dat$Tree.No == 570)] <- 25 #33.5 must be mistake. giving it same dbh as 2018 and 2022

  #corrections to forked trees not measured properly, entering the same dbh as 2022 and adding missed trees
  dat$DBH[which(dat$Unit == "C1" & dat$Tree.No == 102)] <- 74
  dat<-rbind(dat, list("C1", "G050", 5.64, "854", "Cw", 68, 1, "missed tree", NA, NA))

  dat<-rbind(dat, list("C1", "K050", 3.99, "855", "Hw", 25, 2, "missed tree", NA, NA))

  dat$DBH[which(dat$Unit == "C1" & dat$Tree.No == 183)] <- 27
  dat<-rbind(dat, list("C1", "D100", 5.64, "851", "Hw", 25, 3, "missed tree", NA, NA))
  dat<-rbind(dat, list("C1", "D100", 5.64, "852", "Cw", 22.6, 2, "missed tree", NA, NA))

  #adding large missed Ac to C2, entering same DBH as 2022
  dat<-rbind(dat, list("C2", "C150", 7.98, "914", "Ac", 67.5, 2, "missed tree", NA, NA))
  #adding missed tree 113 to D4 E100, entering same DBH as 2018
  dat<-rbind(dat, list("D4", "E100", 5.64, "113", "Hw", 21.9, 1, "missed tree", NA, NA))

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
  #B5 F200 tree # 269 and 270 had switched DBHs
  subset(dat, dat$Unit == "B5" & dat$Tree.No == 269)
  subset(dat, dat$Unit == "B5" & dat$Tree.No == 270)
  dat$DBH[which(dat$Unit == "B5" &
                  dat$Tree.No == 269)] <- 12.8
  dat$DBH[which(dat$Unit == "B5" &
                  dat$Tree.No == 270)] <- 16



  #check species codes
  #unique(dat$Spp)
  #change Sw to Sx
  dat$Spp[which(dat$Spp == "Sw")] <- "Sx"
  #unique(dat$Spp)
  #hist(dat$DBH)
  test.DBH <- subset(dat$DBH, dat$DBH > 0)
  #min(test.DBH) #no diameters less than 10 in dataset

  #check for duplicates (999 duplicate is OK)
  uniqueID <-
    paste(dat$Unit,
          dat$Gd.Pt,
          dat$Tree.No,
          dat$Spp,
          dat$DBH,
          sep = ".")
  uniqueID[duplicated(uniqueID)]
  return(dat)


}


#' Remove trees or plots from data
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
#' eliminates trees or plots from data to match missing plots from other years
#'
remove_plots_trees <- function(dat){


  #Eliminate plots C2 J050  which could not be found
  dat <- subset(dat, dat$Unit != "C2" | dat$Gd.Pt != "J050")
  #Keep plot C2 J300 in for 2010 and 2022 when it was found and let it fall out of 2018 data
  #dat <- subset(dat, dat$Unit != "C2" | dat$Gd.Pt != "J300")

  #match elimination for other plots to match 2010 and 201x
  #Eliminate plots B5 A200 which had 4 trees cut down in 2022
  dat <- subset(dat, dat$Unit != "B5" | dat$Gd.Pt != "A200")
  #Eliminate plot B2 I000, which changed location between 2010 (zero trees) and 2018
  dat <- subset(dat, dat$Unit != "B2" | dat$Gd.Pt != "I000")
  # don't Eliminate plot B5 A200, which had a tree felled by chainsaw in 2019
  #dat<-subset(dat, dat$Unit != "B5"| dat$Gd.Pt != "A200")

  #Keep plot C1 G050, which had a big CW forked below dbh that was not measured correctly in all years
  #the trees were corrected in 2010 and 2018 data
  #dat <-subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "G050")
  #keep plot C1 K050, which had a tree missed in 2010 and 2018
  #the trees were corrected in 2010 and 2018 data
  #dat <-subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "K050")
  #Keep plot C1 D100, which had a tree missed in 2010 and 2018 and a forked tree measured incorrectly
  #the trees were corrected in 2010 and 2018 data
  #dat <- subset(dat, dat$Unit != "C1" | dat$Gd.Pt != "D100")
  #Keep plot D4 C250, which had a 4 trees missed in 2018
  #the trees were added in using diameters in between the other years
  #dat <- subset(dat, dat$Unit != "D4" | dat$Gd.Pt != "C250")
  #Eliminate plot C3 F350, which changed location between 2010 and 2018
  dat <- subset(dat, dat$Unit != "C3" | dat$Gd.Pt != "F350")

  # keep plot in F100 in A3 which was reconstructed for 2018 from measurement of the plot at 5.64 radius in spring 2018 and with 2022 data
  #dat <-subset(dat, dat$Unit != "A3" | dat$Gd.Pt != "F100")

  #check for duplicates (999 duplicate is OK)
  uniqueID <-
    paste(dat$Unit,
          dat$Gd.Pt,
          dat$Tree.No,
          dat$Spp,
          dat$DBH,
          sep = ".")
  uniqueID[duplicated(uniqueID)]

  return(dat)
}








#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
corr_trees_2018 <- function(dat){
  dat.2018 <- dat

  #data corrections based on 2022 remeasurement
  #plot radius corrections
  dat.2018$Plot.Size[which(dat.2018$Unit == "A3" &
                             dat.2018$Gd.Pt == "A300")] <- 7.98

  #DBH corrections... for trees more than 20 cm dbh out. many more corrections needed after
  #looking at raw dataforms these are guesses to the correct dbh based on common errors
  #(14 sounds like 40, etc.)

  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 367)] <- 17.9
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 368)] <- 18.7

  #correction based on looking at subplot growth,
  dat.2018$DBH[which(dat.2018$Unit == "D3" &
                       dat.2018$Tree.No == 233)] <-
    76.3 #assuming dbh between 2010 and 2022 measurements


  #tree class corrections
  #giving class 1 to all trees missing a class
  dat.2018$Tree.Class[is.na(dat.2018$Tree.Class)] <-
    "1"
  #had a height in 2022 so couldn't be class 5 for dead fallen in 2018
  dat.2018$Tree.Class[which(dat.2018$Unit == "A2" &
                              dat.2018$Tree.No == 123)] <-
    "1" #checked in field and tree is healthy - class 1
  dat.2018$DBH[which(dat.2018$Unit == "A2" &
                       dat.2018$Tree.No == 123)] <-
    13.7 #this tree also needs a dbh, entering something in between other two years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 251)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D3" &
                       dat.2018$Tree.No == 251)] <-
    34 #this tree also needs a dbh, entering something in between other two years
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 521)] <- "2"

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

  dat.2018$DBH[which(dat.2018$Unit == "C3" &
                       dat.2018$Tree.No == 506)] <-
    17 #entering 2022 diameter
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 561)] <- "4"
  dat.2018$DBH[which(dat.2018$Unit == "D3" &
                       dat.2018$Tree.No == 561)] <-
    22 #entering something in between other years

  #corrections to forked trees not measured properly, entering the same dbh as 2022 and adding missed trees
  dat.2018$DBH[which(dat.2018$Unit == "C1" & dat.2018$Tree.No == 102)] <- 74
  dat.2018<-rbind(dat.2018, list("C1", "G050", 5.64, "854", "Cw", 68, 1, "missed tree"))

  dat.2018<-rbind(dat.2018, list("C1", "K050", 3.99, "855", "Hw", 25, 2, "missed tree"))

  dat.2018$DBH[which(dat.2018$Unit == "C1" & dat.2018$Tree.No == 183)] <- 27
  dat.2018<-rbind(dat.2018, list("C1", "D100", 5.64, "851", "Hw", 25, 3, "missed tree"))
  dat.2018<-rbind(dat.2018, list("C1", "D100", 5.64, "852", "Cw", 22.6, 2, "missed tree"))

  #adding large missed Ac to C2, entering same DBH as 2022
  dat.2018<-rbind(dat.2018, list("C2", "C150", 7.98, "914", "Ac", 67.5, 2, "missed tree"))

  #adding missed trees to D4 C250 so plot doesn't have to be dropped
  subset(dat.2018, dat.2018$Unit == "D4" & dat.2018$Gd.Pt == "C250")
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 633)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 633)] <- 58 #give DBH from partway 2010-2022
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 634)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 634)] <- 34 #give DBH from partway 2010-2022
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 635)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 635)] <- 13 #give DBH from partway 2010-2022
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 638)] <- "1"
  dat.2018$DBH[which(dat.2018$Unit == "D4" &
                       dat.2018$Tree.No == 638)] <- 15 #give DBH from partway 2010-2022

  #adding diameters to missed plot D5 G200 so plot doesn't have to be dropped
  subset(dat.2018, dat.2018$Unit == "D5" & dat.2018$Gd.Pt == "G200")
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 523)] <- 54.7 #give DBH from first data entry 2018 that can't be found now except 4 andd 7 switched
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 524)] <- 52.3 #give DBH from 2022 (DBH from first data entry 2018 that can't be found was bigger than 2022 DBH)
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 525)] <- 21.8 #give DBH from first data entry 2018that can't be found
  dat.2018$DBH[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 526)] <- 52.2 #give DBH from 2022 (DBH from first data entry 2018 that can't be found was bigger than 2022 DBH)

  #recorded as dead in 2010 and 2022, so could not have been live in  2018
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 96)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 100)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 243)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "A2" &
                              dat.2018$Tree.No == 171)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 363)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B1" &
                              dat.2018$Tree.No == 649)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "B3" &
                              dat.2018$Tree.No == 36)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 121)] <- "3"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C2" &
                              dat.2018$Tree.No == 331)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 694)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D3" &
                              dat.2018$Tree.No == 668)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 618)] <- "4"
  dat.2018$Tree.Class[which(dat.2018$Unit == "D4" &
                              dat.2018$Tree.No == 592)] <- "4"

  #more corrections... these were not dead in 2018
  dat.2018$Tree.Class[which(dat.2018$Unit == "B1" &
                              dat.2018$Tree.No == 436)] <- "2"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C1" &
                              dat.2018$Tree.No == 61)] <- "1"
  dat.2018$Tree.Class[which(dat.2018$Unit == "C3" &
                              dat.2018$Tree.No == 515)] <- "2"
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
                       dat.2018$Tree.No == 91)] <- "Pl"
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
  dat.2018$Spp[which(dat.2018$Unit == "A3" &
                       dat.2018$Tree.No == 115)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "A3" &
                       dat.2018$Tree.No == 123)] <- "Hw"
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
                       dat.2018$Tree.No == 312)] <- "At"
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
  dat.2018$Spp[which(dat.2018$Unit == "B3" &
                       dat.2018$Tree.No == 94)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B3" &
                       dat.2018$Tree.No == 200)] <- "Sx"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 227)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No == 259)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "C2" &
                       dat.2018$Tree.No == 312)] <-
    "At" #conifer to decid mistake - could check in field
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
  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 414)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 488)] <- "Hw"
  dat.2018$Spp[which(dat.2018$Unit == "D5" &
                       dat.2018$Tree.No == 527)] <- "Bl"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No ==227)] <- "Cw"
  dat.2018$Spp[which(dat.2018$Unit == "B5" &
                       dat.2018$Tree.No ==259)] <- "Cw"

  return(dat.2018)

}








#' Internal function to correct 2019 tree data
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
corr_trees_2019 <- function(dat){
  #data corrections based on 2022 remeasurement
  #tree class corrections
  dat$Tree.Class[which(dat$Unit == "B5" & dat$Tree.No ==341)]<-"1"

  #Species corrections
  dat$Spp[which(dat$Unit == "B5" & dat$Tree.No ==351)]<-"Cw" #call correct this year but missed on data entry

  dat$Spp[which(dat$Unit == "C3" & dat$Tree.No ==484)]<-"Sx" #previous two years called this Bl, perhaps check next year

  dat$Spp[which(dat$Unit == "D2" & dat$Tree.No ==369)]<-"Hw"
  dat$Spp[which(dat$Unit == "D2" & dat$Tree.No ==377)]<-"Sx"
  dat$Spp[which(dat$Unit == "D2" & dat$Tree.No ==110)]<-"Ba"
  dat$Spp[which(dat$Unit == "A3" & dat$Tree.No == 341)] <- "Ep"

  return(dat)

}


#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
corr_trees_2022 <- function(dat){
  #data corrections based on 2010 and 2018 data

  #dbh corrections
  dat$DBH.22[which(dat$Unit == "C1" & dat$Tree.No ==88)] <- 29.3 #guessing that 39 should have been 29



  return(dat)

}



