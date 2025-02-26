
#' DBH-height allometry choices
#'
#' @param dat
#' @param lrg_trees_2010 where is the 2010 data - this is where tree ids are flagged as planted or not
#' @param planted_hgt Specify the height cutoff (also need to specify species) to use to determine
#' whether the tree is less than a certain diameter and therefore planted
#' (requires specifying sp_plant as well)
#' @param planted_diam Specify the diameter cutoff to use to determine whether the tree
#' is less than a certain diameter and therefore planted
#' @param sp_plant which species was planted and expected to be less than planted_hgt to be planted
#' @param id_gap_trees TRUE = use whether the plot id falls within a known gap to determine whether
#' it was planted or not
#' @param use_standard TRUE = standard dbh-height allometry, FALSE = residual dbh-height allometry
#' @param use_size TRUE = use the DBH of the tree in this measurement year to determine whether it
#' was planted or not
#' @param use_cc  TRUE = use whether the tree was in a clear cut to determine whether it is planted
#' or not
#'
#' @details
#' the use size parameter is a simple way to not overestimate the height of planted spruce,
#' (to differentiate planted vs residual spruce).
#'
#' The planted trees are only 26 years old in 201x and 30 years old in 2022,
#' so they weren’t over 20 m tall yet whereas residual spruce trees mostly were canopy height.
#' Then for the heavy treatments any spruce <20 cm dbh were likely growing in gap cuts and
#' larger trees were likely  residual trees
#'
#'
#' @return
#' @export
#'
#' @examples
height_allom_choices <- function(dat,
                                 lrg_trees_2010 = "./data-raw/Trees/Data Creek 2010 Data large trees.csv",
                                 planted_hgt = 20,
                                 planted_diam = 20,
                                 sp_plant = "Sx",
                                 use_standard = FALSE, #TRUE = standard, FALSE = residual
                                 id_gap_trees = TRUE,
                                 use_size = TRUE,
                                 use_cc = TRUE){

  clean_dat <- copy(dat)
  #should we use standard or residual allometry for non-planted trees
  if(use_standard){
    clean_dat[, Height :=  treeCalcs::height_dbh(Species = Spp,
                                                 DBH = DBH, BECzone = "ICH"),
              by = seq_len(nrow(clean_dat))]
  }else{
    clean_dat[, Height :=  treeCalcs::height_dbh_Residuals(Species = Spp,
                                                           DBH = DBH, BECzone = "ICH"),
              by = seq_len(nrow(clean_dat))]
  }
  # calculate plantation height for every tree and then we'll figure out which to use it for
  clean_dat[, PlantationHeight := treeCalcs::height_dbh_plantations(Species=Spp,
                                                                    DBH=DBH,
                                                                    BECzone = "ICH"),
            by = seq_len(nrow(clean_dat))]
  #clean_dat[, BA :=  pi*(DBH/200)^2] #this is nice to have, but doesn't have to be here
  #setnames(clean_dat, "Gd.Pt", "PlotNum")

  #now figure out which of these heights to use based on a series of questions:


  #### NEED TO DOUBLE CHECK THAT THIS IS CORRECT #####
  clean_dat[, PlantedYN := "N",
            by = seq_len(nrow(clean_dat))]
  #1. use the size of the tree and whether it's in the heavy harvest to assign allometry:
  if(use_size){
    #Erica code
    # dat.201x_all$PlantedYN<-ifelse(dat.201x_all$Height < 20 &
    #                                 dat.201x_all$Spp ==  "Sx", "Y", dat.201x_all$PlantedYN)
    #dat.201x_all$PlantedYN<-ifelse(dat.201x_all$DBH < 20 &
    #                                dat.201x_all$Unit %in%  heavyunits, "Y", dat.201x_all$PlantedYN)
    clean_dat[, PlantedYN := ifelse(Height < planted_hgt & #are they less than a given hgt for a given planted species?
                                      Spp ==  sp_plant, "Y",
                                    PlantedYN)]
    heavyunits <- c("B2", "B3", "C2", "D4")
    clean_dat[, PlantedYN := ifelse(DBH < planted_diam & Unit %in%  heavyunits,
                                    "Y", PlantedYN)] #are they smaller than a given DBH in the heavy unit?
  }

  #2. use whether it's in a clearcut or not:
  if(use_cc){
    clearcuts <- c("A3", "A1", "B4", "D2")

    #use whether the trees are in a clearcut to assign allometry
    clean_dat[, PlantedYN := ifelse(Unit %in% clearcuts, "Y", "N")] # are the trees in a clearcut?

  }

  #3. use whether the trees are in a known gap fixed plot to determine whether planted
  if(id_gap_trees){
    #this is all hard-coded and possibly better ways to do this
    #B3 H050 and J050 are gap plots with fast growing planted spruce
    exclude_B3 <- c("H050", "J050")
    #B2 G150 is gap plots with fast growing planted spruce
    exclude_B2 <- c("G150")
    #also C2 F300 and I450 and E150 and B050
    exclude_C2 <- c("F300", "I450", "E150", "B050")
    #also D4 C350, E300, F350 maybe E200
    exclude_D4 <- c("C350", "E300", "F350", "E200")
    clean_dat[, PlantedYN := ifelse(Unit == "B3" & PlotNum %in% exclude_B3, "Y", PlantedYN)]
    clean_dat[, PlantedYN := ifelse(Unit == "C2" & PlotNum %in% exclude_C2, "Y", PlantedYN)]
    clean_dat[, PlantedYN := ifelse(Unit == "D4" & PlotNum %in% exclude_D4, "Y", PlantedYN)]
    clean_dat[, PlantedYN := ifelse(Unit == "B2" & PlotNum %in% exclude_B2, "Y", PlantedYN)]
  }

  #assign the height based on all these decisions
  clean_dat[, Height := ifelse(PlantedYN == "Y", PlantationHeight, Height)]
  #clean_dat <- clean_dat[,.(Unit, PlotNum, Tree.No, Plot.Size, Spp, DBH,Height, Tree.Class,
   #                           StubYN, PHF)]

  heights <- clean_dat$Height

  return(heights)

}

