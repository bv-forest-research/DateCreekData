
#' Calculate stems per hectare from prism plots
#'
#' @param pBands which prism band used (numeric)
#' @param DBH diameter of tree
#' @return The equivalent stems/ha of a given diameter
#' @export
calculateSPH <- function(pBands, DBH){
  BAPT <- pi*(DBH/200)^2 ##basal area per tree
  SPH <- pBands/BAPT
  return(SPH)
}



#' Create an initial tree list for SORTIE
#'
#' @param diam_sizeClass what size diameter classes to use for SORTIE. Default set to 2.5. If changing to 2, this code
#' will be inaccurate as the lower DBH limit for the prism plots was 7.5.
#' @param dateCreek92_data filename (and pathway) for the 1992 Date Creek prism data
#' @param dateCreek_92_tallies file name (and pathway) for the 1992 Date Creek small tree fixed radius plots data
#' @param liveTrees Do you want live trees (TRUE = default) or snags (FALSE)
#'
#' @description This function creates an output compatible with SORTIE from dat92_sph_diamClass datatable. You can either
#' pass the output data table explicitly, or this function will run the dat92_sph_diamClass function if not already available
#'
#' If you change how the species are spelled in a SORTIE parameter file (i.e. Western_Hemlock become westernHemlock),
#' you would have to change it here - the species spelling is hard coded here and MUST MATCH EXACTLY what is in the
#' associated parameter file
#' @param
#'
#' @return
#' @export
#'
#' @examples
createTreeListSortie <- function(diam_sizeClass =2.5, dateCreek92_data, dateCreek_92_tallies, liveTrees = TRUE){

  SummaryDataTable <- calc_sph_92data(dateCreek92_data = dateCreek92_data,
                                      dateCreek_92_tallies = dateCreek_92_tallies,
                                      liveTrees = liveTrees)

  #keep only needed columns and rename to match 2018 data
  dat92 <- dat92[,.(Unit = BLOCK, PrismBands = MAIN_BAF, Ht = CRUISED_HEIGHT,
                    Spp = SPECIES,DBH, TC = TREE_CLASS,PlotNum)]

  SummaryDataTable[,Spp:=ifelse(Spp=="Hw","Western_Hemlock",
                      ifelse(Spp=="Cw","Western_redcedar",
                             ifelse(Spp=="Ba","Amabalis_Fir",
                                    ifelse(Spp=="Bl","Subalpine_Fir",
                                           ifelse(Spp =="Sx","Hybrid_spruce",
                                                  ifelse(Spp=="Pl","Lodgepole_Pine",
                                                         ifelse(Spp=="At","Trembling_Aspen",
                                                                ifelse(Spp=="Ac","Black_Cottonwood",
                                                                       ifelse(Spp=="Ep","Paper_Birch",Spp)))))))))]

  SummaryDataTable[,Spp:=as.factor(Spp)][,Unit:=as.factor(Unit)]

  if(liveTrees==TRUE){
    DC_dat2 <- data.table::dcast(SummaryDataTable, Unit + DiamClass ~ Spp, value.var = "SPH")
  }else{
    sizeClasses <- 7.5 #can't have snags <5-7.5cm diameter class
    labels92 <- Unit_Plot_Labels
    labels_unit <- unique(labels92[,.(Unit)])
    #add species to every plot
    Spp_labels <- rep(unique(SummaryDataTable$Spp)[!is.na(unique(SummaryDataTable$Spp))], nrow(labels_unit))
    labels_unit <- labels_unit[rep(seq(.N), length(unique(SummaryDataTable$Spp)[!is.na(unique(SummaryDataTable$Spp))]))]
    labels_unit[, Spp:= Spp_labels]

    #add diameter class to every species in every plot
    diam_labels <- rep(sizeClasses, nrow(labels_unit))
    labels_unit <- labels_unit[rep(seq(.N), length(sizeClasses))]
    data.table::setorder(labels_unit, Unit, Spp)
    labels_unit[, DiamClass:= diam_labels]
    labels_unit[, DiamClass := as.factor(DiamClass)]

    TC_labels <- rep(seq(1,5), nrow(labels_unit))
    labels_unit <- labels_unit[rep(seq(.N), length(seq(1,5)))]
    data.table::setorder(labels_unit, Unit, Spp, DiamClass)
    labels_unit[, TC := TC_labels]

    DC_dat <- merge(labels_unit, SummaryDataTable, by =c("Unit","Spp","DiamClass","TC"), all =TRUE)
    DC_dat[is.na(SPH), SPH := 0]

    DC_dat2 <- data.table::dcast(DC_dat, Unit + DiamClass + TC ~ Spp, value.var = "SPH")
  }


  return(DC_dat2)
}




#' summarize Woody debris by unit
#'
#'
#'
