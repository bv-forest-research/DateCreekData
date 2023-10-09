#' read and prep harvest boundaries for sortie
#'
#' @param Units_path
#' @param Gaps_path
#'
#' @return
#' @export
#' @description
#' not sure we actually need this function - would need to return 2 objects...
#'
#' @examples
prepSpatialBounds <- function(Units_path, Gaps_path){
  bounds <- readBlockBounds(Units_path = Units_path)
  partCuts <- modifyPartCuts(Gaps_path = Gaps_path)

  # Give a value of 1 to area covered by cuts, unless HR, then 2
  boundsHR <- bounds %>% filter(Unit %in% DateCreekData::Treatments[Treatment == "HR"]$Unit) %>%
    mutate(.,Harvest = 2)
  boundsLC <- bounds %>%
    filter(Unit %in% DateCreekData::Treatments[Treatment == "LR"]$Unit|Unit
           %in% DateCreekData::Treatments[Treatment == "CC"]$Unit) %>%
    mutate(.,Harvest = 1)
  bounds <- rbind(boundsLC, boundsHR)
  partCuts$Harvest <- 1

  return()

}



#' Read the Date Creek spatial unit boundaries
#'
#' @param Units_path
#' @return
#' @export
#'
#' @examples
readBlockBounds <- function(Units_path){

  spatialbounds <- lapply(paste0(Units_path,
                                 DateCreekData::Treatments$Unit,
                                 ".shp"), sf::st_read)
  sb <- list()
  for(ii in 1:length(spatialbounds)){
    sb[[ii]] <- spatialbounds[[ii]] %>%
          sf::st_cast(.,"GEOMETRY")%>%
          dplyr::mutate(Unit = DateCreekData::Treatments$Unit[ii]) %>%
          dplyr::select(Unit,geometry)
    #remove internal polygon from A3 block
    if(DateCreekData::Treatments$Unit[ii] == "A3"){
      sb[[ii]] <- sb[[ii]][1,]
    }

  }

  spatialbounds <- do.call(rbind, sb)
  return(spatialbounds)

}





#' Modify the Date Creek treatment boundaries for heavy and light removals
#'
#' @param Gaps_path The directory with the kmls and kmzs that define the cuts
#' @export
#'
#' @return an sf object
#' @examples
#'
modifyPartCuts <- function(Gaps_path){

  LR<-c("A2", "B5", "C3", "D5") # light removal (30% BasalArea)
  HR<-c("B2", "B3", "C2", "D4") #heavy removal (60% BasalArea)

  ######### Reading in cut boundaries for heavy and light removal that were stored as kmz
  kmz_blocks <- list()
  for(ii in 1:length(c(HR,LR))){
    kmz_blocks[[ii]] <- paste0(paste0(Gaps_path,c(HR,LR)[ii],"_openings.kmz"))
  }
  kmz_list <- list()
  for(ii in 1:length(kmz_blocks)){
    target_file <- '.temp.kml.zip'
    fs::file_copy(kmz_blocks[[ii]], target_file, overwrite = T)
    unzip(target_file, overwrite = T)
    kmz_list[[ii]] <- sf::read_sf('doc.kml')
    fs::file_delete(grep(".xsl",list.files(getwd()), value = TRUE))
    fs::file_delete('.temp.kml.zip')
    fs::file_delete('doc.kml')
    kmz_list[[ii]]$Id <- c(HR,LR)[ii]
  }

  ##### Cleaning heavy removal polygons
  HR_Cuts <- do.call(rbind.data.frame, kmz_list[1:4])
  HR_Cuts <- HR_Cuts %>%
    dplyr::select(Id,geometry) %>%
    sf::st_transform(.,crs=3005)
  #For B2, need to remove row 9 - but might lose a few openings
  HR_Cuts_B2 <- HR_Cuts %>% filter(Id=="B2")
  HR_Cuts_B2 <- HR_Cuts_B2[-2,]

  #For B3, need to remove row 9 - but might lose a few openings
  HR_Cuts_B3 <- HR_Cuts %>% filter(Id=="B3")
  HR_Cuts_B3 <- HR_Cuts_B3[-9,]

  #For C2, need to remove row 16 - but might loss an opening that is attached
  HR_Cuts_C2 <- HR_Cuts %>% filter(Id=="C2")
  HR_Cuts_C2 <- HR_Cuts_C2[-16,]

  #For D4, need to remove row 16 - but might loss an opening that is attached
  HR_Cuts_D4 <- HR_Cuts %>% filter(Id=="D4")
  HR_Cuts_D4 <- HR_Cuts_D4[-c(7,16),]

  #### Light removals gaps
  #we are going to ignore the gap boundaries, but use unit boundaries to harvest BA within
  LR_Cuts <- do.call(rbind.data.frame, kmz_list[5:8])

  HR_Cuts_clean <- rbind(HR_Cuts_B2,HR_Cuts_B3,HR_Cuts_C2,HR_Cuts_D4)

  return(HR_Cuts_clean)

}

#' (helper function) Read keyhole (kml) files into sf objects
#' @param file filename (character)
#'
#' @return an sf object
read_keyhole <- function(file) {
  # get file extension
  ext <- strsplit(basename(file), split = '//.')[[1]][-1]
  # if kml
  if (ext == 'kml') {
    layers <- sf::st_layers(file)$name
    if (length(layers) > 1) {
      return(Reduce('rbind', lapply(layers, sG::read_sf, dsn = file)))
    }
    return(read_sf(file))
  } else {
    target_file <- '.temp.kml.zip'
    fs::file_copy(file, target_file, overwrite = T)
    unzip(target_file, overwrite = T)
    sf_out <- sf::read_sf('doc.kml')
    fs::file_delete(target_file)
    fs::file_delete('doc.kml')
    return(sf_out)
  }
}




#' BROKEN!!!!! write out harvest and planting for xml updates
#'
#' @param Units_path
#' @param bounds
#' @param partCuts
#' @param Unit_i
#' @param PlotSize
#'
#' @return
#' @export
#'
#' @examples
read_write_harvests_xmls <- function(Units_path, bounds, partCuts, Unit_i, PlotSize){
  #Set pathway
  HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")

  if(TreatType=="LR"|TreatType=="CC"){
    PlotRast <- fasterize::fasterize(bounds %>% filter(Unit==Unit_i),
                                     PlotSize, field="Harvest",
                                     background=0)
  }else {
    PlotRast <- fasterize::fasterize(partCuts %>% filter(Id==Unit_i),
                                     PlotSize, field="Harvest",
                                     background=0)
  }


  for(ii in 1:ncell(PlotRast)){
    if (values(PlotRast)[ii] > 0){
      locCol <- colFromCell(PlotRast,ii)
      locRow <- rowFromCell(PlotRast,ii)
      h1 <- paste0("<ha_applyToCell x=\"",locCol-1,"\" y=\"", nrow(PlotRast)-locRow,"\"/>")
      write(h1, file= paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"),append=TRUE)
      p1 <- stringr::str_replace(h1,"ha","pl")
      write(p1, file=paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"),append=TRUE)
    }
  }
  if(TreatType == "HR"){
    bb <- st_bbox(st_buffer(bounds %>% filter(Unit==Unit_i), dist = 10))
    Max_length <- plyr::round_any(max(abs(bb[1]-bb[3]),abs(bb[2]-bb[4])), accuracy=8,f=ceiling)
    PlotSize <- raster(xmn=bb[1], xmx=bb[1]+Max_length, ymn=bb[2],ymx=bb[2]+Max_length,
                       res=8, crs = crs(bounds))

    MatrixHarv <- fasterize::fasterize(bounds %>% filter(Unit == Unit_i),PlotSize, field="Harvest", background=0)
    MatrixHarv <- (MatrixHarv-PlotRast) > 1
    plot(MatrixHarv)
    for(ii in 1:ncell(MatrixHarv)){
      if (values(MatrixHarv)[ii] > 0){
        locCol <- colFromCell(MatrixHarv,ii)
        locRow <- rowFromCell(MatrixHarv,ii)
        h1 <- paste0("<ha_applyToCell x=\"",locCol-1,"\" y=\"", nrow(MatrixHarv)-locRow,"\"/>")
        write(h1, file= paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_Harv.xml"),append=TRUE)
        m1 <- paste0("<pl_applyToCell x=\"",locCol-1,"\" y=\"", nrow(MatrixHarv)-locRow,"\"/>")
        write(m1, file= paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_pl.xml"),append=TRUE)
      }
    }
  }

}





















