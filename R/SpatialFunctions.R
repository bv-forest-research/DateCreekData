
#' Import .kmz
#'
#' @param file filename (character)
#' @return an sf object

read_keyhole <- function(file) {
  # get file extension
  ext <- strsplit(basename(file), split = '//.')[[1]][-1]
  # if kml
  if (ext == 'kml') {
    layers <- st_layers(file)$name
    if (length(layers) > 1) {
      return(Reduce('rbind', lapply(layers, sG::read_sf, dsn = file)))
    }
    return(read_sf(file))
  } else {
    target_file <- '.temp.kml.zip'
    fs::file_copy(file, target_file, overwrite = T)
    unzip(target_file, overwrite = T)
    sf_out <- read_sf('doc.kml')
    fs::file_delete(target_file)
    fs::file_delete('doc.kml')
    return(sf_out)
  }
}


#' Import the Date Creek treatment boundaries
#' @param Units_path The directory with the shapefiles that define the edge of the boundaries
#' @return an sf object with
#'
ReadSpatialBounds <- function(Units_path){
  ########################## 1. Read in spatial harvests #############################
  ## Date Creek Treatments
  NH<-c("A4", "B1", "C1", "D3") #No harvest
  LR<-c("A2", "B5", "C3", "D5") # light removal (30% BasalArea)
  HR<-c("B2", "B3", "C2", "D4") #heavy removal (60% BasalArea)
  CC<-c("A1", "A3", "B4", "D2") #Clear-cut (100% removal) with some caveats (some deciduous left standing, one small

  # Reading in unit boundaries and creating shapefiles group by removal class
  Blocks <- c(NH,LR,HR,CC)
  NH_blocks <- c()
  LR_blocks <- c()
  HR_blocks <- c()
  CC_blocks <- c()

  #Need to split into blocks because heavy removal we need to gap boundaries cleaned
  for(i in 1:length(Blocks)){
    Unit_i <- Blocks[i]
    TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                        ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                               ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
    Unit_sh <- sf::read_sf(paste0(Units_path,Blocks[i],".shp"))
    Unit_sh$Unit <- Blocks[i]
    Unit_sh <- Unit_sh %>%
      dplyr::select(Unit,geometry)
    if(TreatType == "NH"){
      NH_blocks <- rbind(NH_blocks,Unit_sh[1,])
    } else if(TreatType == "LR"){
      LR_blocks <- rbind(LR_blocks,Unit_sh[1,])
    } else if(TreatType == "HR"){
      HR_blocks <- rbind(HR_blocks,Unit_sh[1,])
    } else {
      CC_blocks <- rbind(CC_blocks,Unit_sh[1,])
    }
  }

  spatialbounds <- list(NH_blocks,LR_blocks,HR_blocks,CC_blocks)
  names(spatialbounds) <- c("NH","LR","HR","CC")
  return(spatialbounds)
}

#' Modify the Date Creek treatment boundaries for heavy and light removals
#' @param Gaps_path The directory with the kmls and kmzs that define the cuts
#' @return an sf object
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
}

#'
























