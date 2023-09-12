#' @import sf


#'
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



#' Read the Date Creek spatial unit boundaries
#'
#' @param Units_path
#' @return
#' @export
#'
#' @examples
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
  spatialbounds <- do.call(rbind, spatialbounds)
  return(spatialbounds)

}



#' Modify the Date Creek treatment boundaries for heavy and light removals
#'
#' @param Gaps_path The directory with the kmls and kmzs that define the cuts
#' @export
#' @importFrom dplyr %>%
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



#'subplot_outputs
#'
#'
#' @export
#'
#'
#' @param out_path
#' @param run_name
#' @param Units_path
#' @param yrs
#' @param Units_to_output a vector of character names for which units to include for subplotting outputs
#' @param dist_edge [numeric()] how far from unit boundary to allow subplots (in m)
#' @param num_subplots [numeric()] how many subplots
#' @param size_subplot [numeric()] radius of plot (standard is 7.98m)
#' @param plotting TRUE/FALSE - whether or not to display plots with the unit and subplot location
#'
#' @import data.table
#' @import sf
#' @import dplyr
#' @importFrom raster crs
#'
#'
subplot_outputs <- function(out_path, run_name, Units_path, yrs, subplot_type = multiple , Units_to_output = "all",
                            dist_edge = 20, num_subplots = 30, size_subplot = 7.98, plotting = TRUE){

  # Reading in unit boundaries and creating shapefiles group by removal class

  #to add: run through the output names and see which years are missing
  #to add: instead of passing years as a parameter, get it from the file names (would solve the above)

  spatialBlocks <- ReadSpatialBounds(Units_path)

  if(any(Units_to_output == "all")){
    Blocks <- spatialBlocks$Unit

  }else{
    Blocks <- Units_to_output
  }

  for(ij in 1:length(Blocks)){
    dt_table <- data.table()
    #restrict trees to within the treatment
    Unit_i <- Blocks[ij]
    TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                        ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                               ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
    #Forest size
    if(TreatType=="NH"){
      NameEnd <- paste0(run_name,"_nh_det_")
    }else{
      NameEnd <- paste0(run_name,"_det_")
    }

    #create sample points here so they are the same for every year
    bb <- st_bbox(st_buffer(spatialBlocks %>% filter(Unit==Unit_i), dist = 10))
    Unit_b <- spatialBlocks %>%
                dplyr::filter(Unit==Unit_i)


    if(num_subplots == 1){
      size_subplot <- 56.41897 #1ha central plot
      xp <-unname(bb$xmin + (bb$xmax - bb$xmin)/2)
      yp <- unname(bb$ymin +(bb$ymax - bb$ymin)/2)
      sampPtsSF <- sf::st_as_sf(data.table(pt=1,x = xp, y = yp), coords = c("x","y"),
                                crs = crs(Unit_b))
      sampPtsSFP <- st_buffer(sampPtsSF, dist=size_subplot)

    }else{
      Unit_b_edge <- st_buffer(Unit_b, dist=-dist_edge) #20m from the edge
      sampPts <- sp::spsample(as_Spatial(Unit_b_edge),n=num_subplots,type="regular")
      sampPtsSF <- st_as_sf(sampPts)
      sampPtsSFP <- st_buffer(sampPtsSF, dist=size_subplot)
    }

    if(plotting == TRUE){
      plot(Unit_b$geometry)
      #plot(Unit_b_edge$geometry, add=TRUE)
      #plot(sampPts,add=TRUE)
      plot(sampPtsSF$geometry,add=TRUE)
      plot(sampPtsSFP$geometry,add=TRUE)
    }

    #add subplot label
    sampPtsDT <- as.data.table(sampPtsSFP)
    sampPtsDT[,SubPlot:=seq(1:nrow(sampPtsSF))]
    sampPts <- st_as_sf(sampPtsDT)

    for(i in 1:length(yrs)){

      file_to_read <- paste0(out_path,"ext_ICH-",TreatType,"-",Unit_i,NameEnd,yrs[i])

      if(file.exists(file_to_read)){
        dt <- fread(file_to_read,sep="\t", header=T,na.strings = "--", skip=1)
        dt[, ':='(timestep = yrs[i],Treatment = TreatType, Unit=Unit_i)]
        dt[, ':='(x_utm =bb[1]+X ,y_utm=bb[2]+Y)] #put the SORTIE outputs in the coordinates of sf
        TreeXY <- st_as_sf(dt, coords = c("x_utm","y_utm"), crs=crs(spatialBlocks))
        plot_trees <- st_intersection(TreeXY,sampPts) #this is the slow part
        print(paste(TreatType,Unit_i,"year",yrs[i],"sampled"))

        if(nrow(plot_trees)==0){ #if no trees (i.e in a clearcut before a plant)
          plot_trees <- as.data.table(plot_trees)
          plot_trees <- rbind(data.table(Species="NA"),plot_trees,fill=TRUE)
          plot_trees[, ':='(timestep = yrs[i],Treatment = TreatType, Unit=Unit_i)]
          plot_trees[,geometry:=NULL]
          print(paste(TreatType,Unit_i,"year",yrs[i],"No trees"))
        }else{
          plot_trees <- as.data.table(plot_trees)
          subPl_area <- length(unique(plot_trees$SubPlot))*(pi*size_subplot^2)
          print(paste(TreatType,Unit_i,"year",yrs[i],"Subplot BA =",
                      round(sum(plot_trees[!is.na(DBH),pi*(DBH/2)^2])/subPl_area,0)))

          plot_trees[,geometry:=NULL]
        }
        dt_table <- rbind(dt_table,plot_trees)

      }else{
        print(paste(file_to_read,"does not exist"))
        dt_table <- dt_table
      }

    }
    write.csv(dt_table,
              paste0(out_path,TreatType,"-",Unit_i,run_name,".csv"), row.names = FALSE)
  }
}





#' Sample the Date Creek Grid outputs from SORTIE
#'
#' @param Blocks treatment blocks
#' @param Units_path where are the unit boundaries
#' @param Gaps_path where are the gap boundaries
#' @param grid_to_output name(s) of grid(s) to output
#' @param NoCells_ToSample defaul = NA (no sampling), otherwise numeric value
#' @param output output type: "raster" or "table"
#' @param grid_dat data.table object containing parsed grid outputs
#'
#' @return
#' @export
#' @import terra
#'
#' @examples
maskGrids <- function(Blocks = DateCreekData::Treatments$Unit,
                      Units_path = Units_path, Gaps_path = Gaps_path,
                      NoCells_ToSample = NA,
                      grid_dat, output = "table",
                      grid_to_output){


  UnitBounds <- ReadSpatialBounds(Units_path = Units_path)
  HR_gaps <- modifyPartCuts(Gaps_path = Gaps_path)


  #get the rasters, and put them in the right crs
  for(i in 1:length(Blocks)){
    UnitBounds_l <- UnitBounds %>% filter(Unit== Blocks[i])
    bb <- st_bbox(UnitBounds_l)
    grid_dat[Unit==Blocks[i],
             ':='(x_utm = bb[1]+(x*8), y_utm = bb[2]+(y*8))]
  }
  grid_dat[,':='(x = NULL, y = NULL)]
  setnames(grid_dat, c("x_utm","y_utm"), c("x","y"))

  rast_l <- list()
  rast_table <- list()
  for(j in 1:length(grid_to_output)){
    rast_t_dt <- c()

    rast_g <- grid_dat[colnames == grid_to_output[j]] %>%
      arrange(.$timestep) %>%
      split(.$Unit) %>%
      lapply(., function(x) {x <- x %>% tidyr::pivot_wider(names_from = timestep, values_from = values)})%>%
      lapply(., function(x) {x <- x %>% dplyr::select(-c("Unit","point_id","mapnm","colnames"))}) %>%
      purrr::map(.,rast)

    for(ii in 1: length(Blocks)){
      #mask by unit
      UnitBounds_l <- UnitBounds %>% filter(Unit== Blocks[ii])
      rasts_i <- rast_g[[Blocks[ii]]]
      crs(rasts_i) <- crs(UnitBounds_l)
      #mask by the unit boundaries - all units
      rast_unit <- mask(rasts_i, UnitBounds_l)

      if(output == "raster"){
        rast_l[[ii]] <- rast_unit
      }

      if(output == "table"){
        #make a raster for gaps and a raster for matrix for HR
        if(Blocks[ii] %in% c("B2", "B3", "C2", "D4")){
          gap_mask <- HR_gaps %>% filter(Id==Blocks[[ii]])
          gaps_rast <- mask(rast_unit, gap_mask) #matrix = NA
          matrix_rast <- mask(rast_unit, gap_mask, inverse=TRUE) #gaps = NA
        }

        #do you want to sample the unit or export all non-na values?
        if(!is.na(NoCells_ToSample)){

          if(Blocks[ii] %in% c("B2", "B3", "C2", "D4")){
            #the point locations stay the same for all the rasters in the list
            gap_pts <- spatSample(gaps_rast, size = NoCells_ToSample, na.rm=TRUE,as.points=TRUE)
            gap_table <- as.data.table(gap_pts)
            gap_table[, ':=' (Unit = Blocks[ii], OpenType = "Gap")]

            matrix_pts <- spatSample(matrix_rast, size = NoCells_ToSample, na.rm=TRUE,as.points=TRUE)
            matrix_table <- as.data.table(matrix_pts)
            matrix_table[, ':=' (Unit = Blocks[ii], OpenType = "Matrix")]

            rast_t <- rbind(gap_table,matrix_table)
            rast_ti <- melt(rast_t,id.vars = c("Unit","OpenType"),
                            variable.name = "timestep",
                            value.name = grid_to_output[j])
          }else{
            #sample anywhere in the unit
            unit_pts <- spatSample(rast_unit, size = NoCells_ToSample, na.rm=TRUE,
                                   as.points=TRUE, method = "random")

            #store the samples in a table
            rast_t <- as.data.table(unit_pts)
            rast_t[, ':=' (Unit =Blocks[ii], OpenType = "Unit")]

            rast_ti <- melt(samp_table_u,id.vars = c("Unit","OpenType"),
                            variable.name = "timestep",
                            value.name = grid_to_output[j])
          }
        }else{
          if(Blocks[ii] %in% c("B2", "B3", "C2", "D4")){
            #the point locations stay the same for all the rasters in the list
            gap_table <- as.data.table(gaps_rast)
            gap_table[, ':=' (Unit = Blocks[ii], OpenType = "Gap")]

            matrix_table <- as.data.table(matrix_rast)
            matrix_table[, ':=' (Unit = Blocks[ii], OpenType = "Matrix")]

            rast_t <- rbind(gap_table,matrix_table)
            rast_ti <- melt(rast_t,id.vars = c("Unit","OpenType"),
                            variable.name = "timestep",
                            value.name = grid_to_output[j])

          }else{
            #create data.table with just cells that are not NA
            rast_t <- as.data.table(rast_unit)
            rast_ti <- melt(rast_t, measure.vars = colnames(rast_t),
                            value.name = grid_to_output[j],
                            variable.name = "timestep")
            rast_ti[,`:=`(Unit = Blocks[[ii]], OpenType = "Unit")]
          }



        }
        rast_t_dt <- rbind(rast_t_dt, rast_ti)
      }

    }
    if(output == "raster"){
      names(rast_l) <- Blocks

    }else{
      rast_t_dt_m <- melt(rast_t_dt,
                          measure.vars = grid_to_output[j],
                          variable.name = "grid",
                          value.name = "value")
      rast_table <- rbind(rast_table, rast_t_dt_m)
    }

  }
  if(output == "raster"){
    return(rast_l)
  }else{
    return(rast_table)
  }

}



#' Update Harvest Function
#' @description Formerly UpdateHarvestsFn, This function adds the spatial harvest details for each block
#' with brushing. This function will be removed if we no longer wish to brush and replace with retained
#' deciduous trees in clear cuts (addSpatialHarvest)
#' @param NewxmlPath string - directory where the newly created parameter files are located
#' @param Units_path string - directory where the spatial files for each unit is located
#' @param Gaps_path string - directory where the spatial files for the gap curs are located
#' @param ParamFile_Suffix string - what is the ending of the parameter files - represents a given parameter file update
#'
#' @return
#' @export
#'
#' @examples
UpdateHarvestsFn <- function(NewxmlPath, Units_path, Gaps_path, ParamFile_Suffix){


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
  for(j in 1:length(Blocks)){
    Unit_i <- Blocks[j]
    TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                        ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                               ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
    Unit_sh <- read_sf(paste0(Units_path,Blocks[j],".shp"))
    Unit_sh$Unit <- Blocks[j]
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
    st_transform(.,crs=crs(HR_blocks))
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
  LR_Cuts <- LR_Cuts %>%
    dplyr::select(Id,geometry) %>%
    st_transform(.,crs=crs(LR_blocks))
  HR_Cuts_B2$Harvest <- 1
  HR_Cuts_B3$Harvest <- 1
  HR_Cuts_C2$Harvest <- 1
  HR_Cuts_D4$Harvest <- 1
  LR_blocks$Harvest <- 1
  CC_blocks$Harvest <- 1
  HR_blocks$Harvest <- 2
  NH_blocks$Harvest <- 1

  ########################## 2. Update spatial harvests #############################
  #Update the parameter files with Spatial harvests and plants
  for(i in 1:length(c(LR,HR,CC))){
    Unit_i <- c(LR,HR,CC)[i]
    TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                        ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                               ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
    #Forest size
    if(TreatType=="LR"){
      bb <- st_bbox(st_buffer(LR_blocks %>% filter(Unit==Unit_i), dist = 10))
    }else if(TreatType=="CC"){
      bb <- st_bbox(st_buffer(CC_blocks %>% filter(Unit==Unit_i), dist = 10))
    }else{
      bb <- st_bbox(st_buffer(HR_blocks %>% filter(Unit==Unit_i), dist = 10))
    }
    Max_length <- plyr::round_any(max(abs(bb[1]-bb[3]),abs(bb[2]-bb[4])), accuracy=HarvGridRes,f=ceiling)
    PlotSize <- raster(xmn=bb[1], xmx=bb[1]+Max_length, ymn=bb[2],ymx=bb[2]+Max_length,
                       res=HarvGridRes, crs=crs(CC_blocks))
    if(TreatType=="LR"){
      PlotRast <- fasterize::fasterize(LR_blocks %>% filter(Unit==Unit_i),PlotSize, field="Harvest", background=0)
    }else if(TreatType=="CC"){
      PlotRast <- fasterize::fasterize(CC_blocks %>% filter(Unit==Unit_i),PlotSize, field="Harvest", background=0)
    }else if(Unit_i=="B2"){
      PlotRast <- fasterize::fasterize(HR_Cuts_B2,PlotSize, field="Harvest", background=0)
    }else if(Unit_i=="B3"){
      PlotRast <- fasterize::fasterize(HR_Cuts_B3,PlotSize, field="Harvest", background=0)
    }else if(Unit_i=="C2"){
      PlotRast <- fasterize::fasterize(HR_Cuts_C2,PlotSize, field="Harvest", background=0)
    }else{
      PlotRast <- fasterize::fasterize(HR_Cuts_D4,PlotSize, field="Harvest", background=0)
    }
    plot(PlotRast)
    print(paste("Updating",TreatType,Unit_i))
    #All treatments need their area defined in SORTIE x,y coordinates:

    #make the directory if it doesn't already exist
    if(!dir.exists(paste0(Units_path,"Harvest_PlantLocations"))) {
      dir.create(paste0(Units_path,"Harvest_PlantLocations"),
                 showWarnings=FALSE)
      #Set pathway
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
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
    }else{
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
      print("Harvest and plant location file directory exists")
    }

    ##### Apply harvest to parameter file
    if(TreatType!="HR"){
      if(TreatType !="CC"){
        ###### LIGHT REMOVAL #######
        #if it's light removal, one harvest and one planting
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        #Get the first and last line numbers for the harvested area
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
        lni <- grep(grouptext,Basexml) #get the location of harvest from base .xml
        lnm <- c(lni[1],tail(lni,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ##### PLANTING #####
        #Do the same for plantings
        grouptext <- "pl_applyToCell"  #get the name of the group we are replacing
        lni <- grep(grouptext,rf2) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf2)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf3 <- append(x,PlantLines,lnm[1]-1)
        rf3 <- gsub("//", "\\\\", rf3)
        #write the new parameter file out
        writeLines(rf3,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }else if(Unit_i=="A3"|Unit_i=="B4"){  # if it's a clearcut, Single harvest and plant, but include brushing
        # Brush twice 1998 & 1999 for A3 and 1998 & 2000 for B4
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        #set the years for brushing
        grouptext <- "ha_timestep"
        lni <- grep(grouptext,Basexml)
        #1998 brushing:
        st_start <- str_locate(Basexml[lni][2],">")
        st_end <- str_locate(Basexml[lni][2],"</")
        newln <- str_replace(Basexml[lni][2],paste0(">",substr(Basexml[lni][2],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(7),"<"))
        Basexml[lni][2] <- newln
        if(Unit_i=="A3"){
          #1999 brushing:
          st_start <- str_locate(Basexml[lni][3],">")
          st_end <- str_locate(Basexml[lni][3],"</")
          newln <- str_replace(Basexml[lni][3],paste0(">",substr(Basexml[lni][3],st_start[1]+1,st_end[1]-1),"<"),
                               paste0(">",as.character(8),"<"))
          Basexml[lni][3] <- newln
        }else{
          #2000 brushing:
          st_start <- str_locate(Basexml[lni][3],">")
          st_end <- str_locate(Basexml[lni][3],"</")
          newln <- str_replace(Basexml[lni][3],paste0(">",substr(Basexml[lni][3],st_start[1]+1,st_end[1]-1),"<"),
                               paste0(">",as.character(9),"<"))
          Basexml[lni][3] <- newln
        }
        #Now update the harvest boundaries
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
        lni <- split(grep(grouptext,Basexml),cumsum(c(1, diff(grep(grouptext,Basexml)) != 1)))
        #### Replace all 3 harvests with cut boundary
        ### Harvest ####
        lnm <- c(lni$`1`[1],tail(lni$`1`,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ### Brushing 1 ####
        lni <- split(grep(grouptext,rf2),cumsum(c(1, diff(grep(grouptext,rf2)) != 1)))
        lnm <- c(lni$`2`[1],tail(lni$`2`,n=1))
        x <- removeRow(lnm, rf2)
        rf3 <- append(x,HarvLines,lnm[1]-1)
        ### Brushing 2 ####
        lni <- split(grep(grouptext,rf3),cumsum(c(1, diff(grep(grouptext,rf3)) != 1)))
        lnm <- c(lni$`3`[1],tail(lni$`3`,n=1))
        x <- removeRow(lnm, rf3)
        rf4 <- append(x,HarvLines,lnm[1]-1)
        ###################
        #### Planting ####
        grouptext <- "pl_applyToCell"
        lni <- grep(grouptext,rf4) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf4)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf5 <- append(x,PlantLines,lnm[1]-1)
        rf5 <- gsub("//", "\\\\", rf5)
        writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }else if(Unit_i=="A1"){ # Brush once for A1 (in 2000)
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        #set the year for brushing
        grouptext <- "ha_timestep"
        lni <- grep(grouptext,Basexml)
        #2000 brushing:
        st_start <- str_locate(Basexml[lni][2],">")
        st_end <- str_locate(Basexml[lni][2],"</")
        newln <- str_replace(Basexml[lni][2],paste0(">",substr(Basexml[lni][2],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(9),"<"))
        Basexml[lni][2] <- newln
        #### Spatial cuts
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing

        lni <- split(grep(grouptext,Basexml),cumsum(c(1, diff(grep(grouptext,Basexml)) != 1)))
        #### Replace all 3 harvests with cut boundary
        ### Harvest ####
        lnm <- c(lni$`1`[1],tail(lni$`1`,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ### Brushing 1 ####
        lni <- split(grep(grouptext,rf2),cumsum(c(1, diff(grep(grouptext,rf2)) != 1)))
        lnm <- c(lni$`2`[1],tail(lni$`2`,n=1))
        x <- removeRow(lnm, rf2)
        rf3 <- append(x,HarvLines,lnm[1]-1)
        ### Delete Brushing 2 ####
        grouptext <- "ha_cutEvent"
        lni <- grep(grouptext,rf3)
        lnm <- lni[5:6]
        x <- removeRow(lnm, rf3)
        rf4 <- x
        ###################
        #### Planting ####
        grouptext <- "pl_applyToCell"
        lni <- grep(grouptext,rf4) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf4)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf5 <- append(x,PlantLines,lnm[1]-1)
        rf5 <- gsub("//", "\\\\", rf5)
        writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }else{ #don't brush D2
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
        #lni <- grep(grouptext,Basexml) #get the location of harvest from base .xml
        lni <- split(grep(grouptext,Basexml),cumsum(c(1, diff(grep(grouptext,Basexml)) != 1)))
        #### Replace all 3 harvests with cut boundary
        ### Harvest ####
        lnm <- c(lni$`1`[1],tail(lni$`1`,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ### Delete Brushing 1 ####
        grouptext <- "ha_cutEvent"
        lni <- grep(grouptext,rf2)
        lnm <- lni[5:6]
        rf3 <- removeRow(lnm, rf2)
        ### Delete Brushing 2 ####
        lni <- grep(grouptext,rf3)
        lnm <- lni[3:4]
        rf4 <- removeRow(lnm, rf3)
        ###################
        #### Planting ####
        grouptext <- "pl_applyToCell"
        lni <- grep(grouptext,rf4) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf4)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf5 <- append(x,PlantLines,lnm[1]-1)
        rf5 <- gsub("//", "\\\\", rf5)
        writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }
    }else{
      #if it's heavy removal, gap cuts + matrix removal + gap planting + matrix planting
      #### Map the non-gap matrix for heavy removal treatments
      MatrixHarv <- fasterize::fasterize(HR_blocks %>% filter(Unit==Unit_i),PlotSize, field="Harvest", background=0)
      MatrixHarv <- (MatrixHarv-PlotRast)>1
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
      res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      xml2::write_xml(res, "temp.xml")
      tmp <- readLines("temp.xml", encoding="UTF-8")
      Basexml <- gsub("\\\\", "//",tmp)
      #Get the first and last line numbers for the harvested area
      grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
      lni <- grep(grouptext,Basexml) #get the location of harvest from base .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1))) #find the break in numbers
      #because heavy removals have two harvests - get the first and last of the two harvests:
      #Harvest 1 - gap cuts
      lni_gap <- harv_gr[1]$`1` #first harvest chunk
      StartCuts <- lni_gap[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_gap)-1)){
        if(lni_gap[ii+1]==(lni_gap[ii]+1)){ #use while
          EndCuts <- lni_gap[ii+1]
        }else{
          EndCuts <- lni_gap[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, Basexml)
      #bring in the gaps
      HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
      rf2 <- append(x,HarvLines,lnm[1]-1)

      #Plant the gaps
      grouptext <- "pl_applyToCell"  #get the name of the group we are replacing
      lni <- grep(grouptext,rf2) #get the location of harvest from base .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1)))
      lni_gap <- harv_gr[1]$`1` #first harvest chunk
      StartCuts <- lni_gap[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_gap)-1)){
        if(lni_gap[ii+1]==(lni_gap[ii]+1)){ #use while
          EndCuts <- lni_gap[ii+1]
        } else{
          EndCuts <- lni_gap[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf2)
      PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
      rf3 <- append(x,PlantLines,lnm[1]-1)

      ###### Harvest 2 - matrix cut
      grouptext <- "ha_applyToCell"
      lni <- grep(grouptext,rf3) #need to get the new location of the second harvest from updated .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1))) #find the break in numbers
      #because heavy removals have two harvests - get the first and last of the two harvests:
      lni_matrix <- harv_gr[2]$`2`
      StartCuts <- lni_matrix[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_matrix)-1)){
        if(lni_matrix[ii+1]==(lni_matrix[ii]+1)){ #use while
          EndCuts <- lni_matrix[ii+1]
        }else{
          EndCuts <- lni_matrix[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf3)
      #bring in the matrix cells
      HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_Harv.xml"))
      rf4 <- append(x,HarvLines,lnm[1]-1)

      ####Plant the heavy removal matrix
      #Set the planting area
      grouptext <- "pl_applyToCell"  #get the name of the group we are replacing
      lni <- grep(grouptext,rf4) #get the location of plant from base .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1)))
      lni_matrix <- harv_gr[2]$`2` #get the second chunk
      StartCuts <- lni_matrix[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_matrix)-1)){
        if(lni_matrix[ii+1]==(lni_matrix[ii]+1)){ #use while
          EndCuts <- lni_matrix[ii+1]
        }else{
          EndCuts <- lni_matrix[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf4)
      PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_pl.xml"))
      rf5 <- append(x,PlantLines,lnm[1]-1)

      #update the heavy harvest matrix with planting dens
      pl2_dens <- PlantDensDat[GM=="M",.(Density=sum(.SD)),by=c("Unit","GM")]
      grouptext <-"pl_distanceOrDensity"
      pl2 <- grep(grouptext,rf5)[2] #just the second planting density
      st_start <- str_locate(rf5[pl2],">")
      st_end <- str_locate(rf5[pl2],"</")
      #and replace the value
      newln <- str_replace(rf5[pl2],paste0(">",substr(rf5[pl2],st_start[1]+1,st_end[1]-1),"<"),
                           paste0(">",as.character(pl2_dens[Unit==Unit_i]$Density),"<"))
      rf5[pl2] <- newln
      #update the matrix with planting %
      pl_comp <- PlantPCDat[Unit==Unit_i & GM =="M",.(Init.Values ="PlantingProp_2",Amabalis_Fir, Black_Cottonwood,
                                                      Hybrid_spruce,Lodgepole_Pine, Paper_Birch,Subalpine_Fir,
                                                      Trembling_Aspen,Western_Hemlock, Western_redcedar)]
      pl_comp <- melt(pl_comp,measure = 2:10) #hard coded to number of species that are in the table
      grouptext <-"pl_atpVal"
      pl2 <- grep(grouptext,rf5)
      pl2_c <- split(pl2,cumsum(c(1, diff(pl2) != 1)))
      lni_mc <- pl2_c[2]$`2` #get the second chunk
      sp_list <- c("Amabalis_Fir","Black_Cottonwood","Hybrid_spruce","Lodgepole_Pine","Paper_Birch",
                   "Subalpine_Fir","Trembling_Aspen","Western_Hemlock","Western_redcedar")
      for(iii in 1:length(sp_list)){
        lni_mc1 <- lni_mc[grep(sp_list[iii],rf5[lni_mc[1]:max(lni_mc)])]
        st_start <- str_locate(rf5[lni_mc1],">")
        st_end <- str_locate(rf5[lni_mc1],"</")
        newln <- str_replace(rf5[lni_mc1],paste0(">",substr(rf5[lni_mc1],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(pl_comp[variable==sp_list[iii]]$value),"<"))
        rf5[lni_mc1] <- newln
      }
      rf5 <- gsub("//", "\\\\", rf5)
      writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
    }
  }

}

#' Add Spatial Harvest Function
#' @description This function differs from UpdateHarvestsFn where we used brushing. This function retains
#' deciduous trees in the clearcuts, with each species retained in different % in each block.
#' @param NewxmlPath string - directory where the newly created parameter files are located
#' @param Units_path string - directory where the spatial files for each unit is located
#' @param Gaps_path string - directory where the spatial files for the gap curs are located
#' @param ParamFile_Suffix string - what is the ending of the parameter files - represents a given parameter file update
#'
#' @return
#' @export
#'
#' @examples
AddSpatialHarvest <- function(NewxmlPath, Units_path, Gaps_path, ParamFile_Suffix){


  ########################## 1. Read in spatial harvests #############################
  ## Date Creek Treatments
  NH<-c("A4", "B1", "C1", "D3") #No harvest
  LR<-c("A2", "B5", "C3", "D5") # light removal (30% BasalArea)
  HR<-c("B2", "B3", "C2", "D4") #heavy removal (60% BasalArea)
  CC<-c("A1", "A3", "B4", "D2") #Clear-cut (100% removal) with some caveats (some deciduous left standing, one small

  bounds <- ReadSpatialBounds(Units_path = Units_path)
  partCuts <- modifyPartCuts(Gaps_path = Gaps_path)

  # Give a value of 1 to area covered by cuts, unless HR, then 2
  boundsHR <- bounds %>% filter(Unit %in% HR) %>% mutate(.,Harvest = 2)
  boundsLC <- bounds %>% filter(Unit %in% LR|Unit %in% CC) %>% mutate(.,Harvest = 1)
  bounds <- rbind(boundsLC, boundsHR)
  partCuts$Harvest <- 1

  ########################## 2. Update spatial harvests #############################
  #Update the parameter files with Spatial harvests and plants
  for(i in 1:length(c(LR,HR,CC))){
    Unit_i <- c(LR,HR,CC)[i]
    TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                        ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                               ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
    print(paste("Updating",TreatType,Unit_i))

    ##### Convert the harvest and planting locations to SORTIE
    if(!dir.exists(paste0(Units_path,"Harvest_PlantLocations"))) {
      dir.create(paste0(Units_path,"Harvest_PlantLocations"),
                 showWarnings=FALSE)
      # make the directory and do the conversion if it doesn't already exist

      #Set pathway
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")

      if(TreatType=="LR"|TreatType=="CC"){
        PlotRast <- fasterize::fasterize(bounds %>% filter(Unit==Unit_i),PlotSize, field="Harvest", background=0)
      }else {
        PlotRast <- fasterize::fasterize(partCuts %>% filter(Id==Unit_i) ,PlotSize, field="Harvest", background=0)
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

    }else{
      #if the directory already exists, just use the conversion already there
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
      print("Harvest and plant location file directory exists")
    }

    res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
    xml2::write_xml(res, "temp.xml")
    tmp <- readLines("temp.xml", encoding="UTF-8")
    Basexml <- gsub("\\\\", "//",tmp)
    HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
    PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))


    harv_text <- "ha_applyToCell"  #get the name of the group we are replacing
    plant_text <- "pl_applyToCell"  #get the name of the group we are replacing

    if(TreatType == "LR"){
      ###### LIGHT REMOVAL #######
      #if it's light removal, one harvest and one planting
      rf2 <- Basexml

      #### HARVEST #####
      lni <- grep(harv_text,rf2) #get the location of harvest from base .xml
      lnm <- c(lni[1],tail(lni,n=1)) #get just the first and last element
      x <- removeRow(lnm, rf2)
      rf2 <- append(x,HarvLines,lnm[1]-1)

      ##### PLANTING #####
      #Do the same for plantings
      lni <- grep(plant_text,rf2) #get the location of planting from base .xml
      lnm <- c(lni[1],tail(lni,n=1))
      x <- removeRow(lnm, rf2)

      rf2 <- append(x,PlantLines,lnm[1]-1)
      rf2 <- gsub("//", "\\\\", rf2)
      #write the new parameter file out
      writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))

    }else if(TreatType == "CC"){
      ######## CLEAR CUTS #########
      rf2 <- Basexml

      #### HARVESTS ####
      ### Update all harvests with the right pattern
      numHarvs <- length(split(grep(harv_text,rf2),cumsum(c(1, diff(grep(harv_text,rf2)) != 1))))
      for(iii in 1:numHarvs){
        lni <- split(grep(harv_text,rf2),cumsum(c(1, diff(grep(harv_text,rf2)) != 1)))
        lnm <- c(lni[[iii]][1],tail(lni[[iii]],n=1)) #get just the first and last element
        x <- removeRow(lnm, rf2)
        rf2 <- append(x,HarvLines,lnm[1]-1)
      }

      #### PLANTING ####
      lni <- grep(plant_text,rf2) #get the location of planting
      lnm <- c(lni[1],tail(lni,n=1))
      x <- removeRow(lnm, rf2)
      rf2 <- append(x,PlantLines,lnm[1]-1)

      #### DECIDUOUS HARVESTS ####
      #get the % from deciduous retention data
      rf2[grep("ha_applyToSpecies", rf2)][7:9]

      # Trembling Aspen (1st cut), Black Cottonwood (2nd cut), Paper Birch (3rd cut)
      rt_amt <-c()
      rt_amt[1] <- Deciduous_retention[Unit == Unit_i & Species == "At"]$Harvest_Intensity*100
      rt_amt[2] <- Deciduous_retention[Unit == Unit_i & Species == "Ac"]$Harvest_Intensity*100
      rt_amt[3] <- Deciduous_retention[Unit == Unit_i & Species == "Ep"]$Harvest_Intensity*100

      #loop through the deciduous trees
      for(ix in 1:3){
        lni <- grep("ha_amountToCut",rf2)[ix+1]
        st_start <- str_locate(rf2[lni],">")
        st_end <- str_locate(rf2[lni],"</")
        newln <- str_replace(rf2[lni],paste0(">",substr(rf2[lni],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(rt_amt[ix]),"<"))
        rf2[lni] <- newln
      }

      rf2 <- gsub("//", "\\\\", rf2)
      writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))

    }else{
      ######## HEAVY REMOVAL #########
      #if it's heavy removal, gap cuts + matrix removal + gap planting + matrix planting
      #### Map the non-gap matrix for heavy removal treatments
      #Forest size
      rf2 <- Basexml
      lni <- grep(harv_text,rf2) #get the location of harvest from base .xml

      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1))) #find the break in numbers
      #because heavy removals have two harvests - get the first and 2nd of the two harvests:
      #Harvest 1 - gap cuts
      lni_gap <- harv_gr[1]$`1` #first harvest chunk
      StartCuts <- lni_gap[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_gap)-1)){
        if(lni_gap[ii+1]==(lni_gap[ii]+1)){ #use while
          EndCuts <- lni_gap[ii+1]
        }else{
          EndCuts <- lni_gap[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, Basexml)
      #bring in the gaps
      rf2 <- append(x,HarvLines,lnm[1]-1)

      #Plant the gaps
      lni <- grep(plant_text,rf2) #get the location of harvest from base .xml
      plant_gr <- split(lni,cumsum(c(1, diff(lni) != 1)))
      lni_gap <- plant_gr[1]$`1` #first harvest chunk
      StartCuts <- lni_gap[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_gap)-1)){
        if(lni_gap[ii+1]==(lni_gap[ii]+1)){ #use while
          EndCuts <- lni_gap[ii+1]
        } else{
          EndCuts <- lni_gap[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf2)
      rf2 <- append(x,PlantLines,lnm[1]-1)

      ###### Harvest 2 - matrix cut
      lni <- grep(harv_text,rf2) #need to get the new location of the second harvest from updated .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1))) #find the break in numbers
      #because heavy removals have two harvests - get the first and last of the two harvests:
      lni_matrix <- harv_gr[2]$`2`
      StartCuts <- lni_matrix[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_matrix)-1)){
        if(lni_matrix[ii+1]==(lni_matrix[ii]+1)){ #use while
          EndCuts <- lni_matrix[ii+1]
        }else{
          EndCuts <- lni_matrix[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf2)
      #bring in the matrix cells
      MatrixLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_Harv.xml"))
      rf2 <- append(x,MatrixLines,lnm[1]-1)

      ####Plant the heavy removal matrix
      #Set the planting area
      lni <- grep(plant_text,rf2) #get the location of plant from base .xml
      plant_gr <- split(lni,cumsum(c(1, diff(lni) != 1)))
      lni_matrix <- plant_gr[2]$`2` #get the second chunk
      StartCuts <- lni_matrix[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_matrix)-1)){
        if(lni_matrix[ii+1]==(lni_matrix[ii]+1)){ #use while
          EndCuts <- lni_matrix[ii+1]
        }else{
          EndCuts <- lni_matrix[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf2)
      MatrixLine_Plant <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_pl.xml"))
      rf2 <- append(x,MatrixLine_Plant,lnm[1]-1)

      #update the heavy harvest matrix with planting dens
      PlantDensDat <- data.table::fread("D:/Github/DateCreekData/data-raw/Harvests_Plants/PlantingDensities.csv")
      PlantPCDat <- data.table::fread("D:/Github/DateCreekData/data-raw/Harvests_Plants/PlantingPercents.csv")

      pl2_dens <- PlantDensDat[GM=="M",.(Density=sum(.SD)),by=c("Unit","GM")]
      grouptext <-"pl_distanceOrDensity"
      pl2 <- grep(grouptext,rf2)[2] #just the second planting density
      st_start <- str_locate(rf2[pl2],">")
      st_end <- str_locate(rf2[pl2],"</")
      #and replace the value
      newln <- str_replace(rf2[pl2],paste0(">",substr(rf2[pl2],st_start[1]+1,st_end[1]-1),"<"),
                           paste0(">",as.character(pl2_dens[Unit==Unit_i]$Density),"<"))
      rf2[pl2] <- newln
      #update the matrix with planting %
      pl_comp <- PlantPCDat[Unit==Unit_i & GM =="M",.(Init.Values ="PlantingProp_2",Amabalis_Fir, Black_Cottonwood,
                                                      Hybrid_spruce,Lodgepole_Pine, Paper_Birch,Subalpine_Fir,
                                                      Trembling_Aspen,Western_Hemlock, Western_redcedar)]
      pl_comp <- melt(pl_comp, id.vars = "Init.Values")
      grouptext <-"pl_atpVal"
      pl2 <- grep(grouptext,rf2)
      pl2_c <- split(pl2,cumsum(c(1, diff(pl2) != 1)))
      lni_mc <- pl2_c[2]$`2` #get the second chunk
      sp_list <- c("Amabalis_Fir","Black_Cottonwood","Hybrid_spruce","Lodgepole_Pine","Paper_Birch",
                   "Subalpine_Fir","Trembling_Aspen","Western_Hemlock","Western_redcedar")
      for(iii in 1:length(sp_list)){
        lni_mc1 <- lni_mc[grep(sp_list[iii],rf2[lni_mc[1]:max(lni_mc)])]
        st_start <- str_locate(rf2[lni_mc1],">")
        st_end <- str_locate(rf2[lni_mc1],"</")
        newln <- str_replace(rf2[lni_mc1],paste0(">",substr(rf2[lni_mc1],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(pl_comp[variable==sp_list[iii]]$value),"<"))
        rf2[lni_mc1] <- newln
      }

      #snag harvest
      lni <- grep(harv_text,rf2)
      lni_grs <- split(lni,cumsum(c(1, diff(lni) != 1)))
      lni_snag <- lni_grs$`3` #first harvest chunk
      StartCuts <- lni_snag[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_snag)-1)){
        if(lni_snag[ii+1]==(lni_snag[ii]+1)){ #use while
          EndCuts <- lni_snag[ii+1]
        }else{
          EndCuts <- lni_snag[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf2)
      #bring in the gaps
      rf2 <- append(x,HarvLines,lnm[1]-1)

      rf2 <- gsub("//", "\\\\", rf2)
      writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
    }
  }
}



#Old function

old_spatialHarvest <- function(){
  for(i in 1:length(c(LR,HR,CC))){
    Unit_i <- c(LR,HR,CC)[i]
    TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                        ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                               ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
    #Forest size
    bb <- st_bbox(st_buffer(bounds %>% filter(Unit==Unit_i), dist = 10))

    Max_length <- plyr::round_any(max(abs(bb[1]-bb[3]),abs(bb[2]-bb[4])), accuracy=HarvGridRes,f=ceiling)
    PlotSize <- raster(xmn=bb[1], xmx=bb[1]+Max_length, ymn=bb[2],ymx=bb[2]+Max_length,
                       res=HarvGridRes, crs=crs(CC_blocks))
    if(TreatType=="LR"){
      PlotRast <- fasterize::fasterize(LR_blocks %>% filter(Unit==Unit_i),PlotSize, field="Harvest", background=0)
    }else if(TreatType=="CC"){
      PlotRast <- fasterize::fasterize(CC_blocks %>% filter(Unit==Unit_i),PlotSize, field="Harvest", background=0)
    }else if(Unit_i=="B2"){
      PlotRast <- fasterize::fasterize(HR_Cuts_B2,PlotSize, field="Harvest", background=0)
    }else if(Unit_i=="B3"){
      PlotRast <- fasterize::fasterize(HR_Cuts_B3,PlotSize, field="Harvest", background=0)
    }else if(Unit_i=="C2"){
      PlotRast <- fasterize::fasterize(HR_Cuts_C2,PlotSize, field="Harvest", background=0)
    }else{
      PlotRast <- fasterize::fasterize(HR_Cuts_D4,PlotSize, field="Harvest", background=0)
    }
    plot(PlotRast)
    print(paste("Updating",TreatType,Unit_i))
    #All treatments need their area defined in SORTIE x,y coordinates:

    #make the directory if it doesn't already exist
    if(!dir.exists(paste0(Units_path,"Harvest_PlantLocations"))) {
      dir.create(paste0(Units_path,"Harvest_PlantLocations"),
                 showWarnings=FALSE)
      #Set pathway
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
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
    }else{
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
      print("Harvest and plant location file directory exists")
    }

    ##### Apply harvest to parameter file
    if(TreatType!="HR"){
      if(TreatType !="CC"){
        ###### LIGHT REMOVAL #######
        #if it's light removal, one harvest and one planting
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        #Get the first and last line numbers for the harvested area
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
        lni <- grep(grouptext,Basexml) #get the location of harvest from base .xml
        lnm <- c(lni[1],tail(lni,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ##### PLANTING #####
        #Do the same for plantings
        grouptext <- "pl_applyToCell"  #get the name of the group we are replacing
        lni <- grep(grouptext,rf2) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf2)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf3 <- append(x,PlantLines,lnm[1]-1)
        rf3 <- gsub("//", "\\\\", rf3)
        #write the new parameter file out
        writeLines(rf3,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }else if(Unit_i=="A3"|Unit_i=="B4"){  # if it's a clearcut, Single harvest and plant, but include brushing
        # Brush twice 1998 & 1999 for A3 and 1998 & 2000 for B4
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        #set the years for brushing
        grouptext <- "ha_timestep"
        lni <- grep(grouptext,Basexml)
        #1998 brushing:
        st_start <- str_locate(Basexml[lni][2],">")
        st_end <- str_locate(Basexml[lni][2],"</")
        newln <- str_replace(Basexml[lni][2],paste0(">",substr(Basexml[lni][2],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(7),"<"))
        Basexml[lni][2] <- newln
        if(Unit_i=="A3"){
          #1999 brushing:
          st_start <- str_locate(Basexml[lni][3],">")
          st_end <- str_locate(Basexml[lni][3],"</")
          newln <- str_replace(Basexml[lni][3],paste0(">",substr(Basexml[lni][3],st_start[1]+1,st_end[1]-1),"<"),
                               paste0(">",as.character(8),"<"))
          Basexml[lni][3] <- newln
        }else{
          #2000 brushing:
          st_start <- str_locate(Basexml[lni][3],">")
          st_end <- str_locate(Basexml[lni][3],"</")
          newln <- str_replace(Basexml[lni][3],paste0(">",substr(Basexml[lni][3],st_start[1]+1,st_end[1]-1),"<"),
                               paste0(">",as.character(9),"<"))
          Basexml[lni][3] <- newln
        }
        #Now update the harvest boundaries
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
        lni <- split(grep(grouptext,Basexml),cumsum(c(1, diff(grep(grouptext,Basexml)) != 1)))
        #### Replace all 3 harvests with cut boundary
        ### Harvest ####
        lnm <- c(lni$`1`[1],tail(lni$`1`,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ### Brushing 1 ####
        lni <- split(grep(grouptext,rf2),cumsum(c(1, diff(grep(grouptext,rf2)) != 1)))
        lnm <- c(lni$`2`[1],tail(lni$`2`,n=1))
        x <- removeRow(lnm, rf2)
        rf3 <- append(x,HarvLines,lnm[1]-1)
        ### Brushing 2 ####
        lni <- split(grep(grouptext,rf3),cumsum(c(1, diff(grep(grouptext,rf3)) != 1)))
        lnm <- c(lni$`3`[1],tail(lni$`3`,n=1))
        x <- removeRow(lnm, rf3)
        rf4 <- append(x,HarvLines,lnm[1]-1)
        ###################
        #### Planting ####
        grouptext <- "pl_applyToCell"
        lni <- grep(grouptext,rf4) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf4)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf5 <- append(x,PlantLines,lnm[1]-1)
        rf5 <- gsub("//", "\\\\", rf5)
        writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }else if(Unit_i=="A1"){ # Brush once for A1 (in 2000)
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        #set the year for brushing
        grouptext <- "ha_timestep"
        lni <- grep(grouptext,Basexml)
        #2000 brushing:
        st_start <- str_locate(Basexml[lni][2],">")
        st_end <- str_locate(Basexml[lni][2],"</")
        newln <- str_replace(Basexml[lni][2],paste0(">",substr(Basexml[lni][2],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(9),"<"))
        Basexml[lni][2] <- newln
        #### Spatial cuts
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing

        lni <- split(grep(grouptext,Basexml),cumsum(c(1, diff(grep(grouptext,Basexml)) != 1)))
        #### Replace all 3 harvests with cut boundary
        ### Harvest ####
        lnm <- c(lni$`1`[1],tail(lni$`1`,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ### Brushing 1 ####
        lni <- split(grep(grouptext,rf2),cumsum(c(1, diff(grep(grouptext,rf2)) != 1)))
        lnm <- c(lni$`2`[1],tail(lni$`2`,n=1))
        x <- removeRow(lnm, rf2)
        rf3 <- append(x,HarvLines,lnm[1]-1)
        ### Delete Brushing 2 ####
        grouptext <- "ha_cutEvent"
        lni <- grep(grouptext,rf3)
        lnm <- lni[5:6]
        x <- removeRow(lnm, rf3)
        rf4 <- x
        ###################
        #### Planting ####
        grouptext <- "pl_applyToCell"
        lni <- grep(grouptext,rf4) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf4)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf5 <- append(x,PlantLines,lnm[1]-1)
        rf5 <- gsub("//", "\\\\", rf5)
        writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }else{ #don't brush D2
        res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        xml2::write_xml(res, "temp.xml")
        tmp <- readLines("temp.xml", encoding="UTF-8")
        Basexml <- gsub("\\\\", "//",tmp)
        HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
        grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
        #lni <- grep(grouptext,Basexml) #get the location of harvest from base .xml
        lni <- split(grep(grouptext,Basexml),cumsum(c(1, diff(grep(grouptext,Basexml)) != 1)))
        #### Replace all 3 harvests with cut boundary
        ### Harvest ####
        lnm <- c(lni$`1`[1],tail(lni$`1`,n=1)) #get just the first and last element
        x <- removeRow(lnm, Basexml)
        rf2 <- append(x,HarvLines,lnm[1]-1)
        ### Delete Brushing 1 ####
        grouptext <- "ha_cutEvent"
        lni <- grep(grouptext,rf2)
        lnm <- lni[5:6]
        rf3 <- removeRow(lnm, rf2)
        ### Delete Brushing 2 ####
        lni <- grep(grouptext,rf3)
        lnm <- lni[3:4]
        rf4 <- removeRow(lnm, rf3)
        ###################
        #### Planting ####
        grouptext <- "pl_applyToCell"
        lni <- grep(grouptext,rf4) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- removeRow(lnm, rf4)
        PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
        rf5 <- append(x,PlantLines,lnm[1]-1)
        rf5 <- gsub("//", "\\\\", rf5)
        writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      }
    }else{
      #if it's heavy removal, gap cuts + matrix removal + gap planting + matrix planting
      #### Map the non-gap matrix for heavy removal treatments
      MatrixHarv <- fasterize::fasterize(HR_blocks %>% filter(Unit==Unit_i),PlotSize, field="Harvest", background=0)
      MatrixHarv <- (MatrixHarv-PlotRast)>1
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
      res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
      xml2::write_xml(res, "temp.xml")
      tmp <- readLines("temp.xml", encoding="UTF-8")
      Basexml <- gsub("\\\\", "//",tmp)
      #Get the first and last line numbers for the harvested area
      grouptext <- "ha_applyToCell"  #get the name of the group we are replacing
      lni <- grep(grouptext,Basexml) #get the location of harvest from base .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1))) #find the break in numbers
      #because heavy removals have two harvests - get the first and last of the two harvests:
      #Harvest 1 - gap cuts
      lni_gap <- harv_gr[1]$`1` #first harvest chunk
      StartCuts <- lni_gap[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_gap)-1)){
        if(lni_gap[ii+1]==(lni_gap[ii]+1)){ #use while
          EndCuts <- lni_gap[ii+1]
        }else{
          EndCuts <- lni_gap[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, Basexml)
      #bring in the gaps
      HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
      rf2 <- append(x,HarvLines,lnm[1]-1)

      #Plant the gaps
      grouptext <- "pl_applyToCell"  #get the name of the group we are replacing
      lni <- grep(grouptext,rf2) #get the location of harvest from base .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1)))
      lni_gap <- harv_gr[1]$`1` #first harvest chunk
      StartCuts <- lni_gap[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_gap)-1)){
        if(lni_gap[ii+1]==(lni_gap[ii]+1)){ #use while
          EndCuts <- lni_gap[ii+1]
        } else{
          EndCuts <- lni_gap[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf2)
      PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))
      rf3 <- append(x,PlantLines,lnm[1]-1)

      ###### Harvest 2 - matrix cut
      grouptext <- "ha_applyToCell"
      lni <- grep(grouptext,rf3) #need to get the new location of the second harvest from updated .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1))) #find the break in numbers
      #because heavy removals have two harvests - get the first and last of the two harvests:
      lni_matrix <- harv_gr[2]$`2`
      StartCuts <- lni_matrix[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_matrix)-1)){
        if(lni_matrix[ii+1]==(lni_matrix[ii]+1)){ #use while
          EndCuts <- lni_matrix[ii+1]
        }else{
          EndCuts <- lni_matrix[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf3)
      #bring in the matrix cells
      HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_Harv.xml"))
      rf4 <- append(x,HarvLines,lnm[1]-1)

      ####Plant the heavy removal matrix
      #Set the planting area
      grouptext <- "pl_applyToCell"  #get the name of the group we are replacing
      lni <- grep(grouptext,rf4) #get the location of plant from base .xml
      harv_gr <- split(lni,cumsum(c(1, diff(lni) != 1)))
      lni_matrix <- harv_gr[2]$`2` #get the second chunk
      StartCuts <- lni_matrix[1]
      EndCuts <- c()
      for(ii in 1:(length(lni_matrix)-1)){
        if(lni_matrix[ii+1]==(lni_matrix[ii]+1)){ #use while
          EndCuts <- lni_matrix[ii+1]
        }else{
          EndCuts <- lni_matrix[ii]
        }
      }
      lnm <- c(StartCuts,EndCuts)
      x <- removeRow(lnm, rf4)
      PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_pl.xml"))
      rf5 <- append(x,PlantLines,lnm[1]-1)

      #update the heavy harvest matrix with planting dens
      pl2_dens <- PlantDensDat[GM=="M",.(Density=sum(.SD)),by=c("Unit","GM")]
      grouptext <-"pl_distanceOrDensity"
      pl2 <- grep(grouptext,rf5)[2] #just the second planting density
      st_start <- str_locate(rf5[pl2],">")
      st_end <- str_locate(rf5[pl2],"</")
      #and replace the value
      newln <- str_replace(rf5[pl2],paste0(">",substr(rf5[pl2],st_start[1]+1,st_end[1]-1),"<"),
                           paste0(">",as.character(pl2_dens[Unit==Unit_i]$Density),"<"))
      rf5[pl2] <- newln
      #update the matrix with planting %
      pl_comp <- PlantPCDat[Unit==Unit_i & GM =="M",.(Init.Values ="PlantingProp_2",Amabalis_Fir, Black_Cottonwood,
                                                      Hybrid_spruce,Lodgepole_Pine, Paper_Birch,Subalpine_Fir,
                                                      Trembling_Aspen,Western_Hemlock, Western_redcedar)]
      pl_comp <- melt(pl_comp,measure = 2:10) #hard coded to number of species that are in the table
      grouptext <-"pl_atpVal"
      pl2 <- grep(grouptext,rf5)
      pl2_c <- split(pl2,cumsum(c(1, diff(pl2) != 1)))
      lni_mc <- pl2_c[2]$`2` #get the second chunk
      sp_list <- c("Amabalis_Fir","Black_Cottonwood","Hybrid_spruce","Lodgepole_Pine","Paper_Birch",
                   "Subalpine_Fir","Trembling_Aspen","Western_Hemlock","Western_redcedar")
      for(iii in 1:length(sp_list)){
        lni_mc1 <- lni_mc[grep(sp_list[iii],rf5[lni_mc[1]:max(lni_mc)])]
        st_start <- str_locate(rf5[lni_mc1],">")
        st_end <- str_locate(rf5[lni_mc1],"</")
        newln <- str_replace(rf5[lni_mc1],paste0(">",substr(rf5[lni_mc1],st_start[1]+1,st_end[1]-1),"<"),
                             paste0(">",as.character(pl_comp[variable==sp_list[iii]]$value),"<"))
        rf5[lni_mc1] <- newln
      }
      rf5 <- gsub("//", "\\\\", rf5)
      writeLines(rf5,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
    }
  }

}



















