

#' Add Spatial Harvest Function with brushing
#' @description This function differs from UpdateHarvestsFn where we used brushing. This function retains
#' deciduous trees in the clearcuts, with each species retained in different % in each block.
#' I added the snags harvest to HR and CC, so needed to update the 3rd harvest with gap pattern in HR
#' @param NewxmlPath string - directory where the newly created parameter files are located
#' @param Units_path string - directory where the spatial files for each unit is located
#' @param Gaps_path string - directory where the spatial files for the gap curs are located
#' @param file_names string - vector of parameter file name(s)
#' @param Units default = "all", else which units to update harvest boundaries (a vector of characters)?
#'
#' @return
#' @export
#'
#' @examples
apply_harvest <- function(NewxmlPath, Units_path, Gaps_path,
                             file_names, Units="all"){

  if(any(Units == "all")){
    Units_to_Update <- DateCreekData::Treatments[Treatment !="NH"]$Unit
  }else{
    Units_to_Update <- Units
  }



  ########################## 1. Read in spatial harvests #############################
  bounds <- ReadSpatialBounds(Units_path = Units_path)
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

  ########################## 2. Update spatial harvests #############################
  if(!dir.exists(paste0(Units_path,"Harvest_PlantLocations"))) {
    dir.create(paste0(Units_path,"Harvest_PlantLocations"),
               showWarnings=FALSE)}

  HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations")
  hpb_file_names <- list.files(path = HarvPlanLocs_Path)
  elements_to_check <- DateCreekData::Treatments$Unit

  #do we need this? shouldn't it always process - I think this was the start of the re-implementation
  #of not always writing x y locations to file.
  elements_in_files <- all(sapply(elements_to_check, function(element) {
    any(grepl(element, hpb_file_names))
  }))

  #Update the parameter files with Spatial harvests and plants
  if(elements_in_files == FALSE){
    for(i in 1:length(Units_to_Update)){
      Unit_i <- Units_to_Update[i]
      TreatType <- DateCreekData::Treatments[Unit == Unit_i]$Treatment
      print(paste("Updating",TreatType,Unit_i))

      ##### Convert the harvest and planting locations to SORTIE
      #this doesn't work if you need to make
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")

      #Set pathway
      #HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
      bb <- st_bbox(st_buffer(bounds %>% filter(Unit==Unit_i), dist = 10))
      Max_length <- plyr::round_any(max(abs(bb[1]-bb[3]),abs(bb[2]-bb[4])), accuracy=8,f=ceiling)
      PlotSize <- raster::raster(xmn=bb[1], xmx=bb[1]+Max_length, ymn=bb[2],ymx=bb[2]+Max_length,
                                 resolution=8, crs = raster::crs(bounds))

      if(TreatType=="LR"|TreatType=="CC"){
        PlotRast <- fasterize::fasterize(bounds %>% filter(Unit==Unit_i),PlotSize,
                                         field="Harvest", background=0)
      }else {
        PlotRast <- fasterize::fasterize(partCuts %>% filter(Id==Unit_i) ,PlotSize,
                                         field="Harvest", background=0)
      }

      if(TreatType != "CC"){
        for(ii in 1:raster::ncell(PlotRast)){
          if (raster::values(PlotRast)[ii] > 0){
            locCol <- raster::colFromCell(PlotRast,ii)
            locRow <- raster::rowFromCell(PlotRast,ii)
            h1 <- paste0("<ha_applyToCell x=\"",locCol-1,"\" y=\"", nrow(PlotRast)-locRow,"\"/>")
            write(h1, file= paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"),append=TRUE)
            p1 <- stringr::str_replace(h1,"ha","pl")
            write(p1, file=paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"),append=TRUE)
          }
        }
      }

      if(TreatType == "HR"){
        MatrixHarv <- fasterize::fasterize(bounds %>% filter(Unit == Unit_i),PlotSize,
                                           field="Harvest", background=0)
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

      if(TreatType == "CC"){
        for(ii in 1:raster::ncell(PlotRast)){
          if (raster::values(PlotRast)[ii] > 0){
            locCol <- raster::colFromCell(PlotRast,ii)
            locRow <- raster::rowFromCell(PlotRast,ii)
            h1 <- paste0("<ha_applyToCell x=\"",locCol-1,"\" y=\"", nrow(PlotRast)-locRow,"\"/>")
            write(h1, file= paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"),append=TRUE)
            p1 <- stringr::str_replace(h1,"ha","pl")
            write(p1, file=paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"),append=TRUE)
            b1 <- stringr::str_replace(h1,"ha","ds")
            write(b1, file=paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Brush.xml"),append=TRUE)
          }
        }
      }

      #}else{
      #if the directory already exists, just use the conversion already there
      #HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
      #print("Harvest and plant location file directory exists")
      #}

      res <- xml2::read_xml(paste0(NewxmlPath,
                                   grep(Units_to_Update[i],file_names, value = TRUE)))

      #res <- xml2::read_xml(paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
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
        x <- rsortie::removeRow(lnm, rf2)
        rf2 <- append(x,HarvLines,lnm[1]-1)

        ##### PLANTING #####
        #Do the same for plantings
        lni <- grep(plant_text,rf2) #get the location of planting from base .xml
        lnm <- c(lni[1],tail(lni,n=1))
        x <- rsortie::removeRow(lnm, rf2)

        rf2 <- append(x,PlantLines,lnm[1]-1)
        rf2 <- gsub("//", "\\\\", rf2)
        #write the new parameter file out
        #writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        writeLines(rf2,paste0(NewxmlPath,
                              grep(Units_to_Update[i],file_names, value = TRUE)))

      }else if(TreatType == "CC"){
        BrushLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Brush.xml"))
        brush_text <- "ds_applyToCell" #get the name of the group we are replacing


        ######## CLEAR CUTS #########
        rf2 <- Basexml

        #### HARVESTS ####
        ### Update all harvests with the right pattern
        numHarvs <- length(split(grep(harv_text,rf2),cumsum(c(1, diff(grep(harv_text,rf2)) != 1))))
        for(iii in 1:numHarvs){
          lni <- split(grep(harv_text,rf2),cumsum(c(1, diff(grep(harv_text,rf2)) != 1)))
          lnm <- c(lni[[iii]][1],tail(lni[[iii]],n=1)) #get just the first and last element
          x <- rsortie::removeRow(lnm, rf2)
          rf2 <- append(x,HarvLines,lnm[1]-1)
        }

        #### PLANTING ####
        lni <- grep(plant_text,rf2) #get the location of planting
        lnm <- c(lni[1],tail(lni,n=1))
        x <- rsortie::removeRow(lnm, rf2)
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
          lni <- grep("ha_amountToCut",rf2)[ix+1] #added deciduous harvest in position 2-4 - super duper hard coded!!!!
          st_start <- stringr::str_locate(rf2[lni],">")
          st_end <- stringr::str_locate(rf2[lni],"</")
          newln <- stringr::str_replace(rf2[lni],paste0(">",substr(rf2[lni],st_start[1]+1,st_end[1]-1),"<"),
                                        paste0(">",as.character(rt_amt[ix]),"<"))
          rf2[lni] <- newln
        }

        #### BRUSHINGS ####
        ### Update all brushing with the right pattern
        numBrush <- length(split(grep(brush_text,rf2),cumsum(c(1, diff(grep(brush_text,rf2)) != 1))))
        for(iii in 1:numBrush){
          lni <- split(grep(brush_text,rf2),cumsum(c(1, diff(grep(brush_text,rf2)) != 1)))
          lnm <- c(lni[[iii]][1],tail(lni[[iii]],n=1)) #get just the first and last element
          x <- rsortie::removeRow(lnm, rf2)
          rf2 <- append(x,BrushLines,lnm[1]-1)
        }


        rf2 <- gsub("//", "\\\\", rf2)
        #writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        writeLines(rf2,paste0(NewxmlPath,
                              grep(Units_to_Update[i],file_names, value = TRUE)))

      }else{
        ######## HEAVY REMOVAL #########
        #if it's heavy removal, gap cuts + matrix removal + gap planting + matrix planting
        #### Map the non-gap matrix for heavy removal treatments
        #Forest size
        rf2 <- Basexml
        lni <- grep(harv_text,rf2) #get the location of harvest from base .xml

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
        x <- rsortie::removeRow(lnm, Basexml)
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
        x <- rsortie::removeRow(lnm, rf2)
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
        x <- rsortie::removeRow(lnm, rf2)
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
        x <- rsortie::removeRow(lnm, rf2)
        MatrixLine_Plant <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"Mat_pl.xml"))
        rf2 <- append(x,MatrixLine_Plant,lnm[1]-1)

        #update the heavy harvest matrix with planting dens
        PlantDensDat <- data.table::fread("D:/Github/DateCreekData/data-raw/Harvests_Plants/PlantingDensities.csv")
        PlantPCDat <- data.table::fread("D:/Github/DateCreekData/data-raw/Harvests_Plants/PlantingPercents.csv")

        pl2_dens <- PlantDensDat[GM=="M",.(Density=sum(.SD)),by=c("Unit","GM")]
        grouptext <-"pl_distanceOrDensity"
        pl2 <- grep(grouptext,rf2)[2] #just the second planting density
        st_start <- stringr::str_locate(rf2[pl2],">")
        st_end <- stringr::str_locate(rf2[pl2],"</")
        #and replace the value
        newln <- stringr::str_replace(rf2[pl2],paste0(">",substr(rf2[pl2],st_start[1]+1,st_end[1]-1),"<"),
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
          st_start <- stringr::str_locate(rf2[lni_mc1],">")
          st_end <- stringr::str_locate(rf2[lni_mc1],"</")
          newln <- stringr::str_replace(rf2[lni_mc1],paste0(">",substr(rf2[lni_mc1],st_start[1]+1,st_end[1]-1),"<"),
                                        paste0(">",as.character(pl_comp[variable==sp_list[iii]]$value),"<"))
          rf2[lni_mc1] <- newln
        }

        #snag harvest
        #lni <- grep(harv_text,rf2)
        #lni_grs <- split(lni,cumsum(c(1, diff(lni) != 1)))
        #lni_snag <- lni_grs$`3` #first harvest chunk
        #StartCuts <- lni_snag[1]
        #EndCuts <- c()
        #for(ii in 1:(length(lni_snag)-1)){
        #  if(lni_snag[ii+1]==(lni_snag[ii]+1)){ #use while
        #    EndCuts <- lni_snag[ii+1]
        #  }else{
        #    EndCuts <- lni_snag[ii]
        #  }
        #}
        #lnm <- c(StartCuts,EndCuts)
        #x <- rsortie::removeRow(lnm, rf2)
        #bring in the gaps
        #rf2 <- append(x,HarvLines,lnm[1]-1)

        rf2 <- gsub("//", "\\\\", rf2)
        #writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
        writeLines(rf2,paste0(NewxmlPath,
                              grep(Units_to_Update[i],file_names, value = TRUE)))
      }
    }
    print("Harvest and plant location file directory exists")
  }


}
