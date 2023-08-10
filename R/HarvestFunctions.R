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
      for(iii in 1:4){
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
      rf2 <- gsub("//", "\\\\", rf2)
      writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
    }
  }
}


#' Add Spatial Harvest Function with brushing
#' @description This function differs from UpdateHarvestsFn where we used brushing. This function retains
#' deciduous trees in the clearcuts, with each species retained in different % in each block.
#' I added the snags harvest to HR and CC, so needed to update the 3rd harvest with gap pattern in HR
#' @param xmlsPath string - directory where the parameter files are located
#' @param xml_names names of files to update (does not include path)
#' @param Units_path string - directory where the spatial files for each unit is located
#' @param Gaps_path string - directory where the spatial files for the gap curs are located
#' @param ParamFile_Suffix string - what is the ending of the parameter files - represents a given parameter file update
#' @param Units default = "all", else which units to update harvest boundaries (a vector of characters)?
#'
#' @return
#' @export
#'
#' @examples
AddHarvestWbrush <- function(xmlsPath, xml_names, Units_path, Gaps_path,
                             Units="all"){

  if(any(Units == "all")){
    Units_to_Update <- DateCreekData::Treatments[Treatment !="NH"]$Unit
  }else{
    Units_to_Update <- Units
  }

  ########################## 1. Read in spatial harvests #############################
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

  ########################## 2. Update spatial harvests #############################
  #Update the parameter files with Spatial harvests and plants
  for(i in 1:length(xml_names)){
    #which treatment is getting updated
    #maybe turn this into a function one day?
    up <- gregexpr(paste(Units_to_Update, collapse = "|"), xml_names[i])[[1]][1]
    Unit_i <- substr(xml_names[i], up,up+1)
    tp <- gregexpr(paste(c("NH","CC","HR","LR"), collapse="|"), xml_names[i])[[1]][1]
    TreatType <- substr(xml_names[i], tp,tp+1)
    print(paste("Updating:",TreatType,unn))

    ##### Convert the harvest and planting locations to SORTIE
    if(!dir.exists(paste0(Units_path,"Harvest_PlantLocations"))) {
      dir.create(paste0(Units_path,"Harvest_PlantLocations"),
                 showWarnings=FALSE)
      # make the directory and do the conversion if it doesn't already exist

      #this function doesn't work
      read_write_harvests_xmls(Units_path, bounds, partCuts, Unit_i, PlotSize)

    }else{
      #if the directory already exists, just use the conversion already there
      HarvPlanLocs_Path <- paste0(Units_path,"Harvest_PlantLocations/")
      print("Harvest and plant location file directory exists")
    }

    res <- xml2::read_xml(paste0(xmlsPath,xml_names[i]))
    xml2::write_xml(res, "temp.xml")
    tmp <- readLines("temp.xml", encoding="UTF-8")
    Basexml <- gsub("\\\\", "//",tmp)
    HarvLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Harv.xml"))
    PlantLines <- readLines(paste0(HarvPlanLocs_Path,TreatType,"_",Unit_i,"_Plant.xml"))


    harv_text <- "ha_applyToCell"  #get the name of the group we are replacing
    plant_text <- "pl_applyToCell"  #get the name of the group we are replacing

    if(TreatType == "LR"){
      lr_harvest

    }else if(TreatType == "CC"){


    }else{
      ######## HEAVY REMOVAL #########

    }
  }

}

cc_harvest <- function(Basexml){

  #Current harvest function ---------------------------------------------------------------
  #all brushing the same - happens once at year 10
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

  #### BRUSHING ####
  # which "harvests" == brushing (timing is already in file)
  if(Unit_i=="A3"|Unit_i=="B4"){

    brush(Basexml, numBrush = c(2,3),timeBrush = c(7,8))

  }else if(Unit_i == ""){

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
  rt_amt[1] <- DateCreekData::Deciduous_retention[Unit == Unit_i &
                                     Species == "At"]$Harvest_Intensity*100
  rt_amt[2] <- DateCreekData::Deciduous_retention[Unit == Unit_i &
                                     Species == "Ac"]$Harvest_Intensity*100
  rt_amt[3] <- DateCreekData::Deciduous_retention[Unit == Unit_i &
                                     Species == "Ep"]$Harvest_Intensity*100

  #loop through the deciduous trees
  for(ix in 1:3){
    lni <- grep("ha_amountToCut",rf2)[ix+1] #added deciduous harvest in position 2-4
    st_start <- str_locate(rf2[lni],">")
    st_end <- str_locate(rf2[lni],"</")
    newln <- str_replace(rf2[lni],paste0(">",substr(rf2[lni],st_start[1]+1,st_end[1]-1),"<"),
                         paste0(">",as.character(rt_amt[ix]),"<"))
    rf2[lni] <- newln
  }

  rf2 <- gsub("//", "\\\\", rf2)
  writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))

}

hr_harvest <- function(Basexml){

  #Current brushing file----------------------------------------------------------
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

  rf2 <- gsub("//", "\\\\", rf2)
  writeLines(rf2,paste0(NewxmlPath,"ICH-",TreatType,"-",Unit_i,ParamFile_Suffix))
}


lr_harvest <- function(Basexml){


  # CUrrent brushing file ------------------------------------------------
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




