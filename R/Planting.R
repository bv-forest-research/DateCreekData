#' Initiate Date Creek planting
#'
#' @param My_newvalsPath
#' @param PlantDensDat
#' @param PlantPCDat
#' @param Plant_param_name
#' @param InitValues_csv_end
#' @param update_plant_pos_only
#'
#' @return
#' @export
#'
#' @examples
InitPlant <- function(My_newvalsPath,PlantDensDat,PlantPCDat, Plant_param_name,
                      InitValues_csv_end, update_plant_pos_only){
  if(update_plant_pos_only == TRUE){
    harvested_treats <-c("A2", "B5", "C3", "D5", "B2", "B3", "C2", "D4",
                         "A1", "A3", "B4", "D2")
    for(i in 1:length(harvested_treats)){
      pv <- fread(paste0(My_newvalsPath,harvested_treats[i],InitValues_csv_end))
      old_plant <- as.character(pv[grep(paste0("Plant","*[[:digit:]]"), pv[[1]]),1])
      pv[[1]] <- str_replace(pv[[1]],old_plant,Plant_param_name)
      write.csv(pv,paste0(My_newvalsPath,harvested_treats[i],InitValues_csv_end),
                quote=TRUE,row.names=FALSE)
    }
  }else{ #add the planting
    pl_dens <- PlantDensDat[,.(Init.Values ="PlantingDens", Amabalis_Fir=sum(.SD)),by=c("Unit","GM")]
    ## Date Creek Treatments
    NH<-c("A4", "B1", "C1", "D3") #No harvest
    LR<-c("A2", "B5", "C3", "D5") # light removal (30% BasalArea)
    HR<-c("B2", "B3", "C2", "D4") #heavy removal (60% BasalArea)
    CC<-c("A1", "A3", "B4", "D2") #Clear-cut (100% removal) with some caveats (some deciduous left standing, one small

    # Reading in unit boundaries and creating shapefiles group by removal class
    Blocks <- c(NH,LR,HR,CC)

    for(j in 1:length(Blocks)){
      Unit_i <- Blocks[j]
      TreatType <- ifelse(Unit_i=="A2"||Unit_i=="B5"||Unit_i== "C3"||Unit_i== "D5","LR",
                          ifelse(Unit_i=="B2"||Unit_i=="B3"||Unit_i=="C2"||Unit_i=="D4","HR",
                                 ifelse(Unit_i=="A1"||Unit_i=="A3"||Unit_i=="B4"||Unit_i=="D2","CC","NH")))
      #For the harvested treatments:
      if(TreatType=="CC"||TreatType=="HR"||TreatType=="LR"){
        d <- fread(paste0(My_newvalsPath,Unit_i,InitValues_csv_end))
        setnames(d, 1,"Init.Values")
        if(TreatType=="HR"){
          ### For heavy removal (low retention), two plants: 1st in the gaps, 2nd in matrix
          e <- rbind(d,data.table(Init.Values=Plant_param_name,Amabalis_Fir=NA),fill=TRUE)
          pd1 <- rbind(e,PlantPCDat[Unit==Blocks[j]& GM =="G",
                                    .(Init.Values ="PlantingProp_1",Amabalis_Fir, Black_Cottonwood,
                                      Hybrid_spruce,Lodgepole_Pine, Paper_Birch,Subalpine_Fir,
                                      Trembling_Aspen,Western_Hemlock, Western_redcedar)])

          ff <- rbind(pd1,pl_dens[Unit==Blocks[j]& GM=="G",
                                  .(Init.Values="PlantingDens_1",Amabalis_Fir)],fill=TRUE)
        }else{
          ### For Clearcuts and low removal (high retention), just one planting
          #Add the planting
          e <- rbind(d,data.table(Init.Values=Plant_param_name,Amabalis_Fir=NA),fill=TRUE)
          pd1 <- rbind(e,PlantPCDat[Unit==Blocks[j],.(Init.Values ="PlantingProp",Amabalis_Fir, Black_Cottonwood,
                                                      Hybrid_spruce,Lodgepole_Pine, Paper_Birch,Subalpine_Fir,
                                                      Trembling_Aspen,Western_Hemlock, Western_redcedar)])
          ff <- rbind(pd1,pl_dens[Unit==Blocks[j],.(Init.Values,Amabalis_Fir)],fill=TRUE)
        }
      }else{ #not harvested
        ff <- fread(paste0(My_newvalsPath,Unit_i,InitValues_csv_end))
        setnames(ff, 1,"Init.Values")
      }
      #Drop the column name from column 1
      setnames(ff, "Init.Values"," ")
      #write out the parameter value updates to file
      write.csv(ff,paste0(My_newvalsPath,Unit_i,InitValues_csv_end),quote=TRUE,row.names=FALSE)
    }
  }

}
