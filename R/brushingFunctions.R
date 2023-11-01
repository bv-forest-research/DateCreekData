
#' Title
#'
#' @param Basexml
#' @param numBrush a vector of the harvest numbers that correspond to brushing
#' @param timeBrush Which year(s) to brush
#' @param amtBrush % of trees to brush
#'
#' @return
#' @export
#'
#' @details
#' could add another parameter to define lower and upper limit of brushing
#'
#'
#' @examples
brush <- function(Basexml, numBrush, timeBrush, amtBrush = 98){
  #set the years for brushing
  tb <- grep("ha_timestep",Basexml) #all timesteps
  ab <- grep("ha_amountToCut", Basexml)
  for(iii in 1:length(numBrush)){
    #timing of brushing -------------
    #assumes brushing always starts in the second harvest
    st_start <- stringr::str_locate(Basexml[tb][numBrush[iii]],">")
    st_end <- stringr::str_locate(Basexml[tb][numBrush[iii]],"</")
    newln <- stringr::str_replace(Basexml[tb][numBrush[iii]],
                         paste0(">",substr(Basexml[tb][numBrush[iii]],
                                           st_start[1]+1,st_end[1]-1),"<"),
                         paste0(">",as.character(timeBrush[iii]),"<"))
    Basexml[tb][numBrush[iii]] <- newln

    #amount to brush---------------
    if(amtBrush == 98){
      print("98% brushing")
    }else{
      st_start <- stringr::str_locate(Basexml[ab][numBrush[iii]],">")
      st_end <- stringr::str_locate(Basexml[ab][numBrush[iii]],"</")
      newln <- stringr::str_replace(Basexml[ab][numBrush[iii]],
                              paste0(">",substr(Basexml[ab][numBrush[iii]],
                                              st_start[1]+1,st_end[1]-1),"<"),
                              paste0(">",as.character(amtBrush[iii]),"<"))
      Basexml[ab][numBrush[iii]] <- newln
    }

  }

  return(Basexml)

}

actual_brush <- function(){
  if(Unit_i=="A3"|Unit_i=="B4"){  # if it's a clearcut, Single harvest and plant, but include brushing
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
}
