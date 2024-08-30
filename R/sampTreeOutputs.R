
#'subplot_outputs for trees
#'
#'
#' @export
#'
#'
#' @param det_out_path
#' @param out_path where to locate the subplotted outputs
#' @param run_name
#' @param Units_path
#' @param yrs
#' @param Units_to_output a vector of character names for which units to include for subplotting outputs
#' @param dist_edge [numeric()] how far from unit boundary to allow subplots (in m)
#' @param num_subplots [numeric()] how many subplots
#' @param size_subplot [numeric()] radius of plot (standard is 7.98m)
#' @param plotting TRUE/FALSE - whether or not to display plots with the unit and subplot location
#'
#'
#' @description
#' simplfy this so that we can pass a single unit instead of all at once
#'
subplot_outputs <- function(det_out_path, out_path, run_name, Units_path, yrs,
                            subplot_type = multiple , Units_to_output = "all",
                            dist_edge = 20, num_subplots = 30, size_subplot = 7.98,
                            plotting = TRUE){

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
    }else if(TreatType == "CC"){
      NameEnd <- paste0(run_name,"_cc_det_")
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

      file_to_read <- paste0(det_out_path,"ext_ICH-",TreatType,"-",Unit_i,NameEnd,yrs[i])

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

