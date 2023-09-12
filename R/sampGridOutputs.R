

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

