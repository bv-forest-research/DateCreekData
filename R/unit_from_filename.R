




#' Title
#'
#' @param file_names
#'
#' @return
#' @export
#'
#' @examples
unit_from_filename <- function(file_names){

  for(i in 1:length(file_names)){
    as.numeric(stringr::str_split(stringr::str_split(basename(file_names[i]),
                                   DateCreekData::Treatments$Unit, simplify = T)[,2],
                                   "-ds",simplify=TRUE)[,1])
    yr <- as.numeric(str_split(basename(out_files[i]), "det_", simplify = T)[,2])
    dt[, ':='(timestep = yr,unit=plotID)]
    dt_table <- rbind(dt_table,dt)
    print(paste("reading plot",plotID,"year",yr))
  }

  parFile <- grep(DateCreekData::Treatments$Unit[i],
                  file_names, value = TRUE)#, value = TRUE)

}

