
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
