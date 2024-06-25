
#' Calculate stems per hectare from prism plots
#'
#' @param pBands which prism band used (numeric)
#' @param DBH diameter of tree
#' @return The equivalent stems/ha of a given diameter
#' @export
calculateSPH <- function(pBands, DBH){
  BAPT <- pi*(DBH/200)^2 ##basal area per tree
  SPH <- pBands/BAPT
  return(SPH)
}
