% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HarvestFunctions.R
\name{AddHarvestWbrush}
\alias{AddHarvestWbrush}
\title{Add Spatial Harvest Function with brushing}
\usage{
AddHarvestWbrush(
  NewxmlPath,
  Units_path,
  Gaps_path,
  ParamFile_Suffix,
  Units = "all"
)
}
\arguments{
\item{NewxmlPath}{string - directory where the newly created parameter files are located}

\item{Units_path}{string - directory where the spatial files for each unit is located}

\item{Gaps_path}{string - directory where the spatial files for the gap curs are located}

\item{ParamFile_Suffix}{string - what is the ending of the parameter files - represents a given parameter file update}

\item{Units}{default = "all", else which units to update harvest boundaries (a vector of characters)?}
}
\description{
This function differs from UpdateHarvestsFn where we used brushing. This function retains
deciduous trees in the clearcuts, with each species retained in different \% in each block.
I added the snags harvest to HR and CC, so needed to update the 3rd harvest with gap pattern in HR
}
