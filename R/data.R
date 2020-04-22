#' Reference table for biomarkers contribution to BHS
#'
#'A reference table to calculate Biological Health Scores (BHS)
#'
#' @format A reference table to calculate Biological Health Scores (BHS)
#' \describe{
#'   \item{Biomarker}{Biomarker names}
#'   \item{System}{System biomarker belongs to}
#'   \item{HarmfulInExcess?}{Whether biomarker is considered to be harfmul in excess or not
#'   Possible values are 1 (Yes), 0 (No), -1 (Not applicable), NA (Not considered)}
#' }
#'
"bio.dict"

#' Example of clean biomarker data frame
#'
#' Toy biomarker dataframe containing measurements for ~2000 individuals (not real data)
#'
"bio.example"

#' Example of clean covariate data frame
#'
#' Toy covariate dataframe containing measurements for ~2000 individuals (not real data)
#'
"cov"


#' Example of raw biomarker set
#'
#' A dataset containing biomarker measurement with code-like numbers for biomarkers
#' as well as measurement at two time points
#'
"bio.original"
