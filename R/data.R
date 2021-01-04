#' Cover class data for Bromus Tectorum and Artemisia Tridentata.
#'
#' A dataset containing presence, absence, and abundance data for Bromus
#' Tectorum and Artemisia Tridentata for 877 locations. The dataset is
#' transformed from the original wide format to a long format with species
#' moved to a single column.
#'
#' @format A data frame with 1754 rows and 6 variables:
#' \describe{
#'   \item{Species}{Name of the plant species}
#'   \item{Cover Class}{Level of abundance as measured by a cover class; 0 represents absence while values 1 - 6 represent daubenmire scale increments}
#'   \item{Fire}{Whether or not fire was previously present in this area}
#'   \item{Topography}{Gradient of the area in which the plants were measured}
#'   \item{Dist. to Road}{Distance to the nearest road measured in meters}
#'   \item{Dist. to Bound}{Distance to the nearest boundary measured in meters}
#' }
#' @source \url{https://doi.org/10.1007/s13253-016-0265-2}
"sagebrush"
