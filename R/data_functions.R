#' NPS Data Format Transformation
#'
#' @param nps_data Tibble containing NPS Formatted Data
#' @param covariate_cols Columns which refer to covariates such as Year, Location, Grade, etc.
#'
#' @return Long format tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   pivot_nps_data(nps_data_ex, c(Year, Loc_Name, SCOSA))
#' }
pivot_nps_data <- function(nps_data, covariate_cols = NULL){
  tidyr::pivot_longer(
    data = nps_data,
    cols = -c({{ covariate_cols }}),
    names_to = 'Species',
    values_to = 'Cover Class'
    ) %>%
    dplyr::mutate(
      `Cover Class` = forcats::fct_rev(forcats::as_factor(`Cover Class`))
      )
}


