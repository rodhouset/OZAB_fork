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
#' @importFrom rlang .data
pivot_nps_data <- function(nps_data, covariate_cols = NULL){
  tidyr::pivot_longer(
      data = nps_data,
      cols = -c({{ covariate_cols }}),
      names_to = 'Species',
      values_to = 'Cover Class'
    ) %>%
    dplyr::mutate(
      `Cover Class` = forcats::fct_rev(forcats::as_factor(.data$`Cover Class`))
      )
}

#' Add Presence Column
#'
#' @param df Tibble containing Cover Class Data
#' @param cover_class_col Column Name for Cover Class Data
#' @param absence_value Value for Absence -- Assumed to be Zero
#'
#' @return Provided tibble with column of logicals indicating presence / absence appended
#' @export
#'
#' @examples
#' sagebrush %>%
#'   add_presence()
#' @importFrom rlang .data
add_presence <- function(df, cover_class_col = .data$`Cover Class`, absence_value = 0){
  df %>%
    dplyr::mutate(Presence = ifelse({{ cover_class_col }} == absence_value, FALSE, TRUE))
}

compose_ozab_data <- function(df, presence_formula, abundance_formula, cutpoint_scheme){

  ## Check if response of presence_formula is in dataframe

  presence_response_col <- all.vars(presence_formula)[1]
  if(!(presence_response_col %in% names(df))){
    stop(glue::glue('Response of presence-absence formula, { presence_response_col }, not found in provided data'))
  }

  ## Check if response of abundance_formula is in dataframe

  abundance_response_col <- all.vars(abundance_formula)[1]
  if(!(abundance_response_col %in% names(df))){
    stop(glue::glue('Response of abundance formula, { abundance_response_col }, not found in provided data'))
  }

  ## Make sure a single column is not response for both abundance and presence formula

  if(presence_response_col == abundance_response_col){
    stop('Response columns of abundance and presence-absence cannot be identical')
  }

  ## Make sure presence_column only has two levels
  ## TODO

  ## Data Composition
  y <- as.numeric(df[[presence_response_col]]) * as.numeric(df[[abundance_response_col]])
  N <- length(y)
  presence_matrix <- as.matrix(modelr::model_matrix(df, presence_formula))
  Kp <- ncol(presence_matrix)
  abundance_matrix <- as.matrix(modelr::model_matrix(df, abundance_formula))
  Ka <- ncol(abundance_matrix)
  c <- cutpoint_scheme
  K <- length(c) + 1

  list(
    N = N,
    K = K,
    c = c,
    y = y,
    Kp = Kp,
    Xp = presence_matrix,
    Ka = Ka,
    Xa = abundance_matrix
  )
}
