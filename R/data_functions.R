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

  ## Data Composition - Common Components
  N <- nrow(df)
  y <- all.vars(abundance_formula)[1]
  print(validate_cutpoint(cutpoint_scheme))
  if(validate_cutpoint(cutpoint_scheme)) {
    c <- cutpoint_scheme
  }

  K <- length(cutpoint_scheme) + 1

  ## Data Composition - Presence
  presence_matrix <- model.matrix(lme4::nobars(presence_formula), data = df)
  Kp <- ncol(presence_matrix)

  ## Data Composition - Presence Random Effects
  presence_bars <- findbars(presence_formula)
  presence_random_matrix <- mkReTrms(presence_bars, presence_matrix)
  Zp <- as.matrix(t(presence_random_matrix$Zt))
  Qp <- ncol(Zp)

  ## Data Composition - Abundance
  abundance_matrix <- model.matrix(lme4::nobars(abundance_formula), data = df)
  Ka <- ncol(abundance_matrix)

  ## Data Composition - Abundance Random Effects
  abundance_bars <- findbars(abundance_formula)
  abundance_random_matrix <- mkReTrms(abundance_bars, abundane_matrix)
  Za <- as.matrix(t(abundance_random_matrix$Zt))
  Qa <- ncol(Za)

  list(
    N = N,
    K = K,
    c = c,
    y = y,
    Kp = Kp,
    Xp = presence_matrix,
    Ka = Ka,
    Xa = abundance_matrix,
  )
}
