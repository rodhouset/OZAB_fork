#' Fit OZAB Model
#'
#' @param df Tibble containing Species, Cover Class, and Covariates
#' @param presence_formula Formula for Specifying Presence / Absence
#' @param abundance_formula Formula for Specifying Abundance
#' @param cutpoint_scheme Vector of Cutpoint Scheme Used
#' @param ... Additional Parameters to be Passed to Stan
#'
#' @return Stan Object Fit
#' @export
#'
#' @examples
ozab <- function(df, presence_formula, abundance_formula, cutpoint_scheme, ...){

  ## Get Names
  presence_vector_name <- all.vars(presence_formula)[1]
  abundance_vector_name <- all.vars(abundance_formula)[1]

  ## Data Composition
  y <- as.numeric(df[[presence_vector_name]]) * as.numeric(df[[abundance_vector_name]])
  N <- length(y)
  presence_matrix <- as.matrix(modelr::model_matrix(df, presence_formula))
  Kp <- ncol(presence_matrix)
  abundance_matrix <- as.matrix(modelr::model_matrix(df, abundance_formula))
  Ka <- ncol(abundance_matrix)
  c <- cutpoint_scheme
  K <- length(c) + 1

  data <- list(
    N = N,
    K = K,
    c = c,
    y = y,
    Kp = Kp,
    Xp = presence_matrix,
    Ka = Ka,
    Xa = abundance_matrix
  )

  result <- rstan::sampling(stanmodels$OZAB, data = data, ...)

  return(result)
}
