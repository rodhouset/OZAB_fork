#' Fit OZAB Model
#'
#' @param df Tibble containing Species, Cover Class, and Covariates
#' @param presence_formula Formula for Specifying Presence / Absence
#' @param abundance_formula Formula for Specifying Abundance
#' @param cutpoint_scheme Vector of Cutpoint Scheme Used
#'
#' @return Stan Object Fit
#' @export
#'
#' @examples
ozab <- function(df, presence_formula, abundance_formula, cutpoint_scheme, ...){

  ## Data Composition
  cover_class_vector <- df[all.vars(presence_formula)[1]][[1]] ## Known bug, make the response index dynamic not fixed
  N <- length(cover_class_vector)
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
    y = cover_class_vector,
    Kp = Kp,
    Xp = presence_matrix,
    Ka = Ka,
    Xa = abundance_matrix
  )

  result <- rstan::sampling(stanmodels$OZAB, data = data, chains = 1)

  return(result)
}
