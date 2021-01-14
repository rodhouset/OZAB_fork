#' Fit OZAB Model
#'
#' @param df Tibble containing Species, Cover Class, and Covariates
#' @param presence_formula Formula for Specifying Presence / Absence
#' @param abundance_formula Formula for Specifying Abundance
#' @param cutpoint_scheme Vector of Cutpoint Scheme Used
#' @param link_function Link Function to Use; Options are logit and probit; Defaults to logit
#' @param ... Additional Parameters to be Passed to Stan
#'
#' @return Stan Object Fit
#' @export
#'
#' @examples
ozab <- function(df, presence_formula, abundance_formula, cutpoint_scheme, link_function = 'logit', ...){

  data <- compose_ozab_data(df, presence_formula = presence_formula, abundance_formula = abundance_formula, cutpoint_scheme = cutpoint_scheme)

  if(link_function == 'logit'){
    result <- rstan::sampling(stanmodels$OZAB_model_logit, data = data, ...)
  } else if(link_function == 'probit'){
    result <- rstan::sampling(stanmodels$OZAB_model_probit, data = data, ...)
  } else {
    stop('Supported link functions are "logit" and "probit"')
  }

  ## Reconstruct names
  presence_names <- attr(terms(presence_formula), 'term.labels')
  if(attr(terms(presence_formula), 'intercept')){
    presence_names <- c('intercept', presence_names)
  }
  presence_names <- paste0('presence_', presence_names)

  result@sim$fnames_oi[1:data$Kp] <- presence_names

  abundance_names <- attr(terms(abundance_formula), 'term.labels')
  if(attr(terms(abundance_formula), 'intercept')){
    abundance_names <- c('intercept', abundance_names)
  }
  abundance_names <- paste0('abundance_', abundance_names)

  result@sim$fnames_oi[(data$Kp + 1):(data$Kp + data$Ka)] <- abundance_names

  return(result)
}
