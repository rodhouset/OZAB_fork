#' Fit OZAB Model
#'
#' @param df Tibble containing Species, Cover Class, and Covariates
#' @param presence_formula Formula for Specifying Presence / Absence
#' @param abundance_formula Formula for Specifying Abundance
#' @param cutpoint_scheme Vector of Cutpoint Scheme Used
#' @param link_function Link Function to Use; Options are logit and probit; Defaults to logit
#' @param ... Additional Parameters to be Passed to Stan
#' @param prior_presence_mean A vector of mean values for prior presence coefficients
#' @param prior_abundance_mean A vector of mean values for prior abundance coefficients
#' @param prior_presence_var A vector of variance values for prior presence coefficients
#' @param prior_abundance_var A vector of variance values for prior abundance coefficients
#'
#' @return Stan Object Fit
#' @export
#'
#' @examples
ozab <- function(df, presence_formula, abundance_formula, cutpoint_scheme, link_function = 'logit', ..., prior_presence_mean = 0, prior_abundance_mean = 0, prior_presence_var = 10, prior_abundance_var = 10){

  ## Compose Data
  data <- compose_ozab_data(df, presence_formula = presence_formula, abundance_formula = abundance_formula, cutpoint_scheme = cutpoint_scheme)

  ## Add Prior Value Information
  if(length(prior_presence_mean) == 1){
    data$prior_presence_mean <- rep(prior_presence_mean, data$Kp)
  } else if(length(prior_presence_mean) == data$Kp){
    data$prior_presence_mean <- prior_presence_mean
  } else {
    stop('Length of prior value vectors must match number of parameters')
  }

  if(length(prior_abundance_mean) == 1){
    data$prior_abundance_mean <- rep(prior_abundance_mean, data$Ka)
  } else if(length(prior_abundance_mean) == data$Ka) {
    data$prior_abundance_mean <- prior_abundance_mean
  } else {
    stop('Length of  prior value vectors must match number of parameters')
  }

  if(length(prior_presence_var) == 1){
    data$prior_presence_var <- rep(prior_presence_var, data$Kp)
  } else if(length(prior_presence_var) == data$Kp) {
    data$prior_presence_var <- prior_presence_var
  } else {
    stop('Length of prior value vectors must match the number of parameters')
  }

  if(length(prior_abundance_var) == 1){
    data$prior_abundance_var <- rep(prior_abundance_var, data$Kp)
  } else if(length(prior_presence_var) == data$Kp) {
    data$prior_abundance_var <- prior_abundance_var
  } else {
    stop('Length of prior value vectors must match the number of parameters')
  }

  ## Choose Link Function
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

  prior_information <-
    list(
      prior_presence_mean = data$prior_presence_mean,
      prior_abundance_mean = data$prior_abundance_mean,
      prior_presence_var = data$prior_presence_var,
      prior_abundance_var = data$prior_abundance_var
    )

  attr(result, 'prior') <- prior_information

  return(result)
}
