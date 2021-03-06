#' Plot Cumulative Empirical Proportions of Species by Covariate Bins
#'
#' First found in Irvine, Rodhouse, Keren 2016
#'
#' @param data Dataframe in long format
#' @param covariate Name of Categorical Covariate to Plot By
#' @param cover_class_col Name of Cover Class column if it differs from Default 'Cover Class'
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#'   plot_cover_class_by_covariate(sagebrush, Fire)
#' @importFrom rlang .data
plot_cover_class_by_covariate <- function(data, covariate, cover_class_col = .data$`Cover Class`){

  # if(nrow(dplyr::filter(data, {{ species_col }} == species)) == 0){
  #   stop('Species Not Found')
  # }

  data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ covariate }}, fill = {{ cover_class_col }})) +
      ggplot2::geom_bar(position = ggplot2::position_fill()) +
      ggplot2::ylab("Empirical Cumulative Proportions")
}

#' Basic Alluvial Plots for Long Format Dataframes
#'
#' @param .data Dataframe from which the alluvial plot will be constructed
#' @param ... Columns for the Alluvial Plot in Order; The first column will be used as the fill color
#' @param alluvium_width Width of the alluvium
#' @param stratum_width Width of the stratum
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' sagebrush %>%
#'   alluvial_plot(`Cover Class`, Fire)
alluvial_plot <- function(.data, ..., alluvium_width = 1/12, stratum_width = 1/8){

  columns <- rlang::enquos(...)

  non_null_args <- length(columns)

  # Check if covariates were provided
  if(non_null_args == 0){
    stop('No Covariates Provided')
  }

  # Check if columns are found in the df
  for(col in columns){
    if(!(rlang::quo_name(col) %in% names(.data))){
      stop(glue::glue('{ rlang::quo_name(col) } not found in provided data'))
    }
  }

  for(i in (length(columns) + 1):4){
    columns[[i]] <- rlang::quo(NULL)
  }

  .data %>%
    dplyr::select(!!! columns) %>%
    dplyr::group_by(!!! columns) %>%
    dplyr::tally() %>%
    ggplot2::ggplot(ggplot2::aes(y = n, axis1 = !! columns[[1]], axis2 = !! columns[[2]], axis3 = !! columns[[3]], axis4 = !! columns[[4]] )) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = !! columns[[1]]), width = alluvium_width) +
    ggalluvial::geom_stratum(width = stratum_width) +
    ggplot2::geom_label(stat = ggalluvial::StatStratum, ggplot2::aes(label = ggplot2::after_stat(stratum))) +
    ggplot2::scale_x_discrete(limits = sapply(columns[1:non_null_args], rlang::quo_name), expand = c(.05, .05))
}

#' Basic Mosaic Plots
#'
#' @param df Dataframe from which the mosaic plot will be constructed
#' @param factor1 The first factor to be included in the mosaic plot
#' @param factor2 The second factor to be included in the mosaic plot
#' @param fill The factor to be used for the fill color for the mosaic plot
#'
#' @return
#' @export
#'
#' @examples
#' sagebrush %>%
#'    mosaic_plot(Fire, )
mosaic_plot <- function(df, factor1, factor2, fill){
  # Check if columns are found in the df

  # Check if more than one covariate was provided

  factor1 <- rlang::ensym(factor1)
  factor2 <- rlang::ensym(factor2)
  fill <- rlang::ensym(fill)

  df %>%
    ggplot2::ggplot() +
    ggmosaic::geom_mosaic(aes(x = ggmosaic::product(!! factor1, !! factor2), fill = !! fill))
}

plot_prior_sensitivity <- function(OZABfit, pattern){

  # Extract Selected Posteriors
  param_names <- names(bsage_result)[grepl(pattern, names(OZABfit))]
  print(names(bsage_result))
  posteriors <-
    rstan::extract(OZABfit, param_names) %>%
    as.tibble() %>%
    tidyr::pivot_longer(everything(), names_to = 'parameter', values_to = 'value')

  print(param_names)

  limits <-
    posteriors %>%
    summarise(min = min(value), max = max(value))

  x <- seq(from = limits[['min']],
           to = limits[['max']],
           length.out = nrow(posteriors))

  priors <- NULL

  # Compute Prior Densities
  for(param_name in param_names) {
    priors <- rbind(priors, tibble(parameter = param_name, x = x, y = dnorm(x, mean = OZABfit@prior[[param_name]][1], sd = OZABfit@prior[[param_name]][2])))
  }

  # Plot Posteriors
  posteriors %>%
  ggplot2::ggplot(aes(x = value)) +
    ggplot2::geom_histogram(aes(y = stat(density))) +
    ggplot2::facet_wrap(~parameter) +
    ggplot2::geom_line(aes(x = x, y = y), data=filter(priors, parameter == parameter))
}
