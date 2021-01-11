#' Plot Cumulative Empirical Proportions of Species by Covariate Bins
#'
#' First found in Irvine, Rodhouse, Keren 2016
#'
#' @param data Dataframe in long format
#' @param species Name of Species to plot
#' @param covariate Name of Categorical Covariate to Plot By
#' @param species_col Name of Species column if it differs from Default 'Species'
#' @param cover_class_col Name of Cover Class column if it differs from Default 'Cover Class'
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' @importFrom rlang .data
plot_cover_class_by_covariate <- function(data, species, covariate, species_col = .data$Species, cover_class_col = .data$`Cover Class`){

  if(nrow(dplyr::filter(data, {{ species_col }} == species)) == 0){
    stop('Species Not Found')
  }

  data %>%
    dplyr::filter({{ species_col }} == species) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ covariate }}, fill = {{ cover_class_col }})) +
      ggplot2::geom_bar(position = ggplot2::position_fill()) +
      ggplot2::ylab("Empirical Cumulative Proportions")
}

#' Plot Cover class over space and time
#'
#' First found in Ito 2020
#'
#' @param data Dataframe in long format
#' @param species Name of Species to Plot
#' @param location_col Name of Location Column (Default: Location)
#' @param datetime_col Name of Datetime Column (Default: Datetime)
#' @param species_col Name of Species Column (Default: Species)
#' @param cover_class_col Name of Cover Class Column (Default: Cover Class)
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' @importFrom rlang .data
plot_cover_class_by_time_and_location <- function(data, species, datetime_col, location_col, species_col = .data$Species, cover_class_col = .data$`Cover Class`){

  if(nrow(dplyr::filter(data, {{ species_col }} == species)) == 0){
    stop('Species not found')
  }

  data %>%
    dplyr::filter({{ species_col }} == species) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ datetime_col }}, y = {{ location_col }}, fill = {{ cover_class_col }})) +
    ggplot2::geom_tile()
}

alluvial_plot <- function(.data, covariate, cover_class_col = .data$`Cover Class`){
  ## Known Bugs:
  ##  1. cover class column name needs to be generalizable
  ##  2. find a replacement for rlang::as_stirng(rlang::enym(.))
  ##  3. generalize function to arbitrarily many variables
  ##  4. add variables for widths
  ##  5. add variables for y-axis name (or remove custom naming)
  ##  6. add stratum names

  ## Add Tests For:
  ## 1. covariate not provided
  ## 2. covariate not found
  ## 3. covariate is not discrete

  .data %>%
    dplyr::group_by({{ cover_class_col }}, {{ covariate }}) %>%
    dplyr::tally(name = 'Freq') %>%
    ggplot2::ggplot(ggplot2::aes(y = Freq, axis1 = {{ cover_class_col }}, axis2 = {{ covariate }})) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = {{ cover_class_col }}), width = 1/12) +
    ggalluvial::geom_stratum(width = 1/8) +
    ggplot2::scale_x_discrete(limits = c('Cover Class', rlang::as_string(rlang::ensym(covariate))), expand = c(.05, .05))
}
