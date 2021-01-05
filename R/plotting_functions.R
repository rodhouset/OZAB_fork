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



