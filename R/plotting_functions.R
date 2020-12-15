#' Plot Empirical Proportions of Species by Covariate Bins
#'
#' First found in Irvine, Rodhouse, Keren 2016
#'
#' @param data Dataframe in long format
#' @param species Name of Species to plot
#' @param covariate Name of Categorical Covariate to Plot By
#' @param species_col Name of Species column if it differs from Default 'Species'
#' @param cover_class_col Name of Cover Class column if it differs from Default 'Cover Class'
#'
#' @return
#' @export
#'
#' @examples
plot_cover_class_by_covariate <- function(data, species, covariate, species_col = Species, cover_class_col = `Cover Class`){
  data %>%
    filter({{ species_col }} == species) %>%
    ggplot(aes(x = {{ covariate }}, fill = {{ cover_class_col }})) +
      geom_bar(position = position_fill()) +
      ylab("Empirical Proportions")
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
#' @return
#' @export
#'
#' @examples
plot_cover_class_by_time_and_location <- function(data, species, location_col = Location, datetime_col = Datetime, species_col = Species, cover_class_col = `Cover Class`){
  data %>%
    filter({{ species_col }} == species) %>%
    ggplot(aes(x = {{ datetime_col }}, y = {{ location_col }}, fill = {{ cover_class_col }})) +
    geom_tile()
}



