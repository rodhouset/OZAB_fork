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
    dplyr::group_by(!!! columns) %>%
    dplyr::tally() %>%
    ggplot2::ggplot(ggplot2::aes(y = n, axis1 = !! columns[[1]], axis2 = !! columns[[2]], axis3 = !! columns[[3]], axis4 = !! columns[[4]] )) +
      ggalluvial::geom_alluvium(ggplot2::aes(fill = !! columns[[1]]), width = alluvium_width) +
      ggalluvial::geom_stratum(width = stratum_width) +
      ggplot2::geom_label(stat = ggalluvial::StatStratum, ggplot2::aes(label = ggplot2::after_stat(stratum))) +
      ggplot2::scale_x_discrete(limits = sapply(columns[1:non_null_args], rlang::quo_name), expand = c(.05, .05))
}
