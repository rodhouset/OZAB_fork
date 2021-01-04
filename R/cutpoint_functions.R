#' Validate a Cutpoint Vector / Scheme
#'
#' Ensures cutpoint values are:
#'
#'   * Strictly Greater Than Zero
#'   * Strictly Less Than One
#'   * Distinct
#'   * Monotonically Increasing
#'
#' @param cutpoint_vector A Vector of Decimal Cutpoints Bounded Between 0 and 1
#'
#' @return
#' @export
#'
#' @examples
#' validate_cutpoint(c(0.5)) # Okay
#' validate_cutpoint(c(0.6, 0.4)) # Not Okay
validate_cutpoint <- function(cutpoint_vector){
  if(length(cutpoint_vector) == 0){
    stop("Cutpoint Vector Must Contain At Least One Value")
  }
  if(min(cutpoint_vector) <= 0){
    stop("Lowest Cutpoint Must Be Greater Than Zero")
  }
  if(max(cutpoint_vector) >= 1){
    stop("Highest Cutpoint Must Be Less Than One")
  }
  if(any(diff(cutpoint_vector) == 0)){
    stop("Cutpoint Vector Must Be Distinct Values")
  }
  if(!all(cutpoint_vector == cummax(cutpoint_vector))){
    stop("Cutpoint Vector Must Be Monotonically Increasing")
  }
}

#' Braun-Blanquet Cutpoint Scheme
#'
#' @return Vector of Values Representing Braun-Blanquet Cover Class Cutpoint Scheme
#' @export
#'
#' @examples
#' braun_blanquet()
braun_blanquet <- function(){
  return( c(0.01, 0.05, 0.25, 0.5, 0.75) )
}

#' Daubenmire Cutpoint Scheme
#'
#' @return Vector of Values Representing Braun-Blanquet Cover Class Cutpoint Scheme
#' @export
#'
#' @examples
#' daubenmire()
daubenmire <- function(){
  return( c(0.05, 0.25, 0.5, 0.75, 0.95) )
}

#' Hult-Sernander Cutpoint Scheme
#'
#' @return Vector of Values Representing Hult-Sernander Cover Class Cutpoint Scheme
#' @export
#'
#' @examples
#' hult_sernander()
hult_sernander <- function(){
  return( c(0.0625, 0.125, 0.25, 0.5) )
}

#' Domin Cutpoint Scheme
#'
#' @return Vector of Values Representing Domin Cover Class Cutpoint Scheme
#' @export
#'
#' @examples
#' domin()
domin <- function(){
  return( c(0.01, 0.05, 0.1, 0.25, 0.33, 0.5, 0.75, 0.95) )
}
