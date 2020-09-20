#' Sample LKJ correlation matrices.
#'
#' This function was copied from Richard McElreath's rethinking package hosted
#' at https://github.com/rmcelreath/rethinking. In turn, he appears to have
#' copied it from Ben Bolker's rLKJ function from the emdbook package, although
#' I cannot find it there (else I would have imported it).
#'
#' @param n Number of matrices to sample.
#' @param K dimenstion of matrix to sample.
#' @param eta Distribution parameter
#' @return matrix
#'
#' @importFrom stats rbeta rnorm
#' @export
rlkjcorr <- function (n, K, eta = 1) {

  stopifnot(is.numeric(K), K >= 2, K == as.integer(K))
  stopifnot(eta > 0)
  #if (K == 1) return(matrix(1, 1, 1))

  f <- function() {
    alpha <- eta + (K - 2)/2
    r12 <- 2 * rbeta(1, alpha, alpha) - 1
    R <- matrix(0, K, K) # upper triangular Cholesky factor until return()
    R[1,1] <- 1
    R[1,2] <- r12
    R[2,2] <- sqrt(1 - r12^2)
    if(K > 2) for (m in 2:(K - 1)) {
      alpha <- alpha - 0.5
      y <- rbeta(1, m / 2, alpha)

      # Draw uniformally on a hypersphere
      z <- rnorm(m, 0, 1)
      z <- z / sqrt(crossprod(z)[1])

      R[1:m,m+1] <- sqrt(y) * z
      R[m+1,m+1] <- sqrt(1 - y)
    }
    return(crossprod(R))
  }
  R <- replicate( n , f() )
  if ( dim(R)[3]==1 ) {
    R <- R[,,1]
  } else {
    # need to move 3rd dimension to front, so conforms to array structure that Stan uses
    R <- aperm(R,c(3,1,2))
  }
  return(R)
}

#' Simulate Presence-Absence Data
#'
#' @param S The Number of Species
#' @param N The Number of Plots or Sites Observed
#' @param spp_mean_presence The Gaussian latent species mean for prob. presence
#' @param spp_presence_correlation An S by S correlation matrix between mean presence of species
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' simulate_presence_absence_data()
#' }
simulate_presence_absence_data <- function(S = 10,
                                           N = 100,
                                           spp_mean_presence = stats::rnorm(S),
                                           spp_presence_correlation = rlkjcorr(1, S, eta = 1)) {

  # Input Error Checking

  if (S < 1) {
    stop("The number of species, S, must be a positive integer.")
  }

  if (N < 1) {
    stop("The number of observations, N, must be a positive integer.")
  }

  if (length(spp_mean_presence) != S) {
    stop("The vector of latent mean presence must match the number of species, S.")
  }

  if (dim(spp_presence_correlation)[1] != dim(spp_presence_correlation)[2]) {
    stop("The presence correlation matrix must be square.")
  }

  ## Generate sample of P plots for each species from MVN with spp x spp correlation
  latent_presence <- mvtnorm::rmvnorm(
      N,
      mean = spp_mean_presence,
      sigma = spp_presence_correlation
    )

  ## Generate Binary Response
  ## so this is where could add a toggle for ordinal data as a function input.
  ## SO IF data_type="Ordinal", require a cutpoint vector, then transform cutpoints to Guassian quantiles
  ## then you cut up the latent MVN into ordinal cover classes ---walaa!

  presence_data <- latent_presence > 0

  ## Combine Detection Error and Presence data to get Observation Data
  observation_data <- presence_data

  ### Prepare Mean Vector
  latent_mean_tibble <-
    rep(spp_mean_presence, N) %>%
    matrix(ncol = S, byrow = TRUE) %>%
    dplyr::as_tibble(rownames = "plotid", .name_repair = "unique") %>%
    dplyr::mutate(plotid = as.numeric(plotid)) %>%
    tidyr::pivot_longer(cols = -contains("plotid"),
                 names_to = "species",
                 values_to = "latent_mean") %>%
    dplyr::mutate(species = rep(1:S, N))

  ### Prepare Latent Presence Realizations
  latent_presence_tibble <-
    latent_presence %>%
    dplyr::as_tibble(rownames = "plotid", .name_repair = "unique") %>%
    dplyr::mutate(plotid = as.numeric(plotid)) %>%
    tidyr::pivot_longer(
      cols = -contains("plotid"),
      names_to = "species",
      values_to = "latent_presence"
      ) %>%
    dplyr::mutate(species = rep(1:S, N))

  ### Prepare True Presence Values
  presence_tibble <-
    presence_data %>%
    dplyr::as_tibble(rownames = "plotid", .name_repair = "unique") %>%
    dplyr::mutate(plotid = as.numeric(plotid)) %>%
    tidyr::pivot_longer(
      cols = -contains("plotid"),
      names_to = "species",
      values_to = "presence"
    ) %>%
    dplyr::mutate(
      species = rep(1:S, N),
      presence = as.logical(presence)
    )

  ### Prepare Observed Presence Values
  observation_tibble <-
    observation_data %>%
    dplyr::as_tibble(rownames = "plotid", .name_repair = "unique") %>%
    dplyr::mutate(plotid = as.numeric(plotid)) %>%
    tidyr::pivot_longer(
      cols = -contains("plotid"),
      names_to = "species",
      values_to = "observed_presence"
    ) %>%
    dplyr::mutate(
      species = rep(1:S, N),
      observed_presence = as.logical(observed_presence)
    )

  ### Create Combined Dataframe
  presence_df <-
    latent_mean_tibble %>%
    dplyr::full_join(latent_presence_tibble, by = c("plotid", "species")) %>%
    dplyr::full_join(presence_tibble, by = c("plotid", "species")) %>%
    dplyr::full_join(observation_tibble, by = c("plotid", "species"))

  return(list(
    presence_df = presence_df,
    spp_mean_presence = spp_mean_presence,
    spp_presence_correlation = spp_presence_correlation,
    latent_presence = latent_presence
  ))
}
