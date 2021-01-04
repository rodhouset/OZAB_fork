ozab <- function(df, presence_formula, abundance_formula, cutpoint_scheme){

  ## Data Composition
  cover_class_vector <- df[all.vars(presence_formula)[1]][[1]] ## Known bug, make the response index dynamic not fixed
  N <- length(cover_class_vector)
  presence_matrix <- as.matrix(modelr::model_matrix(df, presence_formula))
  Kp <- ncol(presence_matrix)
  abundance_matrix <- as.matrix(modelr::model_matrix(df, abundance_formula))
  print(abundance_matrix)
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

  result <- rstan::stan('OZAB_model.stan', data = data, chains = 1)

  result
}
