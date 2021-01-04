functions {
  vector beta_class_cover(real alpha, real beta, vector c){

    // Get Dimensions
    int K = num_elements(c) + 1;

    vector[K] result;

    result[1] = beta_cdf(c[1], alpha, beta);
    for(k in 2:(K-1)){
      result[k] = beta_cdf(c[k], alpha, beta) - sum(result[1:(k-1)]);
    }
    result[K] = 1 - sum(result[1:(K-1)]);

    return(result);
  }

  real ozab_lpmf(int[ ] y, vector theta, vector alpha, vector beta, vector c){
    real result = 0;

    // Get Dimensions
    int N = num_elements(y);

    for(n in 1:N){
      if(y[n] == 0){
        result += bernoulli_lpmf(1 | theta[n]); // Flip presence / absence indicator
      } else {
        result += bernoulli_lpmf(0 | theta[n]) + categorical_lpmf(y[n] | beta_class_cover(alpha[n], beta[n], c));
      }
    }

    return result;
  }
}

data {
  int<lower=0> N; // Num Obs
  int<lower=2> K; // Number of Categories
  positive_ordered[K-1] c; // Category Dividers
  int<lower=0,upper=K> y[N]; // Vector of Cover Class Obs
  int<lower=0> Kp; // Num. Cols of Presence Design Matrix
  int<lower=0> Ka; // Num. Cols of Abundance Design Matrix
  matrix[N, Ka] Xa; // Presence Design Matrix
  matrix[N, Kp] Xp; // Abundance Design Matrix
}

parameters {
  vector[Ka] mu_beta;
  vector[Kp] theta_beta;
  real<lower=0> phi;
}

// transformed parameters {
//   vector<lower=0, upper=1>[N] mu = Phi(Xa * mu_beta);
//   vector<lower=0, upper=1>[N] theta = Phi(Xa * mu_beta);
// }

model {
  // Priors
  mu_beta ~ normal(0, 100);
  theta_beta ~ normal(0, 100);

  // Each species is multinomial on the backend
  y ~ ozab(inv_logit(Xp * theta_beta), phi * inv_logit(Xa * mu_beta), phi * (1 - inv_logit(Xa * mu_beta)), c);
}

