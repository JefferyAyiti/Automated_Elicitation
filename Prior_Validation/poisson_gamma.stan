// Hierarchical Poisson-Gamma model for adverse event counts in multi-center
// clinical trials.
//
// Model (Equations 1-4 from Arai et al.):
//   y_ij ~ Poisson(lambda_j)          [patient-level likelihood]
//   lambda_j ~ Gamma(alpha, beta)     [site-specific AE rates]
//   alpha ~ Exponential(rate_alpha)   [hyperprior for shape]
//   beta  ~ Exponential(rate_beta)    [hyperprior for rate]
//
// rate_alpha and rate_beta are supplied as data, allowing the same model
// to be run with LLM-elicited priors or the meta-analytical baseline
// (Barmaz & Menard 2021: rate_alpha = 0.1, rate_beta = 0.1).

data {
  int<lower=1> N;                   // total number of patients
  int<lower=1> J;                   // number of sites
  array[N] int<lower=0> y;          // observed AE counts per patient
  array[N] int<lower=1, upper=J> site; // site index for each patient (1-indexed)

  real<lower=0> rate_alpha;         // Exponential rate for alpha hyperprior
  real<lower=0> rate_beta;          // Exponential rate for beta hyperprior
}

parameters {
  real<lower=0> alpha;              // Gamma shape hyperparameter
  real<lower=0> beta;               // Gamma rate hyperparameter
  vector<lower=0>[J] lambda;        // site-specific AE rates
}

model {
  // Hyperpriors
  alpha ~ exponential(rate_alpha);
  beta  ~ exponential(rate_beta);

  // Site-level priors
  lambda ~ gamma(alpha, beta);

  // Patient-level likelihood
  y ~ poisson(lambda[site]);
}

generated quantities {
  // Log-likelihood per patient for LPD computation and LOO-CV.
  // LPD for held-out patient with count y_obs:
  //   LPD = log E_posterior[ Poisson(y_obs | lambda_new) ]
  // approximated via the posterior predictive:
  //   lambda_new ~ Gamma(alpha, beta)
  // Here we expose the per-patient log-likelihood on training sites so
  // that rstan::extract() can be used downstream for LPD calculation on
  // held-out test patients using posterior samples of alpha and beta.
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = poisson_lpmf(y[n] | lambda[site[n]]);
  }
}
