data {
  // prior
  real<lower=0.0> result_sd;
  real<lower=0.0> beta_sd;
  real home_advantage_mean;
  real<lower=0> home_advantage_sd;


  // observed data
  int<lower=0> n_players;
  int<lower=0> n_obs;
  matrix[n_obs, n_players] indicators;
  vector[n_obs] result;
  vector[n_obs] time;
}

parameters {
  vector[n_players] beta;
  real home_advantage;
}

transformed parameters {
  vector[n_obs] rate;
  rate = home_advantage + indicators * beta;
}

model {
  home_advantage ~ normal(home_advantage_mean, home_advantage_sd);

  beta ~ normal(0.0, beta_sd);
  result ~ normal(rate .* time, result_sd * sqrt(time));
}
