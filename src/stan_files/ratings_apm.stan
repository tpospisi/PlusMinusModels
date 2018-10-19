data {
  // priors
  real<lower=0.0> beta_sd;
  real<lower=0.0> result_sd;
  real home_advantage_mean;
  real<lower=0.0> home_advantage_sd;
  real ratings_slope_mean;
  real<lower=0.0> ratings_slope_sd;

  // observed data
  int<lower=0> n_players;
  int<lower=0> n_obs;
  matrix[n_obs, n_players] indicators;
  vector[n_players] rating;
  vector[n_obs] result;
  vector[n_obs] time;
}

parameters {
  real rating_slope;
  vector[n_players] beta;
  real home_advantage;
}

transformed parameters {
  vector[n_obs] rate;
  rate = home_advantage + indicators * beta;
}

model {
  home_advantage ~ normal(home_advantage_mean, home_advantage_sd);
  rating_slope ~ normal(ratings_slope_mean, ratings_slope_sd);

  beta ~ normal(rating_slope * rating, beta_sd);
  result ~ normal(rate .* time, result_sd * sqrt(time));
}
