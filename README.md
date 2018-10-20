# PlusMinusModels

This package fits plus-minus models for sports data.

## Installation

```{r}
devtools::install_github("tpospisi/PlusMinusModels")
```

## Available models

+ `regularized_apm` which fits regularized adjusted plus minus.
+ `ratings_apm` which fits augmented plus minus with subjective
ratings. See [https://arxiv.org/abs/1810.08032] for an example using
FIFA videogame ratings to augment plus-minus for EPL data

## Data Example

We showcase an example using soccer data from the
[PlusMinusData](https://github.com/fmatano/PlusMinusData) package.

```{r}
library(PlusMinusData) # devtools::install_github("fmatano/PlusMinusData")

# Set up data
data <- get_model_data("2017", "epl", data_dir) # see PlusMinusData for data_dir info
X <- as.matrix(data$design_matrix) # 1/-1/0 indicators for home/away/off players
y <- with(data$responses, home_goal - away_goal) # model goal differential
t <- with(data$responses, (tf - ts) / 90) # segment length
ratings <- data$player_covariates$overall # use FIFA ratings for prior

# Set prior
prior_params <- list(beta_sd = 0.1,
                     result_sd = 1.0,
                     home_advantage_mean = 0.0,
                     home_advantage_sd = 1.0,
                     ratings_slope_mean = 0.0,
                     ratings_slope_sd = 1.0)

# Fit model using MAP estimate (quick point estimates)
map_model <- ratings_apm_map(X, y, t, ratings, prior_params)

coef(map_model) # see coefficients for the model
predict(map_model, Xnew, tnew) # predict for a new design matrix (for new games)

# Fit model using posterior samples (allows uncertainty quantification)
model <- ratings_apm(X, y, t, ratings, prior_params)
coef(model) # see coefficients for the model
predict(model, Xnew, tnew) # predict for a new design matrix (for new games)
```