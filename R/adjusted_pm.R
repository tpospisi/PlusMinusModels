#' Helper function to process and validate data for regularized_apm models
#'
#' @param design Design matrix for plus-minus model; contains -1/0/1
#'     for whether player is on away team / not playing / home team
#'     for each segment.
#' @param y Response vector for plus-minus model.
#' @param t Vector of segment times.
#' @param prior_params A list of prior parameters for the
#'     regularized_apm model; will have 4 components (1) beta_sd and
#'     (2) result_sd the standard deviations for the player effects
#'     and result standard deviation respectively. (3)
#'     home_advantage_mean and (4) home_advantage_sd which are the
#'     mean and standard deviation for the home advantage.
#'
#' @return A list of data for input into Stan.
regularized_apm_data <- function(design, y, t, prior_params) {
    stopifnot(nrow(design) == length(y))
    stopifnot(nrow(design) == length(t))

    return(list(n_obs = length(y),
                n_players = ncol(design),
                result = y,
                time = t,
                indicators = as.matrix(design),
                beta_sd = prior_params$beta_sd,
                result_sd = prior_params$result_sd,
                home_advantage_mean = prior_params$home_advantage_mean,
                home_advantage_sd = prior_params$home_advantage_sd))
}

#' Fit a regularized_apm model.
#'
#' @param design Design matrix for plus-minus model; contains -1/0/1 for
#'     whether player is on away team / not playing / home team for
#'     each segment.
#' @param y Response vector for plus-minus model.
#' @param t Vector of segment times.
#' @param prior_params A list of prior parameters for the
#'     regularized_apm model; will have 4 components (1) beta_sd and
#'     (2) result_sd the standard deviations for the player effects
#'     and result standard deviation respectively. (3)
#'     home_advantage_mean and (4) home_advantage_sd which are the
#'     mean and standard deviation for the home advantage.
#' @param n_samples Number of samples to draw from model.
#' @param warmup Number of warmup samples.
#' @param chains Number of independent chains to run.
#' @param thin Number of samples to draw between saving.
#'
#' @return A fitted regularized_apm object
#' @export
regularized_apm <- function(design, y, t, prior_params, n_samples = 1000,
                      warmup = 1000, chains = 1, thin = 1) {
    data <- regularized_apm_data(design, y, t, prior_params)

    iter <- (n_samples + warmup) * thin

    stan_object <- rstan::sampling(stanmodels$regularized_apm, data = data,
                                   chains = chains, iter = iter,
                                   warmup = warmup, thin = thin)

    samples <- rstan::extract(stan_object)

    estimates <- list(home_advantage = mean(samples$home_advantage),
                      beta = colMeans(samples$beta))

    return(structure(list(stan_object = stan_object,
                          samples = samples), class = "regularized_apm"))
}

#' Predict segment results for regularized_apm object.
#'
#' @param object A regularized_apm object
#' @param design_new Design matrix for plus-minus model; contains -1/0/1 for
#'     whether player is on away team / not playing / home team for
#'     each segment.
#' @param tnew Vector for segment times for new segments
#' @param \dots Other arguments
#'
#' @return Predicted outcomes for each game segment in the design matrix
#' @export
predict.regularized_apm <- function(object, design_new, tnew, ...) {
    stopifnot(nrow(design_new) == length(tnew))
    design_new <- as.matrix(design_new)

    rate_hat <- object$estimates$home_advantage + design_new %*% object$estimates$beta
    yhat <- rate_hat * tnew
    return(as.vector(yhat))
}

#' @export
coef.regularized_apm <- function(object, ...) {
    return(list(rating_slope = mean(object$samples$rating_slope),
                home_advantage = mean(object$samples$home_advantage),
                beta = colMeans(object$samples$beta)))
}

#' @export
summary.regularized_apm <- function(object, ...) {
    return(summary(object$stan_object))
}

## Functions for MAP estimate

#' Fit a MAP estimate for regularized_apm model.
#'
#' @param design Design matrix for plus-minus model; contains -1/0/1 for
#'     whether player is on away team / not playing / home team for
#'     each segment.
#' @param y Response vector for plus-minus model.
#' @param t Vector of segment times.
#' @param prior_params A list of prior parameters for the
#'     regularized_apm model; will have 4 components (1) beta_sd and
#'     (2) result_sd the standard deviations for the player effects
#'     and result standard deviation respectively. (3)
#'     home_advantage_mean and (4) home_advantage_sd which are the
#'     mean and standard deviation for the home advantage.#'
#' @return A regularized_apm_map object
#' @export
regularized_apm_map <- function(design, y, t, prior_params) {
    data <- regularized_apm_data(design, y, t, prior_params)

    stan_fit <- rstan::optimizing(stanmodels$regularized_apm, data = data)
    beta_vars <- grep("beta\\[.*\\]", names(stan_fit$par))

    estimates <- list(home_advantage = stan_fit$par["home_advantage"],
                      beta = as.vector(stan_fit$par[beta_vars]))
    return(structure(list(stan_fit = stan_fit,
                          estimates = estimates), class = "regularized_apm_map"))
}

#' Predict segment results for regularized_apm_map object.
#'
#' @param object A regularized_apm_map object
#' @param design_new Design matrix for plus-minus model; contains -1/0/1 for
#'     whether player is on away team / not playing / home team for
#'     each segment.
#' @param tnew Vector for segment times for new segments
#' @param \dots Other arguments
#' @export
predict.regularized_apm_map <- function(object, design_new, tnew, ...) {
    stopifnot(nrow(design_new) == length(tnew))
    design_new <- as.matrix(design_new)

    rate_hat <- object$estimates$home_advantage + design_new %*% object$estimates$beta
    yhat <- rate_hat * tnew
    return(as.vector(yhat))
}

#' @export
coef.regularized_apm_map <- function(object, ...) {
    return(object$estimates)
}

#' @export
summary.regularized_apm <- function(object, ...) {
    return(summary(object$stan_object))
}
