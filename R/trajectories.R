# Internal helpers to build the predictive trajectories from the sampled
# missing cells of the delay triangle. Shared by nowcasting_no_age_mgcv(),
# nowcasting_age_mgcv(), nowcasting_no_age_inla() and nowcasting_age_inla().

#' Aggregate sampled delay triangles into trajectories
#'
#' Fills the missing cells of the delay triangle with each set of sampled
#' counts and sums the counts over the delays, for each week of onset (and age
#' class), from the first week with missing cells onwards. All samples are
#' aggregated at once, as a matrix.
#'
#' @param dataset Delay triangle, with columns `Time`, `dt_event`, `Y` and, for
#'   the structured model, `fx_etaria` and `fx_etaria.num`. Missing cells have
#'   `Y = NA`.
#' @param idx Row indices of the missing cells, `which(is.na(dataset$Y))`.
#' @param draws Matrix of sampled counts for the missing cells, with one row
#'   per element of `idx` and one column per sample.
#' @param age If `TRUE`, aggregate by week of onset and age class.
#'
#' @return A tibble with columns `sample` (character), `Time`, `dt_event`,
#'   (`fx_etaria`, `fx_etaria.num`,) and `Y`, ordered by sample, week of onset
#'   and age class.
#' @keywords internal
#' @noRd
aggregate_trajectories <- function(dataset, idx, draws, age = FALSE) {

  draws <- as.matrix(draws)
  n.samples <- ncol(draws)

  ## Only the weeks from the first week with missing cells onwards
  Tmin <- min(dataset$Time[idx])
  keep <- dataset$Time >= Tmin

  ## One group per week of onset (and age class)
  if (age) {
    key <- paste(dataset$Time, dataset$fx_etaria.num, sep = "_")
  } else {
    key <- as.character(dataset$Time)
  }
  first <- which(keep & !duplicated(key))
  if (age) {
    first <- first[order(dataset$Time[first], dataset$fx_etaria.num[first])]
  } else {
    first <- first[order(dataset$Time[first])]
  }
  groups <- key[first]
  n.groups <- length(groups)

  ## Observed counts by group
  observed <- keep & !is.na(dataset$Y)
  totals.obs <- rowsum(dataset$Y[observed],
                       factor(key[observed], levels = groups),
                       reorder = TRUE)
  obs <- numeric(n.groups)
  obs[match(rownames(totals.obs), groups)] <- totals.obs[, 1]

  ## Sampled counts by group, for all samples at once
  totals.missing <- rowsum(draws, factor(key[idx], levels = groups),
                           reorder = TRUE)
  tot <- matrix(obs, nrow = n.groups, ncol = n.samples)
  rows <- match(rownames(totals.missing), groups)
  tot[rows, ] <- tot[rows, ] + totals.missing

  ## Long format: one row per sample and group
  out <- tibble::tibble(
    sample = rep(as.character(seq_len(n.samples)), each = n.groups),
    Time = rep(dataset$Time[first], times = n.samples),
    dt_event = rep(dataset$dt_event[first], times = n.samples)
  )
  if (age) {
    out$fx_etaria <- rep(dataset$fx_etaria[first], times = n.samples)
    out$fx_etaria.num <- rep(dataset$fx_etaria.num[first], times = n.samples)
  }
  out$Y <- as.vector(tot)

  out
}

#' Fit the negative binomial GAM with mgcv::gam() or mgcv::bam()
#'
#' @param formula Model formula.
#' @param data Delay triangle.
#' @param fit `"gam"`, `"bam"` or `"bam_discrete"`, see [nowcasting_mgcv()].
#'
#' @return The fitted model.
#' @keywords internal
#' @noRd
fit_nb_gam <- function(formula, data, fit = c("gam", "bam", "bam_discrete")) {
  fit <- match.arg(fit)
  switch(fit,
    gam = mgcv::gam(formula, family = "nb", data = data),
    bam = mgcv::bam(formula, family = mgcv::nb(), data = data,
                    method = "fREML"),
    bam_discrete = mgcv::bam(formula, family = mgcv::nb(), data = data,
                             method = "fREML", discrete = TRUE)
  )
}

#' Sample the missing cells from a negative binomial GAM
#'
#' @param fit A `mgcv::gam()` fit with family `"nb"`.
#' @param newdata Rows of the delay triangle with missing counts.
#' @param n.samples Number of samples.
#'
#' @return Matrix of sampled counts, one row per row of `newdata` and one
#'   column per sample.
#' @keywords internal
#' @noRd
sample_missing_gam <- function(fit, newdata, n.samples = 1000) {

  ## Step 1: sample the coefficients from their approximate posterior
  betas <- mgcv::rmvn(n = n.samples, stats::coef(fit), fit$Vp)

  ## Step 2: linear predictor for the missing cells, for each sample
  Xp <- stats::predict(fit, type = "lpmatrix", newdata = newdata)
  mu <- exp(Xp %*% t(betas))

  ## Step 3: sample the counts, fixing the negative binomial dispersion
  theta <- fit$family$getTheta(TRUE)
  n.missing <- nrow(newdata)
  vapply(seq_len(n.samples),
         function(s) stats::rnbinom(n = n.missing, mu = mu[, s], size = theta),
         numeric(n.missing))
}

#' Sample the missing cells from an 'INLA' fit
#'
#' @param samples Output of `INLA::inla.posterior.sample()`.
#' @param idx Row indices of the missing cells.
#' @param zero_inflated Was the zero-inflated negative binomial used?
#'
#' @return Matrix of sampled counts, one row per element of `idx` and one
#'   column per posterior sample.
#' @keywords internal
#' @noRd
sample_missing_inla <- function(samples, idx, zero_inflated = FALSE) {
  vapply(samples, function(x) {
    if (zero_inflated) {
      # p = 1 - (exp(eta) / (1 + exp(eta)))^a ; eta -> large; p -> 0
      unif.log <- as.numeric(
        stats::runif(idx, 0, 1) >
          1 - (exp(x$latent[idx]) / (1 + exp(x$latent[idx])))^x$hyperpar[2]
      )
    } else {
      unif.log <- 1
    }
    stats::rnbinom(n = idx, mu = exp(x$latent[idx]), size = x$hyperpar[1]) *
      unif.log
  }, numeric(length(idx)))
}
