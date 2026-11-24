# The trajectories are built with a vectorised aggregation (aggregate_trajectories()).
# These tests compare it with the straightforward per-sample dplyr version it replaced.

reference_aggregation <- function(dataset, idx, draws, age = FALSE) {
  one_sample <- function(x) {
    d <- dataset
    d$Y[idx] <- x
    d <- d[d$Time >= min(dataset$Time[idx]), ]
    keys <- if (age) c("Time", "dt_event", "fx_etaria", "fx_etaria.num") else c("Time", "dt_event")
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(Y = sum(Y), .groups = "drop")
  }
  samples <- lapply(seq_len(ncol(draws)), function(s) one_sample(draws[, s]))
  dplyr::bind_rows(samples, .id = "sample")
}

# A small delay triangle: 12 weeks of onset, delays 0-4, optionally 3 age classes
make_triangle <- function(age = FALSE) {
  tri <- expand.grid(Time = 1:12, delay = 0:4)
  if (age) {
    tri <- merge(tri, data.frame(fx_etaria = factor(c("0-19", "20-59", "60+"))))
    tri$fx_etaria.num <- as.numeric(tri$fx_etaria)
  }
  tri$dt_event <- as.Date("2020-01-05") + 7 * (tri$Time - 1)
  set.seed(10)
  tri$Y <- stats::rpois(nrow(tri), 5)
  tri$Y[tri$Time + tri$delay > 12] <- NA
  tri[order(tri$Time, tri$delay), ]
}

test_that("aggregate_trajectories() matches the per-sample aggregation", {
  for (age in c(FALSE, TRUE)) {
    tri <- make_triangle(age)
    idx <- which(is.na(tri$Y))
    set.seed(11)
    draws <- matrix(stats::rpois(length(idx) * 50, 4), nrow = length(idx))

    fast <- aggregate_trajectories(tri, idx, draws, age = age)
    reference <- reference_aggregation(tri, idx, draws, age = age)

    expect_equal(as.data.frame(fast), as.data.frame(reference),
                 ignore_attr = TRUE)
    expect_identical(names(fast), names(reference))
    expect_equal(nrow(fast), 50 * length(unique(tri$Time[tri$Time >= min(tri$Time[idx])])) *
                   (if (age) 3 else 1))
  }
})

test_that("observed cells are kept and only missing cells are sampled", {
  tri <- make_triangle()
  idx <- which(is.na(tri$Y))
  zero_draws <- matrix(0, nrow = length(idx), ncol = 3)
  out <- aggregate_trajectories(tri, idx, zero_draws)
  observed <- stats::aggregate(Y ~ Time, data = tri[!is.na(tri$Y) & tri$Time >= min(tri$Time[idx]), ], FUN = sum)
  expect_equal(out$Y[out$sample == "1"], observed$Y)
  expect_identical(unique(out$sample), c("1", "2", "3"))
})

test_that("sample_missing_gam() returns one column per sample", {
  tri <- make_triangle()
  fit <- mgcv::gam(Y ~ 1 + s(Time, k = 5) + s(delay, k = 4), family = "nb",
                   data = tri)
  idx <- which(is.na(tri$Y))
  set.seed(12)
  draws <- sample_missing_gam(fit, newdata = tri[idx, ], n.samples = 20)
  expect_equal(dim(draws), c(length(idx), 20L))
  expect_true(all(draws >= 0))
  expect_true(all(draws == round(draws)))
})
