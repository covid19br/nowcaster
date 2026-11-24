# Stop with an informative message when 'INLA' is not installed.
# 'INLA' is not on CRAN, so it is only suggested. 'INLA' also needs 'sn' to
# sample from the posterior (INLA::inla.posterior.sample()), which 'INLA'
# itself only suggests.
check_inla <- function() {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is needed for this function. Install it with\n",
         "  install.packages(\"INLA\", repos = c(getOption(\"repos\"), ",
         "INLA = \"https://inla.r-inla-download.org/R/stable\"), dep = TRUE)\n",
         "or use nowcasting_mgcv(), which does not need 'INLA'.",
         call. = FALSE)
  }
  if (!requireNamespace("sn", quietly = TRUE)) {
    stop("Package 'sn' is needed by 'INLA' to sample from the posterior. ",
         "Install it with install.packages(\"sn\").",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Check 'trim.data' (number of most recent weeks to drop) and tell the user
# when data is trimmed.
check_trim_data <- function(trim.data, silent = FALSE) {
  if (is.null(trim.data)) trim.data <- 0
  if (!is.numeric(trim.data) || length(trim.data) != 1 ||
      is.na(trim.data) || trim.data < 0) {
    stop("'trim.data' must be a single non-negative number of weeks.",
         call. = FALSE)
  }
  if (!silent && trim.data > 0) {
    message("Trimming out the last ", trim.data, " week(s) of data.")
  }
  trim.data
}

# Labels for the age classes defined by 'breaks', with intervals closed on the
# left as in cut(right = FALSE): c(0, 5, 12, 130) gives "0-4", "5-11", "12+".
# The last class is open-ended.
age_class_labels <- function(breaks) {
  n <- length(breaks)
  lower <- breaks[-n]
  upper <- breaks[-1]
  if (all(breaks == round(breaks))) {
    labels <- ifelse(upper - 1 == lower,
                     as.character(lower),
                     paste0(lower, "-", upper - 1))
  } else {
    labels <- paste0("[", lower, ", ", upper, ")")
  }
  labels[n - 1] <- paste0(lower[n - 1], "+")
  labels
}

# Number of basis functions for a smooth of 'x': mgcv's default of 10, or the
# number of distinct values when there are fewer. Smooths need at least 3.
gam_basis_dim <- function(x, what) {
  n <- length(unique(x))
  if (n < 3) {
    stop("Too few distinct values of ", what, " to fit the model (", n,
         "; at least 3 are needed).", call. = FALSE)
  }
  min(10L, n)
}
