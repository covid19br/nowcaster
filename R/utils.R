# Stop with an informative message when 'INLA' is not installed.
# 'INLA' is not on CRAN, so it is only suggested.
check_inla <- function() {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is needed for this function. Install it with\n",
         "  install.packages(\"INLA\", repos = c(getOption(\"repos\"), ",
         "INLA = \"https://inla.r-inla-download.org/R/stable\"), dep = TRUE)\n",
         "or use nowcasting_mgcv(), which does not need 'INLA'.",
         call. = FALSE)
  }
  invisible(TRUE)
}
