test_that("setup-INLA", {
  if (requireNamespace("INLA", quietly = TRUE)) {
    INLA::inla.setOption(fmesher.evolution = 2L)
    INLA::inla.setOption(fmesher.evolution.warn = TRUE)
    INLA::inla.setOption(fmesher.evolution.verbosity = "warn")
  }
})
