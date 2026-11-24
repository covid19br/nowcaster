# Small subset of the example data to keep the tests fast
srag_small <- sragBH[sragBH$DT_DIGITA <= as.Date("2020-07-04"), ]

test_that("nowcasting_mgcv() returns the documented structure", {
  set.seed(1)
  # nowcasting_mgcv() warns about default arguments by design
  out <- suppressWarnings(
    nowcasting_mgcv(dataset = srag_small,
                    date_onset = DT_SIN_PRI,
                    date_report = DT_DIGITA,
                    wdw = 20,
                    data.by.week = TRUE)
  )
  expect_type(out, "list")
  expect_true(all(c("total", "data") %in% names(out)))
  expect_true(all(c("dt_event", "Median", "LI", "LS", "LIb", "LSb") %in%
                    names(out$total)))
  expect_true(all(out$total$LI <= out$total$Median))
  expect_true(all(out$total$Median <= out$total$LS))
  expect_true(all(c("dt_event", "delay", "Y") %in% names(out$data)))
})

test_that("nowcasting_inla() validates its inputs before fitting", {
  expect_error(nowcasting_inla(), "Dataset is missing")
  expect_error(nowcasting_inla(dataset = srag_small, date_onset = DT_SIN_PRI),
               "missing")
  expect_error(nowcasting_inla(dataset = srag_small,
                               date_onset = DT_SIN_PRI,
                               date_report = DT_DIGITA, K = -1),
               "K less than 0")
})

test_that("old INLA function names are deprecated aliases", {
  expect_true(is.function(nowcasting_age_inla))
  expect_true(is.function(nowcasting_no_age_inla))
  # The deprecation warning comes before 'INLA' is needed
  expect_warning(try(nowcasting_no_age(), silent = TRUE), "deprecated")
  expect_warning(try(nowcasting_age(), silent = TRUE), "deprecated")
})
