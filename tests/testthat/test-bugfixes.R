# Tests for bugs fixed in the 1.1.0 development cycle.
srag_small <- sragBH[sragBH$DT_DIGITA <= as.Date("2020-07-04"), ]

nowcast_quiet <- function(..., wdw = 20) {
  suppressMessages(suppressWarnings(
    nowcasting_mgcv(dataset = srag_small, date_onset = DT_SIN_PRI,
                    date_report = DT_DIGITA, wdw = wdw, ...)
  ))
}

test_that("trim.data drops the most recent weeks, also with silent = TRUE", {
  set.seed(1); full <- nowcast_quiet()
  set.seed(1); trimmed <- nowcast_quiet(trim.data = 2)
  set.seed(1); trimmed_silent <- nowcast_quiet(trim.data = 2, silent = TRUE)
  expect_equal(max(trimmed$total$dt_event), max(full$total$dt_event) - 14)
  expect_equal(trimmed_silent, trimmed)
})

test_that("trim.data is reported only when data is trimmed", {
  expect_no_message(
    check_trim_data(0)
  )
  expect_message(check_trim_data(2), "Trimming out the last 2 week")
  expect_no_message(check_trim_data(2, silent = TRUE))
  expect_error(check_trim_data(-1), "non-negative")
  expect_error(check_trim_data("2"), "non-negative")
})

test_that("nowcasting_mgcv() works with Dmax below 9", {
  set.seed(1)
  out <- nowcast_quiet(Dmax = 5)
  expect_true(all(c("dt_event", "Median", "LI", "LS") %in% names(out$total)))
  expect_true(all(is.finite(out$total$Median)))
})

test_that("too short windows give an informative error", {
  expect_error(nowcast_quiet(wdw = 1), "increase 'wdw'")
  expect_error(nowcast_quiet(Dmax = 1), "increase 'Dmax'")
})

test_that("age classes have readable labels", {
  expect_equal(age_class_labels(c(0, 20, 60, 130)), c("0-19", "20-59", "60+"))
  expect_equal(age_class_labels(c(seq(0, 90, by = 10), 130))[c(1, 10)],
               c("0-9", "90+"))
  expect_equal(age_class_labels(c(0, 1, 5, 130)), c("0", "1-4", "5+"))

  set.seed(1)
  out <- nowcast_quiet(age_col = Idade, bins_age = c(0, 20, 60, 130),
                       fit = "bam_discrete")
  expect_equal(levels(out$age$fx_etaria), c("0-19", "20-59", "60+"))
  expect_equal(sort(unique(out$age$fx_etaria.num)), 1:3)
})

test_that("unknown age bins give an error", {
  expect_error(nowcast_quiet(age_col = Idade, bins_age = "3 years"),
               "Age bins are not options")
})

test_that("nowcasting_inla() handles trim.data", {
  skip_if_not_installed("INLA")
  skip_if_not_installed("sn")
  set.seed(1)
  out <- suppressMessages(suppressWarnings(
    nowcasting_inla(dataset = srag_small, date_onset = DT_SIN_PRI,
                    date_report = DT_DIGITA, wdw = 20, trim.data = 2)
  ))
  expect_true(all(c("dt_event", "Median", "LI", "LS") %in% names(out$total)))
})
