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

test_that("complete epiweek ending on Saturday is retained by data.w helpers", {
  # 2022-03-05 is a Saturday, completing epiweek 2022-02-27 .. 2022-03-05
  d_sat <- sragBH[!is.na(sragBH$DT_DIGITA) &
                    sragBH$DT_DIGITA <= as.Date("2022-03-05"), ]
  expect_equal(max(d_sat$DT_DIGITA), as.Date("2022-03-05"))

  w_epi_noage <- suppressWarnings(
    data.w_no_age(d_sat, trim.data = 0,
                  date_onset = DT_SIN_PRI, date_report = DT_DIGITA,
                  use.epiweek = TRUE, silent = TRUE)
  )
  w_noepi_noage <- suppressWarnings(
    data.w_no_age(d_sat, trim.data = 0,
                  date_onset = DT_SIN_PRI, date_report = DT_DIGITA,
                  use.epiweek = FALSE, silent = TRUE)
  )
  w_epi_age <- suppressWarnings(
    data.w(d_sat, trim.data = 0, bins_age = "SI-PNI",
           date_onset = DT_SIN_PRI, date_report = DT_DIGITA,
           age_col = Idade, use.epiweek = TRUE, silent = TRUE)
  )
  w_noepi_age <- suppressWarnings(
    data.w(d_sat, trim.data = 0, bins_age = "SI-PNI",
           date_onset = DT_SIN_PRI, date_report = DT_DIGITA,
           age_col = Idade, use.epiweek = FALSE, silent = TRUE)
  )

  expect_equal(max(w_epi_noage$date_report), as.Date("2022-02-27"))
  expect_equal(max(w_noepi_noage$date_report), as.Date("2022-02-27"))
  expect_equal(max(w_epi_age$date_report), as.Date("2022-02-27"))
  expect_equal(max(w_noepi_age$date_report), as.Date("2022-02-27"))

  # Incomplete epiweek ending on Friday 2022-03-04 is still trimmed when
  # use.epiweek = TRUE, and kept when use.epiweek = FALSE
  d_fri <- sragBH[!is.na(sragBH$DT_DIGITA) &
                    sragBH$DT_DIGITA <= as.Date("2022-03-04"), ]
  w_epi_fri <- suppressWarnings(
    data.w_no_age(d_fri, trim.data = 0,
                  date_onset = DT_SIN_PRI, date_report = DT_DIGITA,
                  use.epiweek = TRUE, silent = TRUE)
  )
  w_noepi_fri <- suppressWarnings(
    data.w_no_age(d_fri, trim.data = 0,
                  date_onset = DT_SIN_PRI, date_report = DT_DIGITA,
                  use.epiweek = FALSE, silent = TRUE)
  )
  expect_equal(max(w_epi_fri$date_report), as.Date("2022-02-20"))
  expect_equal(max(w_noepi_fri$date_report), as.Date("2022-02-26"))
})
