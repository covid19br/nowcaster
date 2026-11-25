# The workhorse functions should only print messages that report a choice made
# for the user (age classes, trimming), never warnings about defaults.
srag_small <- sragBH[sragBH$DT_DIGITA <= as.Date("2020-07-04"), ]

test_that("a default nowcasting_mgcv() call is silent", {
  set.seed(1)
  expect_silent(
    nowcasting_mgcv(dataset = srag_small, date_onset = DT_SIN_PRI,
                    date_report = DT_DIGITA, wdw = 20)
  )
})

test_that("the age model only reports the age classes", {
  set.seed(1)
  expect_no_warning(
    expect_message(
      nowcasting_mgcv(dataset = srag_small, date_onset = DT_SIN_PRI,
                      date_report = DT_DIGITA, wdw = 20, age_col = Idade,
                      bins_age = c(0, 20, 60, 130), fit = "bam_discrete"),
      "Age classes: 0-19, 20-59, 60\\+"
    )
  )
})

test_that("slope.estimate.quant() is silent with its default end.week", {
  skip_if_not_installed("lme4")
  set.seed(1)
  now <- nowcasting_mgcv(dataset = srag_small, date_onset = DT_SIN_PRI,
                         date_report = DT_DIGITA, wdw = 20,
                         trajectories = TRUE)
  expect_silent(slope.estimate.quant(trajectories = now$trajectories))
})

test_that("nowcasting_mgcv() checks its inputs", {
  expect_error(nowcasting_mgcv(), "Dataset is missing")
  expect_error(nowcasting_mgcv(dataset = srag_small, date_onset = DT_SIN_PRI),
               "missing")
  expect_error(nowcasting_mgcv(dataset = srag_small, date_onset = DT_SIN_PRI,
                               date_report = DT_DIGITA, K = -1),
               "K less than 0")
  expect_error(nowcasting_mgcv(dataset = srag_small, date_onset = DT_SIN_PRI,
                               date_report = DT_DIGITA,
                               method.group.gam = "xx"),
               "should be one of")
})
