test_that("use_isoplotr reproduces the 91500 and Plesovice standards", {
  skip_if_not_installed("IsoplotR")
  s <- utils::read.csv(system.file("extdata", "stds_zircon.csv", package = "ztR"))
  names(s) <- gsub("_2se_int", "_2s", gsub("_mean|_ppm", "", names(s)))
  s <- s[s$sample %in% c("91500", "PL"), ]
  s <- do.call(rbind, lapply(split(s, s$sample), function(x) x[1:40, ]))
  r <- suppressWarnings(use_isoplotr(s))

  # 206/238 ages match Iolite, and so do their 2-sigma errors
  expect_equal(r$age_68, r$pb206_u238_age, tolerance = 0.01)
  expect_equal(r$age_68_2s, r$pb206_u238_age_2s, tolerance = 0.05)
  # legacy columns are 1-sigma
  expect_equal(r$s_2_68, r$age_68_2s / 2)
  # reference ages (Wiedenbeck et al. 1995; Slama et al. 2008)
  expect_equal(median(r$age_conc[r$sample == "91500"]), 1065, tolerance = 0.02)
  expect_equal(median(r$age_conc[r$sample == "PL"]), 337, tolerance = 0.02)
})

test_that("rows with bad ratios give NA instead of breaking the call", {
  skip_if_not_installed("IsoplotR")
  d <- tibble::tibble(pb207_u235 = c(1.87, -1), pb207_u235_2s = 0.1, pb206_u238 = c(0.179, 0.18),
                      pb206_u238_2s = 0.004, pb207_pb206 = c(0.075, 0.075), pb207_pb206_2s = 0.004)
  r <- suppressMessages(use_isoplotr(d, legacy = FALSE, concordia = FALSE))
  expect_false(is.na(r$age_68[1]))
  expect_true(is.na(r$age_68[2]))
})
