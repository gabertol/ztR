test_that("Watson & Harrison 2005 (legacy 'watson')", {
  t <- zircon_ti_t(ti_ppm = 10)
  expect_equal(10^(6.01 - 5080 / (t + 273.15)), 10, tolerance = 1e-10)
  expect_equal(zircon_ti_t(ti_ppm = 10), zircon_ti_t(ti_ppm = 10, equation = "watson_harrison2005"))
})

test_that("Ferry & Watson 2007 inverts its own equation", {
  t <- zircon_ti_t(ti_ppm = 10, equation = "ferry_watson2007", aSiO2 = 1, aTiO2 = 0.6)
  expect_equal(10^(5.711 - 4800 / (t + 273.15) - log10(1) + log10(0.6)), 10, tolerance = 1e-10)
})

test_that("zircon_ti_p is vectorized and returns NA without a real root", {
  p <- zircon_ti_p(c(10, 400, 1000))
  expect_length(p, 3)
  expect_true(is.na(p[1]))
  expect_equal(0.013 * p[2]^2 - 0.21 * p[2] + 3.41, log10(400), tolerance = 1e-10)
})

test_that("non-positive Ti gives NA instead of -273 C", {
  expect_true(all(is.na(zircon_ti_t(ti_ppm = c(0, -1, NA)))))
  expect_true(all(is.na(zircon_ti_t(ti_ppm = c(0, -1), equation = "ferry_watson2007"))))
})

test_that("FMQ gives NA (no NaN warning) for non-positive inputs", {
  expect_no_warning(r <- FMQ(c(10, -1, 10), c(100, 100, 0), c(5, 5, 5), 300))
  expect_false(is.na(r[1]))
  expect_true(all(is.na(r[2:3])))
})
