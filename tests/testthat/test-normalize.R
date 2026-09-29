test_that("McDonough & Sun table reads without BOM and entirely in ppm", {
  ref <- ztR:::.read_ref("mcdon_sun_1995.csv")
  expect_true("element" %in% names(ref))
  expect_true(all(ref$unit == "ppm"))
  expect_equal(ref$value[ref$element == "P"], 1080)     # was "1,080" (text)
  expect_equal(ref$value[ref$element == "Al"], 8600)    # was 0.86 wt%
  expect_false(anyNA(ref$value))
})

test_that("normalize() works with the default database", {
  out <- normalize(tibble::tibble(la139 = 0.237, ce140 = 6.13, al27 = 86),
                   element_vector = c(la139, ce140, al27))
  expect_equal(out$la139_N, 1)
  expect_equal(out$ce140_N, 10)
  expect_equal(out$al27_N, 0.01)
})
