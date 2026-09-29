test_that("values exactly at a threshold are classified", {
  d <- tibble::tibble(lu175 = 20.7, approx_u = 38, ta181 = 0.58, hf = 0.8, ce_ce = 1,
                      nb93 = 100, th_u = 0.5)
  expect_false(classify_rock_long(d)$classification == "Unclassified")
})

test_that("NA gives Unclassified", {
  d <- tibble::tibble(lu175 = NA_real_, approx_u = 38, ta181 = 0.5, hf = 1, ce_ce = 1,
                      nb93 = 1, th_u = 0.5)
  expect_equal(classify_rock_long(d)$classification, "Unclassified")
})
