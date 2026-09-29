zr <- function() {
  n <- normalize(tibble::tibble(
    la139 = 0.05, ce140 = 20, pr141 = 0.1, nd146 = 1.5, sm147 = 2.5, eu153 = 0.8, gd157 = 15,
    tb159 = 5, dy163 = 60, ho165 = 22, er166 = 110, tm169 = 25, yb172 = 250, lu175 = 50,
    y89 = 1200, hf177 = 9000, ti49 = 8, approx_u = 300, best_age = 300),
    element_vector = c(la139:lu175))
  n
}

test_that("FMQ uses raw Ce in ppm", {
  d <- zr()
  r <- suppressWarnings(ratio_calculator(d))
  expect_equal(r$fmq, FMQ(20, 300, 8, 300))
})

test_that("legacy = TRUE keeps old definitions, legacy = FALSE names what it computes", {
  d <- zr()
  old <- suppressWarnings(ratio_calculator(d, legacy = TRUE))
  new <- ratio_calculator(d, legacy = FALSE)
  expect_equal(old$dy_nd, d$dy163_N / d$yb172_N)
  expect_equal(old$yb_gd, d$y89 / d$gd157_N)
  expect_equal(new$dy_yb, d$dy163_N / d$yb172_N)
  expect_equal(new$yb_gd, d$yb172_N / d$gd157_N)
  expect_false("dy_nd" %in% names(new))
})

test_that("crisp needs pressure; ratio_calculator passes it", {
  expect_error(zircon_ti_t(ti_ppm = 10, equation = "crisp"), "pressure")
  d <- zr()
  expect_no_error(ratio_calculator(d, equation = "crisp", pressure = 0.5, legacy = FALSE))
})
