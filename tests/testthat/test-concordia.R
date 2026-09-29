d <- tibble::tibble(x = c(1.8, 1.9, 1.85), y = c(0.178, 0.180, 0.179), sigma_x = 0.05,
                    sigma_y = 0.002, rho = 0.5, s = c("a", "a", "b"))

test_that("geom_concordia works without fill and with add_concordia_line()", {
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, sigma_x = sigma_x, sigma_y = sigma_y, rho = rho)) +
    geom_concordia() + add_concordia_line() + geom_concordia_line()
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("one polygon per row, even when rows share a fill", {
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, sigma_x = sigma_x, sigma_y = sigma_y, rho = rho,
                                       fill = s)) + geom_concordia(n = 50)
  b <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(length(unique(b$group)), 3)
  expect_equal(nrow(b), 150)
})

test_that("level scales the ellipse", {
  one <- ggplot2::ggplot_build(ggplot2::ggplot(d[1, ], ggplot2::aes(x, y, sigma_x = sigma_x,
          sigma_y = sigma_y, rho = rho)) + geom_concordia(level = NULL))$data[[1]]
  ci  <- ggplot2::ggplot_build(ggplot2::ggplot(d[1, ], ggplot2::aes(x, y, sigma_x = sigma_x,
          sigma_y = sigma_y, rho = rho)) + geom_concordia(level = 0.95))$data[[1]]
  expect_equal(diff(range(ci$x)) / diff(range(one$x)), sqrt(qchisq(0.95, 2)), tolerance = 1e-3)
  expect_equal(max(one$x) - 1.8, 0.05, tolerance = 1e-3)
})
