#' Zircon trace-element ratios and proxies
#'
#' Calculates common zircon ratios and proxies from chondrite-normalized REE (`*_N` columns, as
#' produced by [normalize()]) and raw concentrations in ppm (`ce140`, `approx_u`, `ti49`, `hf177`,
#' `y89`).
#'
#' @param dataframe Data frame with `*_N` columns and raw `ce140`, `approx_u`, `ti49`, `hf177`,
#'   `y89` and `best_age` (Ma).
#' @param equation Ti-in-zircon calibration passed to [zircon_ti_t()].
#' @param pressure Pressure in GPa passed to [zircon_ti_t()] (needed only for `"crisp"`).
#' @param legacy If `TRUE` (default), returns the column names of previous versions, keeping their
#'   old definitions (see Details) and warning once per session. If `FALSE`, returns columns named
#'   after what they compute.
#' @param ... Further arguments passed to [zircon_ti_t()] (e.g. `aSiO2`, `aTiO2`).
#'
#' @details
#' Column definitions with `legacy = TRUE` (kept for backwards compatibility):
#' * `dy_nd` = Dy_N / Yb_N (despite the name, this is Dy/Yb);
#' * `yb_gd` = Y (ppm) / Gd_N (despite the name, this is Y/Gd, mixing raw and normalized values);
#' * `H_L_ree` = (La..Sm)_N / (Eu, Gd, Tb, Ho..Lu)_N, i.e. LREE/HREE, without Dy.
#'
#' Column definitions with `legacy = FALSE`:
#' * `dy_yb` = Dy_N / Yb_N;
#' * `yb_gd` = Yb_N / Gd_N;
#' * `lree_hree` = (La..Sm)_N / (Gd..Lu)_N, Dy included.
#'
#' In both modes `fmq` is computed with raw Ce, U and Ti in ppm (Loucks et al. 2020), and `ti_temp`
#' receives `pressure`. Before version 0.0.0.9001 `fmq` was computed with chondrite-normalized Ce,
#' which was wrong.
#' @return `dataframe` with the ratio columns added.
#' @export
ratio_calculator <- function(dataframe, equation = "watson", pressure = NULL, legacy = TRUE, ...) {

  out <- dataframe %>%
    dplyr::mutate(
      ce_nd  = ce140_N / nd146_N,
      hf_y   = hf177 / y89,
      sumREE = la139_N + ce140_N + pr141_N + nd146_N + sm147_N + eu153_N + gd157_N + tb159_N +
               dy163_N + ho165_N + er166_N + tm169_N + yb172_N + lu175_N,
      eu_eu  = anomaly(eu153_N, sm147_N, gd157_N),
      ce_ce  = anomaly(ce140_N, la139_N, pr141_N),
      crust  = crustal_thickness(eu_eu),
      fmq    = FMQ(ce140, approx_u, ti49, best_age),
      ti_temp = zircon_ti_t(pressure = pressure, ti_ppm = ti49, equation = equation, ...)
    )

  if (legacy) {
    .ztr_warn_once("ratio_legacy", paste(
      "ratio_calculator(): `dy_nd` is Dy/Yb, `yb_gd` is Y/Gd and `H_L_ree` is LREE/HREE without Dy.",
      "These legacy definitions are kept for compatibility; use `legacy = FALSE` for correctly",
      "named columns (`dy_yb`, `yb_gd`, `lree_hree`)."))
    out %>%
      dplyr::mutate(
        dy_nd   = dy163_N / yb172_N,
        yb_gd   = y89 / gd157_N,
        H_L_ree = (la139_N + ce140_N + pr141_N + nd146_N + sm147_N) /
                  (eu153_N + gd157_N + tb159_N + ho165_N + er166_N + tm169_N + yb172_N + lu175_N),
        .after = ce_nd)
  } else {
    out %>%
      dplyr::mutate(
        dy_yb     = dy163_N / yb172_N,
        yb_gd     = yb172_N / gd157_N,
        lree_hree = (la139_N + ce140_N + pr141_N + nd146_N + sm147_N) /
                    (gd157_N + tb159_N + dy163_N + ho165_N + er166_N + tm169_N + yb172_N + lu175_N),
        .after = ce_nd)
  }
}
