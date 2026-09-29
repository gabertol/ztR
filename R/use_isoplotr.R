#' Single-grain U-Pb ages with IsoplotR
#'
#' Computes 207Pb/235U, 206Pb/238U, 207Pb/206Pb and concordia ages, their 2-sigma uncertainties
#' and the concordia-distance discordance for every row, using `IsoplotR::age(type = 1)` on the
#' whole data frame at once.
#'
#' @param df Data frame with `pb207_u235`, `pb207_u235_2s`, `pb206_u238`, `pb206_u238_2s`,
#'   `pb207_pb206`, `pb207_pb206_2s` (2-sigma absolute) and, optionally, the error correlations
#'   `rho_206pb_238u_v_207pb_235u` (Wetherill) and `rho_207pb_206pb_v_238u_206pb`
#'   (Tera-Wasserburg). Missing correlations are inferred by IsoplotR from the redundancy of the
#'   three ratios.
#' @param age_type Only `1` (single-grain ages) is supported.
#' @param discordance IsoplotR discordance option: `"c"` (concordia distance, default), `"a"`,
#'   `"r"`, `"t"` or `"sk"`. See `IsoplotR::discfilter()`.
#' @param concordia If `TRUE` (default), also computes the single-grain concordia age, its
#'   p-value and the discordance. This is what makes IsoplotR slow (~30 ms per grain); with
#'   `FALSE` only the 7/5, 6/8 and 7/6 ages are computed (~20 times faster).
#' @param legacy If `TRUE` (default), also returns the columns of previous versions
#'   (`s_2_75`, `s_2_68`, `s_2_76`, `age_concordia`, `s_2_concordia`, `discordance_concordia`).
#'   **The `s_2_*` columns hold 1-sigma values**, as they always did (IsoplotR returns standard
#'   errors); a warning is issued once per session.
#'
#' @return `df` with the new columns `age_75`, `age_75_2s`, `age_68`, `age_68_2s`, `age_76`,
#'   `age_76_2s`, `age_conc`, `age_conc_2s`, `p_conc` and `disc_<option>` (plus the legacy columns
#'   when `legacy = TRUE`). Rows with missing ratios get `NA`.
#'
#' @details Changes in version 0.0.0.9001:
#' * the data are now read as IsoplotR format 3. Previous versions passed `type = 3` to
#'   `IsoplotR::read.data()`, whose argument is `format`, so the data were read as format 1 and
#'   the 207Pb/206Pb ratio was used as the error correlation. 206Pb/238U and 207Pb/235U ages were
#'   unaffected; 207Pb/206Pb ages, concordia ages and discordance changed;
#' * the Wetherill correlation is used as `rXY`, and the Tera-Wasserburg correlation (sign
#'   inverted) as `rYZ`;
#' * the whole table is processed in one call (much faster than row by row);
#' * works with IsoplotR >= 7 (extra `p[conc]` column).
#' @export
use_isoplotr <- function(df, age_type = 1, discordance = "c", concordia = TRUE, legacy = TRUE) {

  if (!requireNamespace("IsoplotR", quietly = TRUE)) {
    stop("Package 'IsoplotR' is required: install.packages('IsoplotR').", call. = FALSE)
  }
  if (!identical(as.numeric(age_type), 1)) {
    stop("Only age_type = 1 (single-grain ages) is supported.", call. = FALSE)
  }
  need <- c("pb207_u235", "pb207_u235_2s", "pb206_u238", "pb206_u238_2s",
            "pb207_pb206", "pb207_pb206_2s")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "), call. = FALSE)

  rxy <- if ("rho_206pb_238u_v_207pb_235u" %in% names(df)) df$rho_206pb_238u_v_207pb_235u else NA_real_
  ryz <- if ("rho_207pb_206pb_v_238u_206pb" %in% names(df)) -df$rho_207pb_206pb_v_238u_206pb else NA_real_
  X <- cbind(df$pb207_u235, df$pb207_u235_2s, df$pb206_u238, df$pb206_u238_2s,
             df$pb207_pb206, df$pb207_pb206_2s, rxy, ryz)
  X[, 7:8][abs(X[, 7:8]) >= 1] <- NA          # impossible correlations -> let IsoplotR infer
  ok <- stats::complete.cases(X[, 1:6]) & X[, 1] > 0 & X[, 3] > 0 & X[, 5] > 0 &
    X[, 2] > 0 & X[, 4] > 0 & X[, 6] > 0

  disc_name <- paste0("disc_", discordance)
  res <- matrix(NA_real_, nrow(df), 10,
                dimnames = list(NULL, c("age_75", "s_75", "age_68", "s_68", "age_76", "s_76",
                                        "age_conc", "s_conc", "disc", "p_conc")))

  run <- function(rows) {
    ud <- IsoplotR::read.data(X[rows, , drop = FALSE], method = "U-Pb", format = 3, ierr = 2)
    a <- if (concordia) {
      suppressWarnings(IsoplotR::age(ud, type = 1,
                                     discordance = IsoplotR::discfilter(option = discordance)))
    } else {
      suppressWarnings(IsoplotR::age(ud, type = 1, conc = FALSE))
    }
    a <- as.matrix(a)
    pick <- function(nm) if (nm %in% colnames(a)) a[, nm] else NA_real_
    cbind(pick("t.75"), pick("s[t.75]"), pick("t.68"), pick("s[t.68]"), pick("t.76"),
          pick("s[t.76]"), pick("t.conc"), pick("s[t.conc]"), pick("disc"), pick("p[conc]"))
  }

  idx <- which(ok)
  if (length(idx)) {
    # one call for everything; if IsoplotR fails, split in halves until the bad rows are isolated
    solve <- function(rows) {
      out <- tryCatch(run(rows), error = function(e) NULL)
      if (!is.null(out) && nrow(out) == length(rows)) return(out)
      if (length(rows) == 1) return(matrix(NA_real_, 1, 10))
      h <- seq_len(length(rows) %/% 2)
      rbind(solve(rows[h]), solve(rows[-h]))
    }
    res[idx, ] <- solve(idx)
  }
  if (any(!ok)) message(sum(!ok), " row(s) with missing or non-positive ratios/errors: ages set to NA.")

  new <- tibble::tibble(
    age_75 = res[, "age_75"], age_75_2s = 2 * res[, "s_75"],
    age_68 = res[, "age_68"], age_68_2s = 2 * res[, "s_68"],
    age_76 = res[, "age_76"], age_76_2s = 2 * res[, "s_76"],
    age_conc = res[, "age_conc"], age_conc_2s = 2 * res[, "s_conc"],
    p_conc = res[, "p_conc"]
  )
  new[[disc_name]] <- res[, "disc"]

  if (legacy) {
    .ztr_warn_once("isoplotr_legacy", paste(
      "use_isoplotr(): the legacy columns `s_2_75`, `s_2_68`, `s_2_76` and `s_2_concordia` hold",
      "1-sigma uncertainties (IsoplotR standard errors), not 2-sigma. Use the new `*_2s` columns;",
      "set `legacy = FALSE` to drop the legacy ones."))
    new <- dplyr::mutate(new,
      s_2_75 = res[, "s_75"], s_2_68 = res[, "s_68"], s_2_76 = res[, "s_76"],
      age_concordia = res[, "age_conc"], s_2_concordia = res[, "s_conc"],
      discordance_concordia = res[, "disc"])
  }

  dplyr::bind_cols(df[, setdiff(names(df), names(new)), drop = FALSE], new)
}

#' @rdname use_isoplotr
#' @details `tidy_isoplotr()` is kept for compatibility and now simply calls `use_isoplotr()` on the
#'   whole data frame.
#' @export
tidy_isoplotr <- function(df, age_type = 1, discordance = "c", concordia = TRUE, legacy = TRUE) {
  use_isoplotr(df, age_type = age_type, discordance = discordance, concordia = concordia,
               legacy = legacy)
}
