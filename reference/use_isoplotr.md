# Single-grain U-Pb ages with IsoplotR

Computes 207Pb/235U, 206Pb/238U, 207Pb/206Pb and concordia ages, their
2-sigma uncertainties and the concordia-distance discordance for every
row, using `IsoplotR::age(type = 1)` on the whole data frame at once.

## Usage

``` r
use_isoplotr(
  df,
  age_type = 1,
  discordance = "c",
  concordia = TRUE,
  legacy = TRUE
)

tidy_isoplotr(
  df,
  age_type = 1,
  discordance = "c",
  concordia = TRUE,
  legacy = TRUE
)
```

## Arguments

- df:

  Data frame with `pb207_u235`, `pb207_u235_2s`, `pb206_u238`,
  `pb206_u238_2s`, `pb207_pb206`, `pb207_pb206_2s` (2-sigma absolute)
  and, optionally, the error correlations `rho_206pb_238u_v_207pb_235u`
  (Wetherill) and `rho_207pb_206pb_v_238u_206pb` (Tera-Wasserburg).
  Missing correlations are inferred by IsoplotR from the redundancy of
  the three ratios.

- age_type:

  Only `1` (single-grain ages) is supported.

- discordance:

  IsoplotR discordance option: `"c"` (concordia distance, default),
  `"a"`, `"r"`, `"t"` or `"sk"`. See
  [`IsoplotR::discfilter()`](https://rdrr.io/pkg/IsoplotR/man/discfilter.html).

- concordia:

  If `TRUE` (default), also computes the single-grain concordia age, its
  p-value and the discordance. This is what makes IsoplotR slow (~30 ms
  per grain); with `FALSE` only the 7/5, 6/8 and 7/6 ages are computed
  (~20 times faster).

- legacy:

  If `TRUE` (default), also returns the columns of previous versions
  (`s_2_75`, `s_2_68`, `s_2_76`, `age_concordia`, `s_2_concordia`,
  `discordance_concordia`). **The `s_2_*` columns hold 1-sigma values**,
  as they always did (IsoplotR returns standard errors); a warning is
  issued once per session.

## Value

`df` with the new columns `age_75`, `age_75_2s`, `age_68`, `age_68_2s`,
`age_76`, `age_76_2s`, `age_conc`, `age_conc_2s`, `p_conc` and
`disc_<option>` (plus the legacy columns when `legacy = TRUE`). Rows
with missing ratios get `NA`.

## Details

Changes in version 0.0.0.9001:

- the data are now read as IsoplotR format 3. Previous versions passed
  `type = 3` to
  [`IsoplotR::read.data()`](https://rdrr.io/pkg/IsoplotR/man/read.data.html),
  whose argument is `format`, so the data were read as format 1 and the
  207Pb/206Pb ratio was used as the error correlation. 206Pb/238U and
  207Pb/235U ages were unaffected; 207Pb/206Pb ages, concordia ages and
  discordance changed;

- the Wetherill correlation is used as `rXY`, and the Tera-Wasserburg
  correlation (sign inverted) as `rYZ`;

- the whole table is processed in one call (much faster than row by
  row);

- works with IsoplotR \>= 7 (extra `p[conc]` column).

`tidy_isoplotr()` is kept for compatibility and now simply calls
`use_isoplotr()` on the whole data frame.
