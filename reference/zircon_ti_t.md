# Ti-in-zircon crystallization temperature

Ti-in-zircon crystallization temperature

## Usage

``` r
zircon_ti_t(pressure = NULL, aSiO2 = 1, aTiO2 = 1, ti_ppm, equation = "watson")
```

## Arguments

- pressure:

  Pressure in GPa. Required only for `equation = "crisp"`.

- aSiO2:

  SiO2 activity (default 1). Used by `"ferry_watson2007"` and `"crisp"`.

- aTiO2:

  TiO2 activity (default 1). Used by `"ferry_watson2007"` and `"crisp"`.

- ti_ppm:

  Ti in zircon (ppm).

- equation:

  Calibration:

  - `"watson"` (default, kept for compatibility) =
    `"watson_harrison2005"`: log(Ti) = 6.01 - 5080/T (Watson & Harrison
    2005);

  - `"ferry_watson2007"`: log(Ti) = 5.711 - 4800/T - log(aSiO2) +
    log(aTiO2) (Ferry & Watson 2007; equals Watson et al. 2006 when both
    activities are 1);

  - `"crisp"`: pressure-dependent calibration of Crisp et al. (2023).
    The implementation has not yet been checked against the published
    equation; use with care.

## Value

Temperature in degrees Celsius. `NA` where `ti_ppm` is zero, negative or
missing.

## References

Watson, E.B. and Harrison, T.M., 2005. Zircon thermometer reveals
minimum melting conditions on earliest Earth. *Science*, 308, 841-844.

Ferry, J.M. and Watson, E.B., 2007. New thermodynamic models and revised
calibrations for the Ti-in-zircon and Zr-in-rutile thermometers.
*Contributions to Mineralogy and Petrology*, 154, 429-437.

Crisp, L.J., Berry, A.J., Burnham, A.D., Miller, L.A. and Newville, M.,
2023. The Ti-in-zircon thermometer revised: The effect of pressure on
the Ti site in zircon. *Geochimica et Cosmochimica Acta*, 360, 241-258.

## Examples

``` r
zircon_ti_t(ti_ppm = 10)
#> [1] 740.8221
zircon_ti_t(ti_ppm = 10, equation = "ferry_watson2007", aTiO2 = 0.6)
#> [1] 796.0944
zircon_ti_t(ti_ppm = 10, equation = "crisp", pressure = 0.5)
#> [1] 706.2121
```
