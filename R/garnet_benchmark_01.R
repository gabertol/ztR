#' garnet_benchmark_01
#'
#' @return a database of a single XXX variety of garnet with the specimen(single mineral), sample, element and value (weight%).
#' @export
#'
#' @examples
#' garnet_benchmark_01()

garnet_benchmark_01 <- function() {
  tibble::tribble(~specimen,~sample,~element,~value,
                  1,"ex1","SiO2",37.39,
                  1,"ex1","TiO2",0.28,
                  1,"ex1","Al2O3",10.08,
                  1,"ex1","Cr2O3",0.00,
                  1,"ex1","Fe2O3",0.00,
                  1,"ex1","FeO",19.35,
                  1,"ex1","MnO",0.14,
                  1,"ex1","MgO",1.55,
                  1,"ex1","CaO",29.63) |>
    tidyr::pivot_wider(names_from="element",
                       values_from="value")
}
