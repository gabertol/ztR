# Funções internas (começam com ponto e não são exportadas pelo exportPattern).

# Lê uma tabela de referência de inst/extdata de forma robusta:
# - ignora BOM (UTF-8 com assinatura);
# - converte números escritos com vírgula de milhar ("1,080");
# - converte wt% para ppm quando houver coluna `unit`.
.read_ref <- function(database) {
  path <- system.file("extdata", database, package = "ztR")
  if (!nzchar(path)) path <- database            # permite caminho próprio do usuário
  ref <- utils::read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                         check.names = FALSE)
  num_cols <- intersect(c("value", "a", "b"), names(ref))
  for (cl in num_cols) ref[[cl]] <- as.numeric(gsub(",", "", as.character(ref[[cl]])))
  if ("unit" %in% names(ref)) {
    wt <- ref$unit %in% c("wt%", "wt.%", "%")
    ref$value[wt] <- ref$value[wt] * 1e4
    ref$unit[wt]  <- "ppm"
  }
  ref
}

# Aviso de depreciação que aparece uma vez por sessão.
.ztr_warn_once <- function(id, msg) {
  key <- paste0("ztR_warned_", id)
  if (!isTRUE(getOption(key))) {
    warning(msg, call. = FALSE)
    options(stats::setNames(list(TRUE), key))
  }
}
