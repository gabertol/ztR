anomaly <- function(REE,LREE,HREE) {
  return(REE/sqrt(LREE-HREE))

}
