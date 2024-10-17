FMQ <- function(ce,u,ti,age,correct_u=TRUE) {

  ui<-u_corrector(u,age)

  FMQ<-3.998*log10(ce/sqrt(ui*ti))+2.284

  return(FMQ)

}
