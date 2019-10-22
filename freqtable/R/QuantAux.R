#' Criadora de intervalos de classe
#'
#' Cria intervalos de classe para vetores numericos
#'
#' @param y Vetor numérico
#'
#' @return Variável tipo fator
#'
#' @examples
#' var <- mtcars$wt
#' QuantAux(var)
#'
#' @export
QuantAux <- function(y){
  histy <- hist(as.numeric(y), plot=F)
  intervclasse <- cut(y, breaks = histy$breaks, include.lowest=TRUE)
  return(intervclasse)
}
