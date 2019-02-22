#'  Cria tabela de frequências para uma variável de tipo quantitativa
#'
#'  Cria tabela de frequência absoluta,frequência relativa, frequência absoluta acumulada e frequência relativa acumulada com totais por coluna para uma variável de tipo quantitativa
#'
#'  @param y Vetor numérico
#'
#'  @return Matriz
#'
#'  @examples
#'  aux <- mtcars$wt
#'  TabelaQuantitativa(aux)
#'
#'  @export
TabelaQuantitativa <- function(y){
  tabelaquanti <- TabelaOrdinal(QuantAux(y))
  return(tabelaquanti)
}
