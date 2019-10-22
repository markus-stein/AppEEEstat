#'  Cria tabela de frequências para uma variável de tipo ordinal
#'
#'
#'  Cria tabela de frequência absoluta e frequência relativa com totais por coluna para uma variável de tipo ordinal
#'
#'  @param y Vetor fator ordenado
#'
#'  @return Matriz
#'
#'  @examples
#'  aux <- mtcars$cyl
#'  TabelaOrdinal(aux)
#'
#'  @export
TabelaOrdinal <- function(y){
  tabelaux <- Tabelaux(y)
  tabelaord <- cbind(tabelaux, apply(tabelaux, 2, cumsum))
  tabelaord <- round(tabelaord, 3)
  total <- c(colSums(tabelaux), "-", "-")
  tabelaord <- rbind(tabelaord, total)
  colnames(tabelaord)  <-  c("f", "fr", "F", "Fr")
  return(tabelaord)
}
