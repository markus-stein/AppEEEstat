#'  Cria tabela de frequências para variável de tipo nominal
#'
#'
#'  Cria tabela de frequência absoluta e frequência relativa com totais por coluna para uma variável de tipo nominal
#'
#'  @param y Vetor fator não-ordenado
#'
#'  @return Matriz
#'
#'  @examples
#'  aux <- mtcars$vs
#'  TabelaNominal(aux)
#'
#'  @export
TabelaNominal <- function(y){
  tabelanom   <-  Tabelaux(y)
  total <- colSums(tabelanom)
  tabelanom <- rbind(tabelanom, total)
  tabelanom <- round(tabelanom,3)
  return(tabelanom)
}
