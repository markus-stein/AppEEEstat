#'  Cria tabela de frequências
#'
#'
#'  Cria tabela defrequência absoluta e frequência relativa
#'
#'  @param y Vetor fator
#'
#'  @return Matriz
#'
#'  @examples
#'  aux <- mtcars$vs
#'  Tabelaux(aux)
#'
#'  @export
Tabelaux <- function(y){
  tabelaux <- table(y)
  tabelaux <- cbind(tabelaux, tabelaux/length(y))
  colnames(tabelaux) <- c("Freq", "Prop")
  return(tabelaux)
}
