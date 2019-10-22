#'  Cria tabela de frequências para uma variável de tipo qualitativa ou quantitativa
#'
#'
#'  Cria tabela de frequências de acordo com o tipo de variável testada
#'
#'  @param y Vetor fator não-ordenado, fator ordenado ou numérico
#'
#'  @return Matriz
#'
#'  @examples
#'  aux <- mtcars$wt
#'  TabelaFrequenciasimples(aux)
#'  var <- mtcars$cyl
#'  TabelaFrequenciasimples(var)
#'
#'  @export
TabelaFrequenciaSimples <- function(y){
  if (is.factor(y) & !is.ordered(y)){
    TabelaNominal(y)
  } else {
    if (is.factor(y) & is.ordered(y)){
      TabelaOrdinal(y)
    } else {
      if (nrow(table(y)) <= 15){
        TabelaOrdinal(y)
      } else {
        TabelaQuantitativa(y)}
    }
  }
}
