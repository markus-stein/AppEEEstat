#'  Cria tabela de frequências para duas variáveis
#'
#'  Cria tabela de frequência absoluta para arg=1,frequência relativa para arg=2, proporção por linha para arg=3 e proporção por coluna para arg=4
#'
#'  @param y Vetor fator ou vetor numérico
#'
#'  @return Matriz
#'
#'  @examples
#'  TabelaBivariada(mtcars$am, mtcars$carb, 4)
#'  TabelaBivariada(mtcars$hp, mtcars$am, 2)
#'  TabelaBivariada(mtcars$hp, mtcars$mpg)
#'
#'  @export
TabelaBivariada <- function(w, z, arg = 1){

  if (!is.factor(w)) w <- QuantAux(w)
  if (!is.factor(z)) z <- QuantAux(z)

  tabela <- table(w, z)

  if(arg == 1){
    tabela <- addmargins(tabela)
  } else if(arg == 2){
    tabela <- prop.table(tabela)
    tabela <- addmargins(tabela)
  } else if(arg == 3){
    tabela <- prop.table(tabela, 1)
    tabela <- addmargins(tabela, 2)
  } else if(arg == 4){
    tabela <- prop.table(tabela, 2)
    tabela <- addmargins(tabela, 1)
  } else{
    stop("'arg' deve ser um valor inteiro de 1 a 4!!!")
  }

  return(tabela)
}
