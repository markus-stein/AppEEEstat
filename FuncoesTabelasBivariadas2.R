###################################### funções auxiliares ############################

#### Criadora de intervalos de classes ####
QuantAux <- function(y){
  histy <- hist(as.numeric(y), plot=F)
  intervclasse <- cut(y, breaks = histy$breaks, include.lowest=TRUE)
  return(intervclasse)
}

### Cria tabela bivariada
TabelaBivariada <- function(w, z, arg = 1){   # by default arg=1
  
    # transforma variaveis quanti em intervalos
    if (!is.factor(w)) w <- QuantAux(w)
    if (!is.factor(z)) z <- QuantAux(z)
    
    # cria tabela
    tabela <- table(w, z)             
    
    # monta a saida de acordo com 'arg'
    if(arg == 1){ 
      tabela <- addmargins(tabela)    # frequencia absoluta
    } else if(arg == 2){
      tabela <- prop.table(tabela)    # frequencia relativa
      tabela <- addmargins(tabela)    # add margin
    } else if(arg == 3){ 
      tabela <- prop.table(tabela, 1) # proporcao por linha
      tabela <- addmargins(tabela, 2) # add margin
    } else if(arg == 4){
      tabela <- prop.table(tabela, 2) # proporcao por coluna
      tabela <- addmargins(tabela, 1) # add margin
    } else{
      stop("'arg' deve ser um valor inteiro de 1 a 4!!!")
    }
    
    return(tabela)
}

# not working
# library(RCurl)
# mtcarsNEW <- getURL("https://raw.githubusercontent.com/markus-stein/AppEEEstat/master/mtcarsdeclarado.R", ssl.verifypeer = FALSE)
# eval(parse(text = mtcarsNEW))

# Example
# source "mtcarsdeclarado.R" before
TabelaBivariada(mtcars$am, mtcars$carb)
TabelaBivariada(mtcars$am, mtcars$carb, 1)  # same as default
TabelaBivariada(mtcars$am, mtcars$carb, 2)
TabelaBivariada(mtcars$am, mtcars$carb, 3)
TabelaBivariada(mtcars$am, mtcars$carb, 4)


TabelaBivariada(mtcars$am, mtcars$mpg)
TabelaBivariada(mtcars$am, mtcars$mpg, 2)
TabelaBivariada(mtcars$am, mtcars$mpg, 3)
TabelaBivariada(mtcars$am, mtcars$mpg, 4)

TabelaBivariada(mtcars$hp, mtcars$am)
TabelaBivariada(mtcars$hp, mtcars$am, 2)
TabelaBivariada(mtcars$hp, mtcars$am, 3)
TabelaBivariada(mtcars$hp, mtcars$am, 4)

TabelaBivariada(mtcars$hp, mtcars$mpg)
TabelaBivariada(mtcars$hp, mtcars$mpg, 2)
TabelaBivariada(mtcars$hp, mtcars$mpg, 3)
TabelaBivariada(mtcars$hp, mtcars$mpg, 4)

